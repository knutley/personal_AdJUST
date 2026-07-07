# Author: Katie Nutley  
# Date: 2026-05-07
# Title: Stage 1 Fine-Tuning on Meckling and Allan Data

"""
finetune.py  —  Supervised Classification Fine-tuning
Stage 1: fine-tune on Meckling & Allan data (3 or 4 classes)
Stage 2: fine-tune Stage 1 checkpoint on RA-annotated EU pre-legislative data

Improvements over previous version:
  - label_mapping.json accepts both flat {"Admin-Only": 0} and nested
    {"label2id": {...}, "id2label": {...}} formats
  - warmup_ratio replaced with warmup_steps (no deprecation warning)
  - Two-step fine-tuning via --freeze_epochs / --freeze_lr:
      Phase 1: encoder frozen, classifier head trained at high LR
      Phase 2: full model unfrozen, fine-tuned at lower LR
  - Default eval batch size lowered to 16 (DeBERTa OOM fix)

Usage — Stage 1:
    python finetune.py \
        --model_checkpoint climatebert/distilroberta-base-climate-f \
        --data_dir ./stage1_data \
        --output_dir ./checkpoints/climatebert-stage1-v2 \
        --num_train_epochs 7 \
        --learning_rate 1e-5 \
        --patience 4 \
        --fp16

Usage — Stage 2 (with two-step fine-tuning):
    python finetune.py \
        --model_checkpoint ./checkpoints/climatebert-stage1-v2/best_model \
        --data_dir ./stage1_data \
        --ra_data_dir ./ra_data_aug \
        --label_map ./ra_data_aug/label_mapping.json \
        --output_dir ./checkpoints/climatebert-stage2-v2 \
        --num_train_epochs 7 \
        --learning_rate 1e-5 \
        --freeze_epochs 3 \
        --freeze_lr 1e-3 \
        --patience 4 \
        --fp16
"""

import argparse
import json
import logging
import math
from pathlib import Path

import numpy as np
import pandas as pd
import torch
from datasets import Dataset, DatasetDict
from sklearn.metrics import accuracy_score, classification_report, f1_score
from transformers import (
    AutoModelForSequenceClassification,
    AutoTokenizer,
    DataCollatorWithPadding,
    EarlyStoppingCallback,
    Trainer,
    TrainingArguments,
    set_seed,
)

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# ── Metrics ───────────────────────────────────────────────────────────────────

def compute_metrics(eval_pred):
    logits, labels = eval_pred
    predictions = np.argmax(logits, axis=-1)
    return {
        "accuracy": accuracy_score(labels, predictions),
        "macro_f1": f1_score(labels, predictions, average="macro", zero_division=0),
        "micro_f1": f1_score(labels, predictions, average="micro", zero_division=0),
    }


# ── Label mapping ─────────────────────────────────────────────────────────────

def load_label_mapping(path: Path):
    """
    Accepts both formats:
      Flat:   {"Admin-Only": 0, "Green_Growth": 1, ...}
      Nested: {"label2id": {...}, "id2label": {...}}
    Returns (label2id, id2label).
    """
    with open(path) as f:
        mapping = json.load(f)

    if "label2id" in mapping:
        label2id = {k: int(v) for k, v in mapping["label2id"].items()}
        id2label = {int(k): v for k, v in mapping["id2label"].items()}
    else:
        # Flat format
        label2id = {k: int(v) for k, v in mapping.items()}
        id2label = {int(v): k for k, v in mapping.items()}

    return label2id, id2label


# ── Data loading ──────────────────────────────────────────────────────────────

def load_splits(data_dir: Path, ra_data_dir: Path | None) -> DatasetDict:
    source = ra_data_dir if ra_data_dir else data_dir
    stage  = "Stage 2 (RA)" if ra_data_dir else "Stage 1 (Meckling)"
    logger.info(f"{stage}: loading splits from {source}")

    train_df = pd.read_csv(source / "clf_train.csv")
    val_df   = pd.read_csv(source / "clf_val.csv")
    test_df  = pd.read_csv(source / "clf_test.csv")

    logger.info(f"Train: {len(train_df)}  Val: {len(val_df)}  Test: {len(test_df)}")

    return DatasetDict({
        "train": Dataset.from_pandas(train_df[["text", "label"]]),
        "val":   Dataset.from_pandas(val_df[["text",  "label"]]),
        "test":  Dataset.from_pandas(test_df[["text", "label"]]),
    })


def tokenize_dataset(datasets: DatasetDict, tokenizer, max_length: int = 512) -> DatasetDict:
    def tokenize(batch):
        return tokenizer(
            batch["text"],
            truncation=True,
            max_length=max_length,
            padding=False,
        )
    return datasets.map(tokenize, batched=True, remove_columns=["text"])


# ── Weighted loss trainer ─────────────────────────────────────────────────────

def make_weighted_trainer(class_weights):
    class WeightedTrainer(Trainer):
        def compute_loss(self, model, inputs, return_outputs=False, **kwargs):
            labels  = inputs.pop("labels")
            outputs = model(**inputs)
            loss    = torch.nn.CrossEntropyLoss(
                weight=class_weights.to(
                    device=outputs.logits.device,
                    dtype=outputs.logits.dtype,
                )
            )(outputs.logits, labels)
            return (loss, outputs) if return_outputs else loss
    return WeightedTrainer


# ── Two-step fine-tuning helpers ──────────────────────────────────────────────

def freeze_encoder(model):
    """Freeze all parameters except classifier and pooler heads."""
    for name, param in model.named_parameters():
        if any(k in name for k in ["classifier", "pooler"]):
            param.requires_grad = True
        else:
            param.requires_grad = False
    trainable = sum(p.numel() for p in model.parameters() if p.requires_grad)
    logger.info(f"Phase 1 — encoder frozen. Trainable params: {trainable:,}")


def unfreeze_all(model):
    """Unfreeze all parameters."""
    for param in model.parameters():
        param.requires_grad = True
    trainable = sum(p.numel() for p in model.parameters() if p.requires_grad)
    logger.info(f"Phase 2 — full model unfrozen. Trainable params: {trainable:,}")


# ── Main ──────────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--model_checkpoint", required=True)
    parser.add_argument("--data_dir",         default="./data")
    parser.add_argument("--ra_data_dir",      default=None)
    parser.add_argument("--label_map",        default=None)
    parser.add_argument("--output_dir",       required=True)
    parser.add_argument("--max_length",       type=int,   default=512)
    parser.add_argument("--num_train_epochs", type=int,   default=5)
    parser.add_argument("--per_device_train_batch_size", type=int,   default=16)
    parser.add_argument("--per_device_eval_batch_size",  type=int,   default=16)
    parser.add_argument("--learning_rate",    type=float, default=2e-5)
    parser.add_argument("--weight_decay",     type=float, default=0.01)
    parser.add_argument("--patience",         type=int,   default=3)
    parser.add_argument("--freeze_epochs",    type=int,   default=0,
                        help="Epochs to train with encoder frozen (Phase 1). "
                             "0 = skip two-step and go straight to full fine-tuning.")
    parser.add_argument("--freeze_lr",        type=float, default=1e-3,
                        help="Learning rate for Phase 1 (frozen encoder).")
    parser.add_argument("--fp16",             action="store_true")
    parser.add_argument("--seed",             type=int,   default=42)
    args = parser.parse_args()

    set_seed(args.seed)

    data_dir    = Path(args.data_dir)
    ra_data_dir = Path(args.ra_data_dir) if args.ra_data_dir else None
    output_dir  = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    # ── Label mapping ─────────────────────────────────────────────────────────
    label_map_path = (Path(args.label_map) if args.label_map
                      else (ra_data_dir or data_dir) / "label_mapping.json")
    label2id, id2label = load_label_mapping(label_map_path)
    num_labels = len(label2id)
    logger.info(f"Labels ({num_labels}): {list(id2label.values())}")

    # ── Tokeniser + data ──────────────────────────────────────────────────────
    tokenizer = AutoTokenizer.from_pretrained(args.model_checkpoint)
    datasets  = load_splits(data_dir, ra_data_dir)
    tokenised = tokenize_dataset(datasets, tokenizer, max_length=args.max_length)

    # ── Model ─────────────────────────────────────────────────────────────────
    model = AutoModelForSequenceClassification.from_pretrained(
        args.model_checkpoint,
        num_labels=num_labels,
        id2label=id2label,
        label2id=label2id,
        ignore_mismatched_sizes=True,
    )

    # ── Class weights ─────────────────────────────────────────────────────────
    train_labels  = tokenised["train"]["label"]
    class_counts  = np.bincount(train_labels, minlength=num_labels).astype(float)
    class_weights = torch.tensor(
        class_counts.sum() / (num_labels * class_counts + 1e-8),
        dtype=torch.float,
    )
    logger.info(f"Class weights: {class_weights.tolist()}")

    WeightedTrainer = make_weighted_trainer(class_weights)

    # ── Warmup steps (replaces deprecated warmup_ratio) ───────────────────────
    steps_per_epoch = math.ceil(
        len(tokenised["train"]) / args.per_device_train_batch_size)

    def make_training_args(epochs, lr, output_subdir):
        warmup_steps = max(1, int(0.1 * steps_per_epoch * epochs))
        return TrainingArguments(
            output_dir=str(output_subdir),
            num_train_epochs=epochs,
            per_device_train_batch_size=args.per_device_train_batch_size,
            per_device_eval_batch_size=args.per_device_eval_batch_size,
            learning_rate=lr,
            warmup_steps=warmup_steps,
            weight_decay=args.weight_decay,
            fp16=args.fp16,
            eval_strategy="epoch",
            save_strategy="epoch",
            load_best_model_at_end=True,
            metric_for_best_model="macro_f1",
            greater_is_better=True,
            save_total_limit=2,
            logging_steps=20,
            report_to="none",
            seed=args.seed,
        )

    collator = DataCollatorWithPadding(tokenizer)
    callbacks = [EarlyStoppingCallback(early_stopping_patience=args.patience)]

    # ── Phase 1: frozen encoder (optional) ────────────────────────────────────
    if args.freeze_epochs > 0:
        logger.info(f"\n{'='*60}")
        logger.info(f"Phase 1: encoder frozen — {args.freeze_epochs} epochs "
                    f"@ lr={args.freeze_lr}")
        logger.info('='*60)

        freeze_encoder(model)

        phase1_args = make_training_args(
            epochs=args.freeze_epochs,
            lr=args.freeze_lr,
            output_subdir=output_dir / "phase1",
        )
        trainer = WeightedTrainer(
            model=model,
            args=phase1_args,
            train_dataset=tokenised["train"],
            eval_dataset=tokenised["val"],
            data_collator=collator,
            compute_metrics=compute_metrics,
            callbacks=callbacks,
        )
        trainer.train()

        # Unfreeze for Phase 2
        unfreeze_all(model)

    # ── Phase 2 (or only phase if freeze_epochs=0): full fine-tuning ──────────
    logger.info(f"\n{'='*60}")
    logger.info(f"{'Phase 2' if args.freeze_epochs > 0 else 'Training'}: "
                f"full fine-tune — {args.num_train_epochs} epochs "
                f"@ lr={args.learning_rate}")
    logger.info('='*60)

    phase2_args = make_training_args(
        epochs=args.num_train_epochs,
        lr=args.learning_rate,
        output_subdir=output_dir,
    )
    trainer = WeightedTrainer(
        model=model,
        args=phase2_args,
        train_dataset=tokenised["train"],
        eval_dataset=tokenised["val"],
        data_collator=collator,
        compute_metrics=compute_metrics,
        callbacks=callbacks,
    )
    trainer.train()

    # ── Test set evaluation ───────────────────────────────────────────────────
    logger.info("Evaluating on test set...")
    test_results = trainer.predict(tokenised["test"])
    preds        = np.argmax(test_results.predictions, axis=-1)
    labels       = test_results.label_ids
    label_names  = [id2label[i] for i in range(num_labels)]

    report = classification_report(labels, preds, target_names=label_names, digits=4)
    print("\n" + "=" * 60)
    print(f"TEST SET RESULTS — {args.model_checkpoint}")
    print("=" * 60)
    print(report)

    report_path = output_dir / "test_classification_report.txt"
    with open(report_path, "w") as f:
        f.write(f"Model: {args.model_checkpoint}\n\n{report}")
    logger.info(f"Classification report → {report_path}")

    trainer.save_model(str(output_dir / "best_model"))
    tokenizer.save_pretrained(str(output_dir / "best_model"))
    logger.info(f"Best model saved → {output_dir / 'best_model'}")


if __name__ == "__main__":
    main()

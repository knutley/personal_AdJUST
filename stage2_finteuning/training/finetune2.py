"""
finetune2.py  —  Multi-Label Classification Fine-tuning
Stage 1: fine-tune on Meckling & Allan data (3-class, single-label)
Stage 2: fine-tune Stage 1 checkpoint on RA-annotated EU pre-legislative data
         with multi-label binary cross-entropy loss

Key differences from finetune1.py:
  - Loads label_vector column (JSON string → binary list) instead of single integer
  - Uses BCEWithLogitsLoss (sigmoid outputs, one per class)
  - Metrics: per-class F1, micro/macro F1, exact match accuracy
  - Inference: threshold-based (default 0.5) instead of argmax
  - Stage 1 still uses single-label CrossEntropy (unchanged)

Usage — Stage 2 (multi-label):
    python finetune2.py \
        --model_checkpoint ./checkpoints/climatebert-stage1/best_model \
        --ra_data_dir ./ra_data \
        --label_map   ./ra_data/label_mapping.json \
        --output_dir  ./checkpoints/climatebert-stage2-ml \
        --num_train_epochs 20 \
        --learning_rate 1e-5 \
        --freeze_epochs 2 \
        --freeze_lr 1e-4 \
        --weight_decay 0.01 \
        --patience 5 \
        --fp16 \
        --seed 42
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
from sklearn.metrics import (
    classification_report,
    f1_score,
    accuracy_score,
)
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


# ── Label mapping ─────────────────────────────────────────────────────────────

def load_label_mapping(path: Path):
    with open(path) as f:
        mapping = json.load(f)
    if "label2id" in mapping:
        label2id = {k: int(v) for k, v in mapping["label2id"].items()}
        id2label = {int(k): v for k, v in mapping["id2label"].items()}
    else:
        label2id = {k: int(v) for k, v in mapping.items()}
        id2label = {int(v): k for k, v in mapping.items()}
    return label2id, id2label


# ── Data loading ──────────────────────────────────────────────────────────────

def load_splits_multilabel(ra_data_dir: Path) -> DatasetDict:
    logger.info(f"Stage 2 (RA, multi-label): loading splits from {ra_data_dir}")

    splits = {}
    for split in ("train", "val", "test"):
        df = pd.read_csv(ra_data_dir / f"clf_{split}.csv")

        # Parse label_vector from JSON string → list of floats
        df["labels"] = df["label_vector"].apply(
            lambda x: [float(v) for v in json.loads(x)]
        )
        splits[split] = Dataset.from_dict({
            "text":   df["text"].tolist(),
            "labels": df["labels"].tolist(),
        })
        logger.info(f"  {split}: {len(df)} examples")

    return DatasetDict(splits)


def tokenize_dataset(datasets: DatasetDict, tokenizer, max_length: int = 512) -> DatasetDict:
    def tokenize(batch):
        return tokenizer(
            batch["text"],
            truncation=True,
            max_length=max_length,
            padding=False,
        )
    return datasets.map(tokenize, batched=True, remove_columns=["text"])


# ── Multi-label metrics ───────────────────────────────────────────────────────

def make_compute_metrics(id2label, threshold=0.5):
    label_names = [id2label[i] for i in range(len(id2label))]

    def compute_metrics(eval_pred):
        logits, labels = eval_pred
        probs = torch.sigmoid(torch.tensor(logits)).numpy()
        preds = (probs >= threshold).astype(int)

        # Exact match accuracy (all labels must match)
        exact_match = accuracy_score(labels, preds)

        macro_f1 = f1_score(labels, preds, average="macro",   zero_division=0)
        micro_f1 = f1_score(labels, preds, average="micro",   zero_division=0)

        metrics = {
            "exact_match": exact_match,
            "macro_f1":    macro_f1,
            "micro_f1":    micro_f1,
        }

        # Per-class F1
        per_class = f1_score(labels, preds, average=None, zero_division=0)
        for i, name in enumerate(label_names):
            metrics[f"f1_{name}"] = per_class[i]

        return metrics

    return compute_metrics


# ── Multi-label weighted BCE trainer ─────────────────────────────────────────

def make_multilabel_trainer(pos_weights):
    """
    pos_weights: tensor of shape (num_labels,) — upweights minority classes.
    Uses BCEWithLogitsLoss which expects raw logits + float labels.
    """
    class MultiLabelTrainer(Trainer):
        def compute_loss(self, model, inputs, return_outputs=False, **kwargs):
            labels  = inputs.pop("labels").float()
            outputs = model(**inputs)
            loss    = torch.nn.BCEWithLogitsLoss(
                pos_weight=pos_weights.to(
                    device=outputs.logits.device,
                    dtype=outputs.logits.dtype,
                )
            )(outputs.logits, labels)
            return (loss, outputs) if return_outputs else loss

    return MultiLabelTrainer


# ── Two-step fine-tuning helpers ──────────────────────────────────────────────

def freeze_encoder(model):
    for name, param in model.named_parameters():
        if any(k in name for k in ["classifier", "pooler"]):
            param.requires_grad = True
        else:
            param.requires_grad = False
    trainable = sum(p.numel() for p in model.parameters() if p.requires_grad)
    logger.info(f"Phase 1 — encoder frozen. Trainable params: {trainable:,}")


def unfreeze_all(model):
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
    parser.add_argument("--num_train_epochs", type=int,   default=20)
    parser.add_argument("--per_device_train_batch_size", type=int, default=16)
    parser.add_argument("--per_device_eval_batch_size",  type=int, default=16)
    parser.add_argument("--learning_rate",    type=float, default=1e-5)
    parser.add_argument("--weight_decay",     type=float, default=0.01)
    parser.add_argument("--patience",         type=int,   default=5)
    parser.add_argument("--threshold",        type=float, default=0.5,
                        help="Sigmoid threshold for positive label prediction")
    parser.add_argument("--freeze_epochs",    type=int,   default=0)
    parser.add_argument("--freeze_lr",        type=float, default=1e-4)
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
    datasets  = load_splits_multilabel(ra_data_dir or data_dir)
    tokenised = tokenize_dataset(datasets, tokenizer, max_length=args.max_length)

    # ── Model ─────────────────────────────────────────────────────────────────
    model = AutoModelForSequenceClassification.from_pretrained(
        args.model_checkpoint,
        num_labels=num_labels,
        id2label=id2label,
        label2id=label2id,
        ignore_mismatched_sizes=True,
        problem_type="multi_label_classification",
    )

    # ── Per-class positive weights (inverse frequency) ────────────────────────
    train_labels = np.array(tokenised["train"]["labels"])
    pos_counts   = train_labels.sum(axis=0)
    neg_counts   = len(train_labels) - pos_counts
    pos_weights  = torch.tensor(neg_counts / (pos_counts + 1e-8), dtype=torch.float)
    logger.info(f"Positive weights: {pos_weights.tolist()}")

    MultiLabelTrainer = make_multilabel_trainer(pos_weights)
    compute_metrics   = make_compute_metrics(id2label, threshold=args.threshold)

    # ── Warmup steps ──────────────────────────────────────────────────────────
    steps_per_epoch = math.ceil(
        len(tokenised["train"]) / args.per_device_train_batch_size
    )

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

    collator  = DataCollatorWithPadding(tokenizer)
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
        trainer = MultiLabelTrainer(
            model=model,
            args=phase1_args,
            train_dataset=tokenised["train"],
            eval_dataset=tokenised["val"],
            data_collator=collator,
            compute_metrics=compute_metrics,
            callbacks=callbacks,
        )
        trainer.train()
        unfreeze_all(model)

    # ── Phase 2: full fine-tuning ─────────────────────────────────────────────
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
    trainer = MultiLabelTrainer(
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
    logits       = test_results.predictions
    labels       = test_results.label_ids

    probs = torch.sigmoid(torch.tensor(logits)).numpy()
    preds = (probs >= args.threshold).astype(int)

    label_names = [id2label[i] for i in range(num_labels)]

    print("\n" + "=" * 60)
    print(f"TEST SET RESULTS — {args.model_checkpoint}")
    print("=" * 60)
    print(f"Threshold: {args.threshold}")
    print(f"Exact match accuracy: {accuracy_score(labels, preds):.4f}")
    print(f"Macro F1:             {f1_score(labels, preds, average='macro', zero_division=0):.4f}")
    print(f"Micro F1:             {f1_score(labels, preds, average='micro', zero_division=0):.4f}")
    print()
    report = classification_report(
        labels, preds,
        target_names=label_names,
        zero_division=0,
    )
    print(report)

    report_path = output_dir / "test_classification_report.txt"
    with open(report_path, "w") as f:
        f.write(f"Model: {args.model_checkpoint}\n"
                f"Threshold: {args.threshold}\n\n{report}")
    logger.info(f"Classification report → {report_path}")

    trainer.save_model(str(output_dir / "best_model"))
    tokenizer.save_pretrained(str(output_dir / "best_model"))
    logger.info(f"Best model saved → {output_dir / 'best_model'}")


if __name__ == "__main__":
    main()

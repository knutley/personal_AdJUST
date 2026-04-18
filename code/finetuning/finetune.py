\"""
finetune.py  —  Supervised Classification Fine-tuning
Stage 1: fine-tune ClimateBERT / RoBERTa / DeBERTa on Meckling & Allan data (3 classes)
Stage 2: fine-tune Stage 1 checkpoint on RA-annotated EU pre-legislative data (5 classes)

Usage — Stage 1:
    python finetune.py \
        --model_checkpoint climatebert/distilroberta-base-climate-f \
        --data_dir ./data \
        --output_dir ./checkpoints/climatebert-stage1 \
        --num_train_epochs 5 \
        --learning_rate 2e-5 \
        --patience 3

Usage — Stage 2:
    python finetune.py \
        --model_checkpoint ./checkpoints/climatebert-stage1/best_model \
        --data_dir ./data \
        --ra_data_dir ./ra_data \
        --label_map ./ra_data/label_mapping.json \
        --output_dir ./checkpoints/climatebert-stage2 \
        --num_train_epochs 5 \
        --learning_rate 2e-5 \
        --patience 3
"""

import argparse
import json
import logging
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


# ── Data loading ──────────────────────────────────────────────────────────────

def load_splits(data_dir: Path, ra_data_dir: Path | None) -> DatasetDict:
    """
    Stage 1 (no ra_data_dir): all splits from data_dir (Meckling).
    Stage 2 (ra_data_dir set): all splits from ra_data_dir (RA annotations).
    """
    source = ra_data_dir if ra_data_dir else data_dir
    stage  = "Stage 2 (RA)" if ra_data_dir else "Stage 1 (Meckling)"
    logger.info(f"{stage}: loading splits from {source}")

    train_df = pd.read_csv(source / "clf_train.csv")
    val_df   = pd.read_csv(source / "clf_val.csv")
    test_df  = pd.read_csv(source / "clf_test.csv")

    return DatasetDict({
        "train": Dataset.from_pandas(train_df[["text", "label"]]),
        "val":   Dataset.from_pandas(val_df[["text", "label"]]),
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


# ── Main ──────────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--model_checkpoint", required=True,
                        help="HuggingFace model name or path to local checkpoint")
    parser.add_argument("--data_dir",         default="./data",
                        help="Directory with Meckling clf_train/val/test.csv and label_mapping.json")
    parser.add_argument("--ra_data_dir",      default=None,
                        help="Directory with RA clf_train/val/test.csv (Stage 2 only)")
    parser.add_argument("--label_map",        default=None,
                        help="Path to label_mapping.json. Defaults to data_dir/label_mapping.json")
    parser.add_argument("--output_dir",       required=True)
    parser.add_argument("--max_length",       type=int,   default=512)
    parser.add_argument("--num_train_epochs", type=int,   default=5)
    parser.add_argument("--per_device_train_batch_size", type=int,   default=16)
    parser.add_argument("--per_device_eval_batch_size",  type=int,   default=32)
    parser.add_argument("--learning_rate",    type=float, default=2e-5)
    parser.add_argument("--warmup_ratio",     type=float, default=0.1)
    parser.add_argument("--weight_decay",     type=float, default=0.01)
    parser.add_argument("--patience",         type=int,   default=3,
                        help="Early stopping patience in epochs")
    parser.add_argument("--fp16",             action="store_true",
                        help="Mixed precision — only use on GPU")
    parser.add_argument("--seed",             type=int,   default=42)
    args = parser.parse_args()

    set_seed(args.seed)

    data_dir    = Path(args.data_dir)
    ra_data_dir = Path(args.ra_data_dir) if args.ra_data_dir else None
    output_dir  = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    # ── Label mapping ─────────────────────────────────────────────────────────
    label_map_path = Path(args.label_map) if args.label_map else data_dir / "label_mapping.json"
    with open(label_map_path) as f:
        mapping = json.load(f)

    label2id   = {k: int(v) for k, v in mapping["label2id"].items()}
    id2label   = {int(k): v for k, v in mapping["id2label"].items()}
    num_labels = len(label2id)
    logger.info(f"Labels ({num_labels}): {list(id2label.values())}")

    # ── Tokeniser ─────────────────────────────────────────────────────────────
    tokenizer = AutoTokenizer.from_pretrained(args.model_checkpoint)

    # ── Data ──────────────────────────────────────────────────────────────────
    datasets  = load_splits(data_dir, ra_data_dir)
    tokenised = tokenize_dataset(datasets, tokenizer, max_length=args.max_length)
    logger.info(f"Train: {len(tokenised['train'])}  "
                f"Val: {len(tokenised['val'])}  "
                f"Test: {len(tokenised['test'])}")

    # ── Model ─────────────────────────────────────────────────────────────────
    model = AutoModelForSequenceClassification.from_pretrained(
        args.model_checkpoint,
        num_labels=num_labels,
        id2label=id2label,
        label2id=label2id,
        ignore_mismatched_sizes=True,
    )

    # ── Class weights (handles label imbalance) ────────────────────────────────
    train_labels  = tokenised["train"]["label"]
    class_counts  = np.bincount(train_labels, minlength=num_labels).astype(float)
    class_weights = torch.tensor(
        class_counts.sum() / (num_labels * class_counts + 1e-8),
        dtype=torch.float,
    )
    logger.info(f"Class weights: {class_weights.tolist()}")

    # ── Weighted loss trainer ─────────────────────────────────────────────────
    class WeightedTrainer(Trainer):
        def compute_loss(self, model, inputs, return_outputs=False, **kwargs):
            labels  = inputs.pop("labels")
            outputs = model(**inputs)
            loss    = torch.nn.CrossEntropyLoss(
                weight=class_weights.to(outputs.logits.device)
            )(outputs.logits, labels)
            return (loss, outputs) if return_outputs else loss

    # ── Training arguments ────────────────────────────────────────────────────
    training_args = TrainingArguments(
        output_dir=str(output_dir),
        num_train_epochs=args.num_train_epochs,
        per_device_train_batch_size=args.per_device_train_batch_size,
        per_device_eval_batch_size=args.per_device_eval_batch_size,
        learning_rate=args.learning_rate,
        warmup_ratio=args.warmup_ratio,
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
        gradient_checkpointing=True,
    )

    trainer = WeightedTrainer(
        model=model,
        args=training_args,
        train_dataset=tokenised["train"],
        eval_dataset=tokenised["val"],
        data_collator=DataCollatorWithPadding(tokenizer),
        compute_metrics=compute_metrics,
        callbacks=[EarlyStoppingCallback(early_stopping_patience=args.patience)],
    )

    # ── Train ─────────────────────────────────────────────────────────────────
    trainer.train()

    # ── Evaluate on held-out test set ─────────────────────────────────────────
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

    # ── Save best model ───────────────────────────────────────────────────────
    trainer.save_model(str(output_dir / "best_model"))
    tokenizer.save_pretrained(str(output_dir / "best_model"))
    logger.info(f"Best model saved → {output_dir / 'best_model'}")


if __name__ == "__main__":
    main()

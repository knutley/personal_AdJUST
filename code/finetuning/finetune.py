"""
finetune.py  —  Supervised Classification Fine-tuning (Theme → Register)
Loads a DAPT checkpoint and fine-tunes a classification head on RA-annotated data.

Usage:
    python finetune.py \
        --model_checkpoint ./checkpoints/climatebert-dapt \
        --data_dir ./data \
        --output_dir ./checkpoints/climatebert-clf \
        --num_train_epochs 5 \
        --per_device_train_batch_size 16

For your RA-annotated pre-legislative data, just make sure it's a CSV with
columns: text, label  (integer label matching label_mapping.json)
Pass --ra_data_dir if different from --data_dir.
"""

import argparse
import json
import logging
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
    AutoTokenizer,
    AutoModelForSequenceClassification,
    TrainingArguments,
    Trainer,
    EarlyStoppingCallback,
    set_seed,
    DataCollatorWithPadding,
)

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# ── Metrics ──────────────────────────────────────────────────────────────────
def compute_metrics(eval_pred):
    logits, labels = eval_pred
    predictions = np.argmax(logits, axis=-1)
    macro_f1    = f1_score(labels, predictions, average="macro",  zero_division=0)
    micro_f1    = f1_score(labels, predictions, average="micro",  zero_division=0)
    accuracy    = accuracy_score(labels, predictions)
    return {
        "accuracy":  accuracy,
        "macro_f1":  macro_f1,
        "micro_f1":  micro_f1,
    }


def load_splits(data_dir: Path, ra_data_dir: Path | None) -> DatasetDict:
    """
    Load train/val/test CSVs.
    If RA-annotated data is in a separate directory, use that for train/val,
    keeping the Meckling test split for comparison.
    """
    if ra_data_dir and ra_data_dir != data_dir:
        logger.info(f"Using RA-annotated data from: {ra_data_dir}")
        train_df = pd.read_csv(ra_data_dir / "clf_train.csv")
        val_df   = pd.read_csv(ra_data_dir / "clf_val.csv")
    else:
        train_df = pd.read_csv(data_dir / "clf_train.csv")
        val_df   = pd.read_csv(data_dir / "clf_val.csv")

    test_df = pd.read_csv(data_dir / "clf_test.csv")

    splits = DatasetDict({
        "train": Dataset.from_pandas(train_df[["text", "label"]]),
        "val":   Dataset.from_pandas(val_df[["text", "label"]]),
        "test":  Dataset.from_pandas(test_df[["text", "label"]]),
    })
    return splits


def tokenize_dataset(datasets: DatasetDict, tokenizer, max_length: int = 512):
    def tokenize(batch):
        return tokenizer(
            batch["text"],
            truncation=True,
            max_length=max_length,
            padding=False,       # DataCollatorWithPadding handles dynamic padding
        )
    return datasets.map(tokenize, batched=True, remove_columns=["text"])


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--model_checkpoint", required=True,
                        help="Path to DAPT checkpoint (or raw HF hub name to skip DAPT)")
    parser.add_argument("--data_dir",         default="./data")
    parser.add_argument("--ra_data_dir",      default=None,
                        help="Optional separate dir for RA-annotated fine-tuning data")
    parser.add_argument("--output_dir",       required=True)
    parser.add_argument("--max_length",       type=int,   default=512)
    parser.add_argument("--num_train_epochs", type=int,   default=5)
    parser.add_argument("--per_device_train_batch_size", type=int, default=16)
    parser.add_argument("--per_device_eval_batch_size",  type=int, default=32)
    parser.add_argument("--learning_rate",    type=float, default=2e-5)
    parser.add_argument("--warmup_ratio",     type=float, default=0.1)
    parser.add_argument("--weight_decay",     type=float, default=0.01)
    parser.add_argument("--patience",         type=int,   default=3,
                        help="Early stopping patience (eval epochs)")
    parser.add_argument("--fp16",             action="store_true")
    parser.add_argument("--seed",             type=int,   default=42)
    args = parser.parse_args()

    set_seed(args.seed)

    data_dir    = Path(args.data_dir)
    ra_data_dir = Path(args.ra_data_dir) if args.ra_data_dir else None
    output_dir  = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    # ── Label mapping ──────────────────────────────────────────────────────────
    label_map_path = data_dir / "label_mapping.json"
    with open(label_map_path) as f:
        mapping = json.load(f)

    label2id = {k: int(v) for k, v in mapping["label2id"].items()}
    id2label = {int(k): v for k, v in mapping["id2label"].items()}
    num_labels = len(label2id)
    logger.info(f"Classification task: {num_labels} labels → {list(id2label.values())}")

    # ── Tokeniser ─────────────────────────────────────────────────────────────
    tokenizer = AutoTokenizer.from_pretrained(args.model_checkpoint)

    # ── Data ──────────────────────────────────────────────────────────────────
    datasets = load_splits(data_dir, ra_data_dir)
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
        ignore_mismatched_sizes=True,   # needed when loading MLM head → clf head
    )

    # ── Compute class weights for imbalanced labels ────────────────────────────
    # Weighted cross-entropy helps if some register classes are rare
    train_labels = tokenised["train"]["label"]
    class_counts = np.bincount(train_labels, minlength=num_labels).astype(float)
    class_weights = torch.tensor(
        (class_counts.sum() / (num_labels * class_counts + 1e-8)),
        dtype=torch.float,
    )
    logger.info(f"Class weights: {class_weights.tolist()}")

    # Custom Trainer that uses weighted loss
    class WeightedTrainer(Trainer):
        def compute_loss(self, model, inputs, return_outputs=False, **kwargs):
            labels = inputs.pop("labels")
            outputs = model(**inputs)
            logits  = outputs.logits
            loss_fn = torch.nn.CrossEntropyLoss(
                weight=class_weights.to(logits.device)
            )
            loss = loss_fn(logits, labels)
            return (loss, outputs) if return_outputs else loss

    # ── Training arguments ─────────────────────────────────────────────────────
    training_args = TrainingArguments(
        output_dir=str(output_dir),
        num_train_epochs=args.num_train_epochs,
        per_device_train_batch_size=args.per_device_train_batch_size,
        per_device_eval_batch_size=args.per_device_eval_batch_size,
        learning_rate=args.learning_rate,
        warmup_ratio=args.warmup_ratio,
        weight_decay=args.weight_decay,
        fp16=args.fp16,
        evaluation_strategy="epoch",
        save_strategy="epoch",
        load_best_model_at_end=True,
        metric_for_best_model="macro_f1",   # macro F1 is fair across unbalanced labels
        greater_is_better=True,
        save_total_limit=2,
        logging_steps=20,
        report_to="none",
        seed=args.seed,
        gradient_checkpointing=True,
    )

    data_collator = DataCollatorWithPadding(tokenizer=tokenizer)

    trainer = WeightedTrainer(
        model=model,
        args=training_args,
        train_dataset=tokenised["train"],
        eval_dataset=tokenised["val"],
        data_collator=data_collator,
        compute_metrics=compute_metrics,
        callbacks=[EarlyStoppingCallback(early_stopping_patience=args.patience)],
    )

    # ── Train ──────────────────────────────────────────────────────────────────
    trainer.train()

    # ── Evaluate on held-out test set ─────────────────────────────────────────
    logger.info("Evaluating on test set…")
    test_results = trainer.predict(tokenised["test"])
    preds  = np.argmax(test_results.predictions, axis=-1)
    labels = test_results.label_ids

    label_names = [id2label[i] for i in range(num_labels)]
    report = classification_report(labels, preds, target_names=label_names, digits=4)

    print("\n" + "="*60)
    print(f"TEST SET RESULTS — {args.model_checkpoint}")
    print("="*60)
    print(report)

    # Save report
    report_path = output_dir / "test_classification_report.txt"
    with open(report_path, "w") as f:
        f.write(f"Model: {args.model_checkpoint}\n\n")
        f.write(report)
    logger.info(f"Classification report → {report_path}")

    # ── Save best model ────────────────────────────────────────────────────────
    trainer.save_model(str(output_dir / "best_model"))
    tokenizer.save_pretrained(str(output_dir / "best_model"))
    logger.info(f"Best model saved → {output_dir / 'best_model'}")


if __name__ == "__main__":
    main()

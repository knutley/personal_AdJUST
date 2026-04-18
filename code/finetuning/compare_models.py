"""
compare_models.py  —  Head-to-head comparison of fine-tuned models
Loads all three fine-tuned checkpoints, runs on the same test set,
and prints a summary table + saves results to CSV.

Usage:
    python compare_models.py \
        --model_dirs \
            ./checkpoints/climatebert-clf/best_model \
            ./checkpoints/roberta-clf/best_model \
            ./checkpoints/deberta-clf/best_model \
        --test_csv ./data/clf_test.csv \
        --label_map ./data/label_mapping.json
"""

import argparse
import json
import logging
from pathlib import Path

import numpy as np
import pandas as pd
from datasets import Dataset
from sklearn.metrics import (
    accuracy_score,
    f1_score,
    classification_report,
)
from transformers import (
    AutoTokenizer,
    AutoModelForSequenceClassification,
    Trainer,
    TrainingArguments,
    DataCollatorWithPadding,
)

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def evaluate_model(model_dir: str, test_dataset, label_names: list) -> dict:
    """Run inference with a single model and return metrics dict."""
    logger.info(f"Evaluating: {model_dir}")

    tokenizer = AutoTokenizer.from_pretrained(model_dir)
    model     = AutoModelForSequenceClassification.from_pretrained(model_dir)

    def tokenize(batch):
        return tokenizer(batch["text"], truncation=True, max_length=512, padding=False)

    tokenised = test_dataset.map(tokenize, batched=True, remove_columns=["text"])

    # Use Trainer purely for inference (no training)
    training_args = TrainingArguments(
        output_dir="/tmp/eval_tmp",
        per_device_eval_batch_size=32,
        report_to="none",
    )
    collator = DataCollatorWithPadding(tokenizer)
    trainer  = Trainer(
        model=model,
        args=training_args,
        data_collator=collator,
    )

    preds_output = trainer.predict(tokenised)
    preds  = np.argmax(preds_output.predictions, axis=-1)
    labels = preds_output.label_ids

    macro_f1 = f1_score(labels, preds, average="macro",  zero_division=0)
    micro_f1 = f1_score(labels, preds, average="micro",  zero_division=0)
    accuracy = accuracy_score(labels, preds)
    report   = classification_report(labels, preds, target_names=label_names, digits=4)

    return {
        "model":     Path(model_dir).parent.name,   # e.g. "climatebert-clf"
        "accuracy":  round(accuracy,  4),
        "macro_f1":  round(macro_f1,  4),
        "micro_f1":  round(micro_f1,  4),
        "report":    report,
        "preds":     preds,
        "labels":    labels,
    }


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--model_dirs",  nargs="+", required=True,
                        help="Paths to fine-tuned model checkpoints")
    parser.add_argument("--test_csv",    required=True)
    parser.add_argument("--label_map",   required=True)
    parser.add_argument("--output_dir",  default="./results")
    args = parser.parse_args()

    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    with open(args.label_map) as f:
        mapping = json.load(f)
    id2label   = {int(k): v for k, v in mapping["id2label"].items()}
    label_names = [id2label[i] for i in range(len(id2label))]

    test_df      = pd.read_csv(args.test_csv)
    test_dataset = Dataset.from_pandas(test_df[["text", "label"]])

    # ── Evaluate all models ───────────────────────────────────────────────────
    results = []
    for model_dir in args.model_dirs:
        r = evaluate_model(model_dir, test_dataset, label_names)
        results.append(r)

        # Save per-model full report
        report_path = output_dir / f"{r['model']}_report.txt"
        with open(report_path, "w") as f:
            f.write(f"Model: {model_dir}\n\n{r['report']}")
        print(f"\n{'='*60}\n{r['model']}\n{'='*60}\n{r['report']}")

    # ── Summary table ─────────────────────────────────────────────────────────
    summary = pd.DataFrame([
        {k: v for k, v in r.items() if k not in ("report", "preds", "labels")}
        for r in results
    ]).sort_values("macro_f1", ascending=False)

    print("\n" + "="*60)
    print("SUMMARY — Models ranked by Macro F1")
    print("="*60)
    print(summary.to_string(index=False))

    summary.to_csv(output_dir / "model_comparison.csv", index=False)
    logger.info(f"Summary saved → {output_dir / 'model_comparison.csv'}")

    # ── Recommend best model ──────────────────────────────────────────────────
    best = summary.iloc[0]
    print(f"\n✔ RECOMMENDED MODEL: {best['model']}")
    print(f"  Macro F1: {best['macro_f1']}  |  Accuracy: {best['accuracy']}")


if __name__ == "__main__":
    main()
"""
compare_models.py  —  Head-to-head comparison + soft-vote ensemble
Loads all fine-tuned checkpoints, runs on the same test set,
and prints individual results plus the ensemble.

Usage:
    python compare_models.py \
        --model_dirs \
            ./checkpoints/climatebert-stage1/best_model \
            ./checkpoints/roberta-stage1/best_model \
            ./checkpoints/legalbert-stage1/best_model \
            ./checkpoints/finbert-stage1/best_model \
        --test_csv ./data/clf_test.csv \
        --label_map ./data/label_mapping.json \
        --output_dir ./results/stage1

    # On Colab:
    python /content/drive/MyDrive/AdJUST/data/compare_models.py \
        --model_dirs \
            /content/drive/MyDrive/AdJUST/checkpoints/climatebert-stage1/best_model \
            /content/drive/MyDrive/AdJUST/checkpoints/roberta-stage1/best_model \
            /content/drive/MyDrive/AdJUST/checkpoints/legalbert-stage1/best_model \
            /content/drive/MyDrive/AdJUST/checkpoints/finbert-stage1/best_model \
        --test_csv /content/drive/MyDrive/AdJUST/data/clf_test.csv \
        --label_map /content/drive/MyDrive/AdJUST/data/label_mapping.json \
        --output_dir /content/drive/MyDrive/AdJUST/results/stage1
"""

import os
os.environ["TRANSFORMERS_OFFLINE"] = "1"

import argparse
import json
import logging
from pathlib import Path

import numpy as np
import pandas as pd
from datasets import Dataset
from sklearn.metrics import (
    accuracy_score,
    classification_report,
    f1_score,
)
from transformers import (
    AutoModelForSequenceClassification,
    AutoTokenizer,
    DataCollatorWithPadding,
    Trainer,
    TrainingArguments,
)

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def get_logits(model_dir: str, test_dataset: Dataset) -> np.ndarray:
    """Run inference and return raw logits for a single model."""
    logger.info(f"Getting logits: {model_dir}")

    tokenizer = AutoTokenizer.from_pretrained(model_dir)
    model     = AutoModelForSequenceClassification.from_pretrained(model_dir)

    def tokenize(batch):
        return tokenizer(batch["text"], truncation=True, max_length=512, padding=False)

    tokenised = test_dataset.map(tokenize, batched=True, remove_columns=["text"])

    training_args = TrainingArguments(
        output_dir="/tmp/eval_tmp",
        per_device_eval_batch_size=32,
        report_to="none",
    )
    trainer = Trainer(
        model=model,
        args=training_args,
        data_collator=DataCollatorWithPadding(tokenizer),
    )

    preds_output = trainer.predict(tokenised)
    return preds_output.predictions, preds_output.label_ids


def evaluate(logits: np.ndarray, labels: np.ndarray, label_names: list, model_name: str) -> dict:
    """Compute metrics from logits."""
    preds    = np.argmax(logits, axis=-1)
    macro_f1 = f1_score(labels, preds, average="macro", zero_division=0)
    micro_f1 = f1_score(labels, preds, average="micro", zero_division=0)
    accuracy = accuracy_score(labels, preds)
    report   = classification_report(labels, preds, target_names=label_names, digits=4)

    return {
        "model":    model_name,
        "accuracy": round(accuracy, 4),
        "macro_f1": round(macro_f1, 4),
        "micro_f1": round(micro_f1, 4),
        "report":   report,
        "preds":    preds,
    }


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--model_dirs", nargs="+", required=True,
                        help="Paths to fine-tuned model checkpoints")
    parser.add_argument("--test_csv",   required=True)
    parser.add_argument("--label_map",  required=True)
    parser.add_argument("--output_dir", default="./results")
    args = parser.parse_args()

    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    with open(args.label_map) as f:
        mapping = json.load(f)
    id2label    = {int(k): v for k, v in mapping["id2label"].items()}
    label_names = [id2label[i] for i in range(len(id2label))]

    test_df      = pd.read_csv(args.test_csv)
    test_dataset = Dataset.from_pandas(test_df[["text", "label"]])

    # ── Get logits from each model ─────────────────────────────────────────────
    all_logits = []
    all_labels = None
    results    = []

    for model_dir in args.model_dirs:
        model_name = Path(model_dir).parent.name
        logits, labels = get_logits(model_dir, test_dataset)
        all_logits.append(logits)
        all_labels = labels

        r = evaluate(logits, labels, label_names, model_name)
        results.append(r)

        report_path = output_dir / f"{model_name}_report.txt"
        with open(report_path, "w") as f:
            f.write(f"Model: {model_dir}\n\n{r['report']}")

        print(f"\n{'='*60}\n{model_name}\n{'='*60}")
        print(r['report'])

    # ── Soft-vote ensemble ─────────────────────────────────────────────────────
    ensemble_logits = np.mean(all_logits, axis=0)
    ensemble_result = evaluate(ensemble_logits, all_labels, label_names, "ENSEMBLE")

    print(f"\n{'='*60}\nENSEMBLE (soft vote across all {len(args.model_dirs)} models)\n{'='*60}")
    print(ensemble_result['report'])

    ensemble_path = output_dir / "ensemble_report.txt"
    with open(ensemble_path, "w") as f:
        f.write(f"Ensemble of: {', '.join(args.model_dirs)}\n\n{ensemble_result['report']}")

    # ── Summary table ──────────────────────────────────────────────────────────
    all_results = results + [ensemble_result]
    summary = pd.DataFrame([
        {k: v for k, v in r.items() if k not in ("report", "preds")}
        for r in all_results
    ]).sort_values("macro_f1", ascending=False)

    print(f"\n{'='*60}")
    print("SUMMARY — Ranked by Macro F1")
    print(f"{'='*60}")
    print(summary.to_string(index=False))

    summary.to_csv(output_dir / "model_comparison.csv", index=False)
    logger.info(f"Results saved → {output_dir}")


if __name__ == "__main__":
    main()
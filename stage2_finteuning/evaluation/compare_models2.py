# Author: Katie Nutley
# Date: 2026-05-22
# Title: Comparing Second Stage of Fine Tuning

"""
compare_models2.py — Stage 2 multi-label ensemble evaluation

Loads all six fine-tuned Stage 2 (multi-label) checkpoints, runs them on the
same held-out test set, and reports individual model results plus a soft-vote
ensemble (averaged logits -> sigmoid -> threshold).

Usage:
    python compare_models2.py \
        --model_dirs \
            ./checkpoints/climatebert-stage2-ml/best_model \
            ./checkpoints/finbert-stage2-ml/best_model \
            ./checkpoints/roberta-stage2-ml/best_model \
            ./checkpoints/esgbert-stage2-ml/best_model \
            ./checkpoints/scibert-stage2-ml/best_model \
            ./checkpoints/deberta-stage2-ml/best_model \
        --test_csv ./ra_data/clf_test.csv \
        --label_map ./ra_data/label_mapping.json \
        --output_dir ./results/stage2

    # On Colab:
    python /content/drive/MyDrive/AdJUST/stage2_finetuning/evaluation/compare_models2.py \
        --model_dirs \
            /content/drive/MyDrive/AdJUST/checkpoints/climatebert-stage2-ml/best_model \
            /content/drive/MyDrive/AdJUST/checkpoints/finbert-stage2-ml/best_model \
            /content/drive/MyDrive/AdJUST/checkpoints/roberta-stage2-ml/best_model \
            /content/drive/MyDrive/AdJUST/checkpoints/esgbert-stage2-ml/best_model \
            /content/drive/MyDrive/AdJUST/checkpoints/scibert-stage2-ml/best_model \
            /content/drive/MyDrive/AdJUST/checkpoints/deberta-stage2-ml/best_model \
        --test_csv /content/drive/MyDrive/AdJUST/ra_data/clf_test.csv \
        --label_map /content/drive/MyDrive/AdJUST/ra_data/label_mapping.json \
        --output_dir /content/drive/MyDrive/AdJUST/results/stage2
"""

import os
os.environ["TRANSFORMERS_OFFLINE"] = "1"

import argparse
import json
import logging
from pathlib import Path

import numpy as np
import pandas as pd
import torch
from sklearn.metrics import accuracy_score, classification_report, f1_score
from tqdm.auto import tqdm
from transformers import AutoModelForSequenceClassification, AutoTokenizer

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# ── Label mapping ─────────────────────────────────────────────────────────────

def load_label_names(path: Path) -> list[str]:
    with open(path) as f:
        mapping = json.load(f)
    id2label = {int(k): v for k, v in mapping["id2label"].items()}
    return [id2label[i] for i in sorted(id2label.keys())]


# ── Inference ─────────────────────────────────────────────────────────────────

def get_doc_logits(texts, tokenizer, model, device, batch_size=16) -> np.ndarray:
    """Returns raw logits — sigmoid is applied after ensemble averaging."""
    all_logits = []
    for i in tqdm(range(0, len(texts), batch_size), desc="  batches", leave=False):
        batch = texts[i:i + batch_size]
        enc = tokenizer(
            batch,
            return_tensors="pt",
            padding=True,
            truncation=True,
            max_length=512,
        )
        with torch.no_grad():
            logits = model(**{k: v.to(device) for k, v in enc.items()}).logits
        all_logits.append(logits.cpu().numpy())
    return np.vstack(all_logits)


def run_model(model_dir: str, texts: list[str], device) -> np.ndarray:
    """Load a single checkpoint, run inference, then free GPU memory."""
    logger.info(f"Running {model_dir}...")
    tokenizer = AutoTokenizer.from_pretrained(model_dir)
    model = AutoModelForSequenceClassification.from_pretrained(model_dir).to(device)
    model.eval()

    logits = get_doc_logits(texts, tokenizer, model, device)

    del model
    torch.cuda.empty_cache()
    return logits


# ── Multi-label metrics ────────────────────────────────────────────────────────

def evaluate_multilabel(true_labels, pred_labels, label_names, name="") -> dict:
    exact = accuracy_score(true_labels, pred_labels)
    macro_f1 = f1_score(true_labels, pred_labels, average="macro", zero_division=0)
    micro_f1 = f1_score(true_labels, pred_labels, average="micro", zero_division=0)
    report = classification_report(
        true_labels, pred_labels, target_names=label_names, zero_division=0
    )

    print(f"\n{'=' * 60}")
    print(name)
    print("=" * 60)
    print(f"Exact match: {exact:.4f}  Macro F1: {macro_f1:.4f}  Micro F1: {micro_f1:.4f}")
    print(report)

    return {
        "name": name,
        "exact_match": round(exact, 4),
        "macro_f1": round(macro_f1, 4),
        "micro_f1": round(micro_f1, 4),
        "report": report,
    }


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--model_dirs", nargs="+", required=True,
                        help="Paths to fine-tuned Stage 2 (multi-label) checkpoints")
    parser.add_argument("--test_csv", required=True)
    parser.add_argument("--label_map", required=True)
    parser.add_argument("--output_dir", default="./results/stage2")
    parser.add_argument("--threshold", type=float, default=0.5,
                        help="Sigmoid decision threshold. Tried to change this "
                             "and it was a nightmare, so keeping it constant for now.")
    args = parser.parse_args()

    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    logger.info(f"Using device: {device}")

    # ── Labels + test set ──────────────────────────────────────────────────────
    label_names = load_label_names(Path(args.label_map))

    test_df = pd.read_csv(args.test_csv)
    test_texts = test_df["text"].tolist()
    test_labels = np.array(test_df["label_vector"].apply(json.loads).tolist())

    # ── Run each model ─────────────────────────────────────────────────────────
    model_names = [Path(d).parent.name for d in args.model_dirs]
    all_logits = []
    individual_results = []

    for model_name, model_dir in zip(model_names, args.model_dirs):
        logits = run_model(model_dir, test_texts, device)
        all_logits.append(logits)

        probs = torch.sigmoid(torch.tensor(logits)).numpy()
        preds = (probs >= args.threshold).astype(int)

        r = evaluate_multilabel(test_labels, preds, label_names, name=model_name)
        individual_results.append(r)

        report_path = output_dir / f"{model_name}_report.txt"
        with open(report_path, "w") as f:
            f.write(f"Model: {model_dir}\n\n{r['report']}")

    # ── Soft-vote ensemble: average logits, then sigmoid ───────────────────────
    ensemble_logits = np.mean(all_logits, axis=0)
    ensemble_probs = torch.sigmoid(torch.tensor(ensemble_logits)).numpy()
    ensemble_preds = (ensemble_probs >= args.threshold).astype(int)

    ensemble_result = evaluate_multilabel(
        test_labels, ensemble_preds, label_names, name="STAGE 2 ENSEMBLE (multi-label)"
    )

    ensemble_report_path = output_dir / "ensemble_report.txt"
    with open(ensemble_report_path, "w") as f:
        f.write(f"Ensemble of: {', '.join(args.model_dirs)}\n\n{ensemble_result['report']}")
    logger.info(f"Ensemble report -> {ensemble_report_path}")

    # ── Model comparison table ─────────────────────────────────────────────────
    comparison_rows = [
        {
            "model": "ENSEMBLE",
            "exact_match": ensemble_result["exact_match"],
            "macro_f1": ensemble_result["macro_f1"],
            "micro_f1": ensemble_result["micro_f1"],
        }
    ] + [
        {
            "model": r["name"],
            "exact_match": r["exact_match"],
            "macro_f1": r["macro_f1"],
            "micro_f1": r["micro_f1"],
        }
        for r in individual_results
    ]

    comparison_df = pd.DataFrame(comparison_rows).sort_values(
        "macro_f1", ascending=False
    )
    comparison_path = output_dir / "model_comparison.csv"
    comparison_df.to_csv(comparison_path, index=False)
    logger.info(f"Model comparison -> {comparison_path}")

    # ── Console summary ─────────────────────────────────────────────────────────
    print("\n" + "=" * 60)
    print("INDIVIDUAL MODEL RESULTS")
    print("=" * 60)
    for r in individual_results:
        print(f"{r['name']:12} — exact: {r['exact_match']:.4f}  "
              f"macro F1: {r['macro_f1']:.4f}  micro F1: {r['micro_f1']:.4f}")


if __name__ == "__main__":
    main()

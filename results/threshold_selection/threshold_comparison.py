# Author: Katie Nutley 
# Date: 2026-06-10
# Title: Corpus Threshold Selection

"""
threshold_comparison.py — Sweep sigmoid thresholds over the full-corpus ensemble

Loads the cached per-model logits produced by classify_corpus.py, rebuilds the
ensemble (averaged logits -> sigmoid), and reports how the predicted label
distribution shifts across a range of decision thresholds. Used to compare
against threshold_sensitivity.csv (which sweeps by percentile cutoff instead)
when deciding where to set the final classification threshold.

Usage:
    python threshold_comparison.py \
        --logits_dir ./classification/logits \
        --label_map ./stage2_finetuning/data_prep/outputs/label_mapping.json \
        --thresholds 0.4 0.5 0.6 0.7 \
        --output_dir ./results/threshold_selection
"""

import argparse
import json
import logging
from pathlib import Path

import numpy as np
import pandas as pd
import torch

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def load_label_names(path: Path) -> list[str]:
    with open(path) as f:
        mapping = json.load(f)
    id2label = {int(k): v for k, v in mapping["id2label"].items()}
    return [id2label[i] for i in sorted(id2label.keys())]


def load_ensemble_probs(logits_dir: Path) -> np.ndarray:
    logit_files = sorted(logits_dir.glob("logits_*.npy"))
    if not logit_files:
        raise FileNotFoundError(f"No logits_*.npy files found in {logits_dir}")

    all_logits = [np.load(f) for f in logit_files]
    n_rows = all_logits[0].shape[0]
    for f, arr in zip(logit_files, all_logits):
        if arr.shape[0] != n_rows:
            logger.warning(f"{f.name} has {arr.shape[0]} rows, expected {n_rows}")

    ensemble_logits = np.mean(all_logits, axis=0)
    return torch.sigmoid(torch.tensor(ensemble_logits)).numpy()


def sweep_thresholds(probs: np.ndarray, label_names: list[str], thresholds: list[float]) -> pd.DataFrame:
    rows = []
    n = len(probs)

    for threshold in thresholds:
        preds = (probs >= threshold).astype(int)
        no_label = int((preds.sum(axis=1) == 0).sum())

        print(f"\nThreshold {threshold}:")
        print(f"  No label: {no_label} ({no_label / n * 100:.1f}%)")

        row = {
            "threshold": threshold,
            "no_label": no_label,
            "no_label_pct": round(no_label / n * 100, 1),
        }
        for i, name in enumerate(label_names):
            count = int(preds[:, i].sum())
            print(f"  {name:<15} {count:4d} ({count / n * 100:.1f}%)")
            row[name] = count
            row[f"{name}_pct"] = round(count / n * 100, 1)

        rows.append(row)

    return pd.DataFrame(rows)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--logits_dir", required=True,
                        help="Directory containing cached logits_*.npy files from classify_corpus.py")
    parser.add_argument("--label_map", required=True)
    parser.add_argument("--thresholds", type=float, nargs="+", default=[0.4, 0.5, 0.6, 0.7])
    parser.add_argument("--output_dir", default="./results/threshold_selection")
    args = parser.parse_args()

    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    label_names = load_label_names(Path(args.label_map))
    ensemble_probs = load_ensemble_probs(Path(args.logits_dir))

    summary_df = sweep_thresholds(ensemble_probs, label_names, args.thresholds)

    out_path = output_dir / "threshold_comparison_corpus.csv"
    summary_df.to_csv(out_path, index=False)
    logger.info(f"Threshold comparison -> {out_path}")


if __name__ == "__main__":
    main()

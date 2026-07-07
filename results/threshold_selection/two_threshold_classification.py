# Author: Katie Nutley 
# Date: 2026-06-22
# Title: Two Threshold Classification of AdJUST Corpus 

"""
two_threshold_classification.py — Apply primary + secondary thresholds to the
classified corpus

Takes the single-threshold ensemble classification (corpus_classified_ml.csv,
from classify_corpus.py) and re-derives labels using a two-threshold scheme:

  - Primary label:   prob >= PRIMARY_THRESHOLD (default 0.7)
  - Secondary label: SECONDARY_THRESHOLD <= prob < PRIMARY_THRESHOLD (default
                      0.6-0.7), but ONLY on documents that already have at
                      least one primary label. Secondary labels represent an
                      additional, weaker paradigm signal alongside a
                      confidently-detected primary one.

This is the source of corpus_classified_two_threshold.csv, which the
visualization scripts (plot_5yr_bins.py, plot_either_smoothed.py,
plot_5yr_bins_excl_just_transition.py) read from.

Usage:
    python two_threshold_classification.py \
        --classified_csv ./classification/corpus_classified_ml.csv \
        --primary_threshold 0.7 \
        --secondary_threshold 0.6 \
        --output_dir ./results/threshold_selection
"""

import argparse
import logging
from pathlib import Path

import pandas as pd

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def make_combined_label(row, label_names: list[str]) -> str:
    primary = [n for n in label_names if row[f"label_{n}"] == 1]
    secondary = [n for n in label_names if row[f"label_secondary_{n}"] == 1]
    if not primary:
        return "None"
    return "|".join(primary + [f"{s}(2\u00b0)" for s in secondary])


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--classified_csv", required=True,
                        help="corpus_classified_ml.csv from classify_corpus.py "
                             "(must have prob_{label} columns)")
    parser.add_argument("--primary_threshold", type=float, default=0.7)
    parser.add_argument("--secondary_threshold", type=float, default=0.6)
    parser.add_argument("--output_dir", default="./results/threshold_selection")
    args = parser.parse_args()

    if args.secondary_threshold >= args.primary_threshold:
        raise ValueError(
            f"--secondary_threshold ({args.secondary_threshold}) must be lower "
            f"than --primary_threshold ({args.primary_threshold})"
        )

    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    results_df = pd.read_csv(args.classified_csv)
    prob_cols = [c for c in results_df.columns if c.startswith("prob_")]
    label_names = [c.replace("prob_", "") for c in prob_cols]

    if not label_names:
        raise ValueError(
            f"No prob_{{label}} columns found in {args.classified_csv} — "
            f"is this the output of classify_corpus.py?"
        )

    # ── Rebuild primary labels (same as original label_ columns, for clarity) ──
    for name in label_names:
        results_df[f"label_{name}"] = (
            results_df[f"prob_{name}"] >= args.primary_threshold
        ).astype(int)

    # ── Does each doc have ANY primary label? ────────────────────────────────────
    has_primary = results_df[[f"label_{n}" for n in label_names]].sum(axis=1) > 0

    # ── Secondary: between the two thresholds, only where a primary hit exists ──
    for name in label_names:
        prob = results_df[f"prob_{name}"]
        results_df[f"label_secondary_{name}"] = (
            has_primary
            & (prob >= args.secondary_threshold)
            & (prob < args.primary_threshold)
        ).astype(int)

    results_df["has_primary"] = has_primary.astype(int)
    results_df["has_secondary"] = (
        results_df[[f"label_secondary_{n}" for n in label_names]].sum(axis=1) > 0
    ).astype(int)

    # ── Human-readable combined label column ─────────────────────────────────────
    results_df["combined_labels"] = results_df.apply(
        lambda row: make_combined_label(row, label_names), axis=1
    )

    # ── Save ──────────────────────────────────────────────────────────────────────
    out_path = output_dir / "corpus_classified_two_threshold.csv"
    results_df.to_csv(out_path, index=False)

    # ── Summary ───────────────────────────────────────────────────────────────────
    total = len(results_df)
    print(f"Total documents: {total}")

    print(f"\nPrimary labels (>={args.primary_threshold}):")
    for name in label_names:
        n = int(results_df[f"label_{name}"].sum())
        print(f"  {name:20} {n:5d}  ({n / total * 100:.1f}%)")

    print(f"\nSecondary labels ({args.secondary_threshold}-{args.primary_threshold}, "
          f"only where primary exists):")
    for name in label_names:
        n = int(results_df[f"label_secondary_{name}"].sum())
        print(f"  {name:20} {n:5d}  ({n / total * 100:.1f}%)")

    n_primary_only = int(((results_df["has_primary"] == 1) & (results_df["has_secondary"] == 0)).sum())
    n_primary_and_secondary = int(((results_df["has_primary"] == 1) & (results_df["has_secondary"] == 1)).sum())
    n_unclassified = int((results_df["has_primary"] == 0).sum())

    print(f"\nDocs with primary only:        {n_primary_only}")
    print(f"Docs with primary + secondary: {n_primary_and_secondary}")
    print(f"Docs unclassified:             {n_unclassified}")

    logger.info(f"Saved -> {out_path}")


if __name__ == "__main__":
    main()

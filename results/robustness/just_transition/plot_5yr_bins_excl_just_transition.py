# Author: Katie Nutley
# Date: 2026-06-15
# Title: Excluding Just Transition

"""
plot_5yr_bins_excl_just_transition.py — Paradigm prevalence by 5-year period,
excluding "just transition" documents (validation exercise)

Validation check: re-runs the 5-year period bin analysis after removing any
document whose text mentions "just transition", to see whether that phrase is
driving the Post-Growth trend rather than genuine paradigm content. Compares
against the full (unfiltered) version — see plot_5yr_bins.py.

Usage:
    python plot_5yr_bins_excl_just_transition.py \
        --corpus_csv ./results/threshold_selection/corpus_classified_two_threshold.csv \
        --output_dir ./results/visualizations
"""

import argparse
import logging
from pathlib import Path

import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
import numpy as np
import pandas as pd

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

LABEL_NAMES = ["Admin-Only", "Green_Growth", "Neoclassical", "Post-Growth"]
PLOT_LABELS = ["Green_Growth", "Neoclassical", "Post-Growth"]

COLORS = {
    "Green_Growth": "#16A34A",
    "Neoclassical": "#2563EB",
    "Post-Growth": "#DC2626",
}
DISPLAY_NAMES = {
    "Green_Growth": "Green Growth",
    "Neoclassical": "Neoclassical",
    "Post-Growth": "Post-Growth",
}

REFERENCE_EVENTS = [(2008, "GFC", "grey"), (2019, "Green Deal", "black")]


def period_x(by_period: pd.DataFrame, year: int) -> int:
    """Return the x-axis index closest to a given year."""
    labels = by_period["period"].astype(str).tolist()
    starts = [int(l.split("\u2013")[0]) for l in labels]
    return int(np.argmin([abs(s - year) for s in starts]))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--corpus_csv", required=True,
                        help="Two-threshold classified corpus CSV "
                             "(label_{name} + label_secondary_{name} columns)")
    parser.add_argument("--exclude_phrase", default="just transition",
                        help="Documents whose text contains this phrase (case-insensitive) are excluded")
    parser.add_argument("--output_dir", default="./results/visualizations")
    args = parser.parse_args()

    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    df = pd.read_csv(args.corpus_csv)

    # ── Remove documents mentioning the exclude phrase ──────────────────────────
    if "text" in df.columns:
        mask = df["text"].str.contains(args.exclude_phrase, case=False, na=False)
        n_removed = int(mask.sum())
        df = df[~mask].copy()
        logger.info(f"Removed {n_removed} documents containing '{args.exclude_phrase}'")
    else:
        logger.warning(
            "'text' column not found — no documents removed. "
            f"Available columns: {df.columns.tolist()}"
        )

    df["year"] = pd.to_datetime(df["date"], errors="coerce").dt.year
    df = df[df["year"].notna()].copy()
    df["year"] = df["year"].astype(int)

    # ── Derive "either" columns (primary OR secondary label) ────────────────────
    for name in LABEL_NAMES:
        df[f"either_{name}"] = (
            (df[f"label_{name}"] == 1) | (df[f"label_secondary_{name}"] == 1)
        ).astype(int)

    df["n_either"] = df[[f"either_{n}" for n in LABEL_NAMES]].sum(axis=1)
    df["any_label"] = (df["n_either"] > 0).astype(int)

    # ── Assign 5-year bins ───────────────────────────────────────────────────────
    year_min, year_max = df["year"].min(), df["year"].max()
    bin_edges = list(range((year_min // 5) * 5, year_max + 5, 5))
    bin_labels = [f"{y}\u2013{y + 4}" for y in bin_edges[:-1]]

    df["period"] = pd.cut(df["year"], bins=bin_edges, labels=bin_labels,
                          right=False, include_lowest=True)

    # ── Aggregate by period ──────────────────────────────────────────────────────
    by_period = df.groupby("period", observed=True).agg(
        total=("celex", "count"),
        labelled=("any_label", "sum"),
        **{f"either_{n}": (f"either_{n}", "sum") for n in LABEL_NAMES},
    ).reset_index()

    for name in LABEL_NAMES:
        by_period[f"pct_{name}"] = by_period[f"either_{name}"] / by_period["total"] * 100

    print(by_period[["period", "total", "labelled"] +
                    [f"either_{n}" for n in LABEL_NAMES]].to_string(index=False))

    # ── Plots ────────────────────────────────────────────────────────────────────
    x = np.arange(len(by_period))
    fig, axes = plt.subplots(2, 1, figsize=(14, 10))
    suffix = f'\n[excl. documents mentioning "{args.exclude_phrase}"]'

    # Plot 1: raw counts
    ax1 = axes[0]
    for name in PLOT_LABELS:
        ax1.plot(x, by_period[f"either_{name}"],
                 label=DISPLAY_NAMES[name], color=COLORS[name],
                 linewidth=2, marker="o", markersize=5)
    ax1.set_title(f"Paradigm Prevalence by 5-Year Period (Raw Counts){suffix}",
                  fontsize=13, fontweight="bold")
    ax1.set_ylabel("Number of documents")
    ax1.set_xticks(x)
    ax1.set_xticklabels(by_period["period"], rotation=45, ha="right")
    ax1.legend()
    ax1.grid(True, alpha=0.3)

    # Plot 2: % of period corpus
    ax2 = axes[1]
    for name in PLOT_LABELS:
        ax2.plot(x, by_period[f"pct_{name}"],
                 label=DISPLAY_NAMES[name], color=COLORS[name],
                 linewidth=2, marker="o", markersize=5)
    ax2.set_title(f"Paradigm Prevalence by 5-Year Period (% of Period Corpus){suffix}",
                  fontsize=13, fontweight="bold")
    ax2.set_ylabel("% of documents")
    ax2.set_xlabel("Period")
    ax2.set_xticks(x)
    ax2.set_xticklabels(by_period["period"], rotation=45, ha="right")
    ax2.yaxis.set_major_formatter(mtick.PercentFormatter())
    ax2.legend()
    ax2.grid(True, alpha=0.3)

    # ── Reference lines (GFC, Green Deal) ────────────────────────────────────────
    for ax in axes:
        ymax = ax.get_ylim()[1]
        for year, label, color in REFERENCE_EVENTS:
            xpos = period_x(by_period, year)
            ax.axvline(x=xpos, color=color, linestyle="--", alpha=0.5)
            ax.text(xpos, ymax * 0.97, label, fontsize=8, color=color, ha="center")

    plt.tight_layout()
    out_path = output_dir / "paradigm_trends_5yr_bins_excl_just_transition.png"
    plt.savefig(out_path, dpi=150, bbox_inches="tight")
    plt.close()

    logger.info(f"Plot saved -> {out_path}")


if __name__ == "__main__":
    main()

# Author: Katie Nutley 
# Date: 2026-06-12
# Title: Finbert Robustness Check

"""
robustness_check.py — FinBERT vs Ensemble robustness check on the full corpus

Compares the 6-model Stage 2 ensemble's classification of the full corpus
against a single strong model (FinBERT) run alone, to check whether the
ensemble result is robust to relying on any one model. Reports corpus-level
label distribution differences, document-level agreement (exact label-set
match, per-label agreement, Pearson correlation of predicted probabilities),
and plots paradigm prevalence over time for both.

Usage:
    python robustness_check.py \
        --ensemble_csv ./classification/corpus_classified_ml.csv \
        --single_model_dir ./checkpoints/finbert-stage2-ml/best_model \
        --single_model_name finbert \
        --label_map ./stage2_finetuning/data_prep/outputs/label_mapping.json \
        --logits_cache ./classification/logits/logits_finbert.npy \
        --threshold 0.7 \
        --output_dir ./results/robustness
"""

import argparse
import json
import logging
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import torch
from scipy.stats import pearsonr
from tqdm.auto import tqdm
from transformers import AutoModelForSequenceClassification, AutoTokenizer

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def load_label_names(path: Path) -> list[str]:
    with open(path) as f:
        mapping = json.load(f)
    id2label = {int(k): v for k, v in mapping["id2label"].items()}
    return [id2label[i] for i in sorted(id2label.keys())]


def get_doc_logits(texts, tokenizer, model, device, batch_size=16) -> np.ndarray:
    all_logits = []
    for i in tqdm(range(0, len(texts), batch_size), desc="  batches", leave=False):
        batch = texts[i:i + batch_size]
        enc = tokenizer(batch, return_tensors="pt", padding=True,
                        truncation=True, max_length=512)
        with torch.no_grad():
            logits = model(**{k: v.to(device) for k, v in enc.items()}).logits
        all_logits.append(logits.cpu().numpy())
    return np.vstack(all_logits)


def get_single_model_logits(texts, model_dir, cache_path, device) -> np.ndarray:
    if cache_path.exists():
        logger.info(f"Loading cached logits: {cache_path}")
        return np.load(cache_path)

    logger.info(f"Running single model over corpus: {model_dir}")
    tokenizer = AutoTokenizer.from_pretrained(model_dir)
    model = AutoModelForSequenceClassification.from_pretrained(model_dir).to(device)
    model.eval()

    logits = get_doc_logits(texts, tokenizer, model, device)
    cache_path.parent.mkdir(parents=True, exist_ok=True)
    np.save(cache_path, logits)

    del model
    torch.cuda.empty_cache()
    return logits


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--ensemble_csv", required=True,
                        help="corpus_classified_ml.csv from classify_corpus.py")
    parser.add_argument("--single_model_dir", required=True)
    parser.add_argument("--single_model_name", required=True,
                        help="Short name for the single model, e.g. 'finbert'")
    parser.add_argument("--label_map", required=True)
    parser.add_argument("--logits_cache", required=True,
                        help="Path to cache/load the single model's logits (.npy)")
    parser.add_argument("--threshold", type=float, default=0.7)
    parser.add_argument("--output_dir", default="./results/robustness")
    args = parser.parse_args()

    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    label_names = load_label_names(Path(args.label_map))

    # ── Load ensemble results ───────────────────────────────────────────────────
    ensemble_df = pd.read_csv(args.ensemble_csv)

    # ── Get single-model logits (cached if available) ──────────────────────────
    single_logits = get_single_model_logits(
        ensemble_df["text"].tolist(),
        args.single_model_dir,
        Path(args.logits_cache),
        device,
    )
    single_probs = torch.sigmoid(torch.tensor(single_logits)).numpy()
    single_preds = (single_probs >= args.threshold).astype(int)

    # ── Build single-model results dataframe ────────────────────────────────────
    single_df = ensemble_df[["celex", "date", "text"]].copy()
    for i, name in enumerate(label_names):
        single_df[f"prob_{name}"] = single_probs[:, i]
        single_df[f"label_{name}"] = single_preds[:, i]
    single_df["predicted_labels_str"] = [
        "|".join([label_names[i] for i, v in enumerate(row) if v == 1]) or "None"
        for row in single_preds
    ]
    single_df["n_labels_predicted"] = single_preds.sum(axis=1)

    single_out_path = output_dir / f"corpus_classified_{args.single_model_name}.csv"
    single_df.to_csv(single_out_path, index=False)
    logger.info(f"Single-model classification -> {single_out_path}")

    # ── 1. Corpus-level label distribution comparison ──────────────────────────
    print("=" * 60)
    print("CORPUS-LEVEL LABEL DISTRIBUTIONS")
    print("=" * 60)
    print(f"{'Label':<20} {'Ensemble':>12} {args.single_model_name.capitalize():>12} {'Diff (pp)':>12}")
    print("-" * 60)

    distribution_rows = []
    for name in label_names:
        ens_pct = ensemble_df[f"label_{name}"].mean() * 100
        single_pct = single_df[f"label_{name}"].mean() * 100
        diff = single_pct - ens_pct
        print(f"{name:<20} {ens_pct:>11.1f}% {single_pct:>11.1f}% {diff:>+11.1f}pp")
        distribution_rows.append({
            "label": name,
            "ensemble_pct": round(ens_pct, 1),
            f"{args.single_model_name}_pct": round(single_pct, 1),
            "diff_pp": round(diff, 1),
        })

    ens_none = int((ensemble_df["n_labels_predicted"] == 0).sum())
    single_none = int((single_df["n_labels_predicted"] == 0).sum())
    print("\nNo label assigned:")
    print(f"  Ensemble: {ens_none} ({ens_none / len(ensemble_df) * 100:.1f}%)")
    print(f"  {args.single_model_name.capitalize()}:  {single_none} ({single_none / len(single_df) * 100:.1f}%)")

    # ── 2. Document-level agreement ─────────────────────────────────────────────
    print("\n" + "=" * 60)
    print("DOCUMENT-LEVEL AGREEMENT")
    print("=" * 60)

    ens_preds = np.stack([ensemble_df[f"label_{n}"].values for n in label_names], axis=1)
    single_preds_arr = np.stack([single_df[f"label_{n}"].values for n in label_names], axis=1)
    exact_match = float((ens_preds == single_preds_arr).all(axis=1).mean())
    print(f"Exact label-set agreement: {exact_match:.3f} ({exact_match * 100:.1f}%)")

    print(f"\n{'Label':<20} {'Agreement':>12} {'Pearson r':>12}")
    print("-" * 48)
    agreement_rows = []
    per_label_agreement = []
    for i, name in enumerate(label_names):
        agree = float((ens_preds[:, i] == single_preds_arr[:, i]).mean())
        r, _ = pearsonr(ensemble_df[f"prob_{name}"], single_df[f"prob_{name}"])
        print(f"{name:<20} {agree:>11.3f}  {r:>11.3f}")
        agreement_rows.append({"label": name, "agreement": round(agree, 3), "pearson_r": round(r, 3)})
        per_label_agreement.append(agree)

    mean_agreement = float(np.mean(per_label_agreement))

    # ── 3. Temporal trends comparison ───────────────────────────────────────────
    ensemble_df["year"] = pd.to_datetime(ensemble_df["date"], errors="coerce").dt.year
    single_df["year"] = pd.to_datetime(single_df["date"], errors="coerce").dt.year

    label_cols = [f"label_{n}" for n in label_names]
    yearly_ens = ensemble_df.groupby("year")[label_cols].mean() * 100
    yearly_single = single_df.groupby("year")[label_cols].mean() * 100

    fig, axes = plt.subplots(2, 2, figsize=(14, 10))
    axes = axes.flatten()
    colors = {"Ensemble": "#2563EB", "Single": "#DC2626"}

    for i, name in enumerate(label_names):
        ax = axes[i]
        col = f"label_{name}"
        ax.plot(yearly_ens.index, yearly_ens[col],
                label="Ensemble", color=colors["Ensemble"], linewidth=2)
        ax.plot(yearly_single.index, yearly_single[col],
                label=args.single_model_name.capitalize(), color=colors["Single"],
                linewidth=2, linestyle="--")
        ax.set_title(name.replace("_", " "), fontsize=13)
        ax.set_xlabel("Year")
        ax.set_ylabel("% documents labelled")
        ax.legend()
        ax.grid(True, alpha=0.3)

    plt.suptitle(f"Ensemble vs {args.single_model_name.capitalize()}: Paradigm Trends Over Time", fontsize=15)
    plt.tight_layout()

    plot_path = output_dir / "robustness_temporal_trends.png"
    plt.savefig(plot_path, dpi=150, bbox_inches="tight")
    plt.close()
    logger.info(f"Plot saved -> {plot_path}")

    # ── 4. Save CSV summary ─────────────────────────────────────────────────────
    summary_rows = [
        {"metric": "exact_label_set_agreement", "value": round(exact_match, 3)},
        {"metric": "mean_per_label_agreement", "value": round(mean_agreement, 3)},
        {"metric": "ensemble_no_label_pct", "value": round(ens_none / len(ensemble_df) * 100, 1)},
        {"metric": f"{args.single_model_name}_no_label_pct", "value": round(single_none / len(single_df) * 100, 1)},
    ]

    csv_path = output_dir / "robustness_check.csv"
    with open(csv_path, "w") as f:
        f.write("section,metric,value\n")
        for row in summary_rows:
            f.write(f"summary,{row['metric']},{row['value']}\n")
        f.write("\n")
        f.write("section,label,ensemble_pct,single_model_pct,diff_pp\n")
        for row in distribution_rows:
            single_key = f"{args.single_model_name}_pct"
            f.write(f"distribution,{row['label']},{row['ensemble_pct']},{row[single_key]},{row['diff_pp']}\n")
        f.write("\n")
        f.write("section,label,agreement,pearson_r\n")
        for row in agreement_rows:
            f.write(f"agreement,{row['label']},{row['agreement']},{row['pearson_r']}\n")

    logger.info(f"Robustness summary -> {csv_path}")

    print("\n" + "=" * 60)
    print("ROBUSTNESS SUMMARY")
    print("=" * 60)
    print(f"Exact label-set agreement:  {exact_match:.3f} ({exact_match * 100:.1f}%)")
    print(f"Mean per-label agreement:   {mean_agreement:.3f}")
    print("Temporal trend plots saved for visual inspection.")


if __name__ == "__main__":
    main()

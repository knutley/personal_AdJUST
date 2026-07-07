# Author: Katie Nutley
# Date: 2026-06-06
# Title: Classifying the Full Corpus with AdJUST Ensemble

"""
classify_corpus.py — Stage 2 multi-label ensemble classification of the full corpus

Runs all six fine-tuned Stage 2 (multi-label) checkpoints over the full corpus
(e.g. corpus_85th_percentile.csv), ensembles their predictions (averaged logits
-> sigmoid -> threshold), and writes a classified corpus CSV with per-class
probabilities, predicted labels, and label counts.

Per-model logits are cached to disk as they're computed, so an interrupted run
(e.g. a model crashing partway through) can be resumed without re-running
models that already completed.

Usage:
    python classify_corpus.py \
        --corpus_csv ./data_screening/substantive_relevance/corpus_85th_percentile.csv \
        --model_dirs \
            ./checkpoints/climatebert-stage2-ml/best_model \
            ./checkpoints/finbert-stage2-ml/best_model \
            ./checkpoints/roberta-stage2-ml/best_model \
            ./checkpoints/esgbert-stage2-ml/best_model \
            ./checkpoints/scibert-stage2-ml/best_model \
            ./checkpoints/deberta-stage2-ml/best_model \
        --label_map ./stage2_finetuning/data_prep/outputs/label_mapping.json \
        --output_dir ./classification \
        --threshold 0.7
"""

import argparse
import gc
import json
import logging
import shutil
from pathlib import Path

import numpy as np
import pandas as pd
import torch
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


def load_model(model_dir: str, device, local_cache_dir: Path = None):
    """Optionally stage the checkpoint to a local temp dir before loading
    (useful when model_dir is on a slow network mount, e.g. Google Drive)."""
    load_path = model_dir
    if local_cache_dir is not None:
        model_name = Path(model_dir).parent.name
        tmp = local_cache_dir / model_name
        if not tmp.exists():
            shutil.copytree(model_dir, tmp)
        load_path = str(tmp)

    tokenizer = AutoTokenizer.from_pretrained(load_path)
    model = AutoModelForSequenceClassification.from_pretrained(load_path).to(device)
    return tokenizer, model


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--corpus_csv", required=True,
                        help="CSV of documents to classify. Must have a 'text' column.")
    parser.add_argument("--model_dirs", nargs="+", required=True,
                        help="Paths to fine-tuned Stage 2 (multi-label) checkpoints")
    parser.add_argument("--label_map", required=True)
    parser.add_argument("--output_dir", default="./classification")
    parser.add_argument("--threshold", type=float, default=0.7)
    parser.add_argument("--batch_size", type=int, default=16)
    parser.add_argument("--stage_local_copy", action="store_true",
                        help="Copy each checkpoint to local disk before loading "
                             "(useful if model_dirs are on a slow network mount).")
    args = parser.parse_args()

    output_dir = Path(args.output_dir)
    logits_dir = output_dir / "logits"
    logits_dir.mkdir(parents=True, exist_ok=True)

    local_cache_dir = None
    if args.stage_local_copy:
        local_cache_dir = Path("/tmp/classify_corpus_models")
        local_cache_dir.mkdir(parents=True, exist_ok=True)

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    logger.info(f"Using device: {device}")

    # ── Labels + corpus ──────────────────────────────────────────────────────────
    label_names = load_label_names(Path(args.label_map))

    corpus = pd.read_csv(args.corpus_csv)
    corpus_clean = corpus[corpus["text"].notna()].copy().reset_index(drop=True)
    texts = corpus_clean["text"].tolist()
    logger.info(f"Classifying {len(texts)} documents...")

    torch.cuda.empty_cache()
    gc.collect()

    # ── Run each model, caching logits as we go ─────────────────────────────────
    model_names = [Path(d).parent.name for d in args.model_dirs]
    all_logits = []

    for model_name, model_dir in zip(model_names, args.model_dirs):
        cache_path = logits_dir / f"logits_{model_name}.npy"

        if cache_path.exists():
            logger.info(f"Loading cached logits: {model_name}")
            all_logits.append(np.load(cache_path))
            continue

        logger.info(f"Running {model_name}...")
        tokenizer, model = load_model(model_dir, device, local_cache_dir)
        model.eval()

        logits = get_doc_logits(texts, tokenizer, model, device, batch_size=args.batch_size)
        np.save(cache_path, logits)
        all_logits.append(logits)
        logger.info(f"  done — shape {logits.shape}")

        del model
        torch.cuda.empty_cache()
        gc.collect()

    # ── Ensemble: average logits, then sigmoid, then threshold ─────────────────
    ensemble_logits = np.mean(all_logits, axis=0)
    ensemble_probs = torch.sigmoid(torch.tensor(ensemble_logits)).numpy()
    ensemble_preds = (ensemble_probs >= args.threshold).astype(int)

    # ── Build results dataframe ──────────────────────────────────────────────────
    results_df = corpus_clean.copy()
    for i, name in enumerate(label_names):
        results_df[f"prob_{name}"] = ensemble_probs[:, i]
        results_df[f"label_{name}"] = ensemble_preds[:, i]

    results_df["predicted_labels_str"] = [
        "|".join([label_names[i] for i, v in enumerate(row) if v == 1]) or "None"
        for row in ensemble_preds
    ]
    results_df["n_labels_predicted"] = ensemble_preds.sum(axis=1)

    # ── Save & summarise ─────────────────────────────────────────────────────────
    out_path = output_dir / "corpus_classified_ml.csv"
    results_df.to_csv(out_path, index=False)

    print(f"\nTotal documents: {len(results_df)}")
    for name in label_names:
        count = int(results_df[f"label_{name}"].sum())
        print(f"  {name:15} {count:4d}  ({count / len(results_df) * 100:.1f}%)")
    print("\nDocuments by number of labels:")
    print(results_df["n_labels_predicted"].value_counts().sort_index().to_string())
    print(f"\nSaved -> {out_path}")


if __name__ == "__main__":
    main()

# Author: Katelyn Nutley 
# Date: 2026-06-01
# Title: Benchmark: GPT-4o vs Llama 3.3 vs AdJUST Ensemble

"""
llm_comparison.py — Benchmark: GPT-4o vs Llama 3.3 vs AdJUST Ensemble

Compares zero/few-shot prompting of general-purpose LLMs against the
fine-tuned Stage 2 ensemble on the same held-out test set.

Requirements:
    pip install openai groq torch scikit-learn pandas numpy

API keys required (set as environment variables, NEVER hardcoded):
    export OPENAI_API_KEY="..."   # https://platform.openai.com/api-keys
    export GROQ_API_KEY="..."     # https://console.groq.com/keys (free tier)

Usage:
    python llm_comparison.py \
        --train_csv ./stage2_finetuning/data_prep/outputs/clf_train.csv \
        --test_csv  ./stage2_finetuning/data_prep/outputs/clf_test.csv \
        --ensemble_logits ./classification/logits \
        --output_dir ./results/baseline_comparison
"""

import argparse
import json
import logging
import os
import time
from pathlib import Path

import numpy as np
import pandas as pd
import torch
from sklearn.metrics import accuracy_score, classification_report, f1_score

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

LABEL_NAMES = ["Admin-Only", "Green_Growth", "Neoclassical", "Post-Growth"]

SYSTEM_PROMPT = """You are an expert in political economy and climate policy discourse.
Your task is to classify EU policy documents according to the economic paradigm(s) they express.

The four paradigm labels are:
- Admin-Only: Administrative or procedural content with no substantive economic paradigm
- Green_Growth: Green Keynesianism or evolutionary economics — state investment, green industrial policy, decarbonisation through growth
- Neoclassical: Market-based mechanisms, carbon pricing, cost-benefit analysis, efficiency
- Post-Growth: Degrowth, planetary boundaries, sufficiency, limits to growth

A document can have MULTIPLE labels if it expresses multiple paradigms.

Respond ONLY with a JSON object in this exact format:
{"labels": ["Label1", "Label2"]}

Use only labels from the list above. If no paradigm is present, use {"labels": ["Admin-Only"]}."""


# ── Few-shot example construction ─────────────────────────────────────────────

def get_few_shot_examples(train_df: pd.DataFrame, n_per_class: int = 2):
    examples = []
    for i, label_name in enumerate(LABEL_NAMES):
        subset = train_df[train_df["label_vector_parsed"].apply(lambda v: v[i] == 1)]
        for _, row in subset.head(n_per_class).iterrows():
            active = [LABEL_NAMES[j] for j, v in enumerate(row["label_vector_parsed"]) if v == 1]
            examples.append((row["text"][:500], active))
    return examples


def build_user_prompt(text: str, examples: list) -> str:
    prompt = "Here are some labelled examples:\n\n"
    for ex_text, ex_labels in examples[:8]:  # limit to 8 examples
        prompt += f"Document: {ex_text[:300]}...\n"
        prompt += f"Labels: {json.dumps(ex_labels)}\n\n"
    prompt += f"Now classify this document:\n\nDocument: {text[:800]}\n\nLabels:"
    return prompt


def parse_response(response_text: str) -> list:
    """Parse LLM response into a binary label vector."""
    try:
        start = response_text.find("{")
        end = response_text.rfind("}") + 1
        if start == -1:
            return [0, 0, 0, 0]
        obj = json.loads(response_text[start:end])
        labels = obj.get("labels", [])
        return [1 if name in labels else 0 for name in LABEL_NAMES]
    except Exception:
        return [0, 0, 0, 0]


# ── Model runners ──────────────────────────────────────────────────────────────

def run_gpt4o(client, texts: list, examples: list, sleep: float = 0.5) -> np.ndarray:
    preds = []
    for i, text in enumerate(texts):
        try:
            response = client.chat.completions.create(
                model="gpt-4o",
                messages=[
                    {"role": "system", "content": SYSTEM_PROMPT},
                    {"role": "user", "content": build_user_prompt(text, examples)},
                ],
                temperature=0,
                max_tokens=50,
            )
            pred = parse_response(response.choices[0].message.content)
        except Exception as e:
            logger.warning(f"GPT-4o error on doc {i}: {e}")
            pred = [0, 0, 0, 0]
        preds.append(pred)
        if (i + 1) % 5 == 0:
            logger.info(f"GPT-4o: {i + 1}/{len(texts)}")
        time.sleep(sleep)
    return np.array(preds)


def run_llama(client, texts: list, examples: list, sleep: float = 0.3) -> np.ndarray:
    preds = []
    for i, text in enumerate(texts):
        try:
            response = client.chat.completions.create(
                model="llama-3.3-70b-versatile",
                messages=[
                    {"role": "system", "content": SYSTEM_PROMPT},
                    {"role": "user", "content": build_user_prompt(text, examples)},
                ],
                temperature=0,
                max_tokens=50,
            )
            pred = parse_response(response.choices[0].message.content)
        except Exception as e:
            logger.warning(f"Llama error on doc {i}: {e}")
            pred = [0, 0, 0, 0]
        preds.append(pred)
        if (i + 1) % 5 == 0:
            logger.info(f"Llama 3.3: {i + 1}/{len(texts)}")
        time.sleep(sleep)
    return np.array(preds)


# ── Evaluation ─────────────────────────────────────────────────────────────────

def evaluate(true: np.ndarray, pred: np.ndarray, name: str) -> dict:
    exact = accuracy_score(true, pred)
    macro = f1_score(true, pred, average="macro", zero_division=0)
    micro = f1_score(true, pred, average="micro", zero_division=0)

    print(f"\n{'=' * 60}")
    print(name)
    print("=" * 60)
    print(f"Exact match: {exact:.4f}  Macro F1: {macro:.4f}  Micro F1: {micro:.4f}")
    print(classification_report(true, pred, target_names=LABEL_NAMES, zero_division=0))

    return {"name": name, "exact_match": round(exact, 4),
            "macro_f1": round(macro, 4), "micro_f1": round(micro, 4)}


def load_ensemble_preds(logits_dir: Path, n_test: int, threshold: float) -> np.ndarray:
    """Average pre-computed per-model logits and threshold at `threshold`."""
    logit_files = sorted(logits_dir.glob("logits_*.npy"))
    if not logit_files:
        raise FileNotFoundError(f"No logits_*.npy files found in {logits_dir}")

    all_logits = [np.load(f) for f in logit_files]
    for f, arr in zip(logit_files, all_logits):
        if arr.shape[0] != n_test:
            logger.warning(
                f"{f.name} has {arr.shape[0]} rows, expected {n_test} "
                f"(test set size) — check this is the right logits set."
            )

    ensemble_logits = np.mean(all_logits, axis=0)
    ensemble_probs = torch.sigmoid(torch.tensor(ensemble_logits)).numpy()
    return (ensemble_probs >= threshold).astype(int)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--train_csv", required=True)
    parser.add_argument("--test_csv", required=True)
    parser.add_argument("--ensemble_logits", required=True,
                        help="Directory containing precomputed logits_*.npy files "
                             "for the AdJUST ensemble, aligned with --test_csv rows.")
    parser.add_argument("--output_dir", default="./results/baseline_comparison")
    parser.add_argument("--threshold", type=float, default=0.5)
    parser.add_argument("--n_shot_per_class", type=int, default=2)
    parser.add_argument("--gpt_sleep", type=float, default=0.5)
    parser.add_argument("--llama_sleep", type=float, default=0.3)
    args = parser.parse_args()

    # ── API keys from environment only — never hardcode these ─────────────────
    openai_key = os.environ.get("OPENAI_API_KEY")
    groq_key = os.environ.get("GROQ_API_KEY")
    if not openai_key or not groq_key:
        raise EnvironmentError(
            "Set OPENAI_API_KEY and GROQ_API_KEY as environment variables "
            "before running this script. Never hardcode API keys in source files."
        )

    from openai import OpenAI
    from groq import Groq

    openai_client = OpenAI(api_key=openai_key)
    groq_client = Groq(api_key=groq_key)

    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    # ── Load data ────────────────────────────────────────────────────────────────
    train = pd.read_csv(args.train_csv)
    test = pd.read_csv(args.test_csv)

    train["label_vector_parsed"] = train["label_vector"].apply(json.loads)
    test_texts = test["text"].tolist()
    test_labels = np.array(test["label_vector"].apply(json.loads).tolist())

    few_shot_examples = get_few_shot_examples(train, n_per_class=args.n_shot_per_class)

    # ── Run LLMs ─────────────────────────────────────────────────────────────────
    logger.info("Running GPT-4o on test set...")
    gpt4o_preds = run_gpt4o(openai_client, test_texts, few_shot_examples, sleep=args.gpt_sleep)

    logger.info("Running Llama 3.3 on test set...")
    llama_preds = run_llama(groq_client, test_texts, few_shot_examples, sleep=args.llama_sleep)

    # ── AdJUST ensemble, from precomputed logits (not re-run here) ────────────────
    ensemble_preds = load_ensemble_preds(
        Path(args.ensemble_logits), n_test=len(test_texts), threshold=args.threshold
    )

    # ── Evaluate all three ───────────────────────────────────────────────────────
    results = [
        evaluate(test_labels, gpt4o_preds, "GPT-4o (few-shot)"),
        evaluate(test_labels, llama_preds, "Llama 3.3 (few-shot)"),
        evaluate(test_labels, ensemble_preds, "AdJUST Ensemble"),
    ]

    # ── Summary table ────────────────────────────────────────────────────────────
    summary = pd.DataFrame(results)[["name", "exact_match", "macro_f1", "micro_f1"]]
    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    print(summary.to_string(index=False))

    summary_path = output_dir / "llm_comparison_summary.csv"
    summary.to_csv(summary_path, index=False)
    logger.info(f"Summary -> {summary_path}")

    # ── Save per-document predictions ───────────────────────────────────────────
    out = test[["text", "label_vector"]].copy()
    out["gpt4o_pred"] = [json.dumps(r.tolist()) for r in gpt4o_preds]
    out["llama_pred"] = [json.dumps(r.tolist()) for r in llama_preds]
    out["ensemble_pred"] = [json.dumps(r.tolist()) for r in ensemble_preds]

    predictions_path = output_dir / "llm_comparison.csv"
    out.to_csv(predictions_path, index=False)
    logger.info(f"Predictions -> {predictions_path}")


if __name__ == "__main__":
    main()

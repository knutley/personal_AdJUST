# Author: Katie Nutley
# Date: 2026-04-18
# Title: Domain Pre-Training for Meckling and Allan Data

"""
data_prep.py
Prepares the Meckling CSV for:
  1. Domain-Adaptive Pre-Training (DAPT) — plain text corpus for MLM
  2. Classification fine-tuning — labelled examples from matched_tag

Usage:
    python data_prep.py --input cleaned_meckling_data.csv --output_dir ./data
"""

import argparse
import re
import pandas as pd
from pathlib import Path
from sklearn.model_selection import train_test_split


# ── AdJUST taxonomy mapping ──────────────────────────────────────────────────
# Maps Meckling & Allan's discourse_label values → AdJUST project categories.
# "General Concepts" is dropped (repurposed as retrieval keywords, not a class).
# "Green Growth" maps to a placeholder pending the demand/supply split that
# lives only in the RA-annotated EU corpus (Keynesianism vs. Evolutionary Econ).
ADJUST_MAP = {
    "neoclassical":     "Neoclassical",
    "limits to growth": "Post-Growth",
    "green growth":     "Green_Growth",
    "general concepts": None,   # excluded — used as keywords, not a class
}


def clean_text(text: str) -> str:
    """Strip artefacts, normalise whitespace."""
    if not isinstance(text, str):
        return ""
    # Remove parenthetical page numbers like (8), (54)
    text = re.sub(r"\(\d+\)", "", text)
    # Collapse whitespace / newlines
    text = re.sub(r"\s+", " ", text)
    return text.strip()


def build_input_text(row: pd.Series) -> str:
    """
    Concatenate phrase → sentence → paragraph with separator tokens.
    Gives the model multi-granularity context. Truncation happens
    inside the tokeniser during training.
    """
    parts = []
    for col in ["referenced_text", "full_sentence_context", "full_paragraph_context"]:
        val = clean_text(row.get(col, ""))
        if val:
            parts.append(val)
    # Use [SEP] as explicit boundary (tokeniser will re-encode it correctly)
    return " [SEP] ".join(parts)


def prepare_classification_data(df: pd.DataFrame, output_dir: Path) -> dict:
    """
    Build a labelled DataFrame using discourse_label, remapped to the
    AdJUST project taxonomy. Returns label2id and id2label mappings.
    """
    df = df.copy()

    # Normalise discourse_label: lowercase for matching, strip whitespace
    df["discourse_clean"] = (
        df["discourse_label"]
        .astype(str)
        .str.strip()
        .str.lower()
    )

    # Drop nulls / empty / "nan"
    df = df[~df["discourse_clean"].isin(["", "nan", "na"])].copy()

    print(f"\n[CLF] Raw discourse_label distribution (before mapping):")
    print(df["discourse_clean"].value_counts().to_string())

    # Apply AdJUST taxonomy mapping
    df["label_name"] = df["discourse_clean"].map(ADJUST_MAP)

    # Report anything that didn't match (so you can extend ADJUST_MAP)
    unmapped = df[df["label_name"].isna() & df["discourse_clean"].notna()]
    if len(unmapped) > 0:
        print(f"\n[CLF] ⚠️  Unmapped discourse labels (add to ADJUST_MAP if needed):")
        print(unmapped["discourse_clean"].value_counts().to_string())

    # Drop General Concepts (mapped to None) and any still-unmapped rows
    df = df[df["label_name"].notna()].copy()

    print(f"\n[CLF] AdJUST label distribution (after mapping):")
    print(df["label_name"].value_counts().to_string())
    print(f"\n[CLF] Total labelled examples: {len(df)}")

    # Build label vocabulary (sorted for reproducibility)
    label_names = sorted(df["label_name"].unique())
    label2id    = {l: i for i, l in enumerate(label_names)}
    id2label    = {i: l for l, i in label2id.items()}

    df["label"] = df["label_name"].map(label2id)
    df["text"]  = df.apply(build_input_text, axis=1)

    # Drop rows with empty text
    df = df[df["text"].str.len() > 20].copy()

    print(f"[CLF] Total labelled examples: {len(df)}")
    print(f"[CLF] Label distribution:\n{df['label_name'].value_counts()}\n")

    # Stratified split: 80 / 10 / 10
    train, temp = train_test_split(
        df[["text", "label", "label_name"]],
        test_size=0.2,
        stratify=df["label"],
        random_state=42,
    )
    val, test = train_test_split(
        temp,
        test_size=0.5,
        stratify=temp["label"],
        random_state=42,
    )

    for split, data in [("train", train), ("val", val), ("test", test)]:
        path = output_dir / f"clf_{split}.csv"
        data.to_csv(path, index=False)
        print(f"[CLF] {split}: {len(data)} examples → {path}")

    return label2id, id2label


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--input",      required=True, help="Path to cleaned_meckling_data.csv")
    parser.add_argument("--output_dir", default="./data")
    args = parser.parse_args()

    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    df = pd.read_csv(args.input, on_bad_lines="skip")
    print(f"Loaded {len(df)} rows, columns: {list(df.columns)}")

    # Classification splits
    label2id, id2label = prepare_classification_data(df, output_dir)

    import json
    mapping = {"label2id": label2id, "id2label": id2label}
    with open(output_dir / "label_mapping.json", "w") as f:
        json.dump(mapping, f, indent=2)
    print(f"\n[CLF] Label mapping saved → {output_dir / 'label_mapping.json'}")


if __name__ == "__main__":
    main()

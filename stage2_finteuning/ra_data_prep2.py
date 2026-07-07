"""
ra_data_prep2.py
Merges RA and one or more PI annotated EU pre-legislative documents into
training splits for Stage 2 multi-label fine-tuning.

Label schema (4-class, multi-label):
    Admin-Only | Green_Growth | Neoclassical | Post-Growth

Usage (Google Colab):
    !python /content/drive/MyDrive/AdJUST/data/ra_data_prep2.py \
        --ra_file    /content/drive/MyDrive/AdJUST/data/ra_total_coded.csv \
        --pi_files   /content/drive/MyDrive/AdJUST/data/marion_total.csv \
                     /content/drive/MyDrive/AdJUST/data/fergus_total.csv \
        --output_dir /content/drive/MyDrive/AdJUST/ra_data

NOTE: pi_files order matters. With keep="last", the last file listed wins
for duplicate IDs. Pass marion first and fergus second so that Fergus labels
are kept wherever both PIs coded the same document.
"""

import argparse
import json
import re
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split

LABEL_MAP = {
    "neoclassical":           "Neoclassical",
    "admin-only":             "Admin-Only",
    "admin only":             "Admin-Only",
    "post-growth":            "Post-Growth",
    "post growth":            "Post-Growth",
    "gg-keynesianism":        "Green_Growth",
    "gg keynesianism":        "Green_Growth",
    "keynesianism":           "Green_Growth",
    "gg-evolutionary":        "Green_Growth",
    "gg evolutionary":        "Green_Growth",
    "evolutionary economics": "Green_Growth",
}

DROP_LABELS = {"regulating for behaviour change"}

LABEL_NAMES = ["Admin-Only", "Green_Growth", "Neoclassical", "Post-Growth"]
LABEL2ID    = {l: i for i, l in enumerate(LABEL_NAMES)}
ID2LABEL    = {i: l for i, l in enumerate(LABEL_NAMES)}


def clean_text(text):
    if not isinstance(text, str):
        return ""
    return re.sub(r"\s+", " ", text).strip()


def normalise_label(raw):
    if not isinstance(raw, str):
        return None
    key = raw.strip().lower()
    if key in DROP_LABELS:
        return None
    return LABEL_MAP.get(key, None)


def load_file(filepath):
    path = Path(filepath)
    if path.suffix.lower() in (".xlsx", ".xlsm", ".xls"):
        return pd.read_excel(filepath, sheet_name="Labelling Data")
    return pd.read_csv(filepath)


def build_label_vector(row):
    raw_labels = [row.get("Label"), row.get("Secondary Label"), row.get("Tertiary Label")]
    canonical = set()
    for raw in raw_labels:
        mapped = normalise_label(raw)
        if mapped:
            canonical.add(mapped)
    if not canonical:
        return None
    vector = [0] * len(LABEL_NAMES)
    for label in canonical:
        vector[LABEL2ID[label]] = 1
    return vector


def load_annotations(filepath, source_label):
    df = load_file(filepath)
    df = df[df["Relevant"].astype(str).str.strip().str.lower() == "yes"].copy()

    for raw in sorted(df["Label"].dropna().astype(str).str.strip().str.lower().unique()):
        if raw not in LABEL_MAP and raw not in DROP_LABELS:
            print(f"  ⚠️  [{source_label}] Unmapped label '{raw}' — dropping these rows")

    df["label_vector"] = df.apply(build_label_vector, axis=1)
    df = df[df["label_vector"].notna()].copy()
    df["text"] = df["text"].apply(clean_text)
    df = df[df["text"].str.len() > 50].copy()
    df["source"] = source_label
    df["label_names"] = df["label_vector"].apply(
        lambda v: [LABEL_NAMES[i] for i, x in enumerate(v) if x == 1]
    )
    df["n_labels"] = df["label_vector"].apply(sum)

    return df[["ID", "text", "label_vector", "label_names", "n_labels", "source"]].rename(
        columns={"ID": "id"}
    )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--ra_file",    required=True)
    parser.add_argument("--pi_files",   nargs="+", required=True)
    parser.add_argument("--output_dir", default="./ra_data")
    parser.add_argument("--test_size",  type=float, default=0.15)
    parser.add_argument("--seed",       type=int,   default=42)
    args = parser.parse_args()

    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    print(f"Loading RA: {args.ra_file}")
    ra_df = load_annotations(args.ra_file, source_label="RA")
    print(f"  → {len(ra_df)} usable rows")
    print(f"  → {(ra_df['n_labels'] > 1).sum()} multi-label documents")

    pi_frames = []
    for pi_file in args.pi_files:
        label = f"PI_{Path(pi_file).stem}"
        print(f"Loading PI: {pi_file}")
        pi_df = load_annotations(pi_file, source_label=label)
        print(f"  → {len(pi_df)} usable rows")
        print(f"  → {(pi_df['n_labels'] > 1).sum()} multi-label documents")
        pi_frames.append(pi_df)

    if len(pi_frames) > 1:
        pi_combined = pd.concat(pi_frames, ignore_index=True)
        pi_combined = pi_combined.drop_duplicates(subset="id", keep="last")
        print(f"\nCombined PI files: {len(pi_combined)} unique documents")
    else:
        pi_combined = pi_frames[0]

    ra_pi_overlap = set(ra_df["id"]) & set(pi_combined["id"])
    if ra_pi_overlap:
        print(f"  ⚠️  {len(ra_pi_overlap)} IDs overlap between RA and PI — keeping PI version")
        ra_df = ra_df[~ra_df["id"].isin(ra_pi_overlap)]

    df = pd.concat([ra_df, pi_combined], ignore_index=True)
    print(f"\nFinal merged dataset: {len(df)} rows")

    print(f"\nPer-label counts (multi-label, so sums to > n_docs):")
    for i, name in enumerate(LABEL_NAMES):
        count = df["label_vector"].apply(lambda v: v[i]).sum()
        print(f"  {name:15} {int(count):4d}")

    print(f"\nDocuments by number of labels:")
    print(df["n_labels"].value_counts().sort_index().to_string())

    print(f"\nSource distribution:")
    print(df["source"].value_counts().to_string())

    df["stratify_label"] = df["label_vector"].apply(
        lambda v: next((i for i, x in enumerate(v) if x == 1), 0)
    )

    min_count = df["stratify_label"].value_counts().min()
    stratify_col = df["stratify_label"] if min_count >= 2 else None
    if stratify_col is None:
        print("  ⚠️  Falling back to non-stratified split")

    train, temp = train_test_split(
        df, test_size=args.test_size * 2, stratify=stratify_col, random_state=args.seed,
    )
    temp_stratify = temp["stratify_label"] if min_count >= 2 else None
    val, test = train_test_split(
        temp, test_size=0.5, stratify=temp_stratify, random_state=args.seed,
    )

    for split_name, data in [("train", train), ("val", val), ("test", test)]:
        out = data[["text", "label_vector", "label_names"]].copy()
        out["label_vector"] = out["label_vector"].apply(json.dumps)
        out["label_names"]  = out["label_names"].apply(json.dumps)
        path = output_dir / f"clf_{split_name}.csv"
        out.to_csv(path, index=False)
        print(f"\n[{split_name}]: {len(data)} examples → {path}")
        for i, name in enumerate(LABEL_NAMES):
            count = data["label_vector"].apply(lambda v: v[i]).sum()
            print(f"  {name:15} {int(count):3d}")

    mapping = {"label2id": LABEL2ID, "id2label": ID2LABEL,
               "label_names": LABEL_NAMES, "multilabel": True}
    with open(output_dir / "label_mapping.json", "w") as f:
        json.dump(mapping, f, indent=2)
    print(f"\nLabel mapping saved → {output_dir / 'label_mapping.json'}")
    print(f"Labels: {LABEL_NAMES}")


if __name__ == "__main__":
    main()
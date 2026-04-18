#!/usr/bin/env bash
# run_pipeline.sh  —  Full DAPT → Fine-tune → Compare pipeline
# Edit the variables at the top, then: bash run_pipeline.sh

set -euo pipefail

# ── Config ─────────────────────────────────────────────────────────────────────
DATA_CSV="./cleaned_meckling_data.csv"    # your input
DATA_DIR="./data"
CKPT_DIR="./checkpoints"
RESULTS_DIR="./results"

# Set to true if you have a GPU with FP16 support (V100, A100, etc.)
USE_FP16="--fp16"
# USE_FP16=""  # Comment above and uncomment this if CPU-only

# ── Step 0: Install dependencies ──────────────────────────────────────────────
echo "Installing dependencies..."
pip install -q \
    transformers>=4.40.0 \
    datasets>=2.18.0 \
    accelerate>=0.28.0 \
    scikit-learn \
    pandas \
    torch

# ── Step 1: Prepare data ──────────────────────────────────────────────────────
echo ""
echo "=== Step 1: Data Preparation ==="
python data_prep.py \
    --input "$DATA_CSV" \
    --output_dir "$DATA_DIR"

# ── Step 2: Stage 1 — Fine-tune on Meckling & Allan data ──────────────────────
echo ""
echo "=== Step 2: Stage 1 Fine-tuning (Meckling) ==="

declare -A HF_NAMES=(
    [climatebert]="climatebert/distilroberta-base-climate-f"
    [roberta]="roberta-base"
    [deberta]="microsoft/deberta-v3-base"
)

for MODEL_ALIAS in climatebert roberta deberta; do
    HF_NAME="${HF_NAMES[$MODEL_ALIAS]}"
    echo ""
    echo "--- Stage 1: $MODEL_ALIAS ---"

    python finetune.py \
        --model_checkpoint "$HF_NAME" \
        --data_dir         "$DATA_DIR" \
        --output_dir       "$CKPT_DIR/${MODEL_ALIAS}-stage1" \
        --num_train_epochs 5 \
        --learning_rate    2e-5 \
        --patience         3 \
        $USE_FP16
done

# ── Step 3: Report Stage 1 results ────────────────────────────────────────────
echo ""
echo "=== Step 3: Stage 1 Comparison ==="

python compare_models.py \
    --model_dirs \
        "$CKPT_DIR/climatebert-stage1/best_model" \
        "$CKPT_DIR/roberta-stage1/best_model" \
        "$CKPT_DIR/deberta-stage1/best_model" \
    --test_csv  "$DATA_DIR/clf_test.csv" \
    --label_map "$DATA_DIR/label_mapping.json" \
    --output_dir "$RESULTS_DIR/stage1"

# ── Step 4: Stage 2 — Fine-tune on RA EU pre-legislative data ─────────────────
echo ""
echo "=== Step 4: Stage 2 Fine-tuning (RA EU data) ==="

RA_DATA_DIR="./ra_data"   # ← point this at your RA splits when ready

for MODEL_ALIAS in climatebert roberta deberta; do
    echo ""
    echo "--- Stage 2: $MODEL_ALIAS ---"

    python finetune.py \
        --model_checkpoint "$CKPT_DIR/${MODEL_ALIAS}-stage1/best_model" \
        --data_dir         "$DATA_DIR" \
        --ra_data_dir      "$RA_DATA_DIR" \
        --label_map        "$RA_DATA_DIR/label_mapping.json" \
        --output_dir       "$CKPT_DIR/${MODEL_ALIAS}-stage2" \
        --num_train_epochs 5 \
        --learning_rate    2e-5 \
        --patience         3 \
        $USE_FP16
done

# ── Step 5: Report Stage 2 results ────────────────────────────────────────────
echo ""
echo "=== Step 5: Stage 2 Comparison ==="

python compare_models.py \
    --model_dirs \
        "$CKPT_DIR/climatebert-stage2/best_model" \
        "$CKPT_DIR/roberta-stage2/best_model" \
        "$CKPT_DIR/deberta-stage2/best_model" \
    --test_csv  "$RA_DATA_DIR/clf_test.csv" \
    --label_map "$RA_DATA_DIR/label_mapping.json" \
    --output_dir "$RESULTS_DIR/stage2"

echo ""
echo "✔ Pipeline complete."
echo "  Stage 1 results: $RESULTS_DIR/stage1/model_comparison.csv"
echo "  Stage 2 results: $RESULTS_DIR/stage2/model_comparison.csv"

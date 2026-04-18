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

# ── Step 2: DAPT for each model ───────────────────────────────────────────────
echo ""
echo "=== Step 2: Domain-Adaptive Pre-Training ==="

for MODEL_ALIAS in climatebert roberta deberta; do
    case $MODEL_ALIAS in
        climatebert) HF_NAME="climatebert/distilroberta-base-climate-f" ;;
        roberta)     HF_NAME="roberta-base" ;;
        deberta)     HF_NAME="microsoft/deberta-v3-base" ;;
    esac

    CKPT_OUT="$CKPT_DIR/${MODEL_ALIAS}-dapt"
    echo ""
    echo "--- DAPT: $MODEL_ALIAS ($HF_NAME) ---"

    python dapt.py \
        --model_name          "$HF_NAME" \
        --corpus_file         "$DATA_DIR/dapt_corpus.txt" \
        --output_dir          "$CKPT_OUT" \
        --num_train_epochs    3 \
        --per_device_train_batch_size 16 \
        --gradient_accumulation_steps 2 \
        --learning_rate       5e-5 \
        $USE_FP16
done

# ── Step 3: Fine-tune classification on each DAPT checkpoint ──────────────────
echo ""
echo "=== Step 3: Classification Fine-tuning ==="

for MODEL_ALIAS in climatebert roberta deberta; do
    DAPT_CKPT="$CKPT_DIR/${MODEL_ALIAS}-dapt"
    CLF_OUT="$CKPT_DIR/${MODEL_ALIAS}-clf"
    echo ""
    echo "--- Fine-tuning: $MODEL_ALIAS ---"

    python finetune.py \
        --model_checkpoint    "$DAPT_CKPT" \
        --data_dir            "$DATA_DIR" \
        --output_dir          "$CLF_OUT" \
        --num_train_epochs    5 \
        --per_device_train_batch_size 16 \
        --learning_rate       2e-5 \
        --patience            3 \
        $USE_FP16

    # If using RA-annotated data for fine-tuning instead:
    # --ra_data_dir ./ra_annotated_data \
done

# ── Step 4: Compare all models ────────────────────────────────────────────────
echo ""
echo "=== Step 4: Model Comparison ==="

python compare_models.py \
    --model_dirs \
        "$CKPT_DIR/climatebert-clf/best_model" \
        "$CKPT_DIR/roberta-clf/best_model" \
        "$CKPT_DIR/deberta-clf/best_model" \
    --test_csv   "$DATA_DIR/clf_test.csv" \
    --label_map  "$DATA_DIR/label_mapping.json" \
    --output_dir "$RESULTS_DIR"

echo ""
echo "✔ Pipeline complete. Results in: $RESULTS_DIR"
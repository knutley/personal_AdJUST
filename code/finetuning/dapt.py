"""
dapt.py  —  Domain-Adaptive Pre-Training (MLM) 
Runs continued pre-training on the Meckling corpus for:
  - ClimateBERT  (climatebert/distilroberta-base-climate-f)
  - RoBERTa-base (roberta-base)
  - DeBERTa-v3   (microsoft/deberta-v3-base)

Usage:
    python dapt.py \
        --model_name climatebert/distilroberta-base-climate-f \
        --corpus_file ./data/dapt_corpus.txt \
        --output_dir ./checkpoints/climatebert-dapt \
        --num_train_epochs 3 \
        --per_device_train_batch_size 16
"""

import argparse
import math
import os
import logging
from pathlib import Path

from datasets import load_dataset
from transformers import (
    AutoTokenizer,
    AutoModelForMaskedLM,
    DataCollatorForLanguageModeling,
    TrainingArguments,
    Trainer,
    set_seed,
)

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# ── Model aliases for convenience ───────────────────────────────────────────────
MODEL_ALIASES = {
    "climatebert": "climatebert/distilroberta-base-climate-f",
    "roberta":     "roberta-base",
    "deberta":     "microsoft/deberta-v3-base",
}


def tokenize_function(examples, tokenizer, max_length: int = 512):
    """
    Tokenise raw text. We use return_special_tokens_mask=True so the
    DataCollator can avoid masking special tokens.
    """
    return tokenizer(
        examples["text"],
        truncation=True,
        max_length=max_length,
        return_special_tokens_mask=True,
    )


def group_into_chunks(examples, chunk_size: int = 512):
    """
    Concatenate all token sequences then split into fixed-size chunks.
    This avoids wasting padding budget on short sentences.
    """
    concatenated = {k: sum(examples[k], []) for k in examples.keys()}
    total_length  = len(concatenated["input_ids"])
    # Trim to multiple of chunk_size
    total_length  = (total_length // chunk_size) * chunk_size
    result = {
        k: [v[i : i + chunk_size] for i in range(0, total_length, chunk_size)]
        for k, v in concatenated.items()
    }
    return result


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--model_name",   required=True,
                        help="HF hub name or alias (climatebert/roberta/deberta)")
    parser.add_argument("--corpus_file",  required=True)
    parser.add_argument("--output_dir",   required=True)
    parser.add_argument("--max_length",   type=int,   default=512)
    parser.add_argument("--mlm_prob",     type=float, default=0.15,
                        help="Fraction of tokens to mask")
    parser.add_argument("--num_train_epochs",          type=int,   default=3)
    parser.add_argument("--per_device_train_batch_size", type=int, default=16)
    parser.add_argument("--gradient_accumulation_steps", type=int, default=2,
                        help="Effective batch = batch_size × accum_steps × n_gpus")
    parser.add_argument("--learning_rate", type=float, default=5e-5)
    parser.add_argument("--warmup_ratio",  type=float, default=0.06)
    parser.add_argument("--weight_decay",  type=float, default=0.01)
    parser.add_argument("--seed",          type=int,   default=42)
    parser.add_argument("--fp16",          action="store_true",
                        help="Use mixed precision (recommended on modern GPUs)")
    args = parser.parse_args()

    set_seed(args.seed)

    # Resolve alias → full HF name
    model_name = MODEL_ALIASES.get(args.model_name, args.model_name)
    logger.info(f"Starting DAPT for: {model_name}")

    # ── Load tokeniser & model ─────────────────────────────────────────────────
    tokenizer = AutoTokenizer.from_pretrained(model_name)
    model     = AutoModelForMaskedLM.from_pretrained(model_name)

    logger.info(f"Model params: {model.num_parameters():,}")

    # ── Load & tokenise corpus ─────────────────────────────────────────────────
    raw_dataset = load_dataset(
        "text",
        data_files={"train": args.corpus_file},
        split="train",
    )
    # Remove empty lines used as document separators
    raw_dataset = raw_dataset.filter(lambda ex: len(ex["text"].strip()) > 10)

    tokenised = raw_dataset.map(
        lambda ex: tokenize_function(ex, tokenizer, args.max_length),
        batched=True,
        remove_columns=["text"],
        desc="Tokenising",
    )

    # Chunk into fixed-length blocks (maximises GPU utilisation)
    chunked = tokenised.map(
        lambda ex: group_into_chunks(ex, chunk_size=args.max_length),
        batched=True,
        desc="Chunking",
    )

    logger.info(f"Total training chunks: {len(chunked):,}")

    # ── Data collator — applies random masking on the fly ─────────────────────
    # whole_word_mask=True is recommended for RoBERTa/DeBERTa (masks full words)
    data_collator = DataCollatorForLanguageModeling(
        tokenizer=tokenizer,
        mlm=True,
        mlm_probability=args.mlm_prob,
    )

    # ── Training arguments ─────────────────────────────────────────────────────
    training_args = TrainingArguments(
        output_dir=args.output_dir,
        num_train_epochs=args.num_train_epochs,
        per_device_train_batch_size=args.per_device_train_batch_size,
        gradient_accumulation_steps=args.gradient_accumulation_steps,
        learning_rate=args.learning_rate,
        warmup_ratio=args.warmup_ratio,
        weight_decay=args.weight_decay,
        fp16=args.fp16,
        logging_steps=50,
        save_strategy="epoch",
        save_total_limit=2,           # keep last 2 checkpoints
        report_to="none",             # swap to "wandb" if you use W&B
        seed=args.seed,
        dataloader_num_workers=4,
        # Recommended for DeBERTa (gradient checkpointing saves memory)
        gradient_checkpointing=True,
    )

    trainer = Trainer(
        model=model,
        args=training_args,
        train_dataset=chunked,
        data_collator=data_collator,
    )

    # ── Train ──────────────────────────────────────────────────────────────────
    train_result = trainer.train()

    # Log perplexity (standard MLM metric)
    try:
        perplexity = math.exp(train_result.training_loss)
        logger.info(f"Final train loss: {train_result.training_loss:.4f}  "
                    f"Perplexity: {perplexity:.2f}")
    except OverflowError:
        logger.warning("Perplexity overflow — loss too high, check your data/LR")

    # ── Save final checkpoint ──────────────────────────────────────────────────
    trainer.save_model(args.output_dir)
    tokenizer.save_pretrained(args.output_dir)
    logger.info(f"DAPT checkpoint saved → {args.output_dir}")


if __name__ == "__main__":
    main()
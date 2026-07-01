"""
Keyword Density Scoring and Distribution Analysis
Katelyn Nutley
28-01-2026

Scores corpus documents by keyword density (keyword hits / total tokens),
then plots the distribution to help identify a suitable filtering threshold.

Usage: python keyword_density.py <seeds.csv> <discovered.csv> <corpus.csv> [expert.txt] [output.csv]
"""

import sys
import re
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from nltk.stem.snowball import SnowballStemmer

stemmer = SnowballStemmer('english').stem


# =============================================================================
# KEYWORDS
# =============================================================================

def load_keywords(seeds_path, discovered_path, expert_path=None):
    seeds = pd.read_csv(seeds_path)
    discovered = pd.read_csv(discovered_path)

    all_stems = set(seeds['stem'].dropna().str.strip())
    all_stems.update(discovered['stem'].dropna().str.strip())

    n_expert = 0
    if expert_path:
        with open(expert_path, 'r') as f:
            lines = f.readlines()
        expert_raw = [l.strip()[2:].strip() for l in lines if l.strip().startswith('- ')]
        n_expert = len(expert_raw)
        for phrase in expert_raw:
            stemmed = ' '.join(stemmer(w) for w in phrase.lower().split() if len(w) >= 2)
            if stemmed:
                all_stems.add(stemmed)

    print(f"Seeds: {len(seeds)} | Discovered: {len(discovered)} | Expert: {n_expert} | Unique stems: {len(all_stems)}")
    return list(all_stems)


# =============================================================================
# SCORING
# =============================================================================

def tokenize(text):
    if not isinstance(text, str):
        return []
    return [t for t in re.sub(r'[^a-z\s-]', ' ', text.lower()).split() if len(t) >= 3]


def score_document(text, keyword_set):
    tokens = tokenize(text)
    if not tokens:
        return 0, 0.0
    stemmed = [stemmer(t) for t in tokens]
    hits = sum(1 for t in stemmed if t in keyword_set)
    return hits, hits / len(stemmed)


# =============================================================================
# PLOTS
# =============================================================================

def plot_distribution(corpus, output_dir):
    nonzero = corpus[corpus['density'] > 0]['density']

    fig, axes = plt.subplots(2, 2, figsize=(14, 10))
    fig.suptitle('Keyword Density Distribution', fontsize=14, fontweight='bold')

    axes[0, 0].hist(corpus['density'], bins=100, color='steelblue', edgecolor='none', alpha=0.8)
    axes[0, 0].set(title='Full distribution', xlabel='Keyword density', ylabel='Document count')

    axes[0, 1].hist(nonzero, bins=100, color='steelblue', edgecolor='none', alpha=0.8)
    axes[0, 1].set(title='Non-zero docs only', xlabel='Keyword density', ylabel='Document count')

    nonzero.plot.kde(ax=axes[1, 0], color='steelblue', lw=2)
    axes[1, 0].set(title='KDE — non-zero docs', xlabel='Keyword density', ylabel='Density', xlim=(0, None))

    sorted_density = np.sort(corpus['density'])
    cdf = np.arange(1, len(sorted_density) + 1) / len(sorted_density)
    axes[1, 1].plot(sorted_density, cdf, color='steelblue', lw=2)
    axes[1, 1].set(title='CDF', xlabel='Keyword density', ylabel='Cumulative proportion')
    axes[1, 1].grid(True, alpha=0.3)
    for p, color in [(0.75, 'red'), (0.80, 'orange'), (0.90, 'green')]:
        val = corpus['density'].quantile(p)
        axes[1, 1].axvline(val, color=color, linestyle='--', alpha=0.7, label=f'{int(p*100)}th pctl')
    axes[1, 1].legend(fontsize=8)

    plt.tight_layout()
    plot_path = os.path.join(output_dir, 'keyword_density_distribution.png')
    plt.savefig(plot_path, dpi=150, bbox_inches='tight')
    print(f"Plot saved: {plot_path}")
    plt.close()


# =============================================================================
# MAIN
# =============================================================================

def main(seeds_path, discovered_path, corpus_path, expert_path=None, output_path='corpus_density_scored.csv'):
    output_dir = os.path.dirname(output_path) or '.'
    os.makedirs(output_dir, exist_ok=True)

    print("Loading keywords...")
    keywords = load_keywords(seeds_path, discovered_path, expert_path)
    keyword_set = set(keywords)

    print("Loading corpus...")
    corpus = pd.read_csv(corpus_path)
    print(f"Corpus: {len(corpus):,} documents")

    print("Scoring documents...")
    results = corpus['text'].apply(lambda x: score_document(x, keyword_set))
    corpus['hits'] = [r[0] for r in results]
    corpus['density'] = [r[1] for r in results]

    print(f"\nDensity distribution:")
    print(corpus['density'].describe())
    print(f"Zero-density docs: {(corpus['density'] == 0).sum():,}")

    print()
    for p in [50, 60, 70, 75, 80, 85, 90, 95]:
        val = corpus['density'].quantile(p / 100)
        n = (corpus['density'] >= val).sum()
        print(f"  {p}th percentile: density >= {val:.4f}  →  {n:,} docs")

    plot_distribution(corpus, output_dir)

    keep_cols = [c for c in ['celex', 'work', 'id', 'titles', 'type', 'date', 'author',
                              'directory_code', 'resource_type_used', 'manual_type_used',
                              'url', 'text', 'scraped_title', 'scrape_status',
                              'scrape_timestamp', 'hits', 'density'] if c in corpus.columns]
    corpus[keep_cols].to_csv(output_path, index=False)
    print(f"Scored corpus saved: {output_path}")

    return corpus


if __name__ == '__main__':
    if len(sys.argv) < 4:
        print("Usage: python keyword_density.py <seeds.csv> <discovered.csv> <corpus.csv> [expert.txt] [output.csv]")
        sys.exit(1)

    seeds_path = sys.argv[1]
    discovered_path = sys.argv[2]
    corpus_path = sys.argv[3]
    expert_path = sys.argv[4] if len(sys.argv) > 4 else None
    output_path = sys.argv[5] if len(sys.argv) > 5 else 'corpus_density_scored.csv'

    main(seeds_path, discovered_path, corpus_path, expert_path, output_path)

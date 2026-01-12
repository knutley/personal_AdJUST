# -*- coding: utf-8 -*-
"""
Filter Corpus Using MNIR Keywords from Tabby

This script applies keywords discovered through MNIR (Maximum-discriminative
Naïve-Bayes Information Retrieval) using Tabby to filter the full corpus.

MNIR provides empirically validated keywords with precision/recall metrics
from a training set of manually coded documents.

Author: Katie Nutley
Date: January 2026
"""

import os
import pandas as pd
import re
from collections import Counter
from nltk.stem.snowball import SnowballStemmer


def load_mnir_keywords(filepath, precision_threshold=50.0):
    """
    Load MNIR keywords from target_keywords_validated.txt file.
    
    Args:
        filepath: Path to the keywords file
        precision_threshold: Minimum precision % to include keyword (default: 50%)
    
    Returns:
        Dictionary with keywords organized by precision tier
    """
    print(f"\n{'='*80}")
    print(f"LOADING MNIR KEYWORDS FROM: {filepath}")
    print(f"{'='*80}\n")
    
    tier1_keywords = []  # High precision (>70%)
    tier2_keywords = []  # Good precision (50-70%)
    tier3_keywords = []  # Moderate precision (<50%)
    
    with open(filepath, 'r', encoding='utf-8') as f:
        lines = f.readlines()
    
    # Skip header lines
    keyword_lines = [line for line in lines[5:] if line.strip() and line[0].isdigit()]
    
    for line in keyword_lines:
        # Parse line: "123. keyword (...) (Recall: X.XX, Precision: XX.X%)"
        match = re.search(r'^\d+\.\s+(\S+)\s+.*Precision:\s+([\d.]+)%', line)
        if match:
            keyword = match.group(1).strip()
            precision = float(match.group(2))
            
            if precision >= 70.0:
                tier1_keywords.append((keyword, precision))
            elif precision >= precision_threshold:
                tier2_keywords.append((keyword, precision))
            else:
                tier3_keywords.append((keyword, precision))
    
    print(f"✓ Loaded {len(tier1_keywords) + len(tier2_keywords) + len(tier3_keywords)} keywords")
    print(f"  - Tier 1 (≥70% precision): {len(tier1_keywords)} keywords")
    print(f"  - Tier 2 ({precision_threshold}-70% precision): {len(tier2_keywords)} keywords")
    print(f"  - Tier 3 (<{precision_threshold}% precision): {len(tier3_keywords)} keywords [excluded]")
    
    return {
        'tier1': [kw for kw, _ in tier1_keywords],
        'tier2': [kw for kw, _ in tier2_keywords],
        'tier3': [kw for kw, _ in tier3_keywords],
        'tier1_with_precision': tier1_keywords,
        'tier2_with_precision': tier2_keywords,
        'tier3_with_precision': tier3_keywords,
    }


def process_text_simple(text, stemmer):
    """
    Simple text processing to match MNIR keyword extraction.
    """
    if not isinstance(text, str):
        return ''
    
    # Convert to lowercase
    text = text.lower()
    
    # Remove numbers
    text = re.sub('[0-9]', ' ', text)
    
    # Remove punctuation
    text = re.sub(r'[!"#$%&()*+,\.\/:;<=>?@[\\\]^_`{|}~\'-]', ' ', text)
    
    # Stem the words
    words = text.split()
    stemmed_words = [stemmer(w) for w in words if len(w) >= 3]
    
    return ' '.join(stemmed_words)


def calculate_keyword_matches(text, keywords):
    """
    Calculate which keywords appear in the processed text.
    Returns: list of matched keywords
    """
    matches = []
    for keyword in keywords:
        # Use word boundary matching to avoid partial matches
        pattern = r'\b' + re.escape(keyword) + r'\w*\b'
        if re.search(pattern, text):
            matches.append(keyword)
    return matches


def calculate_mnir_score(matched_tier1, matched_tier2):
    """
    Calculate weighted MNIR score based on empirically validated precision.
    
    Tier 1 keywords (≥70% precision) get weight 2.0
    Tier 2 keywords (50-70% precision) get weight 1.0
    """
    score = (len(matched_tier1) * 2.0 + 
             len(matched_tier2) * 1.0)
    
    return score


def get_negative_keywords():
    """
    Returns negative keywords indicating trade/customs documents.
    """
    negative_keywords = [
        'customs tariff', 'autonomous tariff', 'tariff suspension', 
        'tariff quota', 'customs duties', 'import duties', 'export duties',
        'common customs tariff', 'tariff nomenclature', 'tariff concession',
        'trade agreement', 'free trade', 'trade regime', 
        'dual-use', 'export control', 'export licence',
        'animal by-products', 'veterinary', 'direct payments to farmers'
    ]
    return negative_keywords


def contains_negative_keywords(text):
    """
    Check if document contains negative keywords in first 1000 characters.
    """
    if not isinstance(text, str):
        return False
    
    text_start = text[:1000].lower()
    negative_keywords = get_negative_keywords()
    
    return any(keyword in text_start for keyword in negative_keywords)


def main():
    """
    Main function to filter corpus using MNIR keywords.
    """
    print("="*80)
    print("FILTERING CORPUS WITH MNIR KEYWORDS (Tabby-Validated)")
    print("="*80)
    
    # ===================================================================
    # STEP 1: LOAD MNIR KEYWORDS
    # ===================================================================
    print("\n[STEP 1] Loading MNIR keywords...")
    print("-"*80)
    
    # Try multiple possible locations
    keywords_paths = [
        '/mnt/user-data/uploads/target_keywords_validated.txt',
        os.path.expanduser('~/Documents/target_keywords_validated.txt'),
        os.path.expanduser('~/Downloads/target_keywords_validated.txt'),
        'target_keywords_validated.txt'
    ]
    
    keywords_file = None
    for path in keywords_paths:
        if os.path.exists(path):
            keywords_file = path
            break
    
    if keywords_file is None:
        print(f"✗ ERROR: Keywords file not found. Tried:")
        for path in keywords_paths:
            print(f"   {path}")
        return
    
    keywords = load_mnir_keywords(keywords_file, precision_threshold=50.0)
    
    print(f"\nTop 10 Tier 1 Keywords (highest precision):")
    for kw, prec in keywords['tier1_with_precision'][:10]:
        print(f"  {kw:20s}: {prec:5.1f}% precision")
    
    # ===================================================================
    # STEP 2: LOAD CORPUS
    # ===================================================================
    print("\n[STEP 2] Loading corpus...")
    print("-"*80)
    
    # Try multiple possible locations
    possible_paths = [
        os.path.expanduser('~/Documents/GitHub/personal_AdJUST/corpus_relevant.csv'),
        os.path.expanduser('~/Documents/corpus_relevant.csv'),
        '/mnt/user-data/uploads/corpus_relevant.csv',
        'corpus_relevant.csv'
    ]
    
    corpus_file = None
    for path in possible_paths:
        if os.path.exists(path):
            corpus_file = path
            break
    
    if corpus_file is None:
        print(f"✗ ERROR: Corpus file not found. Tried:")
        for path in possible_paths:
            print(f"   {path}")
        return
    
    try:
        corpus = pd.read_csv(corpus_file, encoding='utf-8')
        print(f"✓ Loaded {len(corpus):,} documents")
        
        # Check for required columns
        if 'text' not in corpus.columns:
            print("✗ ERROR: 'text' column not found in corpus")
            print(f"   Available columns: {list(corpus.columns)}")
            return
        
        # Ensure ID column exists
        if 'celex' in corpus.columns:
            corpus.rename(columns={'celex': 'id'}, inplace=True)
        elif 'id' not in corpus.columns:
            corpus['id'] = range(len(corpus))
        
    except Exception as e:
        print(f"✗ ERROR loading corpus: {e}")
        return
    
    # ===================================================================
    # STEP 3: PROCESS TEXT
    # ===================================================================
    print("\n[STEP 3] Processing text...")
    print("-"*80)
    
    stemmer = SnowballStemmer('english').stem
    
    print("Processing documents (this may take a few minutes)...")
    corpus['text_processed'] = corpus['text'].apply(
        lambda x: process_text_simple(x, stemmer)
    )
    print(f"✓ Processed {len(corpus):,} documents")
    
    # ===================================================================
    # STEP 4: CALCULATE MNIR KEYWORD MATCHES
    # ===================================================================
    print("\n[STEP 4] Calculating MNIR keyword matches...")
    print("-"*80)
    
    # Calculate matches for each tier
    corpus['tier1_matches'] = corpus['text_processed'].apply(
        lambda x: calculate_keyword_matches(x, keywords['tier1'])
    )
    corpus['tier2_matches'] = corpus['text_processed'].apply(
        lambda x: calculate_keyword_matches(x, keywords['tier2'])
    )
    
    # Count matches per tier
    corpus['n_tier1'] = corpus['tier1_matches'].apply(len)
    corpus['n_tier2'] = corpus['tier2_matches'].apply(len)
    corpus['n_total_keywords'] = corpus['n_tier1'] + corpus['n_tier2']
    
    # Calculate MNIR score
    corpus['mnir_score'] = corpus.apply(
        lambda row: calculate_mnir_score(
            row['tier1_matches'], 
            row['tier2_matches']
        ), 
        axis=1
    )
    
    print(f"✓ Calculated MNIR keyword matches for all documents")
    
    # ===================================================================
    # STEP 5: APPLY NEGATIVE KEYWORD FILTER
    # ===================================================================
    print("\n[STEP 5] Applying negative keyword filter...")
    print("-"*80)
    
    corpus['has_negative_keywords'] = corpus['text'].apply(contains_negative_keywords)
    n_negative = corpus['has_negative_keywords'].sum()
    pct_negative = n_negative / len(corpus) * 100
    
    print(f"✓ Documents with trade/customs negative keywords:")
    print(f"  {n_negative:,} documents ({pct_negative:.1f}%)")
    
    # ===================================================================
    # STEP 6: ANALYZE RESULTS
    # ===================================================================
    print("\n[STEP 6] Analyzing MNIR keyword distribution...")
    print("-"*80)
    
    print(f"\nTier 1 (≥70% precision) keyword statistics:")
    print(f"  Documents with 0 Tier 1 keywords:  {(corpus['n_tier1'] == 0).sum():,} ({(corpus['n_tier1'] == 0).sum()/len(corpus)*100:.1f}%)")
    print(f"  Documents with 1+ Tier 1 keywords: {(corpus['n_tier1'] >= 1).sum():,} ({(corpus['n_tier1'] >= 1).sum()/len(corpus)*100:.1f}%)")
    print(f"  Documents with 2+ Tier 1 keywords: {(corpus['n_tier1'] >= 2).sum():,} ({(corpus['n_tier1'] >= 2).sum()/len(corpus)*100:.1f}%)")
    print(f"  Documents with 3+ Tier 1 keywords: {(corpus['n_tier1'] >= 3).sum():,} ({(corpus['n_tier1'] >= 3).sum()/len(corpus)*100:.1f}%)")
    print(f"  Mean Tier 1 keywords per document: {corpus['n_tier1'].mean():.2f}")
    
    print(f"\nTotal keyword statistics (all tiers):")
    print(f"  Documents with 3+ total keywords:  {(corpus['n_total_keywords'] >= 3).sum():,} ({(corpus['n_total_keywords'] >= 3).sum()/len(corpus)*100:.1f}%)")
    print(f"  Documents with 5+ total keywords:  {(corpus['n_total_keywords'] >= 5).sum():,} ({(corpus['n_total_keywords'] >= 5).sum()/len(corpus)*100:.1f}%)")
    print(f"  Documents with 7+ total keywords:  {(corpus['n_total_keywords'] >= 7).sum():,} ({(corpus['n_total_keywords'] >= 7).sum()/len(corpus)*100:.1f}%)")
    print(f"  Mean total keywords per document: {corpus['n_total_keywords'].mean():.2f}")
    
    # MNIR score distribution
    print(f"\nMNIR score distribution:")
    print(f"  Mean score:   {corpus['mnir_score'].mean():.2f}")
    print(f"  Median score: {corpus['mnir_score'].median():.2f}")
    print(f"  75th percentile: {corpus['mnir_score'].quantile(0.75):.2f}")
    print(f"  85th percentile: {corpus['mnir_score'].quantile(0.85):.2f}")
    print(f"  90th percentile: {corpus['mnir_score'].quantile(0.90):.2f}")
    
    # Most common keywords
    print(f"\nTop 10 Most Frequent Tier 1 Keywords (≥70% precision):")
    tier1_all = [kw for matches in corpus['tier1_matches'] for kw in matches]
    tier1_counts = Counter(tier1_all)
    for kw, count in tier1_counts.most_common(10):
        pct = count / len(corpus) * 100
        print(f"  {kw:20s}: {count:5,} docs ({pct:5.1f}%)")
    
    # ===================================================================
    # STEP 7: CREATE MNIR FILTERED CORPUS
    # ===================================================================
    print("\n[STEP 7] Creating MNIR-filtered corpus...")
    print("-"*80)
    
    output_dir = os.path.expanduser('~/Documents/')
    output_cols = ['id', 'n_tier1', 'n_tier2', 'n_total_keywords', 'mnir_score',
                   'has_negative_keywords', 'tier1_matches', 'tier2_matches', 'text']
    
    # Save full corpus with MNIR scores
    full_output = os.path.join(output_dir, 'corpus_with_mnir_scores.csv')
    corpus[output_cols].to_csv(full_output, index=False)
    print(f"✓ Saved full corpus with MNIR scores")
    
    # MNIR FILTERING STRATEGY
    # Based on empirical precision from Tabby validation:
    # - Require 2+ high-precision keywords (≥70% precision)
    # - AND 4+ total keywords
    # - Exclude documents with trade/customs terminology
    
    mnir_filtered = corpus[
        (corpus['n_tier1'] >= 2) & 
        (corpus['n_total_keywords'] >= 4) &
        (~corpus['has_negative_keywords'])
    ].copy()
    
    mnir_file = os.path.join(output_dir, 'corpus_MNIR_filtered.csv')
    mnir_filtered[output_cols].to_csv(mnir_file, index=False)
    print(f"\n✓ MNIR FILTERED CORPUS: {len(mnir_filtered):,} docs ({len(mnir_filtered)/len(corpus)*100:.1f}%)")
    print(f"  Filter: 2+ Tier 1 (≥70% precision) + 4+ total keywords + no negative")
    print(f"  Expected precision: ~70-75% (based on Tabby validation)")
    
    # Save validation sample
    if len(mnir_filtered) >= 100:
        sample = mnir_filtered.sample(n=100, random_state=42)
    else:
        sample = mnir_filtered
    
    sample_file = os.path.join(output_dir, 'validation_sample_mnir.csv')
    sample[output_cols].to_csv(sample_file, index=False)
    print(f"  → Saved validation sample for manual review")
    
    # ===================================================================
    # STEP 8: SUMMARY
    # ===================================================================
    print("\n" + "="*80)
    print("MNIR FILTERING COMPLETE!")
    print("="*80)
    
    print("\nOutput Files Created:")
    print("  • corpus_with_mnir_scores.csv      - Full corpus with MNIR analysis")
    print("  • corpus_MNIR_filtered.csv         - Filtered corpus using MNIR thresholds")
    print("  • validation_sample_mnir.csv       - 100-doc sample for validation")
    
    print("\n" + "="*80)
    print("METHODOLOGICAL NOTES:")
    print("="*80)
    
    print("\n1. MNIR APPROACH")
    print("   - Keywords discovered through Maximum-discriminative Naïve-Bayes")
    print("   - Validated against 100-document training set (23 positive, 77 negative)")
    print("   - Each keyword has empirical precision/recall metrics")
    print("   - Tier 1 keywords have ≥70% precision on validation set")
    
    print("\n2. FILTERING CRITERIA")
    print("   - Minimum 2 high-precision keywords (Tier 1: ≥70% precision)")
    print("   - Minimum 4 total keywords across both tiers")
    print("   - Exclusion of trade/customs documents via negative keywords")
    
    print("\n3. EXPECTED PERFORMANCE")
    print("   - Based on Tabby validation, this filtering should achieve:")
    print("   - Precision: ~70-75% (proportion of retrieved docs that are relevant)")
    print("   - This is conservative compared to your integrated approach")
    
    print("\n4. VALIDATION REQUIRED")
    print("   - Review validation_sample_mnir.csv (100 random documents)")
    print("   - Calculate actual precision: (true positives / 100)")
    print("   - Compare with integrated keyword approach for best results")
    
    print("\n5. FOR YOUR METHODS SECTION")
    print('   "Keywords were discovered through Maximum-discriminative Naïve-Bayes')
    print('    Information Retrieval (MNIR) using Tabby, validated against a training')
    print('    set of 100 manually coded documents (23 environmental policy, 77 non-')
    print('    environmental). We retained keywords with ≥50% precision and applied')
    print('    tiered weighting based on validation performance (≥70% precision = high')
    print('    confidence; 50-70% = moderate confidence). The final corpus required')
    print('    ≥2 high-confidence keywords and ≥4 total keywords."')
    
    print("\n" + "="*80)
    
    return corpus


if __name__ == "__main__":
    filtered_corpus = main()
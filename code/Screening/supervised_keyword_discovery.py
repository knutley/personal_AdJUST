# Title: Supervised Keyword Discovery
# Author: Katelyn Nutley
# Date: 12-11-2025

# Supervised Keyword Discovery via Differential Frequency Analysis

# Description: This script identifies discriminative keywords by comparing word 
# frequencies between positive (environmentally relevant) and negative (non-relevant) 
# document classes, then calculating precision and recall for each term.

# Method: 
# For each stemmed term appearing in the training corpus, we calculate:
# - Recall: P(term appears | document is positive)
#   = (# positive docs containing term) / (# total positive docs)
# - Precision: P(document is positive | term appears)  
#  = (# positive docs containing term) / (# all docs containing term)

# Terms with precision above the baseline rate (positive docs / total docs)
# are considered discriminative of the positive class.

# This is a form of supervised feature selection based on class-conditional
# word frequencies, similar to Naive Bayes feature weighting but used here
# for keyword extraction rather than classification.

import os
import pandas as pd
import re
from collections import defaultdict
from nltk.stem.snowball import SnowballStemmer
import warnings
warnings.filterwarnings('ignore')


def preprocess_text(text, stemmer):
    """
    Preprocess text for keyword extraction.
    
    Steps:
    1. Convert to lowercase
    2. Remove numbers
    3. Remove punctuation
    4. Tokenize
    5. Remove words < 3 characters
    6. Apply Porter/Snowball stemming
    7. Return unique terms (document presence, not frequency)
    
    Args:
        text: Raw document text
        stemmer: NLTK stemmer object
    
    Returns:
        List of unique stemmed terms in document
    """
    if not isinstance(text, str):
        return []
    
    # Lowercase
    text = text.lower()
    
    # Remove numbers
    text = re.sub('[0-9]', ' ', text)
    
    # Remove punctuation
    text = re.sub(r'[!"#$%&()*+,\.\/:;<=>?@[\\\]^_`{|}~\'-]', ' ', text)
    
    # Tokenize and filter
    words = text.split()
    
    # Stem and remove short words
    stemmed = [stemmer.stem(w) for w in words if len(w) >= 3]
    
    # Return unique terms (we care about presence, not frequency)
    return list(set(stemmed))


def calculate_term_statistics(df, label_col='label', terms_col='terms'):
    """
    Calculate precision and recall for each term in the corpus.
    
    Args:
        df: DataFrame with 'label' (0/1) and 'terms' (list) columns
        label_col: Name of label column
        terms_col: Name of terms column
    
    Returns:
        DataFrame with term statistics
    """
    # Count documents containing each term, by class
    term_doc_counts = defaultdict(lambda: {'positive': 0, 'negative': 0, 'total': 0})
    
    for _, row in df.iterrows():
        label = row[label_col]
        for term in row[terms_col]:
            term_doc_counts[term]['total'] += 1
            if label == 1:
                term_doc_counts[term]['positive'] += 1
            else:
                term_doc_counts[term]['negative'] += 1
    
    # Total positive and negative documents
    n_positive = (df[label_col] == 1).sum()
    n_negative = (df[label_col] == 0).sum()
    
    # Calculate precision and recall for each term
    results = []
    
    for term, counts in term_doc_counts.items():
        # Only consider terms appearing in at least 2 documents
        if counts['total'] < 2:
            continue
        
        # Recall: P(term | positive)
        recall = counts['positive'] / n_positive
        
        # Precision: P(positive | term)
        precision = counts['positive'] / counts['total'] if counts['total'] > 0 else 0
        
        results.append({
            'term': term,
            'positive_docs': counts['positive'],
            'negative_docs': counts['negative'],
            'total_docs': counts['total'],
            'recall': recall,
            'precision': precision
        })
    
    return pd.DataFrame(results), n_positive, n_negative


def discover_keywords(input_file, text_col='text', label_col='included', 
                      min_recall=0.09, output_dir='.'):
    """
    Main function to discover discriminative keywords from coded documents.
    
    Args:
        input_file: Path to Excel/CSV file with coded documents
        text_col: Name of column containing document text
        label_col: Name of column containing binary labels (1=positive, 0=negative)
        min_recall: Minimum recall threshold (default: 0.09, ~2 positive docs)
        output_dir: Directory to save output files
    
    Returns:
        DataFrame of filtered keywords with statistics
    """
    print("="*80)
    print("SUPERVISED KEYWORD DISCOVERY")
    print("Differential Frequency Analysis")
    print("="*80)
    
    # Load data
    if input_file.endswith('.xlsx'):
        df = pd.read_excel(input_file)
    else:
        df = pd.read_csv(input_file)
    
    # Remove NaN labels
    df = df[df[label_col].notna()].copy()
    df['label'] = df[label_col].astype(int)
    
    n_pos = (df['label'] == 1).sum()
    n_neg = (df['label'] == 0).sum()
    
    print(f"\nLoaded {len(df)} coded documents")
    print(f"  Positive (relevant): {n_pos}")
    print(f"  Negative (not relevant): {n_neg}")
    
    # Preprocess
    print("\nPreprocessing documents...")
    stemmer = SnowballStemmer('english')
    df['terms'] = df[text_col].apply(lambda x: preprocess_text(x, stemmer))
    
    # Calculate statistics
    print("Calculating term statistics...")
    results_df, n_positive, n_negative = calculate_term_statistics(df)
    
    print(f"\nTotal unique terms: {len(results_df)}")
    
    # Baseline precision (random classifier)
    baseline_precision = n_positive / (n_positive + n_negative)
    print(f"Baseline precision (random): {baseline_precision:.1%}")
    
    # Filter for discriminative terms
    filtered = results_df[
        (results_df['recall'] >= min_recall) &
        (results_df['precision'] > baseline_precision)
    ].copy()
    
    # Sort by precision (primary) and recall (secondary)
    filtered = filtered.sort_values(['precision', 'recall'], ascending=[False, False])
    
    print(f"Discriminative terms: {len(filtered)}")
    
    # Save results
    csv_path = os.path.join(output_dir, 'keyword_discovery_results.csv')
    filtered.to_csv(csv_path, index=False)
    
    # Generate formatted output
    output_lines = [
        "TARGET KEYWORDS",
        f"Total search set: {len(df)}",
        f"Target set size: {n_positive}",
        f"Non-target set size: {n_negative}",
        "",
        "Method: Supervised Keyword Discovery via Differential Frequency Analysis",
        "For each term: Precision = P(positive | term), Recall = P(term | positive)",
        ""
    ]
    
    for i, (_, row) in enumerate(filtered.iterrows(), 1):
        line = f"{i}. {row['term']}    (Recall: {row['recall']:.2f}, Precision: {row['precision']*100:.1f}%)"
        output_lines.append(line)
    
    txt_path = os.path.join(output_dir, 'target_keywords.txt')
    with open(txt_path, 'w') as f:
        f.write('\n'.join(output_lines))
    
    print(f"\n✓ Saved keywords to {txt_path}")
    print(f"✓ Saved full results to {csv_path}")
    
    # Summary
    print("\n" + "="*80)
    print("SUMMARY")
    print("="*80)
    print(f"\nPrecision tiers:")
    print(f"  100% precision: {(filtered['precision'] == 1.0).sum()} terms")
    print(f"  ≥70% precision: {(filtered['precision'] >= 0.70).sum()} terms")
    print(f"  50-70% precision: {((filtered['precision'] >= 0.50) & (filtered['precision'] < 0.70)).sum()} terms")
    print(f"  <50% precision: {(filtered['precision'] < 0.50).sum()} terms")
    
    return filtered


if __name__ == "__main__":
    # Example usage
    keywords = discover_keywords(
        input_file='/Users/katienutley/Documents/GitHub/personal_AdJUST/personal_AdJUST/data/corpus_random.xlsx',
        text_col='text',
        label_col='included',
        output_dir='/Users/katienutley/Documents/GitHub/personal_AdJUST/'
    )

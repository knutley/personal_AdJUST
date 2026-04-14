"""
BART Environmental Classification for Discovered Keywords
Katelyn Nutley, 12-12-2025

Classify discovered keywords as environmental or not
using zero-shot classification with BART-large-MNLI.

Usage: python bart_classifier.py <discovered_keywords.csv> [output.csv]
"""

import sys
import re
import pandas as pd
from transformers import pipeline
from tqdm import tqdm

# ============================================================================
# KNOWN ACRONYMS
# ============================================================================

environmental_acronyms = {
    # Air quality / pollution
    'napcp', 'napcps', 'necd', 'nec', 'clrtap', 'prtr', 'eper',
    # Climate
    'ipcc', 'unfccc', 'cop', 'ndc', 'ndcs', 'ghg', 'ghgs', 'ets', 'euets',
    'lulucf', 'cbam', 'ccus', 'ccs',
    # Biodiversity / nature
    'eep', 'eutr', 'cites', 'iucn', 'natura', 'sssi', 'sac', 'sacs',
    'spa', 'spas', 'ramsar', 'cbd',
    # Water
    'wfd', 'uwwtd', 'msfd', 'rbmp', 'rbmps',
    # Waste / circular economy
    'weee', 'rohs', 'reach', 'clp', 'pops', 'pcb', 'pcbs',
    # Energy
    'res', 'eed', 'epbd', 'nzeb', 'pv', 'csp', 'chp', 'lng', 'tyndp',
    # Environmental assessment
    'eia', 'eias', 'sea', 'emas', 'lca',
    # Other EU environmental
    'eea', 'eionet', 'soer', 'bref', 'brefs', 'bat', 'bats', 'ied',
    'seveso', 'inspire', 'corine', 'cdda',
    # SDGs
    'sdg', 'sdgs',
    # Substances / chemicals
    'pfas', 'pfos', 'pfoa', 'voc', 'vocs', 'nox', 'sox', 'pm10', 'pm25',
    'co2', 'ch4', 'n2o', 'hfc', 'hfcs', 'pfc', 'pfcs', 'sf6',
}

non_environmental_acronyms = {
    'gdp', 'sme', 'smes', 'vat', 'ict', 'r&d', 'rnd', 'hr', 'it', 'ai',
    'eu', 'ec', 'ep', 'mep', 'meps', 'ecj', 'cjeu', 'nato', 'un', 'oecd',
    'imf', 'wto', 'fta', 'ftas', 'ppp', 'ppps', 'ngo', 'ngos', 'cso', 'csos',
    'pdf', 'html', 'http', 'https', 'www', 'url', 'api',
    'ceo', 'cfo', 'coo', 'llc', 'plc', 'inc', 'ltd',
    'rrp', 'rrps',
}

CANDIDATE_LABELS = [
    "environmental science or ecology",
    "business, law, or administration"
]


# ============================================================================
# CLASSIFICATION HELPERS
# ============================================================================

def is_acronym(text):
    text = text.lower().strip()
    base = text[:-1] if text.endswith('s') and len(text) > 2 else text
    if len(base) <= 6:
        vowels = sum(1 for c in base if c in 'aeiou')
        consonants = sum(1 for c in base if c.isalpha() and c not in 'aeiou')
        if consonants > 0 and vowels / (vowels + consonants) < 0.3:
            return True
        if len(base) <= 4 and vowels <= 1:
            return True
    return False


def check_known_acronym(text):
    text_lower = text.lower().strip()
    variants = [text_lower, text_lower[:-1] if text_lower.endswith('s') else text_lower + 's']
    for v in variants:
        if v in environmental_acronyms:
            return 'environmental'
        if v in non_environmental_acronyms:
            return 'not_environmental'
    return None


def classify_keyword(row, classifier):
    stem = str(row['stem'])
    words = str(row.get('original_words', ''))
    input_text = words if words and words != 'nan' else stem

    known = check_known_acronym(stem)
    if known:
        return known, 1.0, 'known_acronym'

    if is_acronym(stem):
        return 'needs_review', 0.0, 'unknown_acronym'

    result = classifier(input_text, CANDIDATE_LABELS, multi_label=False)
    top_label = result['labels'][0]
    top_score = result['scores'][0]
    classification = 'environmental' if 'environmental' in top_label or 'ecology' in top_label else 'not_environmental'

    return classification, round(top_score, 3), 'model'


# ============================================================================
# MAIN
# ============================================================================

def main(input_path, output_path):
    df = pd.read_csv(input_path)
    print(f"Loaded {len(df)} keywords from {input_path}")

    print("Loading BART-large-MNLI...")
    classifier = pipeline("zero-shot-classification", model="facebook/bart-large-mnli", device=0)

    print("Classifying...")
    results = []
    for _, row in tqdm(df.iterrows(), total=len(df)):
        classification, confidence, method = classify_keyword(row, classifier)
        results.append((classification, confidence, method))

    df['classification'], df['confidence'], df['method'] = zip(*results)

    print(f"\nClassification breakdown:")
    print(df['classification'].value_counts().to_string())
    print(f"\nMethod breakdown:")
    print(df['method'].value_counts().to_string())

    df.to_csv(output_path, index=False)
    print(f"\nAll results saved to: {output_path}")

    env_path = output_path.replace('.csv', '_environmental_only.csv')
    df[df['classification'] == 'environmental'].to_csv(env_path, index=False)
    print(f"Environmental only saved to: {env_path}")

    review_path = output_path.replace('.csv', '_needs_review.csv')
    df[df['classification'] == 'needs_review'].to_csv(review_path, index=False)
    print(f"Needs review saved to: {review_path}")


if __name__ == "__main__":
    # Usage: python bart_classifier.py [unfiltered_discovered_keywords.csv] [filtered_discovered_keywords.csv]
    input_path = sys.argv[1] if len(sys.argv) > 1 else 'unfiltered_discovered_keywords.csv'
    output_path = sys.argv[2] if len(sys.argv) > 2 else 'filtered_discovered_keywords.csv'
    main(input_path, output_path)

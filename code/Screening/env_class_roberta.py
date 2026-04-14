"""
Classify discovered keywords as environmental or not environmental
using zero-shot classification - v3 with acronym handling
"""

import pandas as pd
from transformers import pipeline
from tqdm import tqdm
import re

# Load discovered keywords
df = pd.read_csv("./output/discovered_keywords_20260219_055357.csv")

print(f"Loaded {len(df)} keywords to classify")

keyword_column = "stem"
original_words_column = "original_words"

# ============================================================================
# KNOWN ENVIRONMENTAL ACRONYMS
# Add any you know are environmental here
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

# ============================================================================
# KNOWN NON-ENVIRONMENTAL ACRONYMS
# Add any you know are NOT environmental here
# ============================================================================
non_environmental_acronyms = {
    'gdp', 'sme', 'smes', 'vat', 'ict', 'r&d', 'rnd', 'hr', 'it', 'ai',
    'eu', 'ec', 'ep', 'mep', 'meps', 'ecj', 'cjeu', 'nato', 'un', 'oecd',
    'imf', 'wto', 'fta', 'ftas', 'ppp', 'ppps', 'ngo', 'ngos', 'cso', 'csos',
    'pdf', 'html', 'http', 'https', 'www', 'url', 'api',
    'ceo', 'cfo', 'coo', 'llc', 'plc', 'inc', 'ltd',
    'rrp', 'rrps',  # Recovery and Resilience Plan (not specifically environmental)
}

def is_acronym(text):
    """Check if text looks like an acronym."""
    text = text.lower().strip()
    
    # Remove common suffixes
    if text.endswith('s') and len(text) > 2:
        base = text[:-1]
    else:
        base = text
    
    # Short (2-6 chars) and mostly consonants = likely acronym
    if len(base) <= 6:
        vowels = sum(1 for c in base if c in 'aeiou')
        consonants = sum(1 for c in base if c.isalpha() and c not in 'aeiou')
        
        # All caps style or very few vowels
        if consonants > 0 and vowels / (vowels + consonants) < 0.3:
            return True
        
        # Very short with no clear word structure
        if len(base) <= 4 and vowels <= 1:
            return True
    
    return False

def check_known_acronym(text):
    """Check if text is a known acronym. Returns 'environmental', 'not_environmental', or None."""
    text_lower = text.lower().strip()
    
    # Check with and without trailing 's'
    variants = [text_lower]
    if text_lower.endswith('s'):
        variants.append(text_lower[:-1])
    else:
        variants.append(text_lower + 's')
    
    for variant in variants:
        if variant in environmental_acronyms:
            return 'environmental'
        if variant in non_environmental_acronyms:
            return 'not_environmental'
    
    return None

# Load the classifier
print("Loading classifier model...")
classifier = pipeline(
    "zero-shot-classification",
    model="facebook/bart-large-mnli",
    device=0
)

candidate_labels = [
    "environmental science or ecology",
    "business, law, or administration"
]

def classify_keyword(row):
    stem = str(row[keyword_column])
    
    try:
        examples = str(row[original_words_column])
        if examples == 'nan' or not examples:
            examples = stem
    except:
        examples = stem
    
    # Step 1: Check if it's a known acronym
    known = check_known_acronym(stem)
    if known:
        return known, 1.0, 'known_acronym'
    
    # Step 2: Check if it's an unknown acronym -> flag for review
    if is_acronym(stem):
        return 'needs_review', 0.0, 'unknown_acronym'
    
    # Step 3: Use the model for real words
    input_text = examples
    
    result = classifier(
        input_text,
        candidate_labels,
        multi_label=False
    )
    
    top_label = result["labels"][0]
    top_score = result["scores"][0]
    
    if "environmental" in top_label.lower() or "ecology" in top_label.lower():
        classification = "environmental"
    else:
        classification = "not_environmental"
    
    return classification, round(top_score, 3), 'model'

# ============================================================================
# TEST ON FIRST 100
# ============================================================================
print("\n=== TESTING ON FIRST 100 ===")
test_results = []
for idx, row in df.head(100).iterrows():
    classification, confidence, method = classify_keyword(row)
    test_results.append({
        'stem': row[keyword_column],
        'original_words': row[original_words_column],
        'classification': classification,
        'confidence': confidence,
        'method': method
    })

test_df = pd.DataFrame(test_results)

# Summary by classification
print("\nClassification breakdown:")
print(test_df['classification'].value_counts())

print("\nMethod breakdown:")
print(test_df['method'].value_counts())

print("\n--- KNOWN ACRONYMS (auto-classified) ---")
known = test_df[test_df['method'] == 'known_acronym']
if len(known) > 0:
    print(known[['stem', 'original_words', 'classification']].to_string(index=False))
else:
    print("(none in test set)")

print("\n--- UNKNOWN ACRONYMS (needs review) ---")
unknown = test_df[test_df['method'] == 'unknown_acronym']
if len(unknown) > 0:
    print(unknown[['stem', 'original_words']].head(20).to_string(index=False))
else:
    print("(none in test set)")

print("\n--- MODEL CLASSIFIED: Environmental ---")
env_model = test_df[(test_df['method'] == 'model') & (test_df['classification'] == 'environmental')]
print(env_model[['stem', 'original_words', 'confidence']].head(15).to_string(index=False))

print("\n--- MODEL CLASSIFIED: Not Environmental ---")
not_env_model = test_df[(test_df['method'] == 'model') & (test_df['classification'] == 'not_environmental')]
print(not_env_model[['stem', 'original_words', 'confidence']].head(15).to_string(index=False))

# Ask user
response = input("\nDoes this look reasonable? Continue with full classification? (y/n): ")

if response.lower() != 'y':
    print("Exiting. Adjust the script and try again.")
    exit()

# ============================================================================
# FULL CLASSIFICATION
# ============================================================================
print("\nClassifying all keywords...")
results = []
for idx, row in tqdm(df.iterrows(), total=len(df)):
    classification, confidence, method = classify_keyword(row)
    results.append((classification, confidence, method))

df["classification"], df["confidence"], df["method"] = zip(*results)

# Summary stats
print(f"\n=== FINAL RESULTS ===")
print(f"Total keywords: {len(df)}")
print("\nBy classification:")
print(df['classification'].value_counts())
print("\nBy method:")
print(df['method'].value_counts())

# Save all results
output_path = "./output/discovered_keywords_classified_v3.csv"
df.to_csv(output_path, index=False)
print(f"\nAll results saved to: {output_path}")

# Save environmental only
env_df = df[df["classification"] == "environmental"].copy()
env_path = "./output/discovered_keywords_environmental_v3.csv"
env_df.to_csv(env_path, index=False)
print(f"Environmental keywords ({len(env_df)}): {env_path}")

# Save needs_review for manual classification
review_df = df[df["classification"] == "needs_review"].copy()
review_path = "./output/discovered_keywords_needs_review.csv"
review_df.to_csv(review_path, index=False)
print(f"Needs manual review ({len(review_df)}): {review_path}")

# Save not_environmental
not_env_df = df[df["classification"] == "not_environmental"].copy()
not_env_path = "./output/discovered_keywords_not_environmental_v3.csv"
not_env_df.to_csv(not_env_path, index=False)
print(f"Not environmental ({len(not_env_df)}): {not_env_path}")
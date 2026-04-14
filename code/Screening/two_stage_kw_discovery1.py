"""
Title: Two-Stage Keyword Discovery for Environmental Policy Documents
Author: Katelyn Nutley (modified)
Date: 07-11-2025

This script:
1. STAGE 1: Loads PRE-FILTERED seed keywords from a hand-coded file
2. STAGE 2: Uses those filtered keywords to score the full corpus, then discovers NEW 
   co-occurring keywords that weren't in the original sample

Usage:
    python keyword_discovery_v2.py <filtered_seeds.xlsx> <full_corpus.csv> <output_dir>
"""

import pandas as pd
import numpy as np
import re
from collections import defaultdict, Counter
from datetime import datetime
import os
import sys

# ============================================================================
# DOMAIN STOPWORDS - Generic policy/administrative terms to filter out
# ============================================================================
domain_stopwords = set([
    # Geographic/organizational (generic)
    'europ', 'europa', 'union',
    # Policy framework terms
    'approach', 'framework', 'strategi', 'strategy', 'aim', 'object',
    'objective', 'prioriti', 'priority', 'goal', 'target', 'line',
    # Policy mechanisms (generic)
    'mechan', 'mechanism', 'scheme', 'guidelin', 'guideline', 'platform',
    'programm', 'programme', 'program', 'initiative', 'initi', 'roadmap',
    'outlin', 'budgetari', 'properti',
    # Action verbs (generic)
    'strengthen', 'enhanc', 'enhance', 'support', 'contribut',
    'achiev', 'achieve', 'integr', 'integrate', 'promot', 'promote',
    'involv', 'involve', 'participat', 'particip', 'collabor',
    'demonstr', 'demonstrate', 'monitor', 'identifi', 'identify',
    'launch', 'implement', 'establish', 'develop', 'creat',
    'ensu', 'ensure', 'revis', 'review', 'consider', 'demand', 'tackl',
    'instal', 'reflect', 'allow', 'facilit', 'would', 'serv',
    'look', 'determin', 'alloc',
    # Integration/coordination
    'coher', 'coherent', 'coherence', 'comprehens', 'comprehensive',
    'holistic', 'complement', 'complementary',
    # Structural terms
    'differ', 'different', 'unit', 'step', 'phase', 'stage',
    'level', 'structur', 'structure', 'process', 'dimension', 'aspect',
    'element', 'compon', 'component', 'scope', 'advance', 'benchmark',
    # Actors (generic)
    'stakehold', 'agenc', 'actor', 'partner', 'individu', 'human',
    'person', 'entiti', 'membership', 'staff',
    # Descriptors (generic)
    'high', 'higher', 'low', 'lower', 'wide', 'broad', 'strong', 'key',
    'main', 'major', 'essenti', 'essential', 'important', 'critical',
    'clear', 'sufficient', 'suffici', 'better', 'best', 'negat',
    'convent', 'benefici',
    # Temporal (generic)
    'recent', 'recently', 'current', 'currently', 'next', 'futur',
    'future', 'beyond', 'toward', 'towards', 'long', 'term', 'period',
    'decad', 'decade',
    # Quantitative (generic)
    'increas', 'increase', 'decreas', 'decrease', 'growth', 'share',
    'rate', 'percent', 'proportion', 'billion', 'half', 'per',
    'reduct', 'million', 'larg',
    # Qualities/outcomes (generic)
    'qualiti', 'quality', 'success', 'successful', 'benefit',
    'impact', 'effect', 'progress', 'effort', 'achievement',
    'result', 'outcome', 'capac', 'capacity', 'potenti', 'potential',
    'flexibl', 'flexibility', 'innov', 'extens',
    # Processes (generic)
    'introduct', 'introduction', 'start', 'expect', 'expectation', 
    'project', 'projection', 'emerg', 'emerging', 'shift', 'transit', 
    'transition', 'transform', 'transformation', 'indirect',
    # Meta/discourse
    'exampl', 'example', 'show', 'see', 'seen',
    'discuss', 'discussion', 'figur', 'figure', 'illustrat', 'illustrate', 
    'indicat', 'indicate', 'methodolog', 'space', 'asset',
    # Connectors/modifiers
    'often', 'could', 'moreov', 'moreover', 'furthermor',
    'furthermore', 'alread', 'already', 'rather', 'well', 'togeth',
    'together', 'around', 'along', 'close', 'near',
    # Generic nouns
    'meet', 'meeting', 'work', 'working', 'issu', 'issue',
    'challeng', 'challenge', 'risk', 'problem', 'concern',
    'attent', 'attention', 'awar', 'awareness', 'knowledg', 'knowledge',
    # Administrative
    'govern', 'government', 'governance', 'fund', 'funding',
    'cost', 'invest', 'investment', 'economi', 'economic', 'economy',
    'competit', 'competition', 'competitive', 'trade',
    # Composite terms
    'overal', 'overall', 'insuffici', 'insufficient', 'signific', 
    'significant', 'ambiti', 'ambitious',
    # Miscellaneous generic
    'like', 'across', 'play', 'face', 'sound', 'rang', 'range', 
    'compar', 'compare', 'role', 'dimens', 'scale', 'earli', 'early', 
    'focus', 'gap', 'put', 'come', 'remain', 'cycl', 'cycle',
    'revision', 'network', 'option', 'led', 'incorpor', 'incorporate', 
    'combin', 'combine', 'offer', 'intend', 'intended', 'prepar', 
    'prepare', 'construct', 'balanc', 'balance', 'trend', 'scenario',
    'complianc', 'compliance', 'life', 'recoveri', 'recovery',
    'intens', 'intensive', 'site', 'acceler', 'accelerate',
    'int', 'non', 'yet', 'much',
    # Web artifacts
    'html', 'htm', 'pdf', 'http', 'https', 'www'
])

# Standard English stopwords
english_stopwords = {
    'a', 'an', 'the', 'and', 'or', 'but', 'if', 'then', 'else', 'when',
    'at', 'from', 'by', 'on', 'off', 'for', 'in', 'out', 'over', 'to',
    'into', 'with', 'about', 'against', 'between', 'through', 'during',
    'before', 'after', 'above', 'below', 'up', 'down', 'is', 'are', 'was',
    'were', 'be', 'been', 'being', 'have', 'has', 'had', 'having', 'do',
    'does', 'did', 'doing', 'will', 'would', 'could', 'should', 'may',
    'might', 'must', 'shall', 'can', 'need', 'dare', 'ought', 'used',
    'i', 'me', 'my', 'myself', 'we', 'our', 'ours', 'ourselves', 'you',
    'your', 'yours', 'yourself', 'yourselves', 'he', 'him', 'his',
    'himself', 'she', 'her', 'hers', 'herself', 'it', 'its', 'itself',
    'they', 'them', 'their', 'theirs', 'themselves', 'what', 'which',
    'who', 'whom', 'this', 'that', 'these', 'those', 'am', 'as', 'of',
    'such', 'no', 'nor', 'not', 'only', 'own', 'same', 'so', 'than',
    'too', 'very', 's', 't', 'just', 'don', 'now', 'also', 'more',
    'most', 'other', 'some', 'any', 'each', 'all', 'both', 'few'
}


class PorterStemmerSimple:
    """Simple Porter Stemmer implementation for word stemming."""
    
    def __init__(self):
        self.vowels = set('aeiou')
    
    def _is_consonant(self, word, i):
        if word[i] in self.vowels:
            return False
        if word[i] == 'y':
            if i == 0:
                return True
            return not self._is_consonant(word, i - 1)
        return True
    
    def _measure(self, stem):
        cv_sequence = ''
        for i, char in enumerate(stem):
            if self._is_consonant(stem, i):
                cv_sequence += 'c'
            else:
                cv_sequence += 'v'
        compressed = ''
        for char in cv_sequence:
            if not compressed or compressed[-1] != char:
                compressed += char
        return compressed.count('vc')
    
    def stem(self, word):
        word = word.lower()
        if len(word) <= 2:
            return word
        
        # Step 1a: plurals
        if word.endswith('sses'):
            word = word[:-2]
        elif word.endswith('ies'):
            word = word[:-2]
        elif word.endswith('ss'):
            pass
        elif word.endswith('s'):
            word = word[:-1]
        
        # Step 1b: -ed, -ing
        if word.endswith('eed'):
            if self._measure(word[:-3]) > 0:
                word = word[:-1]
        elif word.endswith('ed'):
            stem = word[:-2]
            if any(c in self.vowels for c in stem):
                word = stem
        elif word.endswith('ing'):
            stem = word[:-3]
            if any(c in self.vowels for c in stem):
                word = stem
        
        # Step 1c: y -> i
        if word.endswith('y') and len(word) > 2:
            if not self._is_consonant(word, len(word) - 2):
                word = word[:-1] + 'i'
        
        # Step 2
        suffixes_step2 = [
            ('ational', 'ate'), ('tional', 'tion'), ('enci', 'ence'),
            ('anci', 'ance'), ('izer', 'ize'), ('abli', 'able'),
            ('alli', 'al'), ('entli', 'ent'), ('eli', 'e'),
            ('ousli', 'ous'), ('ization', 'ize'), ('ation', 'ate'),
            ('ator', 'ate'), ('alism', 'al'), ('iveness', 'ive'),
            ('fulness', 'ful'), ('ousness', 'ous'), ('aliti', 'al'),
            ('iviti', 'ive'), ('biliti', 'ble')
        ]
        for suffix, replacement in suffixes_step2:
            if word.endswith(suffix):
                stem = word[:-len(suffix)]
                if self._measure(stem) > 0:
                    word = stem + replacement
                break
        
        # Step 3
        suffixes_step3 = [
            ('icate', 'ic'), ('ative', ''), ('alize', 'al'),
            ('iciti', 'ic'), ('ical', 'ic'), ('ful', ''), ('ness', '')
        ]
        for suffix, replacement in suffixes_step3:
            if word.endswith(suffix):
                stem = word[:-len(suffix)]
                if self._measure(stem) > 0:
                    word = stem + replacement
                break
        
        # Step 4
        suffixes_step4 = [
            'al', 'ance', 'ence', 'er', 'ic', 'able', 'ible', 'ant',
            'ement', 'ment', 'ent', 'ion', 'ou', 'ism', 'ate', 'iti',
            'ous', 'ive', 'ize'
        ]
        for suffix in suffixes_step4:
            if word.endswith(suffix):
                stem = word[:-len(suffix)]
                if self._measure(stem) > 1:
                    word = stem
                break
        
        # Step 5a
        if word.endswith('e'):
            stem = word[:-1]
            if self._measure(stem) > 1:
                word = stem
            elif self._measure(stem) == 1 and len(stem) >= 3:
                if stem[-1] not in 'wxy':
                    last = len(stem) - 1
                    if self._is_consonant(stem, last) and not self._is_consonant(stem, last-1) and self._is_consonant(stem, last-2):
                        pass
                    else:
                        word = stem
        
        # Step 5b
        if word.endswith('ll') and self._measure(word[:-1]) > 1:
            word = word[:-1]
        
        return word


# Global stemmer instance
stemmer = PorterStemmerSimple()


def is_valid_stem(stem):
    """Check if a stem should be included (not a stopword)."""
    if len(stem) < 3:
        return False
    if stem in domain_stopwords:
        return False
    for sw in domain_stopwords:
        if len(sw) >= 3 and (stem.startswith(sw) or sw.startswith(stem)):
            return False
    return True


def tokenize_and_stem(text):
    """Tokenize text and return set of unique stems + mapping to original words."""
    if pd.isna(text):
        return set(), {}
    
    tokens = re.findall(r'\b[a-zA-Z][\w-]*[a-zA-Z]\b|\b[a-zA-Z]\b', str(text).lower())
    
    stems = set()
    stem_to_words = defaultdict(set)
    
    for token in tokens:
        if len(token) < 3 or token in english_stopwords:
            continue
        
        stem = stemmer.stem(token)
        
        if is_valid_stem(stem):
            stems.add(stem)
            stem_to_words[stem].add(token)
    
    return stems, stem_to_words


# ============================================================================
# STAGE 1: Load pre-filtered seed keywords
# ============================================================================

def stage1_load_filtered_seeds(filtered_seeds_path, 
                                stem_column='stem',
                                filter_column='relevant',
                                env_label='environmental',
                                precision_column='precision',
                                recall_column='recall',
                                original_words_column='original_words'):
    """
    Load pre-filtered seed keywords from hand-coded file.
    
    Args:
        filtered_seeds_path: Path to Excel file with filtered seeds
        stem_column: Column containing the stem
        filter_column: Column containing the filter label
        env_label: Value in filter_column that indicates environmental
        precision_column: Column with precision values (optional)
        recall_column: Column with recall values (optional)
        original_words_column: Column with original word forms
    
    Returns:
        seed_keywords: dict mapping stem -> {precision, recall, original_words}
    """
    print("=" * 80)
    print("STAGE 1: Loading pre-filtered seed keywords")
    print("=" * 80)
    
    # Load the filtered seeds file
    seeds_df = pd.read_excel(filtered_seeds_path)
    print(f"Loaded {len(seeds_df)} total keywords from file")
    
    # Filter to only environmental keywords
    env_mask = seeds_df[filter_column].str.lower().str.strip() == env_label.lower()
    env_seeds = seeds_df[env_mask].copy()
    
    n_env = len(env_seeds)
    n_not_env = len(seeds_df) - n_env
    
    print(f"  Environmental keywords: {n_env}")
    print(f"  Non-environmental keywords: {n_not_env}")
    print()
    
    # Build seed keywords dict
    seed_keywords = {}
    
    for _, row in env_seeds.iterrows():
        stem = str(row[stem_column]).strip()
        
        # Get precision/recall if available, otherwise default to 1.0
        try:
            precision = float(row[precision_column]) if pd.notna(row.get(precision_column)) else 1.0
        except:
            precision = 1.0
        
        try:
            recall = float(row[recall_column]) if pd.notna(row.get(recall_column)) else 0.0
        except:
            recall = 0.0
        
        # Get original words
        try:
            orig_words = str(row[original_words_column]) if pd.notna(row.get(original_words_column)) else stem
            original_words = [w.strip() for w in orig_words.split(',')]
        except:
            original_words = [stem]
        
        seed_keywords[stem] = {
            'precision': precision,
            'recall': recall,
            'original_words': original_words
        }
    
    # Show top keywords
    sorted_keywords = sorted(seed_keywords.items(), 
                            key=lambda x: (-x[1]['precision'], -x[1]['recall']))
    
    print(f"Filtered seed keywords loaded: {len(seed_keywords)}")
    print("\nTop 20 filtered seed keywords:")
    print("-" * 60)
    for i, (stem, info) in enumerate(sorted_keywords[:20], 1):
        words = ', '.join(info['original_words'][:3])
        print(f"  {i:2}. {stem:20} P={info['precision']:.2f} R={info['recall']:.2f}  ({words})")
    
    return seed_keywords


# ============================================================================
# STAGE 2: Score full corpus and discover new keywords
# ============================================================================

def score_document(stems, seed_keywords):
    """
    Score a document based on presence of seed keywords.
    Returns weighted score (sum of precisions for matching keywords).
    """
    score = 0.0
    matched_keywords = []
    
    for stem in stems:
        if stem in seed_keywords:
            score += seed_keywords[stem]['precision']
            matched_keywords.append(stem)
    
    return score, matched_keywords


def stage2_discover_keywords(df, seed_keywords, text_column='text',
                             score_threshold_percentile=75,
                             min_high_score_count=10,
                             min_lift=2.0):
    """
    Use seed keywords to score the full corpus, then discover new co-occurring keywords.
    
    Args:
        df: Full corpus DataFrame
        seed_keywords: Dict of filtered seed keywords from Stage 1
        score_threshold_percentile: Documents above this percentile are "high-scoring"
        min_high_score_count: Minimum times a keyword must appear in high-scoring docs
        min_lift: Minimum lift ratio (freq in high-scoring / freq in low-scoring)
    
    Returns:
        discovered_keywords: dict mapping stem -> {lift, high_score_count, ...}
    """
    print("\n" + "=" * 80)
    print("STAGE 2: Discovering new keywords from full corpus")
    print("=" * 80)
    
    n_docs = len(df)
    print(f"Full corpus size: {n_docs} documents")
    print(f"Using {len(seed_keywords)} filtered seed keywords for scoring")
    print()
    
    # Score all documents
    print("Scoring documents with filtered seed keywords...")
    doc_scores = []
    doc_stems = []
    all_stem_to_words = defaultdict(set)
    
    for idx, row in df.iterrows():
        stems, stem_map = tokenize_and_stem(row[text_column])
        score, _ = score_document(stems, seed_keywords)
        doc_scores.append(score)
        doc_stems.append(stems)
        
        for stem, words in stem_map.items():
            all_stem_to_words[stem].update(words)
    
    df = df.copy()
    df['env_score'] = doc_scores
    
    # Determine threshold
    nonzero_scores = [s for s in doc_scores if s > 0]
    if len(nonzero_scores) == 0:
        print("WARNING: No documents matched any seed keywords!")
        return {}, df
    
    score_threshold = np.percentile(nonzero_scores, score_threshold_percentile)
    
    high_scoring_mask = df['env_score'] >= score_threshold
    low_scoring_mask = df['env_score'] < score_threshold
    
    n_high = high_scoring_mask.sum()
    n_low = low_scoring_mask.sum()
    
    print(f"Score distribution:")
    print(f"  - Documents with score > 0: {len(nonzero_scores)}")
    print(f"  - Score threshold ({score_threshold_percentile}th percentile): {score_threshold:.2f}")
    print(f"  - High-scoring documents: {n_high}")
    print(f"  - Low-scoring documents: {n_low}")
    print()
    
    # Count keyword frequencies in high vs low scoring documents
    keyword_in_high = defaultdict(int)
    keyword_in_low = defaultdict(int)
    
    for i, (is_high, stems) in enumerate(zip(high_scoring_mask, doc_stems)):
        for stem in stems:
            if is_high:
                keyword_in_high[stem] += 1
            else:
                keyword_in_low[stem] += 1
    
    # Calculate lift and filter for new keywords
    discovered_keywords = {}
    all_stems = set(keyword_in_high.keys()) | set(keyword_in_low.keys())
    
    for stem in all_stems:
        # Skip if already a seed keyword
        if stem in seed_keywords:
            continue
        
        high_count = keyword_in_high.get(stem, 0)
        low_count = keyword_in_low.get(stem, 0)
        
        if high_count < min_high_score_count:
            continue
        
        # Calculate rates
        rate_in_high = high_count / n_high if n_high > 0 else 0
        rate_in_low = low_count / n_low if n_low > 0 else 0
        
        # Calculate lift (ratio of rates)
        if rate_in_low > 0:
            lift = rate_in_high / rate_in_low
        else:
            lift = float('inf') if rate_in_high > 0 else 0
        
        if lift >= min_lift:
            discovered_keywords[stem] = {
                'lift': lift,
                'high_score_count': high_count,
                'low_score_count': low_count,
                'rate_in_high': rate_in_high,
                'rate_in_low': rate_in_low,
                'original_words': sorted(all_stem_to_words[stem])
            }
    
    # Sort by lift
    sorted_discovered = sorted(discovered_keywords.items(),
                               key=lambda x: (-x[1]['lift'], -x[1]['high_score_count']))
    
    print(f"New keywords discovered: {len(discovered_keywords)}")
    print("\nTop 30 discovered keywords (sorted by lift):")
    print("-" * 70)
    print(f"  {'Stem':<20} {'Lift':>8} {'High':>6} {'Low':>6} {'Examples'}")
    print("-" * 70)
    for i, (stem, info) in enumerate(sorted_discovered[:30], 1):
        words = ', '.join(info['original_words'][:3])
        lift_str = f"{info['lift']:.1f}" if info['lift'] != float('inf') else "inf"
        print(f"  {stem:<20} {lift_str:>8} {info['high_score_count']:>6} {info['low_score_count']:>6}   {words}")
    
    return dict(sorted_discovered), df


# ============================================================================
# Output formatting
# ============================================================================

def save_results(seed_keywords, discovered_keywords, scored_df, output_dir):
    """Save all results to files."""
    os.makedirs(output_dir, exist_ok=True)
    timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
    
    # 1. Save filtered seed keywords (for reference)
    seed_rows = []
    for stem, info in seed_keywords.items():
        seed_rows.append({
            'stem': stem,
            'precision': info['precision'],
            'recall': info['recall'],
            'original_words': ', '.join(info['original_words'])
        })
    seed_df = pd.DataFrame(seed_rows)
    seed_path = os.path.join(output_dir, f'filtered_seeds_used_{timestamp}.csv')
    seed_df.to_csv(seed_path, index=False)
    print(f"\nFiltered seed keywords saved to: {seed_path}")
    
    # 2. Save discovered keywords
    disc_rows = []
    for stem, info in discovered_keywords.items():
        disc_rows.append({
            'stem': stem,
            'lift': info['lift'] if info['lift'] != float('inf') else 9999,
            'high_score_count': info['high_score_count'],
            'low_score_count': info['low_score_count'],
            'rate_in_high': info['rate_in_high'],
            'rate_in_low': info['rate_in_low'],
            'original_words': ', '.join(info['original_words'])
        })
    disc_df = pd.DataFrame(disc_rows)
    disc_path = os.path.join(output_dir, f'discovered_keywords_{timestamp}.csv')
    disc_df.to_csv(disc_path, index=False)
    print(f"Discovered keywords saved to: {disc_path}")
    
    # 3. Save combined keyword list
    combined_rows = []
    for stem, info in seed_keywords.items():
        combined_rows.append({
            'stem': stem,
            'source': 'seed_filtered',
            'precision': info['precision'],
            'recall': info['recall'],
            'lift': None,
            'original_words': ', '.join(info['original_words'])
        })
    for stem, info in discovered_keywords.items():
        combined_rows.append({
            'stem': stem,
            'source': 'discovered',
            'precision': None,
            'recall': None,
            'lift': info['lift'] if info['lift'] != float('inf') else 9999,
            'original_words': ', '.join(info['original_words'])
        })
    combined_df = pd.DataFrame(combined_rows)
    combined_path = os.path.join(output_dir, f'all_keywords_{timestamp}.csv')
    combined_df.to_csv(combined_path, index=False)
    print(f"Combined keyword list saved to: {combined_path}")
    
    # 4. Save scored corpus
    scored_path = os.path.join(output_dir, f'scored_corpus_{timestamp}.csv')
    cols_to_save = ['work', 'celex', 'titles', 'env_score']
    cols_to_save = [c for c in cols_to_save if c in scored_df.columns]
    if 'env_score' not in cols_to_save:
        cols_to_save.append('env_score')
    scored_df[cols_to_save].to_csv(scored_path, index=False)
    print(f"Scored corpus saved to: {scored_path}")
    
    return seed_path, disc_path, combined_path, scored_path


# ============================================================================
# Main
# ============================================================================

def main(filtered_seeds_path, full_corpus_path, output_dir='./output'):
    """
    Main function to run two-stage keyword discovery with pre-filtered seeds.
    """
    # Stage 1: Load filtered seed keywords
    seed_keywords = stage1_load_filtered_seeds(
        filtered_seeds_path,
        stem_column='stem',
        filter_column='relevant',
        env_label='environmental',
        precision_column='precision',
        recall_column='recall',
        original_words_column='original_words'
    )
    
    # Load full corpus
    print("\nLoading full corpus...")
    full_df = pd.read_csv(full_corpus_path)
    print(f"  Loaded {len(full_df)} documents")
    print()
    
    # Stage 2: Discover new keywords from full corpus
    discovered_keywords, scored_df = stage2_discover_keywords(
        full_df,
        seed_keywords,
        text_column='text',
        score_threshold_percentile=75,
        min_high_score_count=20,
        min_lift=2.0
    )
    
    # Save results
    print("\n" + "=" * 80)
    print("SAVING RESULTS")
    print("=" * 80)
    save_results(seed_keywords, discovered_keywords, scored_df, output_dir)
    
    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)
    print(f"  Filtered seed keywords: {len(seed_keywords)}")
    print(f"  Discovered keywords: {len(discovered_keywords)}")
    print(f"  Total keywords: {len(seed_keywords) + len(discovered_keywords)}")
    
    return seed_keywords, discovered_keywords, scored_df


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python keyword_discovery_v2.py <filtered_seeds.xlsx> <full_corpus.csv> [output_dir]")
        print()
        print("Example:")
        print("  python keyword_discovery_v2.py seed_keywords_filtered.xlsx relevant_corpus.csv ./output")
        sys.exit(1)
    
    filtered_seeds_path = sys.argv[1]
    full_corpus_path = sys.argv[2]
    output_dir = sys.argv[3] if len(sys.argv) > 3 else './output'
    
    main(filtered_seeds_path, full_corpus_path, output_dir)

"""
Two-Stage Keyword Discovery for Environmental Policy Documents
Katelyn Nutley, 07-11-2025

Uses filtered seed keywords to score a full corpus, then discovers new
co-occurring keywords via lift ratio.

Usage: python two_stage_keyword_discovery.py <filtered_seeds.xlsx> <full_corpus.csv> [unfiltered_discovered_keywords.csv]
"""

import sys
import re
import pandas as pd
import numpy as np
from collections import defaultdict
from datetime import datetime

# ============================================================================
# STOPWORDS
# ============================================================================

domain_stopwords = {
    # Geographic/organizational
    'europ', 'europa', 'union',
    # Policy framework
    'approach', 'framework', 'strategi', 'strategy', 'aim', 'object',
    'objective', 'prioriti', 'priority', 'goal', 'target', 'line',
    # Policy mechanisms
    'mechan', 'mechanism', 'scheme', 'guidelin', 'guideline', 'platform',
    'programm', 'programme', 'program', 'initiative', 'initi', 'roadmap',
    'outlin', 'budgetari', 'properti',
    # Action verbs
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
    # Structural
    'differ', 'different', 'unit', 'step', 'phase', 'stage',
    'level', 'structur', 'structure', 'process', 'dimension', 'aspect',
    'element', 'compon', 'component', 'scope', 'advance', 'benchmark',
    # Actors
    'stakehold', 'agenc', 'actor', 'partner', 'individu', 'human',
    'person', 'entiti', 'membership', 'staff',
    # Descriptors
    'high', 'higher', 'low', 'lower', 'wide', 'broad', 'strong', 'key',
    'main', 'major', 'essenti', 'essential', 'important', 'critical',
    'clear', 'sufficient', 'suffici', 'better', 'best', 'negat',
    'convent', 'benefici',
    # Temporal
    'recent', 'recently', 'current', 'currently', 'next', 'futur',
    'future', 'beyond', 'toward', 'towards', 'long', 'term', 'period',
    'decad', 'decade',
    # Quantitative
    'increas', 'increase', 'decreas', 'decrease', 'growth', 'share',
    'rate', 'percent', 'proportion', 'billion', 'half', 'per',
    'reduct', 'million', 'larg',
    # Qualities/outcomes
    'qualiti', 'quality', 'success', 'successful', 'benefit',
    'impact', 'effect', 'progress', 'effort', 'achievement',
    'result', 'outcome', 'capac', 'capacity', 'potenti', 'potential',
    'flexibl', 'flexibility', 'innov', 'extens',
    # Processes
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
    # Composite
    'overal', 'overall', 'insuffici', 'insufficient', 'signific',
    'significant', 'ambiti', 'ambitious',
    # Miscellaneous
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
}

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


# ============================================================================
# STEMMER
# ============================================================================

class PorterStemmer:

    def __init__(self):
        self.vowels = set('aeiou')

    def _is_consonant(self, word, i):
        if word[i] in self.vowels:
            return False
        if word[i] == 'y':
            return i == 0 or not self._is_consonant(word, i - 1)
        return True

    def _measure(self, stem):
        cv = ''
        for i in range(len(stem)):
            cv += 'c' if self._is_consonant(stem, i) else 'v'
        compressed = ''.join(c for i, c in enumerate(cv) if i == 0 or c != cv[i-1])
        return compressed.count('vc')

    def stem(self, word):
        word = word.lower()
        if len(word) <= 2:
            return word

        # Step 1a
        if word.endswith('sses'):
            word = word[:-2]
        elif word.endswith('ies'):
            word = word[:-2]
        elif word.endswith('ss'):
            pass
        elif word.endswith('s'):
            word = word[:-1]

        # Step 1b
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

        # Step 1c
        if word.endswith('y') and len(word) > 2:
            if not self._is_consonant(word, len(word) - 2):
                word = word[:-1] + 'i'

        # Step 2
        for suffix, replacement in [
            ('ational', 'ate'), ('tional', 'tion'), ('enci', 'ence'),
            ('anci', 'ance'), ('izer', 'ize'), ('abli', 'able'),
            ('alli', 'al'), ('entli', 'ent'), ('eli', 'e'),
            ('ousli', 'ous'), ('ization', 'ize'), ('ation', 'ate'),
            ('ator', 'ate'), ('alism', 'al'), ('iveness', 'ive'),
            ('fulness', 'ful'), ('ousness', 'ous'), ('aliti', 'al'),
            ('iviti', 'ive'), ('biliti', 'ble')
        ]:
            if word.endswith(suffix):
                stem = word[:-len(suffix)]
                if self._measure(stem) > 0:
                    word = stem + replacement
                break

        # Step 3
        for suffix, replacement in [
            ('icate', 'ic'), ('ative', ''), ('alize', 'al'),
            ('iciti', 'ic'), ('ical', 'ic'), ('ful', ''), ('ness', '')
        ]:
            if word.endswith(suffix):
                stem = word[:-len(suffix)]
                if self._measure(stem) > 0:
                    word = stem + replacement
                break

        # Step 4
        for suffix in [
            'al', 'ance', 'ence', 'er', 'ic', 'able', 'ible', 'ant',
            'ement', 'ment', 'ent', 'ion', 'ou', 'ism', 'ate', 'iti',
            'ous', 'ive', 'ize'
        ]:
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
            elif self._measure(stem) == 1 and len(stem) >= 3 and stem[-1] not in 'wxy':
                if not (self._is_consonant(stem, -1) and
                        not self._is_consonant(stem, -2) and
                        self._is_consonant(stem, -3)):
                    word = stem

        # Step 5b
        if word.endswith('ll') and self._measure(word[:-1]) > 1:
            word = word[:-1]

        return word


# ============================================================================
# PROCESSING
# ============================================================================

stemmer = PorterStemmer()


def is_valid_stem(stem):
    if len(stem) < 3:
        return False
    if stem in domain_stopwords:
        return False
    if any(len(sw) >= 3 and (stem.startswith(sw) or sw.startswith(stem))
           for sw in domain_stopwords):
        return False
    return True


def tokenize_and_stem(text):
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


def load_seeds(seeds_path):
    df = pd.read_excel(seeds_path)
    env_mask = df['relevant'].str.lower().str.strip() == 'environmental'
    env = df[env_mask]

    seeds = {}
    for _, row in env.iterrows():
        stem = str(row['stem']).strip()
        try:
            precision = float(row['precision']) if pd.notna(row.get('precision')) else 1.0
        except:
            precision = 1.0
        seeds[stem] = precision

    print(f"Loaded {len(seeds)} seed keywords")
    return seeds


def score_document(stems, seeds):
    return sum(seeds[s] for s in stems if s in seeds)


def discover_keywords(corpus_df, seeds, text_column='text',
                      score_percentile=75, min_count=20, min_lift=2.0):
    print(f"Scoring {len(corpus_df)} documents...")

    doc_scores = []
    doc_stems = []
    stem_to_words = defaultdict(set)

    for _, row in corpus_df.iterrows():
        stems, word_map = tokenize_and_stem(row[text_column])
        doc_scores.append(score_document(stems, seeds))
        doc_stems.append(stems)
        for stem, words in word_map.items():
            stem_to_words[stem].update(words)

    nonzero = [s for s in doc_scores if s > 0]
    if not nonzero:
        print("WARNING: No documents matched any seed keywords.")
        return {}

    threshold = np.percentile(nonzero, score_percentile)
    high = [s >= threshold for s in doc_scores]

    n_high = sum(high)
    n_low = len(high) - n_high
    print(f"Threshold ({score_percentile}th percentile of non-zero scores): {threshold:.2f}")
    print(f"High-scoring: {n_high} | Low-scoring: {n_low}\n")

    count_high = defaultdict(int)
    count_low = defaultdict(int)

    for is_high, stems in zip(high, doc_stems):
        for stem in stems:
            if is_high:
                count_high[stem] += 1
            else:
                count_low[stem] += 1

    discovered = {}
    for stem in set(count_high) | set(count_low):
        if stem in seeds:
            continue
        if count_high[stem] < min_count:
            continue

        rate_high = count_high[stem] / n_high if n_high > 0 else 0
        rate_low = count_low[stem] / n_low if n_low > 0 else 0
        lift = rate_high / rate_low if rate_low > 0 else (float('inf') if rate_high > 0 else 0)

        if lift >= min_lift:
            discovered[stem] = {
                'stem': stem,
                'lift': lift,
                'high_count': count_high[stem],
                'low_count': count_low[stem],
                'rate_high': rate_high,
                'rate_low': rate_low,
                'original_words': ', '.join(sorted(stem_to_words[stem]))
            }

    discovered = dict(sorted(discovered.items(), key=lambda x: (-x[1]['lift'], -x[1]['high_count'])))
    print(f"Discovered {len(discovered)} candidate keywords")
    return discovered


def main(seeds_path, corpus_path, output_path='unfiltered_discovered_keywords.csv'):
    seeds = load_seeds(seeds_path)

    print(f"\nLoading corpus from {corpus_path}...")
    corpus_df = pd.read_csv(corpus_path)
    print(f"Loaded {len(corpus_df)} documents\n")

    discovered = discover_keywords(corpus_df, seeds)

    if discovered:
        df_out = pd.DataFrame(discovered.values())
        df_out['lift'] = df_out['lift'].replace(float('inf'), 9999)
        df_out.to_csv(output_path, index=False)
        print(f"\nSaved to: {output_path}")
    else:
        print("No keywords discovered.")


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python two_stage_keyword_discovery.py <filtered_seeds.xlsx> <full_corpus.csv> [unfiltered_discovered_keywords.csv]")
        sys.exit(1)
    seeds_path = sys.argv[1]
    corpus_path = sys.argv[2]
    output_path = sys.argv[3] if len(sys.argv) > 3 else 'unfiltered_discovered_keywords.csv'
    main(seeds_path, corpus_path, output_path)

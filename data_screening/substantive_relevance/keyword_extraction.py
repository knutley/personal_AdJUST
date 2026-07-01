"""
Keyword Extraction for Randomly Sampled Environmental Documents 
Katelyn Nutley, 07-11-2025

Identifies keywords in randomly sampled docs that statistically distinguish 
environmental docs (as defined in corpus_random.xlsx) from non-environmental 
ones using precision/recall metrics.

Usage: python keyword_extractor.py <corpus.xlsx> [output.txt]
"""

import sys
import re
import pandas as pd
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
        return [], {}

    tokens = re.findall(r'\b[a-zA-Z][\w-]*[a-zA-Z]\b|\b[a-zA-Z]\b', str(text).lower())
    stems = []
    stem_to_words = defaultdict(set)

    for token in tokens:
        if len(token) < 3 or token in english_stopwords:
            continue
        stem = stemmer.stem(token)
        if is_valid_stem(stem):
            stems.append(stem)
            stem_to_words[stem].add(token)

    return stems, stem_to_words


def calculate_keyword_metrics(df, text_column='text', target_column='included'):
    target_docs = df[df[target_column] == 1]
    non_target_docs = df[df[target_column] == 0]

    n_target = len(target_docs)
    n_non_target = len(non_target_docs)

    print(f"Total: {n_target + n_non_target} | Target: {n_target} | Non-target: {n_non_target}\n")

    keyword_in_target = defaultdict(int)
    keyword_in_non_target = defaultdict(int)
    keyword_to_words = defaultdict(set)

    for _, row in target_docs.iterrows():
        stems, stem_map = tokenize_and_stem(row[text_column])
        for stem in set(stems):
            keyword_in_target[stem] += 1
            keyword_to_words[stem].update(stem_map[stem])

    for _, row in non_target_docs.iterrows():
        stems, stem_map = tokenize_and_stem(row[text_column])
        for stem in set(stems):
            keyword_in_non_target[stem] += 1
            keyword_to_words[stem].update(stem_map[stem])

    keywords_data = []
    for stem in set(keyword_in_target) | set(keyword_in_non_target):
        target_count = keyword_in_target.get(stem, 0)
        non_target_count = keyword_in_non_target.get(stem, 0)
        total_count = target_count + non_target_count

        if target_count < 2:
            continue

        precision = target_count / total_count if total_count > 0 else 0
        recall = target_count / n_target if n_target > 0 else 0

        keywords_data.append({
            'stem': stem,
            'original_words': ' '.join(sorted(keyword_to_words[stem])),
            'target_count': target_count,
            'non_target_count': non_target_count,
            'total_count': total_count,
            'precision': precision,
            'recall': recall,
            'f1': 2 * precision * recall / (precision + recall) if (precision + recall) > 0 else 0
        })

    keywords_data.sort(key=lambda x: (-x['precision'], -x['recall']))
    return keywords_data, n_target, n_non_target


def save_output(keywords_data, output_path):
    pd.DataFrame(keywords_data).to_csv(output_path, index=False)
    print(f"Saved to: {output_path}")


def main(corpus_path, output_path=None):
    df = pd.read_excel(corpus_path)
    df = df[df['included'].notna()]

    print(f"Processing {len(df)} documents...\n")

    keywords_data, n_target, n_non_target = calculate_keyword_metrics(df)
    print(f"Unique keywords extracted: {len(keywords_data)}\n")

    if output_path:
        save_output(keywords_data, output_path)

    return keywords_data


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python keyword_extractor.py [corpus.xlsx] [unfiltered_seeds.csv]")
    corpus_path = sys.argv[1] if len(sys.argv) > 1 else 'corpus_random.xlsx'
    output_path = sys.argv[2] if len(sys.argv) > 2 else 'unfiltered_seeds.csv'
    main(corpus_path, output_path)

"""
Title: Keyword Extraction for Environmental Policy Documents
Author: Katelyn Nutley
Date: 07-11-2025

This script identifies keywords that statistically distinguish environmental policy 
documents (target) from non-environmental policy documents (non-target) in a corpus.
It predominantly relies on precision/recall metrics.
"""

import pandas as pd
import numpy as np
import re
from collections import defaultdict, Counter
from datetime import datetime

# ============================================================================
# DOMAIN STOPWORDS - Generic policy/administrative terms to filter out
# ============================================================================
domain_stopwords = [
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
    'integr', 'integration', 'holistic', 'complement', 'complementary',
    # Structural terms
    'differ', 'different', 'unit', 'line', 'step', 'phase', 'stage',
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
    'impact', 'effect', 'progress', 'effort', 'achiev', 'achievement',
    'result', 'outcome', 'capac', 'capacity', 'potenti', 'potential',
    'flexibl', 'flexibility', 'innov', 'extens',
    # Processes (generic)
    'introduct', 'introduction', 'start', 'launch', 'implement',
    'expect', 'expectation', 'project', 'projection', 'emerg',
    'emerging', 'shift', 'transit', 'transition', 'transform',
    'transformation', 'indirect',
    # Meta/discourse
    'exampl', 'example', 'show', 'see', 'seen', 'demonstr',
    'demonstrate', 'identifi', 'identify', 'discuss', 'discussion',
    'figur', 'figure', 'illustrat', 'illustrate', 'indicat',
    'indicate', 'methodolog', 'space', 'asset',
    # Connectors/modifiers
    'often', 'could', 'moreov', 'moreover', 'furthermor',
    'furthermore', 'alread', 'already', 'rather', 'well', 'togeth',
    'together', 'around', 'along', 'close', 'near',
    # Generic nouns
    'meet', 'meeting', 'work', 'working', 'issu', 'issue',
    'challeng', 'challenge', 'risk', 'problem', 'concern',
    'attent', 'attention', 'awar', 'awareness', 'knowledg',
    'knowledge',
    # Administrative
    'govern', 'government', 'governance', 'fund', 'funding',
    'cost', 'invest', 'investment', 'economi', 'economic', 'economy',
    'competit', 'competition', 'competitive', 'trade',
    # Composite terms (already covered by stems)
    'overal', 'overall', 'comprehens', 'comprehensive', 'insuffici',
    'insufficient', 'signific', 'significant', 'essenti', 'essential',
    'ambiti', 'ambitious',
    # Miscellaneous generic
    'broad', 'like', 'better', 'key', 'across', 'play', 'face',
    'sound', 'rang', 'range', 'compar', 'compare', 'role', 'dimens',
    'scale', 'phase', 'earli', 'early', 'focus', 'gap', 'clear',
    'put', 'come', 'start', 'remain', 'cycl', 'cycle',
    'revis', 'revision', 'strong', 'network', 'option',
    'led', 'complement', 'incorpor', 'incorporate', 'combin',
    'combine', 'offer', 'intend', 'intended', 'prepar', 'prepare',
    'construct', 'balanc', 'balance', 'trend', 'scenario',
    'complianc', 'compliance', 'life', 'recoveri', 'recovery',
    'intens', 'intensive', 'site', 'acceler', 'accelerate',
    'int', 'non', 'yet', 'much',
    # Web artifacts
    'html', 'htm', 'pdf', 'http', 'https', 'www'
]

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
    'most', 'other', 'some', 'any', 'each', 'all', 'both', 'few',
    'more', 'most', 'other', 'some', 'such', 'no', 'nor', 'not'
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
        """Calculate the measure of a stem (number of VC sequences)."""
        cv_sequence = ''
        for i, char in enumerate(stem):
            if self._is_consonant(stem, i):
                cv_sequence += 'c'
            else:
                cv_sequence += 'v'
        # Compress sequences
        compressed = ''
        for char in cv_sequence:
            if not compressed or compressed[-1] != char:
                compressed += char
        # Count VC pairs
        return compressed.count('vc')
    
    def stem(self, word):
        """Stem a word using simplified Porter rules."""
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
        
        # Step 2: common suffixes
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
        
        # Step 5a: remove trailing e
        if word.endswith('e'):
            stem = word[:-1]
            if self._measure(stem) > 1:
                word = stem
            elif self._measure(stem) == 1:
                # Check if stem ends with CVC (not w, x, y)
                if len(stem) >= 3 and not stem[-1] in 'wxy':
                    if self._is_consonant(stem, -1) and not self._is_consonant(stem, -2) and self._is_consonant(stem, -3):
                        pass
                    else:
                        word = stem
        
        # Step 5b: remove double consonant + l
        if word.endswith('ll') and self._measure(word[:-1]) > 1:
            word = word[:-1]
        
        return word


def tokenize_and_stem(text, stemmer):
    """Tokenize text and return stemmed tokens with original word mappings."""
    if pd.isna(text):
        return [], {}
    
    # Tokenize: extract words (including hyphenated compounds)
    tokens = re.findall(r'\b[a-zA-Z][\w-]*[a-zA-Z]\b|\b[a-zA-Z]\b', text.lower())
    
    stemmed_tokens = []
    stem_to_words = defaultdict(set)
    
    for token in tokens:
        # Skip if too short or in stopwords
        if len(token) < 3 or token in english_stopwords:
            continue
        
        # Stem the token
        stem = stemmer.stem(token)
        
        # Skip if stem is too short or in domain stopwords
        if len(stem) < 3:
            continue
        if stem in domain_stopwords:
            continue
        if any(stem.startswith(sw) or sw.startswith(stem) for sw in domain_stopwords if len(sw) >= 3):
            continue
        
        stemmed_tokens.append(stem)
        stem_to_words[stem].add(token)
    
    return stemmed_tokens, stem_to_words


def calculate_keyword_metrics(df, text_column='text', target_column='included'):
    """
    Calculate precision and recall for each stemmed keyword.
    
    Precision = P(target | keyword present) = docs with keyword AND target / docs with keyword
    Recall = P(keyword present | target) = docs with keyword AND target / all target docs
    """
    stemmer = PorterStemmerSimple()
    
    # Separate target and non-target documents
    target_docs = df[df[target_column] == 1]
    non_target_docs = df[df[target_column] == 0]
    
    n_target = len(target_docs)
    n_non_target = len(non_target_docs)
    n_total = n_target + n_non_target
    
    print(f"Total search set: {n_total}")
    print(f"Target set size: {n_target}")
    print(f"Non-target set size: {n_non_target}")
    print()
    
    # Track keyword presence in documents
    keyword_in_target = defaultdict(int)  # Count of target docs containing keyword
    keyword_in_non_target = defaultdict(int)  # Count of non-target docs containing keyword
    keyword_to_words = defaultdict(set)  # Map stems to original words
    
    # Process target documents
    for idx, row in target_docs.iterrows():
        tokens, stem_map = tokenize_and_stem(row[text_column], stemmer)
        unique_stems = set(tokens)
        for stem in unique_stems:
            keyword_in_target[stem] += 1
            keyword_to_words[stem].update(stem_map[stem])
    
    # Process non-target documents
    for idx, row in non_target_docs.iterrows():
        tokens, stem_map = tokenize_and_stem(row[text_column], stemmer)
        unique_stems = set(tokens)
        for stem in unique_stems:
            keyword_in_non_target[stem] += 1
            keyword_to_words[stem].update(stem_map[stem])
    
    # Calculate metrics for each keyword
    keywords_data = []
    all_keywords = set(keyword_in_target.keys()) | set(keyword_in_non_target.keys())
    
    for stem in all_keywords:
        target_count = keyword_in_target.get(stem, 0)
        non_target_count = keyword_in_non_target.get(stem, 0)
        total_count = target_count + non_target_count
        
        # Skip keywords that appear in fewer than 2 target documents
        if target_count < 2:
            continue
        
        # Calculate precision and recall
        precision = target_count / total_count if total_count > 0 else 0
        recall = target_count / n_target if n_target > 0 else 0
        
        # Get original words
        original_words = sorted(keyword_to_words[stem])
        words_str = ' '.join(original_words) if original_words else stem
        
        keywords_data.append({
            'stem': stem,
            'original_words': words_str,
            'target_count': target_count,
            'non_target_count': non_target_count,
            'total_count': total_count,
            'precision': precision,
            'recall': recall,
            'f1': 2 * (precision * recall) / (precision + recall) if (precision + recall) > 0 else 0
        })
    
    # Sort by precision (descending), then by recall (descending)
    keywords_data.sort(key=lambda x: (-x['precision'], -x['recall']))
    
    return keywords_data, n_target, n_non_target


def filter_environmental_keywords(keywords_data, precision_threshold=0.75):
    """
    Filter keywords to retain those that are either:
    1. Obviously environmental policy related, OR
    2. Have precision > threshold
    """
    # Environmental policy related stems (manually curated)
    environmental_stems = {
        'climat', 'energi', 'transport', 'emiss', 'infrastructur', 'water',
        'wast', 'gas', 'agricultur', 'renew', 'natur', 'soil', 'biodivers',
        'fuel', 'air', 'sustain', 'carbon', 'pollut', 'forest', 'greenhous',
        'circular', 'fossil', 'climat-neutral', 'renewabl', 'co2', 'plastic',
        'deforest', 'reforest', 'decarbonis', 'toxicit', 'phaseout', 'bio-bas',
        'fossil-bas', 'climat-relat', 'climat-friendli', 'carbon-neutr',
        'fossil-fuel', 'low-carbon', 'greenhouse-gas', 'zero-carbon',
        'footprint', 'leakag', 'displac', 'mitig', 'offsett', 'electrif',
        'hazard', 'hydroelectr', 'lifecycl', 'mainten', 'retrofit'
    }
    
    filtered = []
    env_keywords = []
    high_precision_keywords = []
    
    for kw in keywords_data:
        stem = kw['stem']
        is_environmental = any(env_stem in stem or stem in env_stem 
                              for env_stem in environmental_stems)
        is_high_precision = kw['precision'] >= precision_threshold
        
        if is_environmental:
            env_keywords.append(kw)
            filtered.append(kw)
        elif is_high_precision:
            high_precision_keywords.append(kw)
            if kw not in filtered:
                filtered.append(kw)
    
    return filtered, env_keywords, high_precision_keywords


def format_output(keywords_data, n_target, n_non_target):
    """Format the output similar to the original MNIR output."""
    output_lines = [
        f"# Author: Katelyn Nutley",
        f"# Title: Target Keywords Output from MNIR ",
        f"# Date: {datetime.now().strftime('%m-%d-%Y')}",
        "",
        "TARGET KEYWORDS",
        f"Total search set: {n_target + n_non_target}",
        f"Target set size: {n_target}",
        f"Non-target set size: {n_non_target}",
        ""
    ]
    
    for i, kw in enumerate(keywords_data, 1):
        words = kw['original_words']
        stem = kw['stem']
        recall = kw['recall']
        precision = kw['precision']
        
        # Format: stem (words)    (Recall: X.XX, Precision: XX.X%)
        if words != stem:
            line = f"{i}. {stem} ({words})    (Recall: {recall:.2f}, Precision: {precision*100:.1f}%)"
        else:
            line = f"{i}. {stem}    (Recall: {recall:.2f}, Precision: {precision*100:.1f}%)"
        
        output_lines.append(line)
    
    return '\n'.join(output_lines)


def main(corpus_path, output_path=None):
    """Main function to run the keyword extraction."""
    # Load corpus
    print("Loading corpus...")
    df = pd.read_excel(corpus_path)
    
    # Filter to valid documents (exclude NaN in included column)
    df = df[df['included'].notna()]
    
    print(f"\nProcessing {len(df)} documents...")
    print()
    
    # Calculate keyword metrics
    keywords_data, n_target, n_non_target = calculate_keyword_metrics(
        df, text_column='text', target_column='included'
    )
    
    print(f"Total unique keywords extracted: {len(keywords_data)}")
    print()
    
    # Format and print full output
    output = format_output(keywords_data, n_target, n_non_target)
    
    if output_path:
        with open(output_path, 'w') as f:
            f.write(output)
        print(f"Full keyword list saved to: {output_path}")
    
    # Filter keywords
    filtered, env_kw, high_prec_kw = filter_environmental_keywords(keywords_data)
    
    print("\n" + "="*80)
    print("FILTERED TARGET KEYWORDS")
    print("Retained words that are environmental policy related OR have precision > 0.75")
    print("="*80)
    
    print("\n    ENVIRONMENTAL POLICY RELATED KEYWORDS:")
    for i, kw in enumerate(env_kw, 1):
        words = kw['original_words']
        stem = kw['stem']
        recall = kw['recall']
        precision = kw['precision']
        if words != stem:
            print(f"    {i}. {stem} ({words})")
            print(f"        (Recall: {recall:.2f}, Precision: {precision*100:.1f}%)")
        else:
            print(f"    {i}. {stem}")
            print(f"        (Recall: {recall:.2f}, Precision: {precision*100:.1f}%)")
    
    print("\n    HIGH PRECISION KEYWORDS (>75%)")
    for i, kw in enumerate(high_prec_kw, 1):
        words = kw['original_words']
        stem = kw['stem']
        recall = kw['recall']
        precision = kw['precision']
        if words != stem:
            print(f"    {i}. {stem} ({words})")
            print(f"        (Recall: {recall:.2f}, Precision: {precision*100:.1f}%)")
        else:
            print(f"    {i}. {stem}")
            print(f"        (Recall: {recall:.2f}, Precision: {precision*100:.1f}%)")
    
    return keywords_data, filtered


if __name__ == "__main__":
    import sys
    
    corpus_path = sys.argv[1] if len(sys.argv) > 1 else '/mnt/user-data/uploads/corpus_random.xlsx'
    output_path = sys.argv[2] if len(sys.argv) > 2 else '/home/claude/target_keywords_output.txt'
    
    keywords, filtered = main(corpus_path, output_path)

# Author: Katelyn Nutley
# Title: Environmental Policy Keyword Scoring and Intial Filtering (Integrated Framework)
# Date: 13-02-2026
# Updated: Integrated complete three-tier keyword framework

"""
Filters corpus using integrated three-tier keyword framework.
Combines expert-curated keywords (Greene et al., 2025) with precision-validated 
terms from frequency analysis (>0.75 precision or clear environmental policy relevance).

Scoring formula:
    Score_i = 3.0 × n_Tier1 + 1.5 × n_Tier2 + 0.5 × n_Tier3 + 2.0 × n_Paradigm

Where paradigm keywords (subset of Tier 1) receive bonus weighting, yielding 
effective weight of 5.0 (3.0 + 2.0).
"""

import os
import pandas as pd
import re
from collections import Counter
from nltk.stem.snowball import SnowballStemmer


# =============================================================================
# KEYWORD DEFINITIONS
# =============================================================================

def get_tier1_keywords():
    """
    Tier 1: High-Confidence
    Core environmental policy terms with consistently strong signal.
    Includes terms with precision >= 75% from frequency analysis plus 
    expert-curated core environmental policy terms.
    """
    return [
        # -----------------------------------------------------------------
        # Climate Change & Emissions
        # -----------------------------------------------------------------
        'climate change', 'global warming', 'global heating', 'carbon',
        'greenhouse gas', 'ghg', 'emissions', 'emission', 'emitting', 'emitters',
        'net zero', 'net-zero', 'carbon-neutral', 'footprint', 'climate-friendly',
        'climate-related', 'decarbonisation', 'decarbonised', 'neutrality',
        'atmospheric', 'atmosphere', 'anthropogenic', 'temperature', 'dioxide',
        'particulate',
        
        # -----------------------------------------------------------------
        # International Climate Policy & Frameworks
        # -----------------------------------------------------------------
        'unfccc', 'paris agreement', 'kyoto protocol', 'copenhagen accord',
        'ipcc', 'ippc', 'climate agreement', 'climate negotiations', 'sdgs', 'sdg',
        'transboundary', 'cop', 'eap', 'e-prtr', 'eir',
        
        # -----------------------------------------------------------------
        # Carbon Pricing & Economic Instruments
        # -----------------------------------------------------------------
        'cap and trade', 'cap-and-trade', 'emissions trading scheme',
        'carbon credit', 'carbon offset', 'carbon pricing', 'fossil fuel tax',
        'fossil fuel subsidy', 'fossil fuel subsidies', 'subsidies', 'market-based',
        'coal tax', 'oil tax', 'gas tax',
        
        # -----------------------------------------------------------------
        # Transition & Mitigation
        # -----------------------------------------------------------------
        'green transition', 'energy transition', 'ecological transition',
        'green deal', 'low-carbon', 'low carbon', 'phaseout', 'phase-out',
        'mitigation', 'mitigating', 'mitigate', 'pathway', 'pathways',
        'decoupling', 'decouple', 'greener',
        
        # -----------------------------------------------------------------
        # Biodiversity & Ecosystems
        # -----------------------------------------------------------------
        'biodiversity', 'ecosystem', 'ecosystems', 'eco-system', 'eco-systems',
        'ecological', 'habitat', 'habitats', 'natura', 'wetland', 'wetlands',
        'flora', 'fauna', 'species', 'nature-based solutions', 'carbon sink',
        'sink', 'sinks', 'lulucf', 'redd', 'redd+', 'diversity', 'wild',
        'bird', 'birds', 'animal', 'animals', 'tropical', 'tropics',
        
        # -----------------------------------------------------------------
        # Land Use & Forestry
        # -----------------------------------------------------------------
        'deforestation', 'reforestation', 'afforestation', 'forest', 'forests',
        'forestry', 'desertification', 'land-based', 'restoration', 'restore',
        'restoring', 'conservation', 'conserve', 'conserving', 'harvest',
        'harvested', 'harvesting',
        
        # -----------------------------------------------------------------
        # Pollution & Contamination
        # -----------------------------------------------------------------
        'pollution', 'pollutant', 'pollutants', 'polluting', 'eutrophication',
        'nitrates', 'nitrate', 'pesticide', 'pesticides', 'sewage', 'sludge',
        'noise', 'discharge', 'discharged', 'discharges', 'discharging',
        'diffuse', 'diffusion', 'litter', 'littering', 'hazard', 'hazardous',
        'hazards',
        
        # -----------------------------------------------------------------
        # Waste & Circular Economy
        # -----------------------------------------------------------------
        'zero waste', 'zero-waste', 'circular', 'landfill', 'landfilling',
        'landfills', 'biodegradable', 'incineration', 'incinerators',
        'composting', 'ecodesign', 'eco-design', 'eco-innovation',
        'eco-innovations', 'eco-innovative', 'resource-efficient',
        'resource-intensive', 'recycling', 'recycled', 'reuse', 'reused',
        'reusing', 're-use', 'disposal', 'dispose', 'disposed', 'wastewater',
        
        # -----------------------------------------------------------------
        # Sustainability Concepts
        # -----------------------------------------------------------------
        'sustainability', 'environmentally', 'unsustainable', 'planetary boundaries',
        'sustainable consumption', 'planet', 'responsibility',
        
        # -----------------------------------------------------------------
        # Green Economy & Growth Models (non-paradigm subset)
        # -----------------------------------------------------------------
        'green growth', 'green economy',
        
        # -----------------------------------------------------------------
        # Innovation & R&D
        # -----------------------------------------------------------------
        'green innovation', 'clean innovation', 'green r&d', 'clean r&d',
        'green research and development', 'clean research and development',
        
        # -----------------------------------------------------------------
        # Justice & Equity (non-paradigm subset)
        # -----------------------------------------------------------------
        'just transition', 'climate justice', 'environmental justice',
        'green new deal'
    ]


def get_paradigm_keywords():
    """
    Paradigm-specific keywords (subset of Tier 1).
    These receive bonus weighting: 3.0 (Tier 1) + 2.0 (paradigm) = 5.0 effective weight.
    
    Three paradigms:
    1. Green Growth / Green Economy
    2. Degrowth / Post-Growth / Limits to Growth
    3. Just Transition / Climate Justice / Environmental Justice
    """
    return [
        # Green Growth paradigm
        'green growth', 'green economy', 'green capitalism',
        
        # Degrowth paradigm
        'degrowth', 'post-growth', 'limits to growth',
        
        # Just Transition paradigm
        'just transition', 'climate justice', 'environmental justice',
        'green new deal'
    ]


def get_tier2_keywords():
    """
    Tier 2: Moderate-Confidence
    Clear environmental relevance but benefits from contextual validation.
    Includes terms with precision 50-75% from frequency analysis plus
    expert-curated terms with clear but context-dependent environmental meaning.
    """
    return [
        # -----------------------------------------------------------------
        # Renewable Energy Sources
        # -----------------------------------------------------------------
        'solar', 'wind', 'hydropower', 'geothermal', 'tidal', 'wave',
        'biogas', 'biomass', 'renewable', 'renewables', 'renewal',
        'clean energy', 'alternative energy', 'hydroelectric', 'lifecycle',
        
        # -----------------------------------------------------------------
        # Fossil Fuels
        # -----------------------------------------------------------------
        'coal', 'oil', 'oils', 'natural gas', 'fossil', 'fossil-based',
        'bio-based', 'methane', 'flaring', 'burn', 'burning',
        
        # -----------------------------------------------------------------
        # Energy Technology & Infrastructure
        # -----------------------------------------------------------------
        'carbon capture and storage', 'carbon capture', 'utilisation and storage',
        'ccs', 'ccus', 'hydrogen', 'battery', 'batteries', 'energy storage',
        'storage', 'nuclear', 'thermal', 'grid', 'grids', 'reactor', 'reactors',
        'fission', 'fusion', 'atom', 'atomic', 'pipeline', 'pipelines',
        'upstream', 'downstream',
        
        # -----------------------------------------------------------------
        # Transport & Mobility
        # -----------------------------------------------------------------
        'electric vehicle', 'electric vehicles', 'ev', 'evs', 'electromobility',
        'public transit', 'modal shift', 'aviation', 'maritime', 'shipping',
        'clean electricity', 'renewable electricity', 'electric buses',
        'electric trucks', 'electric trains', 'public transport', 'mass transit',
        'mass transport', 'walking', 'cycling', 'multi-modal transport',
        'airport', 'airports', 'traffic', 'vehicle', 'vehicles',
        
        # -----------------------------------------------------------------
        # Buildings & Energy Efficiency
        # -----------------------------------------------------------------
        'energy efficiency', 'efficiency', 'energy conservation', 'heat pump',
        'heat pumps', 'insulation', 'renovation', 'appliances', 'household',
        'households', 'low-carbon heat', 'cooling', 'cooled', 'cool',
        'heating', 'heat',
        
        # -----------------------------------------------------------------
        # Industry & Manufacturing
        # -----------------------------------------------------------------
        'green steel', 'green aluminium', 'hydrofluorocarbons', 'chemical',
        'chemicals', 'plastic', 'plastics', 'metal', 'metallic', 'metals',
        'manufacture', 'manufactured', 'manufacturer', 'manufacturers',
        'manufacturing', 'industrial', 'industries', 'raw',
        
        # -----------------------------------------------------------------
        # Agriculture & Food Systems
        # -----------------------------------------------------------------
        'green agriculture', 'green farming', 'sustainable farming',
        'organic farming', 'agroecology', 'carbon farming',
        'climate smart agriculture', 'farm', 'farming', 'farms', 'livestock',
        'fisheries', 'fish', 'fishing', 'blue carbon', 'agricultural',
        'agriculture', 'food', 'foods', 'vegetarian', 'vegetarianism',
        'vegan', 'veganism', 'low-carbon diet',
        
        # -----------------------------------------------------------------
        # Marine & Water Systems
        # -----------------------------------------------------------------
        'marine', 'coastal', 'aquatic', 'ocean', 'oceanic', 'oceans',
        'river', 'rivers', 'sea', 'seas', 'freshwater', 'fresh', 'stream',
        'streams', 'basin', 'basins', 'inland', 'mediterranean', 'island',
        'islands', 'drinking', 'drinks',
        
        # -----------------------------------------------------------------
        # Climate Impacts & Adaptation
        # -----------------------------------------------------------------
        'flood', 'flooding', 'disaster', 'disasters', 'resilience', 'resilient',
        'depletion', 'depleted', 'climate adaptation', 'threat', 'threats',
        'tree-planting', 'tree planting', 'rise', 'rises', 'adverse',
        'loss', 'losses', 'destruction', 'destructive', 'pressure', 'pressures',
        'degradation', 'degraded',
        
        # -----------------------------------------------------------------
        # Green Finance
        # -----------------------------------------------------------------
        'green investment', 'green finance', 'green bonds', 'green technology',
        'clean technology',
        
        # -----------------------------------------------------------------
        # Governance & Planning
        # -----------------------------------------------------------------
        'urban', 'infrastructure', 'municipal', 'municipalities', 'tourism',
        'ngo', 'ngos', 'spatial', 'population', 'populations',
        
        # -----------------------------------------------------------------
        # Health & Wellbeing
        # -----------------------------------------------------------------
        'healthy', 'harm', 'harmful'
    ]


def get_tier3_keywords():
    """
    Tier 3: Ambiguous
    Context-dependent terms — may indicate environmental policy or other domains.
    These terms have precision < 50% or are highly ambiguous without context.
    Should ideally co-occur with Tier 1 or Tier 2 terms to confirm relevance.
    """
    return [
        # -----------------------------------------------------------------
        # Energy (General)
        # -----------------------------------------------------------------
        'energy', 'electricity', 'power', 'powered', 'powers', 'fuel', 'fuels',
        'electric', 'electrical',
        
        # -----------------------------------------------------------------
        # Water & Air (General)
        # -----------------------------------------------------------------
        'water', 'waters', 'air', 'gas', 'gases',
        
        # -----------------------------------------------------------------
        # Environment (General)
        # -----------------------------------------------------------------
        'environment', 'environments', 'environmental', 'green', 'greening',
        'sustainable', 'sustained', 'natural', 'nature', 'resource', 'resources',
        'earth',
        
        # -----------------------------------------------------------------
        # Transport (General)
        # -----------------------------------------------------------------
        'transport', 'transportation', 'transported', 'transports',
        
        # -----------------------------------------------------------------
        # Land & Biology (General)
        # -----------------------------------------------------------------
        'land', 'lands', 'soil', 'soils', 'plant', 'plants', 'biological',
        'movement', 'movements',
        
        # -----------------------------------------------------------------
        # Industry (General)
        # -----------------------------------------------------------------
        'consumption', 'consumer', 'consumers', 'consuming', 'technology',
        'technologies', 'technological', 'solution', 'solutions', 'capture',
        'captured', 'captures', 'release', 'released', 'releases', 'releasing',
        'extract', 'extracted', 'extractive', 'substitute', 'substitutes',
        'substitution', 'circulate', 'circulation', 'preservation', 'preserve',
        'preserved', 'preserves', 'preserving',
        
        # -----------------------------------------------------------------
        # Climate (Unmodified)
        # -----------------------------------------------------------------
        'climate', 'climatic', 'climates',
        
        # -----------------------------------------------------------------
        # Zero (Context-dependent)
        # -----------------------------------------------------------------
        'zero'
    ]


def get_negative_keywords():
    """
    Negative keywords indicating trade/customs documents (for exclusion).
    Documents matching these in the first 1000 characters are flagged.
    """
    return [
        # Customs & Tariffs
        'customs tariff', 'autonomous tariff', 'tariff suspension',
        'tariff quota', 'customs duties', 'import duties', 'export duties',
        'common customs tariff', 'tariff nomenclature', 'tariff concession',
        
        # Trade
        'trade agreement', 'free trade', 'trade regime',
        
        # Export Controls
        'dual-use', 'export control', 'export licence',
        
        # Non-environmental Agriculture
        'animal by-products', 'veterinary', 'direct payments to farmers',
        
        # Financial/Administrative (non-environmental)
        'staff regulations', 'pension scheme', 'allowances and deductions'
    ]


# =============================================================================
# TEXT PROCESSING
# =============================================================================

def process_text(text, stemmer):
    """
    Process text for keyword matching.
    Lowercase, remove numbers/punctuation, stem words.
    
    Args:
        text: Raw text string
        stemmer: Stemmer function (e.g., SnowballStemmer.stem)
    
    Returns:
        Processed and stemmed text string
    """
    if not isinstance(text, str):
        return ''
    
    text = text.lower()
    text = re.sub('[0-9]', ' ', text)
    text = re.sub(r'[!"#$%&()*+,\.\/:;<=>?@[\\\]^_`{|}~\'-]', ' ', text)
    
    words = text.split()
    stemmed_words = [stemmer(w) for w in words if len(w) >= 3]
    
    return ' '.join(stemmed_words)


def stem_keywords(keywords, stemmer):
    """
    Stem a list of keywords (handles multi-word phrases).
    
    Args:
        keywords: List of keyword strings
        stemmer: Stemmer function
    
    Returns:
        List of stemmed keywords
    """
    stemmed = []
    for kw in keywords:
        words = kw.lower().split()
        stemmed_kw = ' '.join([stemmer(w) for w in words if len(w) >= 2])
        if stemmed_kw:
            stemmed.append(stemmed_kw)
    return list(set(stemmed))  # Remove duplicates after stemming


def find_keyword_matches(text, keywords):
    """
    Find which keywords appear in processed text.
    
    Args:
        text: Processed text string
        keywords: List of stemmed keywords
    
    Returns:
        List of matched keywords
    """
    matches = []
    for keyword in keywords:
        # Handle multi-word phrases
        if ' ' in keyword:
            if keyword in text:
                matches.append(keyword)
        else:
            # Single word: use word boundary matching
            pattern = r'\b' + re.escape(keyword) + r'\w*\b'
            if re.search(pattern, text):
                matches.append(keyword)
    return matches


def contains_negative_keywords(text, negative_keywords):
    """
    Check if document contains negative keywords in first 1000 characters.
    
    Args:
        text: Raw text string
        negative_keywords: List of negative keyword strings
    
    Returns:
        Boolean indicating presence of negative keywords
    """
    if not isinstance(text, str):
        return False
    
    text_start = text[:1000].lower()
    return any(kw in text_start for kw in negative_keywords)


# =============================================================================
# SCORING
# =============================================================================

def calculate_score(n_tier1, n_tier2, n_tier3, n_paradigm):
    """
    Calculate weighted environmental policy score.
    
    Formula:
        Score = 3.0 × n_Tier1 + 1.5 × n_Tier2 + 0.5 × n_Tier3 + 2.0 × n_Paradigm
    
    Note: Paradigm keywords are a subset of Tier 1, so they contribute to both
    n_Tier1 (weight 3.0) and n_Paradigm (bonus weight 2.0) = 5.0 effective weight.
    
    Args:
        n_tier1: Count of Tier 1 keyword matches
        n_tier2: Count of Tier 2 keyword matches
        n_tier3: Count of Tier 3 keyword matches
        n_paradigm: Count of paradigm keyword matches
    
    Returns:
        Weighted score (float)
    """
    return (3.0 * n_tier1) + (1.5 * n_tier2) + (0.5 * n_tier3) + (2.0 * n_paradigm)


# =============================================================================
# MAIN
# =============================================================================

def main():
    """
    Main function to filter corpus using integrated keyword framework.
    """
    print("=" * 80)
    print("ENVIRONMENTAL POLICY KEYWORD FILTERING")
    print("Integrated Three-Tier Framework (Updated 13-02-2026)")
    print("=" * 80)
    
    # -----------------------------------------------------------------
    # STEP 1: INITIALISE KEYWORDS
    # -----------------------------------------------------------------
    print("\n[STEP 1] Initialising keywords...")
    print("-" * 80)
    
    stemmer = SnowballStemmer('english').stem
    
    # Get raw keywords
    tier1_raw = get_tier1_keywords()
    tier2_raw = get_tier2_keywords()
    tier3_raw = get_tier3_keywords()
    paradigm_raw = get_paradigm_keywords()
    negative_raw = get_negative_keywords()
    
    # Stem keywords
    tier1_stemmed = stem_keywords(tier1_raw, stemmer)
    tier2_stemmed = stem_keywords(tier2_raw, stemmer)
    tier3_stemmed = stem_keywords(tier3_raw, stemmer)
    paradigm_stemmed = stem_keywords(paradigm_raw, stemmer)
    
    print(f"Tier 1 (High-Confidence):     {len(tier1_raw):>3} raw -> {len(tier1_stemmed):>3} stemmed")
    print(f"Tier 2 (Moderate-Confidence): {len(tier2_raw):>3} raw -> {len(tier2_stemmed):>3} stemmed")
    print(f"Tier 3 (Ambiguous):           {len(tier3_raw):>3} raw -> {len(tier3_stemmed):>3} stemmed")
    print(f"Paradigm (subset of Tier 1):  {len(paradigm_raw):>3} raw -> {len(paradigm_stemmed):>3} stemmed")
    print(f"Negative (exclusion):         {len(negative_raw):>3} keywords")
    
    total_unique = len(set(tier1_stemmed + tier2_stemmed + tier3_stemmed))
    print(f"\nTotal unique stemmed keywords: {total_unique}")
    
    # -----------------------------------------------------------------
    # STEP 2: LOAD CORPUS
    # -----------------------------------------------------------------
    print("\n[STEP 2] Loading corpus...")
    print("-" * 80)
    
    possible_paths = [
        os.path.expanduser('~/Documents/GitHub/personal_AdJUST/relevant_corpus.csv'),
        os.path.expanduser('~/Documents/relevant_corpus.csv'),
        '/mnt/user-data/uploads/relevant_corpus.csv',
        'corpus_relevant.csv',
        'relevant_corpus.csv'
    ]
    
    corpus_file = None
    for path in possible_paths:
        if os.path.exists(path):
            corpus_file = path
            break
    
    if corpus_file is None:
        print("ERROR: Corpus file not found. Tried:")
        for path in possible_paths:
            print(f"   {path}")
        print("\nPlease provide the corpus file path.")
        return None
    
    corpus = pd.read_csv(corpus_file, encoding='utf-8')
    print(f"Loaded {len(corpus):,} documents from:\n   {corpus_file}")
    
    # Validate required columns
    if 'text' not in corpus.columns:
        print("ERROR: 'text' column not found in corpus")
        return None
    
    # Standardise ID column
    if 'celex' in corpus.columns:
        corpus.rename(columns={'celex': 'id'}, inplace=True)
    elif 'id' not in corpus.columns:
        corpus['id'] = range(len(corpus))
    
    # -----------------------------------------------------------------
    # STEP 3: PROCESS TEXT
    # -----------------------------------------------------------------
    print("\n[STEP 3] Processing text...")
    print("-" * 80)
    
    corpus['text_processed'] = corpus['text'].apply(
        lambda x: process_text(x, stemmer)
    )
    
    # Calculate text lengths
    corpus['text_length'] = corpus['text'].apply(lambda x: len(str(x)))
    avg_length = corpus['text_length'].mean()
    print(f"Processed {len(corpus):,} documents")
    print(f"Average document length: {avg_length:,.0f} characters")
    
    # -----------------------------------------------------------------
    # STEP 4: FIND KEYWORD MATCHES
    # -----------------------------------------------------------------
    print("\n[STEP 4] Finding keyword matches...")
    print("-" * 80)
    
    # Find matches for each tier
    corpus['tier1_matches'] = corpus['text_processed'].apply(
        lambda x: find_keyword_matches(x, tier1_stemmed)
    )
    corpus['tier2_matches'] = corpus['text_processed'].apply(
        lambda x: find_keyword_matches(x, tier2_stemmed)
    )
    corpus['tier3_matches'] = corpus['text_processed'].apply(
        lambda x: find_keyword_matches(x, tier3_stemmed)
    )
    corpus['paradigm_matches'] = corpus['text_processed'].apply(
        lambda x: find_keyword_matches(x, paradigm_stemmed)
    )
    
    # Count matches
    corpus['n_tier1'] = corpus['tier1_matches'].apply(len)
    corpus['n_tier2'] = corpus['tier2_matches'].apply(len)
    corpus['n_tier3'] = corpus['tier3_matches'].apply(len)
    corpus['n_paradigm'] = corpus['paradigm_matches'].apply(len)
    
    # Calculate total keywords and score
    corpus['n_total'] = corpus['n_tier1'] + corpus['n_tier2'] + corpus['n_tier3']
    corpus['score'] = corpus.apply(
        lambda row: calculate_score(
            row['n_tier1'], row['n_tier2'], row['n_tier3'], row['n_paradigm']
        ),
        axis=1
    )
    
    print("Keyword matches calculated successfully")
    
    # -----------------------------------------------------------------
    # STEP 5: APPLY NEGATIVE KEYWORD FILTER
    # -----------------------------------------------------------------
    print("\n[STEP 5] Applying negative keyword filter...")
    print("-" * 80)
    
    corpus['has_negative'] = corpus['text'].apply(
        lambda x: contains_negative_keywords(x, negative_raw)
    )
    
    n_negative = corpus['has_negative'].sum()
    pct_negative = n_negative / len(corpus) * 100
    print(f"Documents with trade/customs keywords: {n_negative:,} ({pct_negative:.1f}%)")
    
    # -----------------------------------------------------------------
    # STEP 6: ANALYSE RESULTS
    # -----------------------------------------------------------------
    print("\n[STEP 6] Analysing results...")
    print("-" * 80)
    
    # Keyword match statistics
    print("\nKeyword match statistics:")
    print(f"  {'Tier':<12} {'Mean':>8} {'Median':>8} {'Max':>8} {'Docs>0':>10}")
    print(f"  {'-'*12} {'-'*8} {'-'*8} {'-'*8} {'-'*10}")
    
    for tier_name, col in [('Tier 1', 'n_tier1'), ('Tier 2', 'n_tier2'), 
                            ('Tier 3', 'n_tier3'), ('Paradigm', 'n_paradigm')]:
        mean_val = corpus[col].mean()
        median_val = corpus[col].median()
        max_val = corpus[col].max()
        docs_with = (corpus[col] > 0).sum()
        pct_with = docs_with / len(corpus) * 100
        print(f"  {tier_name:<12} {mean_val:>8.2f} {median_val:>8.0f} {max_val:>8.0f} {docs_with:>6,} ({pct_with:>4.1f}%)")
    
    # Score distribution
    print(f"\nScore distribution:")
    print(f"  Mean:       {corpus['score'].mean():>8.2f}")
    print(f"  Median:     {corpus['score'].median():>8.2f}")
    print(f"  Std Dev:    {corpus['score'].std():>8.2f}")
    print(f"  Min:        {corpus['score'].min():>8.2f}")
    print(f"  Max:        {corpus['score'].max():>8.2f}")
    print(f"  25th pctl:  {corpus['score'].quantile(0.25):>8.2f}")
    print(f"  75th pctl:  {corpus['score'].quantile(0.75):>8.2f}")
    print(f"  90th pctl:  {corpus['score'].quantile(0.90):>8.2f}")
    print(f"  95th pctl:  {corpus['score'].quantile(0.95):>8.2f}")
    
    # Most common keywords per tier
    print(f"\nTop 15 Tier 1 keywords:")
    tier1_all = [kw for matches in corpus['tier1_matches'] for kw in matches]
    for kw, count in Counter(tier1_all).most_common(15):
        pct = count / len(corpus) * 100
        print(f"  {kw:30s}: {count:>6,} ({pct:>5.1f}%)")
    
    print(f"\nTop 15 Tier 2 keywords:")
    tier2_all = [kw for matches in corpus['tier2_matches'] for kw in matches]
    for kw, count in Counter(tier2_all).most_common(15):
        pct = count / len(corpus) * 100
        print(f"  {kw:30s}: {count:>6,} ({pct:>5.1f}%)")
    
    print(f"\nTop 10 Tier 3 keywords:")
    tier3_all = [kw for matches in corpus['tier3_matches'] for kw in matches]
    for kw, count in Counter(tier3_all).most_common(10):
        pct = count / len(corpus) * 100
        print(f"  {kw:30s}: {count:>6,} ({pct:>5.1f}%)")
    
    print(f"\nParadigm keywords:")
    paradigm_all = [kw for matches in corpus['paradigm_matches'] for kw in matches]
    for kw, count in Counter(paradigm_all).most_common():
        pct = count / len(corpus) * 100
        print(f"  {kw:30s}: {count:>6,} ({pct:>5.1f}%)")
    
    # -----------------------------------------------------------------
    # STEP 7: FILTER AND SAVE
    # -----------------------------------------------------------------
    print("\n[STEP 7] Filtering and saving...")
    print("-" * 80)
    
    output_dir = os.path.expanduser('~/Documents/')
    if not os.path.exists(output_dir):
        output_dir = './'
    
    # Define output columns
    output_cols = [
        'id', 'n_tier1', 'n_tier2', 'n_tier3', 'n_paradigm', 'n_total',
        'score', 'has_negative', 'tier1_matches', 'tier2_matches',
        'tier3_matches', 'paradigm_matches', 'text'
    ]
    
    # Ensure all columns exist
    output_cols = [c for c in output_cols if c in corpus.columns]
    
    # Save full corpus with scores
    full_output = os.path.join(output_dir, 'corpus_with_scores1.csv')
    corpus[output_cols].to_csv(full_output, index=False)
    print(f"Saved: {full_output}")
    
    # -----------------------------------------------------------------
    # Apply filtering thresholds
    # -----------------------------------------------------------------
    # Primary filter: High confidence environmental policy documents
    filtered_strict = corpus[
        (corpus['n_tier1'] >= 3) &
        (corpus['score'] >= 10.0) &
        (~corpus['has_negative'])
    ].copy()
    
    # Secondary filter: Moderate confidence
    filtered_moderate = corpus[
        (corpus['n_tier1'] >= 2) &
        (corpus['score'] >= 6.0) &
        (~corpus['has_negative'])
    ].copy()
    
    # Tertiary filter: Broad capture
    filtered_broad = corpus[
        (corpus['n_tier1'] >= 1) &
        (corpus['n_total'] >= 3) &
        (~corpus['has_negative'])
    ].copy()
    
    # Save filtered corpora
    strict_output = os.path.join(output_dir, 'corpus_filtered_strict1.csv')
    filtered_strict[output_cols].to_csv(strict_output, index=False)
    print(f"Saved: {strict_output}")
    print(f"  Strict filter: {len(filtered_strict):,} docs ({len(filtered_strict)/len(corpus)*100:.1f}%)")
    print(f"  Criteria: n_tier1 >= 3, score >= 10.0, no negative keywords")
    
    moderate_output = os.path.join(output_dir, 'corpus_filtered_moderate1.csv')
    filtered_moderate[output_cols].to_csv(moderate_output, index=False)
    print(f"Saved: {moderate_output}")
    print(f"  Moderate filter: {len(filtered_moderate):,} docs ({len(filtered_moderate)/len(corpus)*100:.1f}%)")
    print(f"  Criteria: n_tier1 >= 2, score >= 6.0, no negative keywords")
    
    broad_output = os.path.join(output_dir, 'corpus_filtered_broad1.csv')
    filtered_broad[output_cols].to_csv(broad_output, index=False)
    print(f"Saved: {broad_output}")
    print(f"  Broad filter: {len(filtered_broad):,} docs ({len(filtered_broad)/len(corpus)*100:.1f}%)")
    print(f"  Criteria: n_tier1 >= 1, n_total >= 3, no negative keywords")
    
    # Save validation samples
    for name, df in [('strict', filtered_strict), ('moderate', filtered_moderate)]:
        n_sample = min(100, len(df))
        if n_sample > 0:
            sample = df.sample(n=n_sample, random_state=42)
            sample_output = os.path.join(output_dir, f'validation_sample_{name}.csv')
            sample[output_cols].to_csv(sample_output, index=False)
            print(f"Saved: {sample_output} ({n_sample} docs)")
    
    # -----------------------------------------------------------------
    # SUMMARY
    # -----------------------------------------------------------------
    print("\n" + "=" * 80)
    print("COMPLETE")
    print("=" * 80)
    
    print("\nScoring formula:")
    print("  Score = 3.0 × n_Tier1 + 1.5 × n_Tier2 + 0.5 × n_Tier3 + 2.0 × n_Paradigm")
    print("  (Paradigm keywords contribute to both Tier1 and Paradigm counts,")
    print("   yielding effective weight of 5.0)")
    
    print("\nTier definitions:")
    print("  Tier 1: High-confidence core environmental policy terms (weight 3.0)")
    print("  Tier 2: Moderate-confidence environmental terms (weight 1.5)")
    print("  Tier 3: Ambiguous context-dependent terms (weight 0.5)")
    print("  Paradigm: Green growth, degrowth, just transition terms (bonus +2.0)")
    
    print("\nOutput files:")
    print(f"  corpus_with_scores1.csv       - Full corpus with all scores")
    print(f"  corpus_filtered_strict1.csv   - High-confidence filter")
    print(f"  corpus_filtered_moderate1.csv - Moderate-confidence filter")
    print(f"  corpus_filtered_broad1.csv    - Broad capture filter")
    print(f"  validation_sample_*1.csv      - Samples for manual validation")
    
    return corpus


if __name__ == "__main__":
    result = main()

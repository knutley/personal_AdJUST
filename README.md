
# AdJUST Project

## Overview

[Forthcoming.]

## Project Objectives

- Construct a corpus of EU pre-legislative documents filtered for environmental policy relevance
- Manually annotate a representative subsample to identify environmental policy paradigms (neoclassical economics, green growth, evolutionary economics, post-growth)
- Develop and apply computational classification methods (BERT and few-shot prompting) to classify paradigms across the full corpus
- Analyse paradigm distribution patterns to assess EU policy coherence and identify tensions between economic and environmental priorities
- Create an API and database for continued access to the corpus, thereby ensuring future research on the topic

## Technical Approach

### Phase 1: Corpus Collection and Standardisation

**Data Source:** EurLex Database - utilising the `eurlex` R package for efficient data sourcing
    - 
    - Code for the EurLex scrape entitled **reworking_eurlex_scrape.R**
    - Data produced from code entitled **deduplicated_scrape.csv** 

**Standardisation Framework:**
- Standardised metadata schema capturing:
  - Type: resource type, i.e. PROP_REG, DEC_DRAFT, SWD, etc.
      - Information on resource_types found here: https://op.europa.eu/en/web/eu-vocabularies/concept-scheme/-/resource?        uri=http://publications.europa.eu/resource/authority/resource-type
  - Document ID: celex number (the alphanumeric identifier used by the EU)
  - Date: publication date
  - Author: body responsible
  - Directory: directory
        - Information on directory found here: https://eur-lex.europa.eu/browse/directories/legislation.html
  - Resource Type Used: manual or predefined
  - URL
  - Title
 
  **Deduplication:**
  - Exact Matching: Document ID comparison; automated merging
  - Fuzzy Matching: Title similarity analysis
  - Quality Assurance: Probabilstic sampling for manual review;
 
  Ultimately, nothing was flagged. 

### Phase 2: Text Scraping 

**Text Scraper:** EurLex only gives title names, so it was necessary to build a separate scraper; entitled **text_scraper.py**
- Worth noting is that I had to ex post facto include a secondary script ot clean the checkpoints built into the scraper; entitled **clean_checkpoints.py**
- The output file is called corpus_with_text.csv in the code and is not uploaded here due to space concerns 

### Phase 3: Document Screening

**Structural Relevance:** 
- Random, discrete sampling based on doc_type (28 total in deduplicated_scrape.csv) combined with ad hoc review for structural relevance 
    - Overview available in the corresponding paper; but, AGREE_INTERINSTIT_DRAFT, AMEND_PROP_DEC, AMEND_PROP_REG, RECO, and AMEND_PROP_DIR were excluded (leaving 23 doc_types and 20,316 documents)

**Substantive Relevance (Hybridisation):** 

- **Data-driven Approach (MNIR):**
    - Random sample (n =100) of the remaining 20,316 documents taken and hand-coded for positive vs. negative set class relevance; sample entitled **corpus_random.xlsx**
    - This resulted in a list of 1,694 validated keywords; the code is entitled **mnir_keyword.py** and the keyword list is entitled **target_keywords_validated.txt**
 
- **Expert-led Approach:**
    - An expert-curated list of keywords was supplemented by Fergus Greene and [insert colleague's name here]; the list is entitled **Expert-Curated Keywords**
 
**Integration of Approaches:** 
- Keywords organised into three reliability tiers with paradigm bonus:
  - **Tier 1 (High-Confidence):** Core environmental policy terms with strong signal (weight: 3.0)
  - **Tier 2 (Moderate-Confidence):** Contextually relevant terms requiring validation (weight: 1.5)
  - **Tier 3 (Ambiguous):** Context-dependent terms (weight: 0.5)
- **Scoring Formula:**
```
  Score = 3.0 × n_Tier1 + 1.5 × n_Tier2 + 0.5 × n_Tier3 + 2.0 × n_Paradigm
```
- Output file **corpus_STRATEGY2_moderate.csv**

### Phase 4: Manual Labelling and Machine Learning Classification:

- Bootstrap labelled training set
- Active learning with BERT predictions
- Iterative improvement through expert validation
- Inter-coder reliability assessment (Cohen's kappa)

### Phase 5: Paradigm Classification

**Few-Shot Prompt Engineering:**
- Expert-curated paradigm examples
- Adaptive few-shot prompting with k-means clustering
- Hierarchical classification strategy
- Confidence scoring system

## Technical Requirements

### Dependencies
- **R Packages**: `eurlex`, 'reticulate', 'tidyverse', etc. 
- **Python Libraries**: `BeautifulSoup`, `Scrapy`, `transformers`, 
- **APIs**: EU Publications Office Elasticsearch API
- **ML Models**: BERT (HuggingFace transformers)

### Infrastructure (Needs to be Discussed) 
- API rate limiting and management
- Distributed processing capabilities
- Version control and data lineage tracking

## Contributing

This project involves collaboration between technical developers and domain experts in EU climate policy. Contributions should maintain the balance between technical rigour and policy domain expertise.

## Contact

- **Principal Investigator(s)**: Dr. Marion Dumas and Dr. Fergus Green
- **Computational Researcher**: Katelyn Nutley, kn32@st-andrews.ac.uk
**Institutional Affiliation**: UCL/LSE  

## Contribution Taxonomy ## 
forthcoming
---

*This project represents a novel approach to automated policy analysis, combining computational methods with expert domain knowledge to understand climate policy paradigms in the European Union.*

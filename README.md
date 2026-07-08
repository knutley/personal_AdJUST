
# AdJUST Project

Computational identification of climate policy paradigms in EU pre-legislative documents (1990-2025). 

## Overview

This repository documents the full pipeline architecture — from corpus construction through model fine-tuning to corpus-level classification — necessary to replicate "AdJUST Project: Policy Paradigms in EU Public Bodies" (Green, Dumas & Nutley, submitted to Nature Climate Change). pipeline.txt provides the file-level directory map; this document explains the methodology behind each stage.

Large intermediate/output files (full corpus with text, scored corpus, final classified corpus, and fine-tuned model checkpoints) are not stored in this repository due to size constraints. They are archived on Zenodo with a permanent DOI — 10.5281/zenodo.21263220. See Data and Model Availability for any further questions.

## Project Objectives

- Construct a corpus of EU pre-legislative documents filtered for environmental policy relevance (broader than climate policy relevance)
- Manually annotate a representative subsample to identify climate policy paradigms (neoclassical economics, green growth, evolutionary economics, post-growth)
- Develop and apply computational classification methods (ensemble inclusive of ClimateBERT, DeBERTa-v1, ESG-BERT, FinBERT, RoBERTa-base, and SciBERT) to classify paradigms across the full corpus
- Use few-shot prompting of two frontier LLMs (GPT-4o and Llama 3.3) to benchmark ensemble results
- Perform a battery of robustness tests (individual-model comparison, LLM prompt sensitivity, Just Transition lexical filtering)
- Analyse paradigm distribution patterns to assess EU policy coherence and identify tensions between economic and climate priorities

## Technical Approach

### Phase 1: Corpus Collection and Standardisation

**Data Source:** EurLex Database - utilising the `eurlex` R package for efficient metadata sourcing 
    - Code for the EurLex scrape entitled **reworked_eurlex_scrape.R**

### Phase 2: Text Scraping 

EurLex metadata does not include full document text, so text was retrieved separately. 

- **text_scraper.py** — queries each document's EUR-Lex URL (with CELEX-constructed URLs as fallback), extracts text from parsed HTML via BeautifulSoup/requests, targeting document-body elements with paragraph-level extraction as a fallback.
- **clean_checkpoints.py** — cleans scraper checkpoint files; run in a second terminal while the scraper is active.
- Output: **corpus_with_text.csv** (archived on Zenodo — not included in this repository; see below).
  
### Phase 3: Document Screening

Screening proceeds in two stages: structural relevance (is this document type likely to contain paradigmatic content?) and substantive relevance (does this specific document engage with environmental/climate topics?). 

**Structural Relevance:** 
- A confidence-bound approach using the hypergeometric distribution was used to decide, for each of the 28 resource types, whether it could be excluded with 95% confidence that fewer than 5% of its documents are structurally relevant (full derivation in the paper's Appendix A).
- Rare types (≤8 documents) were reviewed in full; more populous types were sampled at calculated sizes (converging to ~58 documents for large types) and manually reviewed by a legal expert.
Result: 5 of 28 resource types excluded (AGREE_INTERINSTIT_DRAFT, AMEND_PROP_DEC, AMEND_PROP_REG, RECO, AMEND_PROP_DIR), leaving 23 resource types and 20,309 documents.

**Substantive Relevance (Hybridisation):** 

- **Data-driven Approach (Frequency Analysis):**
    - Random sample (n =100) of the remaining 20,316 documents taken and hand-coded for positive vs. negative set class relevance; sample entitled **corpus_random.xlsx**
    - This resulted in a list of 1,833 keywords; the code is entitled **keyword_extraction.py** and the entire keyword list is entitled **frequency_analysis_keywords.txt**
    - This was then further filtered to 187 keywords that had a precision score > 0.75 and/or were ostensibly climate-related; entitled: **retained_frequency_keywords.txt**
 
- **Expert-led Approach:**
    - An expert-curated list of keywords was supplemented by Fergus Greene and [insert colleague's name here]; the list is entitled **Expert-Curated Keywords**
 
- **Integration of Approaches:** 
- Keywords organised into three reliability tiers through iterative review (tiers explained in **tiered_keywords.txt**) with paradigm bonus:
  - **Tier 1 (High-Confidence):** Core environmental policy terms with strong signal (weight: 3.0)
  - **Tier 2 (Moderate-Confidence):** Contextually relevant terms requiring validation (weight: 1.5)
  - **Tier 3 (Ambiguous):** Context-dependent terms (weight: 0.5)
- ***Scoring Formula:***
```
  Score = 3.0 × n_Tier1 + 1.5 × n_Tier2 + 0.5 × n_Tier3 + 2.0 × n_Paradigm
```
- Application of tiered, scoring framework and a small, initial filter entitled **combined_keyword_intial_filtering.py**
- ***Robustness Check:*** 
- As the weighted scoring system is ordinal (and not reflective of some diagnostic value), we performed a robustness check with different values, but similar logic. Below we employed a 4:2:1 split plus a 3-point paradigm bonus:
```
 Score = 4.0 × n_Tier1 + 2.0 × n_Tier2 + 1.0 × n_Tier3 + 3.0 × n_Paradigm
```
- The primary filtering logic remained the same (3 Tier 1 words plus a signal from Tier 2 and 3); 
    - 4:2:1 filter: n_tier1 > 3, score > 14.0 
    - 3:1.5:0.5 filter: n_tier1 > 3, score > 10
 - The outcome was 10,805 docs (3:1.5:0.5) and 10,805 (4:2:1), with 100% overlap across cases. Meaning, of course, that although the logic may change, the substantive findings do not.
 - ***Threshold***: samples in the fourth quartile (above the 75th percentile) were retained; this was done in the expectation that only 

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

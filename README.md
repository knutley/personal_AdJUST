
# AdJUST Project (WP4.4)

This work package (4.4) of the AdJUST Project (HORIZON-CL5-2021-D2-01-12) focuses on the computational identification of climate policy paradigms in EU pre-legislative documents (1990-2025). 

## Overview

This repository documents the full pipeline architecture — from corpus construction through model fine-tuning to corpus-level classification — necessary to replicate "Climate Policy Paradigms at the European Commission" (Green, Dumas & Nutley, submitted to Nature Climate Change). The pipeline.txt provides the file-level directory map; this document explains the methodology behind each stage.

Large or intermediate output files (e.g., full corpus with text, scored corpus, final classified corpus, and fine-tuned model checkpoints) are not stored in this repository due to GitHub's size constraints. They are archived on Zenodo with a permanent DOI — 10.5281/zenodo.21263220. See Data and Model Availability for any further questions.

## Project Objectives

- Construct a corpus of EU pre-legislative documents filtered for environmental policy relevance 
- Manually annotate a representative subsample to identify climate policy paradigms - Green Growth (inclusive of Keynesian and Schumpeterian), Neoclassical, and Post-Growth 
- Develop and apply computational classification methods (ensemble of ClimateBERT, DeBERTa-v1, ESG-BERT, FinBERT, RoBERTa-base, and SciBERT) to classify paradigms across the full corpus
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

Screening proceeds in two stages: structural relevance (is this document type likely to contain paradigmatic content?) and substantive relevance (does this specific document engage with environmental topics?). 

**Structural Relevance:** 
- A confidence-bound approach using the hypergeometric distribution was used to decide, for each of the 28 resource types, whether it could be excluded with 95% confidence that fewer than 5% of its documents are structurally relevant (full derivation in the paper's Appendix A).
- Rare types (≤8 documents) were reviewed in full; more populous types were sampled at calculated sizes (converging to ~58 documents for large types) and manually reviewed by a legal expert.
Result: 5 of 28 resource types excluded (AGREE_INTERINSTIT_DRAFT, AMEND_PROP_DEC, AMEND_PROP_REG, RECO, AMEND_PROP_DIR), leaving 23 resource types and 20,309 documents.

**Substantive Relevance (Hybridisation):** 

- **Data-driven Approach (Seeded Keyword Discovery):**
    - Random sample (n =100) of the remaining 20,309 documents taken and hand-coded for positive vs. negative set class relevance; sample entitled **corpus_random.xlsx**. 
    - This resulted in a list of 1,113 seeds; the code is entitled **keyword_extraction.py** and the unfiltered seed list is entitled **unfiltered_seeds.csv**
    - This was manually filtered to 109 keywords with clear environmental relevance; entitled: **filtered_seeds.csv**
    - The 109 seed keywords were applied to the 20,309-document corpus using **two_stage_keyword_discovery.py**; the output is **unfiltered_discovered_keywords.csv**.
    - The 18,807 unfiltered, discovered keywords were filtered using **bart_classifier.py**; producing the 5,504 filtered keywords in **filtered_discovered_keywords.csv**.
    - The BART-MNLI filtered keywords were then manually filtered to produce 791 keywords found in **double_filtered_discovered_keywords.csv**.
 
- **Expert-led Approach:**
    - An expert-curated list of 138 keywords was supplemented by Fergus Greene and [insert colleague's name here]; the list is entitled **Expert-Curated Keywords.txt**
 
- **Integration of Approaches:**
    - The 109 seeded keywords (**filtered_seeds.csv**), 791 discovered keywords (**double_filtered_discovered_keywords.csv**) and the 138 expert-curated keywords (**Expert-Curated Keywords.txt**) were         combined into a unified keyword set.
    - This set was stemmed, applied to the corpus, and scored according to density; the code is entitled **keyword_density.py**.
    - The distribution was analysed, and a threshold was applied at the 85th percentile (**85per_corpus.py**) to produce **corpus_85th_percentile.csv**.

- ***Robustness Check***
    - 

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

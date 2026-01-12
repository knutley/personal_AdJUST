
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

**Data:**
- **Source:** EurLex Database - utilising the `eurlex` R package for efficient data sourcing
    - Information on resource_types found here: https://op.europa.eu/en/web/eu-vocabularies/concept-scheme/-/resource?uri=http://publications.europa.eu/resource/authority/resource-type
    - Code for the EurLex scrape entitled **reworking_eurlex_scrape.R**
    - Data produced from code entitled **deduplicated_scrape.csv**

**Standardisation Framework:**
- Standardised metadata schema capturing:
  - Document ID
  - Title
  - Publication date
  - Source database
  - URL
  - Document type
  - Actors
  - File hash

### Phase 2: Deduplication System

**Exact Matching:**
- Document ID comparison across EU bodies
- Automated merging of identical documents

**Fuzzy Matching:**
- Title similarity analysis
- Semantic scoring (need to discuss preference between Jaro-Winkler, cosine similarity, Levenshtein distance)
- Temporal similarity windowing

**Quality Assurance:**
- Probabilistic sampling for manual review
- Robust error handling and logging
- Fallback mechanisms for API limitations

### Phase 3: Content Filtering

**Dictionary-Based Method:**
```r
mitigate_terms <- c("emission reduction", "decarbonisation", "carbon neutral")
green_deal_terms <- c("European Green Deal", "Green Deal", "Fit for 55", 
                     "climate law", "climate target plan", "just transition")
```

**Machine Learning Classification:**
- Bootstrap expert-labelled training set
- Active learning with BERT predictions
- Iterative improvement through expert validation
- Inter-coder reliability assessment (Cohen's kappa)

### Phase 4: Paradigm Classification

**Few-Shot Prompt Engineering:**
- Expert-curated paradigm examples
- Adaptive few-shot prompting with k-means clustering
- Hierarchical classification strategy
- Confidence scoring system

## Key Features

### Data Processing
- Automated corpus collection from multiple EU databases
- Intelligent deduplication using multiple matching strategies
- Standardised metadata extraction and storage
- Robust error handling and recovery mechanisms

### Machine Learning
- BERT-based document classification
- Active learning for training set optimisation
- Expert-in-the-loop validation system
- Confidence-based quality assessment

### Policy Analysis
- Multi-paradigm classification (neoclassical, Keynesian, mixed, etc.)
- Temporal analysis of paradigm evolution
- Document type and actor analysis
- Comprehensive validation framework

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

## Methodology

### Document Collection
1. Define query terms and date ranges with domain experts
2. Systematic retrieval from EurLex and EU Publications databases
3. Metadata extraction and standardisation
4. Initial corpus validation

### Deduplication Pipeline
1. Exact matching by document ID
2. Fuzzy matching using multiple similarity metrics
3. Manual validation of detected duplicates
4. Final corpus preparation

### Content Classification
1. Dictionary-based initial filtering
2. Expert annotation of training examples
3. Machine learning model training and validation
4. Iterative improvement through active learning

### Paradigm Analysis
1. Expert-defined paradigm frameworks
2. Few-shot prompt engineering development
3. Large-scale document classification
4. Results validation and quality assessment

## Expected Outcomes

- Comprehensive corpus of EU climate policy documents
- Automated classification system for policy paradigms
- Insights into dominant climate policy approaches in the EU
- Methodological framework for similar policy analysis projects

## Validation Strategy

- **Manual Review**: Probabilistic sampling of classifications
- **Expert Validation**: Domain expert review of paradigm assignments
- **Inter-coder Reliability**: Statistical validation of classification consistency
- **Confidence Scoring**: Uncertainty quantification for all classifications

## Contributing

This project involves collaboration between technical developers and domain experts in EU climate policy. Contributions should maintain the balance between technical rigor and policy domain expertise.

## Contact

- **Principal Investigator(s)**: Dr. Marion Dumas and Dr. Fergus Green
- **Computational Researcher**: Katelyn Nutley, kn32@st-andrews.ac.uk
**Institutional Affiliation**: UCL/LSE  

## Contribution Taxonomy ## 
forthcoming
---

*This project represents a novel approach to automated policy analysis, combining computational methods with expert domain knowledge to understand climate policy paradigms in the European Union.*

# -*- coding: utf-8 -*-
"""
EUR-Lex Document Text Scraper
Scrapes full text from EUR-Lex website using existing metadata (doc IDs, titles, etc.)
"""

import os
import time
import random
import requests
import pandas as pd
from bs4 import BeautifulSoup
import re
from datetime import datetime
import logging

# Set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('eurlex_scraper.log'),
        logging.StreamHandler()
    ]
)


class EurLexScraper:
    """
    Scraper for EUR-Lex documents.
    Fetches full text from EUR-Lex website using document identifiers.
    """
    
    def __init__(self, delay_range=(2, 5), max_retries=3):
        """
        Initialize the EUR-Lex scraper.
        
        Parameters:
        -----------
        delay_range : tuple
            (min, max) seconds to wait between requests
        max_retries : int
            Maximum number of retry attempts for failed requests
        """
        self.base_url = "https://eur-lex.europa.eu"
        self.delay_range = delay_range
        self.max_retries = max_retries
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
        })
    
    def _delay(self):
        """Random delay to be respectful to the server."""
        time.sleep(random.uniform(*self.delay_range))
    
    def construct_url(self, doc_id=None, celex=None, url=None, language='EN'):
        """
        Construct EUR-Lex URL from various identifiers.
        
        Parameters:
        -----------
        doc_id : str, optional
            Document ID (e.g., 'COM(2020) 562')
        celex : str, optional
            CELEX number (e.g., '52020DC0562')
        url : str, optional
            Direct EUR-Lex URL
        language : str
            Language code (EN, FR, DE, etc.)
        
        Returns:
        --------
        str : Full EUR-Lex URL
        """
        if url:
            return url
        
        if celex:
            return f"{self.base_url}/legal-content/{language}/TXT/?uri=CELEX:{celex}"
        
        if doc_id:
            # Try to parse doc_id and convert to CELEX
            # Format: COM(2020) 562 -> 52020DC0562
            match = re.match(r'(COM|SWD|JOIN|SEC)\((\d{4})\)\s*(\d+)', doc_id, re.IGNORECASE)
            if match:
                doc_type, year, number = match.groups()
                
                # Map document types to CELEX codes
                type_map = {
                    'COM': 'DC',
                    'SWD': 'SC',
                    'JOIN': 'JC',
                    'SEC': 'SC'
                }
                
                celex_type = type_map.get(doc_type.upper(), 'DC')
                celex = f"5{year}{celex_type}{number.zfill(4)}"
                return f"{self.base_url}/legal-content/{language}/TXT/?uri=CELEX:{celex}"
        
        return None
    
    def fetch_document_text(self, url, language='EN'):
        """
        Fetch the full text of a document from EUR-Lex.
        
        Parameters:
        -----------
        url : str
            EUR-Lex document URL
        language : str
            Language code (EN, FR, DE, etc.)
        
        Returns:
        --------
        dict : {'text': str, 'success': bool, 'error': str or None, 'title': str or None}
        """
        result = {'text': None, 'title': None, 'success': False, 'error': None}
        
        for attempt in range(self.max_retries):
            try:
                self._delay()
                
                response = self.session.get(url, timeout=30)
                response.raise_for_status()
                
                # Parse HTML
                soup = BeautifulSoup(response.content, 'html.parser')
                
                # Extract text
                text = self._extract_text_from_soup(soup)
                
                # Extract title if available
                title = self._extract_title_from_soup(soup)
                
                if text:
                    result['text'] = text
                    result['title'] = title
                    result['success'] = True
                    logging.info(f"Successfully fetched: {url}")
                    return result
                else:
                    result['error'] = "No text content found"
                    logging.warning(f"No text found for: {url}")
                    
            except requests.exceptions.HTTPError as e:
                if e.response.status_code == 404:
                    result['error'] = "Document not found (404)"
                    logging.error(f"Document not found: {url}")
                    break  # Don't retry 404s
                else:
                    result['error'] = f"HTTP Error: {e.response.status_code}"
                    logging.error(f"Attempt {attempt + 1} failed for {url}: {e}")
                    
            except requests.exceptions.RequestException as e:
                result['error'] = str(e)
                logging.error(f"Attempt {attempt + 1} failed for {url}: {e}")
                
            if attempt < self.max_retries - 1:
                time.sleep(5 * (attempt + 1))  # Exponential backoff
                    
        return result
    
    def _extract_text_from_soup(self, soup):
        """
        Extract text content from BeautifulSoup object.
        Tries multiple methods to find document text.
        """
        # Method 1: Look for main document content
        doc_content = soup.find('div', {'id': 'document'})
        if doc_content:
            # Remove scripts and styles
            for script in doc_content(['script', 'style']):
                script.decompose()
            text = doc_content.get_text(separator=' ', strip=True)
            if len(text) > 100:
                return text
        
        # Method 2: Look for ELI content
        eli_content = soup.find('div', {'class': 'eli-main-content'})
        if eli_content:
            for script in eli_content(['script', 'style']):
                script.decompose()
            text = eli_content.get_text(separator=' ', strip=True)
            if len(text) > 100:
                return text
        
        # Method 3: Look for document body
        doc_body = soup.find('div', {'id': 'text'})
        if doc_body:
            for script in doc_body(['script', 'style']):
                script.decompose()
            text = doc_body.get_text(separator=' ', strip=True)
            if len(text) > 100:
                return text
        
        # Method 4: Look for all paragraphs
        paragraphs = soup.find_all('p')
        if paragraphs:
            text = ' '.join([p.get_text(strip=True) for p in paragraphs])
            if len(text) > 100:
                return text
        
        return None
    
    def _extract_title_from_soup(self, soup):
        """Extract document title from page."""
        # Try page title
        title_tag = soup.find('title')
        if title_tag:
            return title_tag.get_text(strip=True)
        
        # Try h1
        h1 = soup.find('h1')
        if h1:
            return h1.get_text(strip=True)
        
        return None
    
    def scrape_from_metadata(self, metadata_df, doc_id_col=None, celex_col=None, 
                            url_col=None, language='EN', output_dir='eurlex_documents',
                            save_individual=True, checkpoint_every=10):
        """
        Scrape documents using metadata DataFrame.
        
        Parameters:
        -----------
        metadata_df : pd.DataFrame
            DataFrame containing document metadata
        doc_id_col : str, optional
            Column name containing document IDs (e.g., 'COM(2020) 562')
        celex_col : str, optional
            Column name containing CELEX numbers
        url_col : str, optional
            Column name containing direct URLs
        language : str
            Language code for documents to fetch
        output_dir : str
            Directory to save individual text files
        save_individual : bool
            Whether to save each document as individual .txt file
        checkpoint_every : int
            Save progress checkpoint every N documents
        
        Returns:
        --------
        pd.DataFrame : Original metadata with added 'text', 'scraped_title', and 'scrape_status' columns
        """
        # Create output directory
        if save_individual and not os.path.exists(output_dir):
            os.makedirs(output_dir)
        
        # Initialize columns for results
        metadata_df['text'] = None
        metadata_df['scraped_title'] = None
        metadata_df['scrape_status'] = 'pending'
        metadata_df['scrape_timestamp'] = None
        
        total = len(metadata_df)
        successful = 0
        failed = 0
        
        logging.info(f"Starting scrape of {total} documents...")
        
        for idx, row in metadata_df.iterrows():
            # Get identifier
            identifier = None
            if url_col and pd.notna(row.get(url_col)):
                identifier = f"URL from {url_col}"
                url = row[url_col]
            elif celex_col and pd.notna(row.get(celex_col)):
                identifier = row[celex_col]
                url = self.construct_url(celex=row[celex_col], language=language)
            elif doc_id_col and pd.notna(row.get(doc_id_col)):
                identifier = row[doc_id_col]
                url = self.construct_url(doc_id=row[doc_id_col], language=language)
            else:
                logging.warning(f"Row {idx}: No valid identifier found")
                metadata_df.at[idx, 'scrape_status'] = 'failed_no_identifier'
                failed += 1
                continue
            
            logging.info(f"Processing {idx + 1}/{total}: {identifier}")
            
            if not url:
                metadata_df.at[idx, 'scrape_status'] = 'failed_url_construction'
                failed += 1
                continue
            
            # Fetch document
            result = self.fetch_document_text(url, language=language)
            
            if result['success']:
                metadata_df.at[idx, 'text'] = result['text']
                metadata_df.at[idx, 'scraped_title'] = result['title']
                metadata_df.at[idx, 'scrape_status'] = 'success'
                metadata_df.at[idx, 'scrape_timestamp'] = datetime.now().isoformat()
                successful += 1
                
                # Save individual file if requested
                if save_individual:
                    # Create safe filename
                    if celex_col and pd.notna(row.get(celex_col)):
                        doc_name = str(row[celex_col])
                    elif doc_id_col and pd.notna(row.get(doc_id_col)):
                        doc_name = str(row[doc_id_col])
                    else:
                        doc_name = f"doc_{idx}"
                    
                    filename = re.sub(r'[^\w\-_.]', '_', doc_name) + '.txt'
                    filepath = os.path.join(output_dir, filename)
                    
                    with open(filepath, 'w', encoding='utf-8') as f:
                        f.write(result['text'])
            else:
                error_msg = result['error'] if result['error'] else 'unknown_error'
                metadata_df.at[idx, 'scrape_status'] = f'failed: {error_msg}'
                failed += 1
            
            # Checkpoint save
            if (idx + 1) % checkpoint_every == 0:
                checkpoint_file = f'scrape_checkpoint_{idx + 1}.csv'
                metadata_df.to_csv(checkpoint_file, index=False, encoding='utf-8')
                logging.info(f"Checkpoint saved: {checkpoint_file}")
                logging.info(f"Progress: {successful} successful, {failed} failed out of {idx + 1}")
        
        logging.info(f"\nScraping complete!")
        logging.info(f"Successful: {successful}")
        logging.info(f"Failed: {failed}")
        logging.info(f"Total: {total}")
        
        return metadata_df


def scrape_and_save(metadata_file, output_file='eurlex_corpus_with_text.csv',
                   doc_id_col=None, celex_col=None, url_col=None,
                   output_dir='eurlex_documents', language='EN', sample=None):
    """
    Main function to scrape EUR-Lex documents and save results.
    
    Parameters:
    -----------
    metadata_file : str
        Path to CSV/Excel file containing EUR-Lex metadata
    output_file : str
        Path for output CSV with scraped text
    doc_id_col : str, optional
        Column name for document IDs (e.g., 'COM(2020) 562')
    celex_col : str, optional
        Column name for CELEX numbers
    url_col : str, optional
        Column name for direct URLs (if available)
    output_dir : str
        Directory to save individual document text files
    language : str
        Language code (EN, FR, DE, etc.)
    sample : int, optional
        Number of documents to sample (for testing)
    
    Returns:
    --------
    pd.DataFrame : Complete corpus with text
    """
    # Read metadata
    print(f"Reading metadata from: {metadata_file}")
    if metadata_file.endswith('.csv'):
        metadata = pd.read_csv(metadata_file, encoding='utf-8')
    elif metadata_file.endswith('.xlsx') or metadata_file.endswith('.xls'):
        metadata = pd.read_excel(metadata_file)
    else:
        raise ValueError("Metadata file must be .csv, .xls, or .xlsx")
    
    print(f"Loaded {len(metadata)} documents")
    
    # Show available columns
    print(f"\nAvailable columns: {list(metadata.columns)}")
    
    # Sample if requested
    if sample:
        metadata = metadata.sample(n=min(sample, len(metadata)), random_state=42)
        print(f"Sampling {len(metadata)} documents for testing")
    
    # Initialize scraper
    scraper = EurLexScraper(delay_range=(2, 5), max_retries=3)
    
    # Scrape documents
    print(f"\nStarting scrape...")
    corpus = scraper.scrape_from_metadata(
        metadata_df=metadata,
        doc_id_col=doc_id_col,
        celex_col=celex_col,
        url_col=url_col,
        language=language,
        output_dir=output_dir,
        save_individual=True,
        checkpoint_every=10
    )
    
    # Save complete corpus
    corpus.to_csv(output_file, index=False, encoding='utf-8')
    print(f"\n{'='*60}")
    print(f"Complete corpus saved to: {output_file}")
    print(f"Individual documents saved to: {output_dir}/")
    
    # Print summary statistics
    print(f"\n{'='*60}")
    print("SCRAPING SUMMARY")
    print('='*60)
    status_counts = corpus['scrape_status'].value_counts()
    print(status_counts)
    
    # Show success rate
    success_count = len(corpus[corpus['scrape_status'] == 'success'])
    success_rate = (success_count / len(corpus)) * 100
    print(f"\nSuccess rate: {success_rate:.1f}% ({success_count}/{len(corpus)})")
    
    return corpus


# Example usage
if __name__ == "__main__":
    print("EUR-Lex Document Scraper")
    print("="*60)
    
    print("\nEXAMPLE 1: Basic usage with CELEX numbers")
    print("-"*60)
    print("""
    corpus = scrape_and_save(
        metadata_file='Documents/GitHub/personal_AdJUST/data/deduplicated_scrape.csv',
        output_file='corpus_with_text.csv',
        celex_col='celex',  # name of your CELEX column
        language='EN',
        sample=5  # Test with 5 documents first!
    )
    """)
    
    print("\nEXAMPLE 2: Using document IDs")
    print("-"*60)
    print("""
    corpus = scrape_and_save(
        metadata_file='Documents/GitHub/personal_AdJUST/data/deduplicated_scrape.csv,
        output_file='corpus.csv',
        doc_id_col='document_id',  # e.g., column with 'COM(2020) 562'
        language='EN'
    )
    """)
    
    print("\nEXAMPLE 3: Using direct URLs")
    print("-"*60)
    print("""
    corpus = scrape_and_save(
        metadata_file='Documents/GitHub/personal_AdJUST/data/deduplicated_scrape.csv',
        output_file='corpus.csv',
        url_col='eurlex_url',
        language='EN'
    )
    """)
    
    print("\nEXAMPLE 4: Then integrate with analysis")
    print("-"*60)
    print("""
    # After scraping, process with EU analysis tools
    from eu_prelegislative_corpus_analysis import ProcessEUText, ExtractPolicyAreas
    
    # Process text
    processed, stem_map = ProcessEUText(
        corpus=corpus['text'],
        stem=True,
        remove_eu_stopwords=True,
        keep_legal_references=True
    )
    corpus['text_processed'] = processed
    
    # Extract policy areas
    policy_df = ExtractPolicyAreas(corpus['text'])
    corpus = pd.concat([corpus, policy_df], axis=1)
    
    # Save analyzed corpus
    corpus.to_csv('corpus_analyzed.csv', index=False)
    """)

    # Actually run the scraper
# corpus = scrape_and_save(
#   metadata_file='data/deduplicated_scrape.csv',
#    output_file='corpus_with_text.csv',
#    celex_col='celex',
#    url_col='url',
#    language='EN'
#    sample=10 # Test with 10 first!
# )

# Cool, so this worked well, going to apply it to all of them now :D 

corpus = scrape_and_save(
   metadata_file='data/deduplicated_scrape.csv',
    output_file='corpus_with_text.csv',
    celex_col='celex',
    url_col='url',
    language='EN'
    )

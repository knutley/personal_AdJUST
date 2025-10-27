# KLR Keyword Extraction Guide
## Using the Script with corpus_with_text.csv

---

## Quick Start

1. **Place your CSV file** in the same directory as `klr_keyword_extractor.py`
2. **Edit the script** to configure your CSV columns (see Configuration below)
3. **Run the script**: `python klr_keyword_extractor.py`
4. **Review the output files** created

---

## What This Script Does

The KLR (King-Lam-Roberts) method finds **keywords that distinguish one set of documents from another**.

### Example Use Cases:
- **Academic Research**: "What words distinguish environmental policy documents from other policy documents?"
- **Content Analysis**: "What makes successful proposals different from unsuccessful ones?"
- **Text Classification**: "What keywords predict document category?"

---

## Configuration (STEP 1 in script)

### Required: Update these lines in the script

```python
csv_file = 'corpus_with_text.csv'       # Your CSV filename
text_column = 'text'                     # Name of column containing text
id_column = 'id'                         # Name of column with document IDs
```

### What column name contains your text?
Open your CSV and check the column header. Common names:
- `text`
- `content`
- `full_text`
- `body`
- `document_text`

**Replace** `'text'` **with your actual column name.**

### Does your CSV have unique IDs for each document?
- **YES**: Use that column name for `id_column`
- **NO**: Set `id_column = None` (script will auto-generate IDs)

---

## Defining Your Target Set (STEP 4 in script)

**This is the most important step!** You must decide which documents are "target" vs "non-target."

### METHOD A: Keyword-Based (Recommended for first try)

Find documents containing specific keywords:

```python
# Example: Find all documents about climate change
keyword = 'climate'
target_indices = BooleanSearch(data['text_processed'], any_words=[keyword])
target_ids = [data.iloc[i]['id'] for i in target_indices]
```

**Multiple keywords (documents containing ANY of these):**
```python
target_indices = BooleanSearch(data['text_processed'], 
                               any_words=['climate', 'environment', 'emissions'])
```

**Multiple keywords (documents containing ALL of these):**
```python
target_indices = BooleanSearch(data['text_processed'], 
                               all_words=['climate', 'policy', 'eu'])
```

### METHOD B: Random Sample (Default in script)

Use a random sample (useful for testing):

```python
target_sample_size = int(len(data) * 0.3)  # 30% of documents
random.seed(42)
target_ids = random.sample(list(data['id']), target_sample_size)
```

### METHOD C: Category-Based

If your CSV has a category/type column:

```python
category_column = 'document_type'       # Your category column name
target_category = 'legislative_proposal' # The category you want
target_ids = list(data[data[category_column] == target_category]['id'])
```

### METHOD D: ID-Based

If you have a list of specific document IDs:

```python
target_ids = ['DOC001', 'DOC025', 'DOC156']  # Your specific IDs
```

**Or read from a file:**
```python
with open('target_document_ids.txt', 'r') as f:
    target_ids = [line.strip() for line in f.readlines()]
```

---

## Understanding the Output

The script creates three files:

### 1. `target_keywords.txt`
Keywords that are **distinctive to your target documents**.
- High in target documents
- Low in non-target documents
- These are what make your target set special

### 2. `nontarget_keywords.txt`
Keywords that are **distinctive to non-target documents**.
- Low in target documents
- High in non-target documents
- These are what the "other" documents are about

### 3. `reference_keywords.txt`
Most common keywords **across all documents**.
- Overall frequency ranking
- Not necessarily distinctive

---

## Reading the Statistics

When you use `stats=True`, you'll see:

```
1. regulation    (Recall: 0.45, Precision: 78.2%)
```

- **Recall (p1)**: Proportion of target documents containing this word (45%)
- **Precision**: Of all documents with this word, what % are target documents (78.2%)
- **High recall + high precision = strong keyword**

---

## Adjusting Parameters

In STEP 5, you can tune these parameters:

```python
kw = Keywords(
    search_set=data,
    target_set=target_ids,
    min_count=5,         # Increase to get more reliable keywords
    max_proportion=0.7,  # Decrease to exclude very common words
    alpha=1,             # Smoothing (usually 1 is fine)
    C=1                  # Regularization (usually 1 is fine)
)
```

### `min_count` (default: 5)
- Minimum times a keyword must appear in target set
- **Increase** (e.g., 10) → Fewer but more reliable keywords
- **Decrease** (e.g., 3) → More keywords, including rare ones

### `max_proportion` (default: 0.7)
- Maximum proportion of target documents a keyword can appear in
- **Decrease** (e.g., 0.5) → Only keywords that appear in <50% of target docs
- **Increase** (e.g., 0.9) → Allow more common keywords

---

## Different Methods

You can try different classification methods:

```python
kw.CompareKeywords(method='mir')      # Multinomial Inverse Regression (default)
kw.CompareKeywords(method='logreg')   # Logistic Regression
kw.CompareKeywords(method='svm')      # Support Vector Machine
kw.CompareKeywords(method='rf')       # Random Forest
```

**Recommended**: Start with `'mir'` (default) - it's specifically designed for text.

---

## Troubleshooting

### "Could not find corpus_with_text.csv"
- Make sure the CSV is in the same folder as the script
- Check the filename spelling (including .csv extension)

### "Column 'text' not found"
- Check your CSV column names
- Update `text_column` variable in STEP 1

### "No target keywords found" or very few keywords
- Your target set might be too small (need at least 20-30 documents)
- Try decreasing `min_count` parameter
- Try increasing `max_proportion` parameter

### "All keywords are very general"
- Your target set might not be distinctive enough
- Try a more specific target set definition
- Try decreasing `max_proportion` to exclude common words

### Memory errors with large datasets
- Add `sample=1000` to ReadSheet() to test with subset first
- Process in batches if you have >10,000 documents

---

## Example Workflow

### Research Question:
"What distinguishes EU environmental proposals from other EU proposals?"

### Step-by-Step:

1. **Load all EU proposals** (your corpus_with_text.csv)

2. **Define target set** (environmental proposals):
```python
# METHOD A: Keyword-based
env_keywords = ['environment', 'climate', 'emission', 'pollution', 
                'renewable', 'sustainability', 'biodiversity']
target_indices = BooleanSearch(data['text_processed'], any_words=env_keywords)
target_ids = [data.iloc[i]['id'] for i in target_indices]
```

3. **Run extraction**:
```python
kw.CompareKeywords(method='mir')
```

4. **Review results**:
- `target_keywords.txt` → Environmental policy vocabulary
- `nontarget_keywords.txt` → Other policy areas' vocabulary

5. **Refine** if needed:
- Adjust keywords if target set too small/large
- Tune parameters if results not satisfactory

---

## Advanced: Using Results in Analysis

After extraction, you can use the Keywords object:

```python
# Get top 20 target keywords as a list
top_keywords = kw.target_keywords[:20]

# Get statistics for specific keyword
keyword_stats = kw.target_stats.loc['climate']
print(f"Recall: {keyword_stats['p1']:.2f}")
print(f"Appears in {keyword_stats['n1']} target docs")

# Find documents with highest keyword scores
top_docs = kw.target_votecount.sort_values(ascending=False).head(10)

# Export full results to CSV
kw.target_stats.to_csv('target_keywords_full_stats.csv')
```

---

## Need Help?

Common next steps:
1. Start with METHOD B (random sample) to test the script works
2. Once working, switch to METHOD A (keyword-based) for real analysis
3. Review output files and adjust parameters
4. Try different methods (mir, logreg, svm) to compare results

---

## Citation

This code is adapted from:
King, G., Lam, P., & Roberts, M. (2017). "Computer-Assisted Keyword and Document Set Discovery from Unstructured Text." American Journal of Political Science, 61(4), 971-988.

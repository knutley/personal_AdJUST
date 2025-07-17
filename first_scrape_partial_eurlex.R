# Author: Katelyn Nutley
# Title: Accessing EU Pre-Legislative Documents via EurLex R Package 
# Date: 16-07-2025

# Documentation on the eurlex r() package is available via CRAN here: 
# https://cran.r-project.org/web/packages/eurlex/index.html

# Author's Git available here: 
# https://michalovadek.github.io/eurlex/

# 1. Install Eurlex Package and Suggested Dependencies # 
install.packages(c("eurlex", "wordcloud", "purrr", "ggiraph", "revest", "testthat")) # suggested dependencies in documentation
# only call the function if (like me), you don't already have these packages installed
packages <- c("dplyr", "eurlex", "ggiraph", "ggplot2", "httr", "knitr", "library", "purrr", "rmarkdown", "rvest", "testthat", "tidyr", "wordcloud")
lapply(packages, library, character.only = TRUE)

# A few notes on the package's main calls and their utility: 
# elx_curia_list call -	scrape list of court cases from Curia (Court of Justice of the Euopean Union)
# elx_download_xml - download XML notice associated with a URL (basically a list of URLs)
# elx_fetch_data	- retrieve additional data on EU documents (for metadata later)
# elx_label_eurovoc	- label EuroVoc concepts (multilingual thesaurus maintained by Pubs)
# elx_make_query -create SPARQL queries 
# elx_run_query -	execute SPARQL queries

# I think that the call elx_make_query is going to be the most useful here, so I'm going
# to go ahead and create a SPARQL query through the package. I've noticed that there 
# are some bugs in the most recent version 0.4.8, but I think I'll be able to work 
# around that without having to create my own scraper. 

# 2. Define SPARQL Query to Retrieve Pre-Legislative Docs # 

query <- elx_make_query(
  # RESOURCE PARAMETER: specifies what type of EU legal document the SPARQL query 
  # is going to trawl for. The options are "any", "directive", "regulation", "decision"
  # "recommendation", "intagr", "manual", "proposal", and "national_impl". I have chosen
  # "proposal" because it's most relevant to pre-legislative documents
  resource_type = "proposal",
  # MANUAL TYPE PARAMTER: allows for futher specification of document sub-type. The options
  # here are not well defined, but in the documentation I noted the use of "SWD", so I think 
  # the options are between "COM" (Commission documents), "SEC" (staff working documents),
  # "SWD" (newer staff working documents?), and "JOIN" (unclear). I left this empty
  # because I don't think I need to restrict it further atm. 
  manual_type = " ",
  # DIRECTORY CODE PARAMETER: Eur-Lex seems to use hierarchical classification systems
  # with more info found here: https://eur-lex.europa.eu/browse/directories/legislation.html?root_default=CC_1_CODED%3D15&displayProfile=allRelAllConsDocProfile&classification=in-force#arrow_15
  # For environmental topics, the key directory code seems to be 
  # "15" = Environment, consumers, and health protection; 
  # "1510" = Environment/ environmental policy;
  # "1520" = Consumer protection
  # "1540" = Health protection 
  directory = "1510"
  # SECTOR CODE PARAMETER: sector codes are numeric and relate to policy areas according
  # to the documentation. I think that they correspond with the CELEX numbers but I've
  # really not figured this one out, but the error message I received said it had to be 
  # between 0 and 9. 
  # sector = " ",
  # ADDITIONAL PARAMETERS WE CAN ADD: 
  # DATE RANGE: "YYYY-MM-DD" 
  # date_begin = "YYY-MM-DD", # start date 
  # date_end = sys.date(), # today
  # AUTHOR: 
  # Specify for originating institution; maybe: 
  # "European Comission" for more initial proposals
  # "Council of the European Union" for Council positions
  # "European Parliament" for EP ammendments or stated positions
  # Regardless, let's have a look at what this puts out and see if we'd like to
  # restrict the actors a bit
  # author = "European Commission", # Note to Self: I think this operates through
  # exact matches, not partial/fuzzy matches. 
  # LANGUAGE: 
  # Specify document langauge, i.e. "en", "fr", "de", "es", etc.  
  # language = "en",
  # INCLUDE CORRIGENDA AND CONSOLIDATED VERSIONS
  # include_corrigenda = TRUE, 
  # include_consolidated = FALSE, # usually FALSE for pre-legislative docs 
  # FORCE SPARQL PARAMETER
  # Forces the function to return raw SPARQL query instead of executing it
  # Useful for debugging or customizing the query further
  # force_sparql = TRUE  # Set to TRUE to see the actual SPARQL query
)

# 3. Examine the Results Data Structure # 

results <- elx_run_query(query)
str(results)
print(colnames(results))

# Right now, each row in the results contains three pieces of info: 
# 1. work: an internal ID / UUID (e.g., 7cbc770e-d809-11e6-ad7c-01aa75ed71a1)
# 2. type: a document type (e.g., PROP_REG, PROP_DEC)
# 3. celex: a CELEX number (e.g., 52017PC0008, 51997PC0550(05))

# PLEASE NOTE: There's an issue in year extraction from the CELEX numbers and the URL scrape, and I 
# had to re-tool things a bit. For information on CELEX: https://eur-lex.europa.eu/content/tools/eur-lex-celex-infographic-A3.pdf
# CELEX format: 52020PC0008 means 5(sector)2020(year)PC(document type)0008(sequence)

# 4. De-Bug Issues Associated with CELEX Numbers from Results # 

extract_year_from_celex <- function(celex) {
  # For modern CELEX numbers (2000+), extract 4-digit year from positions 2-5
  year_str <- substr(celex, 2, 5)
  return(as.numeric(year_str))
}

# 5. Reload and Reprocess Data # 

# This call only runs under the assumption that you have the 'results' data from 
# the above query. More simply, results <- elx_search_data(query = "environment", type = "proposal").

# Apply corrected year extraction
results$year <- extract_year_from_celex(results$celex)

# Filter for proposals
coms_proposals <- results[grepl("^5[0-9]{4}PC", results$celex), ] # grepl functions just clean everything up
coms_proposals$year <- extract_year_from_celex(coms_proposals$celex)

# Overview of what the data looks like: 
print(paste("`Environmental proposals (1973-2025):", nrow(coms_proposals)))
print("Distribution by year:")
print(table(coms_proposals$year))

# 6. Alternative Data Access Methods # 

# Direct URL construction and web scraping

construct_eurlex_url <- function(celex) {
  paste0("https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:", celex)
}

# As I said above, there was an issue with the eurlex package scrape (I am not sure
# what exactly but the functionality just isn't there). So, here are some alternatives
# to the call above (I prefer simplicity though). 

# Get document metadata using notice type
# get_document_metadata <- function(celex) {
#  tryCatch({
#    # Try to get basic document information
#    doc_info <- elx_fetch_data(celex, type = "text")
#    return(doc_info)
#  }, error = function(e) {
#    return(NULL)
#  })
# }

# Web scraping function for EUR-Lex pages
# scrape_eurlex_content <- function(url) {
#  tryCatch({
#    page <- read_html(url)
#    
#    # Extract title
#    title <- page %>% 
#      html_node("h1") %>% 
#      html_text(trim = TRUE)
#    
#    # Extract main content
#    content <- page %>% 
#      html_nodes("div.eli-main-content") %>% 
#      html_text(trim = TRUE)
#    
#    # Extract summary if available
#    summary <- page %>% 
#      html_nodes("div.eli-summary") %>% 
#      html_text(trim = TRUE)
#    
#    return(list(
#      title = title,
#      content = paste(content, collapse = " "),
#      summary = paste(summary, collapse = " ")
#    ))
#  }, error = function(e) {
#    return(list(title = NA, content = NA, summary = NA))
#  })
# }

# Add URLs to dataframe
coms_proposals$url <- construct_eurlex_url(coms_proposals$celex)

# Sort by year and select top proposals
coms_proposals <- coms_proposals[order(coms_proposals$year, decreasing = TRUE), ]
View(coms_proposals)

# Focus on most relevant document types
relevant_types <- c("PROP_REG", "PROP_DIR", "PROP_DEC", "AMEND_PROP_REG", "AMEND_PROP_DIR") 
# I've focused on these 5 types of documents as I thought they were the most relevant, 
# but I'm very happy to have a dicussion about this if you think we need to expand! 
key_proposals <- coms_proposals[coms_proposals$type %in% relevant_types, ]
View(key_proposals) # Dropped 279 documents here. Again, happy to discuss the 
# selection of relevant document types, though. 

# 7. Further Filtering Based on Dictionary Method # 

# Define environmental keywords to search for:
environmental_keywords <- c(
  "climate", "environment", "pollution", "emission", "carbon", "greenhouse gas",
  "biodiversity", "sustainability", "renewable", "energy efficiency", "waste",
  "water quality", "air quality", "circular economy", "green deal", "taxonomy"
) 

# Please note that this can be expanded, especially as we get into the meat of the 
# documents we have already pulled! 

# Function to score environmental relevance on documents
score_environmental_relevance <- function(text, keywords = environmental_keywords) {
  if(is.na(text) || text == "") return(0)
  
  text_lower <- tolower(text)
  score <- sum(sapply(keywords, function(kw) {
    length(gregexpr(kw, text_lower)[[1]]) - 1
  }))
  
  return(score)
}

scrape_eurlex_title <- function(url) {
  tryCatch({
    res <- GET(url, user_agent("Mozilla/5.0"))
    page <- read_html(res)
    
    # Extract from meta tag
    title <- page %>%
      html_node("meta[name='WT.z_docTitle']") %>%
      html_attr("content")
    
    if (!is.na(title) && nzchar(title)) {
      return(title)
    } else {
      return(NA)
    }
  }, error = function(e) {
    message("Failed on URL: ", url)
    return(NA)
  })
}
key_proposals$title <- map_chr(key_proposals$url, scrape_eurlex_title)
key_proposals$score <- sapply(key_proposals$title, score_environmental_relevance)
View(key_proposals)



# Author: Katelyn Nutley
# Title: Accessing EU Pre-Legislative Documents via EurLex R Package (Rework)
# Date: 28-07-2025

################################################################################

# A note on the script; this is a reworking of our first Eurlex scrape, titled 
# "first_scrape_partial_eurlex.R", with direct input from Fergus and Marion. 

# Documentation on the EurLex r() package is available via CRAN here: 
# https://cran.r-project.org/web/packages/eurlex/index.html

# EurLex Author's Git available here: 
# https://michalovadek.github.io/eurlex/

# Information on EurLex Document Vocabularies here: 
# https://op.europa.eu/en/web/eu-vocabularies/concept-scheme/-/resource?uri=http://publications.europa.eu/resource/authority/resource-type

# Information of EurLex Chapters here: 
# https://eur-lex.europa.eu/browse/directories/legislation.html

################################################################################

# 1. Install Eurlex Package and Suggested Dependencies # 

# install.packages(c("eurlex", "wordcloud", "purrr", "ggiraph", "revest", "testthat")) 
# These are the suggested dependencies that only need to be called if you haven't installed 
# them already. 

packages <- c("dplyr", "eurlex", "ggiraph", "ggplot2", "httr", "knitr", 
              "purrr", "rmarkdown", "rvest", "testthat", "tidyr", "wordcloud")
lapply(packages, library, character.only = TRUE)

# In our meeting, we agreed that we would predominantly rely on the the function 
# elx_make_query to scrape pre-legislative documents from the European Commission. 
# The source code for this function is available here: 
# https://github.com/michalovadek/eurlex/blob/master/R/elx_make_query.R

# 2. Define SPARQL Query to Retrieve Pre-Legislative Docs #

# We decided on using both "proposal" and "recommendation" as a pre-defined resource
# parameter. As a manual type parameter, we are including background documents,
# communications, joint white papers, staff working documents, working documents, 
# green papers, and joint green papers. There is a way to combine these SPARQL queries
# (i.e. multiple resource types in one call), but I found it to be much less reliable
# after some trial and error. So, I will be calling them separately and then stacking 
# the results. 

# The directory parameter in the eurlex package expects a single value, not a vector. Meaning
# that I can't just concatenate it in the call, so I will iterate through each directory
# individually to avoid the "length(url) == 1 is not TRUE" error. 

directories <- c("02", "03", "07", "09", "11", "12", "13", "15")

# From the 1510 directory, we included other chapters: 02 (customs union and free movement of goods
# - trade measures related to climate), 03 (agriculture - agricultural policies often address climate
# adaptation, land use, and agricultural emissions), 07 (transport policy - transport emissions and 
# sustainable mobility policies), 09 (financial services - green finance and climate risk 
# regulations), 11 (external relations - international climate agreements and cooperation),
# 12 (energy - energy policy is intertwined with climate change, covering renewable energy,
# energy efficiency, and emissions), 13 (industrial policy - industrial emissions, carbon
# trading systems, and industrial transition policies), and then 15 (environment, consumers, 
# and health protection - the most obvious location for climate change legislation)
# sector = " ", haven't included this because we opted for the entirety of the chapters instead

# Proposals Query 

proposals_results <- map_dfr(directories, function(dir) {
  tryCatch({
    query_proposals <- elx_make_query(
      resource_type = "proposal", # 
      directory = dir, # Single directory per query (FIXED)
      # sector = " ", haven't included this because we opted for the entirety of the chapters instead
      include_date = TRUE, # Include document dates in the results
      include_author = TRUE # Include author information in the results to filter later
    )
    result <- elx_run_query(query_proposals)
    if (!is.null(result) && nrow(result) > 0) {
      result$directory_code <- dir
      return(result)
    }
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
})

# Recommendations Query

recommendations_results <- map_dfr(directories, function(dir) {
  tryCatch({
    query_recommendations <- elx_make_query(
      resource_type = "recommendation",
      directory = dir, # Single directory per query (FIXED)
      include_date = TRUE,
      include_author = TRUE
    )
    result <- elx_run_query(query_recommendations)
    if (!is.null(result) && nrow(result) > 0) {
      result$directory_code <- dir
      return(result)
    }
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
})

# Manual List Query

# I have to pre-define what manual parameters I would like used because it doesn't 
# enjoy vectors. These are the values that Fergus kicked back to me. 

manual_types <- c("BACKGROUND_DOC", # background document related to a public consultation
                  "COMMUNIC", # non-binding document of an institution providing information
                  "COMMUNIC_DRAFT", # draft communication
                  "JOINT_PAPER_WHITE", # joint white paper 
                  "JOINT_SWD", # joint staff working document
                  "OPIN_OUTLOOK", # opinion adopted in view of pre-legislative document 
                  "PAPER_WHITE", # white paper
                  "PROP_DRAFT", # draft proposal
                  "RES_DRAFT", # draft resolution
                  "SWD", # staff working document
                  "TWD", # technical working document; unsure if I should keep this too? 
                  "WORK_DOC") # document geared towards providing information on certain policies, programmes and legislative proposals

manual_results <- map_dfr(directories, function(dir) {
  map_dfr(manual_types, function(type) {
    tryCatch({
      query_manual <- elx_make_query(
        resource_type = "manual",
        manual_type = type,
        directory = dir, # Single directory per query (FIXED)
        include_date = TRUE,
        include_author = TRUE
      )
      result <- elx_run_query(query_manual)
      if (!is.null(result) && nrow(result) > 0) {
        result$directory_code <- dir
        result$manual_type_used <- type
        return(result)
      }
      return(NULL)
    }, error = function(e) {
      return(NULL)
    })
  })
})

# 3. Execute Queries and Combine # 

if (length(proposals_results) > 0 || length(recommendations_results) > 0 || length(manual_results) > 0) {
  # add resource type identifier to each dataset
  if (nrow(proposals_results) > 0) {
    proposals_results$resource_type_used <- "proposal"
  }
  if (nrow(recommendations_results) > 0) {
    recommendations_results$resource_type_used <- "recommendation"
  }
  if (nrow(manual_results) > 0) {
    manual_results$resource_type_used <- "manual"
  }
  # Combine all results
  combined_results <- bind_rows(
    proposals_results,
    recommendations_results,
    manual_results
  )
} else {
  combined_results <- data.frame()
}

# Now to check the data we scraped: 
str(combined_results)
print(colnames(combined_results)) 
View(combined_results)

# 4. Filter Results by Authorship and Date # 

# I was initially mistaken, I thought if the elx_make_query had function calls like
# "include_author" and "include_date", that it would also let you filter by author 
# and date. This just means that you would have to import so much information and/or
# filter on the front-end, but it's no worries. 

combined_results <- combined_results %>%
  filter(date >= "1990-01-01") %>%
  filter(author == "European Commission")
View(combined_results) # reduced the dataset by ~ 10,000 entries 
# going to double check: 
unique(combined_results$author) # shows that the EC is the only author 
combined_results$date <- as.Date(combined_results$date) # this corrals it into a date format
range(combined_results$date, na.rm = TRUE) # shows that the range is right, too 

# 5. Direct URL Construction and Web-Scraping Titles # 

# As I mentioned previously, there seems to be an issue with the EurLex package. 
# Basically, it's scraping the document information, but not the name of the document
# or the URL, so I need to backfill those. 

# URL Construction Function 
construct_eurlex_url <- function(celex) {
  paste0("https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:", celex)
}

# Add URLs to the dataframe 
combined_results$url <- "0" # create URL column
combined_results$url <- construct_eurlex_url(combined_results$celex)
View(combined_results)

# Web-scraping Titles Function 
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

# Add titles to the dataframe
combined_results$titles <- "0"
combined_results$titles <- map_chr(combined_results$url, scrape_eurlex_title)

# SAVE: 
write.csv(combined_results, "~/Downloads/combined_results.csv")

# 6. Fix or Remove Observations with Non-Working URLs # 

non_working_obs <- combined_results %>% filter(is.na(titles))
View(non_working_obs) # These are the observations where the scrape failed, just
# 27 in total. I'm going to look through them and see if they can't be salvaged.

# I'm going to use the CELEX numbers here to identify and replace the title values: 

combined_results[combined_results$celex == "52022PC0296" & is.na(combined_results$titles), "titles"] <- "Proposal for a REGULATION OF THE EUROPEAN PARLIAMENT AND OF THE COUNCIL amending Council Regulation (EC) No 1217/2009 as regards conversion of the Farm Accountancy Data Network into a Farm Sustainability Data Network"
combined_results[combined_results$celex == "52007PC0712" & is.na(combined_results$titles), "titles"] <- "Proposal for a Council Decision on the conclusion of the Agreement between the European Community and Australia on trade in wine"
combined_results[combined_results$celex == "52012PC0355" & is.na(combined_results$titles), "titles"] <- "Proposal for a DECISION OF THE EUROPEAN PARLIAMENT AND OF THE COUNCIL amending Council Decision 2008/971/EC as regards the inclusion of forest reproductive material of the 'qualified' category within the scope of that Decision and the updating of the name of the authorities responsible for the approval and control of the production"
combined_results[combined_results$celex == "52014PC0438" & is.na(combined_results$titles), "titles"] <- "Proposal for a COUNCIL DECISION on the position to be adopted on behalf of the European Union within the Council of Members of the International Olive Council concerning the prolongation of the 2005 International Agreement on olive oil and table olives"
combined_results[combined_results$celex == "52007PC0737" & is.na(combined_results$titles), "titles"] <- "Proposal for a Directive of the European Parliament and of the Council on safety rules and standards for passenger ships (Recast)"
combined_results[combined_results$celex == "51998PC0426" & is.na(combined_results$titles), "titles"] <- "Proposal for a Council Decision authorising the Portuguese Republic to apply a measure derogating from Articles 21(1)(a) and 22 of the Sixth Council Directive (77/388/EEC) of 17 May 1977 on the harmonisation of the laws of the Member States relating to turnover taxes"
combined_results[combined_results$celex == "52003PC0234" & is.na(combined_results$titles), "titles"] <- "Proposal for a Council Directive amending Directive 77/388/EEC as regards value added tax on services provided in the postal sector"
combined_results[combined_results$celex == "52023PC0342" & is.na(combined_results$titles), "titles"] <- "Proposal for a COUNCIL IMPLEMENTING DECISION amending Implementing Decision (EU) 2017/784 as regards the period of authorisation for, and the scope of, the special measure derogating from Articles 206 and 226 of Directive 2006/112/EC on the common system of value added tax taken by Italy"
combined_results[combined_results$celex == "52010PC0706" & is.na(combined_results$titles), "titles"] <- "Proposition de DÉCISION DU CONSEIL relative à la signature, au nom de l'Union Européenne, et à l'application provisoire de l’accord international sur le cacao de 2010"
combined_results[combined_results$celex == "52016PC0648" & is.na(combined_results$titles), "titles"] <- "Proposal for a COUNCIL DECISION on the signing, on behalf of the European Union, of the Agreement between the European Union and the Republic of Chile on trade in organic products"
combined_results[combined_results$celex == "52005SC1236" & is.na(combined_results$titles), "titles"] <- "Draft Decision of the EEA Joint Committee amending Protocol 31 to the EEA Agreement, on cooperation in specific fields outside the four freedoms - Draft common position of the Community"
combined_results[combined_results$celex == "52005PC0495" & is.na(combined_results$titles), "titles"] <- "Proposal for a Council Regulation terminating the partial interim review of the anti-dumping measures applicable to imports of bicycles originating in the People’s Republic of China"                 
combined_results[combined_results$celex == "52002PC0475" & is.na(combined_results$titles), "titles"] <- "Proposal for a Council Regulation imposing definitive anti-dumping duties on imports of certain welded tubes and pipes, of iron or non-alloy steel originating in the Czech Republic, Poland, Thailand, Turkey and the Ukraine"
combined_results[combined_results$celex == "52011PC0209" & is.na(combined_results$titles), "titles"] <- "Proposition de RÈGLEMENT DU CONSEIL instituant un droit antidumping définitif sur les importations de furfural originaire de la République populaire de Chine à l’issue d’un réexamen au titre de l’expiration des mesures effectué en vertu de l’article 11, paragraphe 2, du règlement (CE) n° 1225/2009"
combined_results[combined_results$celex == "51996PC0324" & is.na(combined_results$titles), "titles"] <- "Proposal for a COUNCIL REGULATION (EC) providing for the reduction of the tariff rate applicable to imports under the WTO tariff quota for certain live bovine animals"
combined_results[combined_results$celex == "52002PC0003(03)" & is.na(combined_results$titles), "titles"] <- "Proposal for a Council Regulation adopting autonomous measures concerning the importation of fish and fishery products originating in the Republic of Hungary"
combined_results[combined_results$celex == "51991PC0074" & is.na(combined_results$titles), "titles"] <- "PROPOSAL FOR A COUNCIL REGULATION (EEC) AMENDING COUNCIL REGULATION (EEC) 2340/90 OF 8 AUGUST 1990 AND COUNCIL REGULATION (EEC) 3155/90 OF 29 OCTOBER 1990 PREVENTING TRADE BY THE COMMUNITY AS REGARDS IRAQ AND KUWAIT"
combined_results[combined_results$celex == "52011PC0197" & is.na(combined_results$titles), "titles"] <- "Proposal for a COUNCIL REGULATION imposing a definitive anti-dumping duty and collecting definitively the provisional duty imposed on imports of melamine originating in the People's Republic of China."
combined_results[combined_results$celex == "52007PC0098" & is.na(combined_results$titles), "titles"] <- "Proposal for a Council and Commission Decision on the conclusion of the Protocol to the Partnership and Cooperation Agreement between the European Communities and their Member States, of the one part, and Georgia, of the other part, to take account of the accession of the Republic of Bulgaria and Romania to the European Union"
combined_results[combined_results$celex == "52013PC0543" & is.na(combined_results$titles), "titles"] <- "Proposal for a COUNCIL DECISION extending the validity of Decision 2012/96/EU"
combined_results[combined_results$celex == "52013PC0040" & is.na(combined_results$titles), "titles"] <- "Proposal for a REGULATION OF THE EUROPEAN PARLIAMENT AND OF THE COUNCIL amending Regulation (EU) No 912/2010 setting up the European GNSS Agency"
combined_results[combined_results$celex == "52007PC0856" & is.na(combined_results$titles), "titles"] <- "Proposal for a Regulation of the European Parliament and of the Council setting emission performance standards for new passenger cars as part of the Community's integrated approach to reduce CO2 emissions from light-duty vehicles"
combined_results[combined_results$celex == "52000PC0394" & is.na(combined_results$titles), "titles"] <- "Proposal for a Regulation of the European Parliament and of the Council on unbundled access to the local loop"
combined_results[combined_results$celex == "52008PC0302" & is.na(combined_results$titles), "titles"] <- "Proposal for a Council Regulation repealing Council Regulation (EC) No 752/2007 of 30 May 2007 on administering certain restrictions on imports of certain steel products from Ukraine"
combined_results[combined_results$celex == "52015XC0327(04)" & is.na(combined_results$titles), "titles"] <- "Commission communication concerning Article 4(3) of Directive 2009/22/EC of the European Parliament and of the Council on injunctions for the protection of consumers' interests, which codifies Directive 98/27/EC, concerning the entities qualified to bring an action under Article 2 of this Directive Text with EEA relevance"
combined_results[combined_results$celex == "52013DC0659" & is.na(combined_results$titles), "titles"] <- "COMMUNICATION FROM THE COMMISSION A new EU Forest Strategy: for forests and the forest-based sector"
combined_results[combined_results$celex == "52011XC0205(05)" & is.na(combined_results$titles), "titles"] <- "Publication of an application pursuant to Article 6(2) of Council Regulation (EC) No 510/2006 on the protection of geographical indications and designations of origin for agricultural products and foodstuffs"

# Now there are no missing titles, I can check if there are other missing values that
# I need to correct. 

# 7. Check Other Missing Values # 

# Check the dataset structure
summary(combined_results)

# Identify other missing values
sapply(combined_results, function(x) sum(is.na(x))) # The only missing values here
# are in the manual_type_used column. I made it so that if I relied on either "proposal" 
# or "recommendation" as a resource_type, it would pass it through as an "NA" in the 
# manual_type_used column.
na_manual_type <- combined_results %>% filter(is.na(manual_type_used))
unique(na_manual_type$resource_type_used) # This assures me that there weren't any issues
# with the scrape and I don't need to fill those missing values retroactively. 

# 8. Deduplication based on CELEX Number # 

# The easiest way to deduplicate is based on CELEX number, since each EurLex document
# has a unique identification code. I'll begin with this, then look at titles
# as additional due diligence. 

all_duplicates <- combined_results %>% 
  group_by(celex) %>% 
  filter(n() > 1) %>% 
  arrange(celex)
View(all_duplicates) # Looking at this, it should be fairly easy to collapse based
# on CELEX number. 

deduplicated_results <- combined_results %>% distinct(celex, .keep_all = TRUE)
View(deduplicated_results) # This seemed to have worked; now going to run the same 
# call above with this dataset. 

all_duplicates <- deduplicated_results %>% 
  group_by(celex) %>% 
  filter(n() > 1) %>% 
  arrange(celex) # There's no longer any CELEX duplicates 

# 9. Deduplication Based on Title # 

# Exact Text Duplicates 
title_duplicates <- deduplicated_results %>%
  group_by(titles) %>%
  filter(n() > 1) %>%
  arrange(titles)
View(title_duplicates) # Looking at this, some of these titles have different CELEX 
# numbers. For example, two entries share the title "AMENDED PROPOSAL FOR A COUNCIL 
# DIRECTIVE ON TRANSITIONAL MEASURES APPLICABLE IN GERMANY IN THE CONTEXT OF THE 
# HARMONIZATION OF TECHNICAL RULES", but have CELEX numbers 51990PC0495(03) and 
# 51990PC0495(04), respectively. Are they continuations? Should they be collapsed?

title_duplicates <- title_duplicates %>% ungroup()
set.seed(20)
title_sample <- title_duplicates %>% slice_sample(n = 20)
View(title_sample) # Randomly selected 20 duplicates to go through by hand and make this decision. 
View(deduplicated_results) 

# CELEX: 52025DC0315 vs. 52025DC0315 - Both titled: COMMUNICATION FROM THE COMMISSION Nuclear 
# Illustrative Programme presented under Article 40 of the Euratom Treaty for the 
# opinion of the European Economic and Social Committee - these are different documents.

# CELEX: 52004XC0327(07) vs. 52004XC0327(04) - Both titled: Commission notification 
# on the issuing of licences to railway undertakings - these are different documents.
# Additionally looked at 5 more randomly and they were all notifications about 
# separate banhofs across Germany. 

# CELEX: 52017PC0145 vs. 52018PC0204 - Both titled: Proposal for a COUNCIL DECISION on 
# the position to be adopted, on behalf of the European Union, within the EEA Joint
# Committee concerning an amendment to Protocol 31 to the EEA Agreement, on cooperation 
# in specific fields outside the four freedoms (Budget Line 12.02.01 Implementation and 
# development of the single market for financial services) - these are different 
# documents. 

# I'm now pretty confident that I don't or rather shouldn't de-duplicate on titles. 

# 9. Save Dataset # 

write.csv(deduplicated_results, "~/Downloads/deduplicated_scrape.csv")






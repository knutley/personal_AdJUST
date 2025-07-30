
# Testing EurLex 

# 1. Install Eurlex Package and Suggested Dependencies # 
cran_packages <- c("eurlex", "wordcloud", "purrr", "ggiraph", "testthat", 
                   "dplyr", "ggplot2", "httr", "knitr", "rmarkdown", 
                   "rvest", "tidyr")

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

invisible(lapply(cran_packages, install_if_missing))

# 2. Install 'revest' from GitHub (not available on CRAN)
if (!requireNamespace("revest", quietly = TRUE)) {
  install.packages("remotes")  # Ensure 'remotes' is installed
  remotes::install_github("fkeck/revest")
}

# 3. Load packages
packages <- c(cran_packages, "revest")
invisible(lapply(packages, library, character.only = TRUE))

# --- Investigative SPARQL Query: Green Papers ---

# Use the official URI for "Green Paper" resource type
query <- elx_make_query(
  resource_type = "manual",
  manual_type = "ACT_PREP"
)

# Run query
results <- elx_run_query(query)

# Inspect results
str(results)
print(colnames(results))

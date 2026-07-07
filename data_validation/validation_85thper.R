# Corpus Validation Report for Keystone Legislation
# Author: Katie Nutley
# Date: 20-02-2025
# Pulls from 85th percentile filtered corpus

library(tidyverse)
library(here)

# ── Paths (relative to repo root, works on any machine) ───────────────────────
# NOTE: corpus_85th_percentile.csv ships in this repo as a zipped file
# (data_screening/corpus_85th_percentile.csv.zip) due to GitHub's file-size
# limit. Unzip it once before running this script:
#   unzip data_screening/corpus_85th_percentile.csv.zip -d data_screening/
corpus_path  <- here("data_screening", "corpus_85th_percentile.csv")
output_dir   <- here("data_validation")

# =============================================================================
# LOAD DATA
# =============================================================================

mod_corpus <- read_csv(corpus_path)
cat("Corpus size:", nrow(mod_corpus), "documents\n")
cat("Density range:", round(min(mod_corpus$density), 4), "to", round(max(mod_corpus$density), 4), "\n")

# =============================================================================
# RANDOM SAMPLE VALIDATION
# =============================================================================

set.seed(123)
sub_mod_corpus <- mod_corpus %>% slice_sample(n = 20)
write_csv(sub_mod_corpus, file.path(output_dir, "sub_mod_corpus_85th.csv"))
cat("Random sample saved to", file.path(output_dir, "sub_mod_corpus_85th.csv"), "\n")

# =============================================================================
# SEARCH FUNCTION
# =============================================================================

search_by_regex <- function(corpus_df, regex_patterns) {
  text_cols <- names(corpus_df)[sapply(corpus_df, is.character)]

  map_df(1:nrow(regex_patterns), function(i) {
    pattern  <- regex_patterns$pattern[i]
    category <- regex_patterns$doc_category[i]

    matches <- corpus_df %>%
      filter(if_any(all_of(text_cols),
                    ~str_detect(., regex(pattern, ignore_case = TRUE))))

    tibble(
      category  = category,
      pattern   = pattern,
      n_matches = nrow(matches),
      found     = nrow(matches) > 0
    )
  })
}

# =============================================================================
# NEOCLASSICAL PARADIGM: ETS DOCUMENTS
# =============================================================================

neoclassical_searches <- tibble(
  doc_category = c(
    "Directive 2003/87/EC - ETS Foundation",
    "Directive 2004/101/EC - Kyoto Link",
    "Directive 2008/101/EC - Aviation Inclusion",
    "Directive 2009/29/EC - Phase III Reform",
    "Decision 377/2013/EU - Aviation Derogation",
    "Regulation 421/2014 - Aviation Scope Reduction",
    "Decision 2015/1814 - MSR Establishment",
    "Regulation 2017/2392 - Aviation CORSIA Prep",
    "Directive 2018/410 - Phase IV Reform",
    "Decision 2023/136 - CORSIA Offsetting",
    "Decision 2023/852 - MSR Amendment Fit for 55",
    "Directive 2023/958 - Aviation ETS Reform",
    "Directive 2023/959 - Major Fit for 55 ETS Reform"
  ),
  pattern = c(
    "Directive\\s+(2003/87/EC|\\(EC\\)\\s+2003/87)",
    "Directive\\s+(2004/101/EC|\\(EC\\)\\s+2004/101)",
    "Directive\\s+(2008/101/EC|\\(EC\\)\\s+2008/101)",
    "Directive\\s+(2009/29/EC|\\(EC\\)\\s+2009/29)",
    "Decision\\s+(No\\s+)?377/2013/EU",
    "Regulation\\s+\\(EU\\)\\s+No\\s+421/2014",
    "Decision\\s+\\(EU\\)\\s+2015/1814",
    "Regulation\\s+\\(EU\\)\\s+2017/2392",
    "Directive\\s+\\(EU\\)\\s+2018/410",
    "Decision\\s+\\(EU\\)\\s+2023/136",
    "Decision\\s+\\(EU\\)\\s+2023/852",
    "Directive\\s+\\(EU\\)\\s+2023/958",
    "Directive\\s+\\(EU\\)\\s+2023/959"
  )
)

neoclassical_results <- search_by_regex(mod_corpus, neoclassical_searches)

cat("\n--- NEOCLASSICAL ETS RESULTS ---\n")
print(neoclassical_results)

cat("\nSummary:\n")
tibble(
  total_searched = nrow(neoclassical_searches),
  found          = sum(neoclassical_results$found),
  missing        = sum(!neoclassical_results$found),
  total_matches  = sum(neoclassical_results$n_matches)
) %>% print()

missing_neo <- neoclassical_results %>% filter(!found) %>% select(category, pattern)
if (nrow(missing_neo) > 0) {
  cat("\nMissing neoclassical documents:\n")
  print(missing_neo)
} else {
  cat("All neoclassical documents found!\n")
}

# =============================================================================
# POST-GROWTH PARADIGM: KEYWORD SEARCH
# =============================================================================

postgrowth_searches <- tibble(
  doc_category = c(
    "European Green Deal - any mention",
    "Just Transition Mechanism - any mention",
    "Territorial Just Transition Plans",
    "European Pillar of Social Rights",
    "Energy poverty",
    "Fair/Just transition - general"
  ),
  pattern = c(
    "european\\s+green\\s+deal|\\bEGD\\b",
    "just\\s+transition\\s+mechanism|\\bJTM\\b",
    "territorial\\s+just\\s+transition\\s+plan",
    "european\\s+pillar.*social\\s+rights|\\bEPSR\\b",
    "energy\\s+poverty",
    "(fair|just)\\s+transition"
  )
)

postgrowth_results <- search_by_regex(mod_corpus, postgrowth_searches)

cat("\n--- POST-GROWTH RESULTS ---\n")
postgrowth_results %>% arrange(desc(n_matches)) %>% print()

cat("\nSummary:\n")
tibble(
  total_searched = nrow(postgrowth_searches),
  found          = sum(postgrowth_results$found),
  missing        = sum(!postgrowth_results$found),
  total_matches  = sum(postgrowth_results$n_matches)
) %>% print()

# =============================================================================
# ECO-SOCIAL REGULATIONS
# =============================================================================

ecosocial_searches <- tibble(
  doc_category = c(
    "Regulation 2023/955 - Social Climate Fund",
    "COM(2021) 568 - SCF Proposal",
    "Directive 2023/1791 - Energy Efficiency (amends SCF)",
    "COM(2024) 538 - Carbon Market Report (SCF updates)",
    "Council Recommendation 2022/950 - Fair Transition",
    "Regulation 2021/1056 - Just Transition Fund",
    "Regulation 2024/795 - STEP (amends JTF)",
    "Commission Implementing Decision 2021/1129 - JTF Allocations",
    "COM(2020) 22 - JTF Proposal",
    "Regulation 2021/1229 - Public Sector Loan Facility",
    "COM(2020) 453 - PSLF Proposal",
    "Regulation 2021/523 - InvestEU Programme",
    "Regulation 2021/1060 - Common Provisions Regulation",
    "Regulation 2018/1999 - Governance Energy Union",
    "Regulation 2021/241 - Recovery and Resilience Facility",
    "Council Regulation 2020/2094 - NextGenerationEU"
  ),
  pattern = c(
    "Regulation\\s+\\(EU\\)\\s+2023/955",
    "COM\\(2021\\)\\s*568",
    "Directive\\s+\\(EU\\)\\s+2023/1791",
    "COM\\(2024\\)\\s*538",
    "(Council\\s+)?Recommendation\\s+\\(EU\\)\\s+2022/950",
    "Regulation\\s+\\(EU\\)\\s+2021/1056",
    "Regulation\\s+\\(EU\\)\\s+2024/795",
    "(Commission\\s+Implementing\\s+)?Decision\\s+\\(EU\\)\\s+2021/1129",
    "COM\\(2020\\)\\s*22",
    "Regulation\\s+\\(EU\\)\\s+2021/1229",
    "COM\\(2020\\)\\s*453",
    "Regulation\\s+\\(EU\\)\\s+2021/523",
    "Regulation\\s+\\(EU\\)\\s+2021/1060",
    "Regulation\\s+\\(EU\\)\\s+2018/1999",
    "Regulation\\s+\\(EU\\)\\s+2021/241",
    "(Council\\s+)?Regulation\\s+\\(EU\\)\\s+2020/2094"
  )
)

ecosocial_results <- search_by_regex(mod_corpus, ecosocial_searches)

cat("\n--- ECO-SOCIAL RESULTS ---\n")
print(ecosocial_results)

cat("\nSummary:\n")
tibble(
  total_searched = nrow(ecosocial_searches),
  found          = sum(ecosocial_results$found),
  missing        = sum(!ecosocial_results$found),
  total_matches  = sum(ecosocial_results$n_matches)
) %>% print()

missing_eco <- ecosocial_results %>% filter(!found) %>% select(category, pattern)
if (nrow(missing_eco) > 0) {
  cat("\nMissing eco-social documents:\n")
  print(missing_eco)
} else {
  cat("All eco-social documents found!\n")
}

# Note: PSLF documents may be missing, but this is not hugely concerning.

# =============================================================================
# GREEN GROWTH PARADIGM: MARION'S DOCUMENTS
# =============================================================================

marion_searches <- tibble(
  doc_category = c(
    "Strategic Energy Technology Plan 2007",
    "NER300 2010",
    "2020 Climate & Energy Package 2009",
    "Net-Zero Industry Act",
    "Critical Raw Materials Act",
    "European Battery Alliance",
    "European Clean Hydrogen Alliance",
    "Circular Economy Action Plan",
    "NextGenerationEU",
    "Recovery and Resilience Facility",
    "Green Deal Industrial Plan",
    "EU Innovation Fund",
    "Important Projects of Common European Interest",
    "Horizon Europe",
    "REPowerEU",
    "European Hydrogen Bank"
  ),
  paradigm = c(
    "green_growth", "green_growth", "green_growth",
    "green_growth", "green_growth", "green_growth", "green_growth",
    "green_growth", "eco_social", "eco_social",
    "green_growth", "green_growth", "green_growth",
    "green_growth", "green_growth", "green_growth"
  ),
  pattern = c(
    "strategic\\s+energy\\s+technology\\s+plan|\\bSET[- ]?plan\\b",
    "\\bNER\\s*300\\b",
    "2020\\s+climate.*energy\\s+package|climate.*energy\\s+package.*2009",
    "net[- ]zero\\s+industry\\s+act|\\bNZIA\\b",
    "critical\\s+raw\\s+materials\\s+act|\\bCRMA\\b",
    "european\\s+battery\\s+alliance|\\bEBA\\b",
    "european\\s+clean\\s+hydrogen\\s+alliance|\\bECHA\\b",
    "circular\\s+economy\\s+action\\s+plan|\\bCEAP\\b",
    "nextgenerationeu|next\\s+generation\\s+eu|\\bNGEU\\b",
    "recovery\\s+and\\s+resilience\\s+facility|\\bRRF\\b",
    "green\\s+deal\\s+industrial\\s+plan|\\bGDIP\\b",
    "eu\\s+innovation\\s+fund|innovation\\s+fund",
    "important\\s+projects\\s+of\\s+common\\s+european\\s+interest|\\bIPCEI\\b",
    "horizon\\s+europe",
    "repowereu|repower\\s+eu",
    "european\\s+hydrogen\\s+bank|\\bEHB\\b"
  )
)

marion_results <- search_by_regex(mod_corpus, marion_searches)
marion_results <- marion_results %>% left_join(marion_searches %>% select(doc_category, paradigm), by = c("category" = "doc_category"))

cat("\n--- MARION'S DOCUMENTS (GREEN GROWTH / ECO-SOCIAL) ---\n")
print(marion_results)

cat("\nSummary:\n")
tibble(
  total_searched = nrow(marion_searches),
  found          = sum(marion_results$found),
  missing        = sum(!marion_results$found),
  total_matches  = sum(marion_results$n_matches)
) %>% print()

cat("\nBy paradigm:\n")
marion_results %>%
  group_by(paradigm) %>%
  summarise(total = n(), found = sum(found), missing = sum(!found)) %>%
  print()

missing_marion <- marion_results %>% filter(!found) %>% select(category, paradigm, pattern)
if (nrow(missing_marion) > 0) {
  cat("\nMissing Marion documents:\n")
  print(missing_marion)
} else {
  cat("All Marion documents found!\n")
}

# =============================================================================
# OVERALL SUMMARY
# =============================================================================

cat("\n--- OVERALL SUMMARY ---\n")
tibble(
  paradigm       = c("Neoclassical (ETS)", "Post-Growth (Keywords)", "Eco-Social (Regulations)", "Green Growth (Marion)"),
  total_searched = c(nrow(neoclassical_searches), nrow(postgrowth_searches), nrow(ecosocial_searches), nrow(marion_searches)),
  found          = c(sum(neoclassical_results$found), sum(postgrowth_results$found), sum(ecosocial_results$found), sum(marion_results$found)),
  missing        = c(sum(!neoclassical_results$found), sum(!postgrowth_results$found), sum(!ecosocial_results$found), sum(!marion_results$found)),
  total_matches  = c(sum(neoclassical_results$n_matches), sum(postgrowth_results$n_matches), sum(ecosocial_results$n_matches), sum(marion_results$n_matches))
) %>%
  mutate(pct_found = round(found / total_searched * 100, 1)) %>%
  print()

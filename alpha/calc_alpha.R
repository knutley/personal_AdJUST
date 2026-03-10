# Title: Calculating Inter-Coder Reliability
# Author: Katie Nutley
# Date: 10-03-2026
#
# Two PIs, Mariona Dumas and Fergus Green, given 60 highly indicative documents
# to hand code for environmental policy paradigms. Alpha calculated treating
# primary, secondary, and tertiary labels as an unordered set (no hierarchy).

library(readxl)
library(dplyr)
library(irr)

setwd("~/Downloads/")

# Load PI Sheets
fergus_sheet <- read_xlsx("pi_coding_sample1_final.xlsx", sheet = 2)
marion_sheet <- read_xlsx("pi_coding_sample1 (1).xlsx", sheet = 2)

# Clean and Deduplicate 
# Both sheets had duplicate IDs 52016PC0395 and 52022SC0267;
# first occurrence retained in both cases (second row was NA or identical)
fergus_clean <- fergus_sheet %>%
  distinct(ID, .keep_all = TRUE) %>%
  select(ID, Relevant, Label, `Secondary Label`, `Tertiary Label`) %>%
  rename(
    relevant_fergus  = Relevant,
    primary_fergus   = Label,
    secondary_fergus = `Secondary Label`,
    tertiary_fergus = `Tertiary Label`
  )

marion_clean <- marion_sheet %>%
  distinct(ID, .keep_all = TRUE) %>%
  select(ID, Relevant, Label, `Secondary Label`, `Tertiary Label`) %>%
  rename(
    relevant_marion  = Relevant,
    primary_marion   = Label,
    secondary_marion = `Secondary Label`,
    tertiary_marion = `Tertiary Label`
  )

# Merge
merged <- inner_join(fergus_clean, marion_clean, by = "ID") # this is just something you 
# apparently have to do to use the IRR package 

# Fix Marion's "Unsure" relevance on 52022PC0451 — she still assigned Post-Growth
merged$relevant_marion[merged$ID == "52022PC0451"] <- "Yes"

# Exclusions
# 52007SC0771, 52013SC0343, 52018PC0546: both coders marked Relevant=No
# 52022SC0110, 52024SC0360, 52025PC0173: relevance disagreements without label consensus
exclude_ids <- c("52007SC0771", "52013SC0343", "52018PC0546",
                 "52022SC0110", "52024SC0360", "52025PC0173")

coding <- merged %>%
  filter(!ID %in% exclude_ids) %>%
  filter(relevant_fergus == "Yes" & relevant_marion == "Yes") %>%
  filter(!is.na(primary_fergus) & !is.na(primary_marion))

cat("N documents in clean coding set:", nrow(coding), "\n")

# Labels! 
all_labels <- c("Admin-only", "GG-Evolutionary", "GG-Keynesianism",
                "Neoclassical", "Post-Growth")

# Per label binary alphas 
# A document gets 1 for a label if it appears in either the primary, secondary, or tertiary
# column for that coder — ordering is ignored.
alphas <- numeric(length(all_labels))
names(alphas) <- all_labels

for (i in seq_along(all_labels)) {
  lbl <- all_labels[i]
  r1 <- as.integer(
    coding$primary_fergus   == lbl & !is.na(coding$primary_fergus)  |
      coding$secondary_fergus == lbl & !is.na(coding$secondary_fergus) |
      coding$tertiary_fergus == lbl & !is.na(coding$tertiary_fergus)
  )
  r2 <- as.integer(
    coding$primary_marion   == lbl & !is.na(coding$primary_marion)  |
      coding$secondary_marion == lbl & !is.na(coding$secondary_marion) |
      coding$tertiary_marion == lbl & !is.na(coding$tertiary_marion)
  )
  alphas[i] <- kripp.alpha(rbind(r1, r2), method = "nominal")$value
}

cat("\n=== Per-label Alphas (primary + secondary labels) ===\n")
print(round(alphas, 3))
cat("\nMean alpha across labels:", round(mean(alphas), 3), "\n")

# Overall alpha (primary label only — conservative lower bound) 
# A single authoritative label per doc is required for overall alpha;
# primary label only is used here. Per-label mean above is the main figure.
r1_int <- as.integer(factor(coding$primary_fergus, levels = all_labels))
r2_int <- as.integer(factor(coding$primary_marion, levels = all_labels))

cat("\n=== Overall Alpha (primary label only, conservative lower bound) ===\n")

print(kripp.alpha(rbind(r1_int, r2_int), method = "nominal")) # Not great, below
# Krippendorff's alpha >0.67

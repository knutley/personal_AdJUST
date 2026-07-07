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

######## 
# Going to try to reconcile the two; don't need it for alpha, but to give to the RA;  

colnames(fergus_clean)

library(writexl)

# Build per-document label sets and find overlap
coding_sets <- coding %>%
  rowwise() %>%
  mutate(
    set_fergus = list(na.omit(c(primary_fergus, secondary_fergus, tertiary_fergus))),
    set_marion = list(na.omit(c(primary_marion, secondary_marion, tertiary_marion))),
    agreed     = list(intersect(set_fergus, set_marion))
  ) %>%
  ungroup()

# Split into reconciled (any overlap) and disputed (no overlap) 
reconciled <- coding_sets %>%
  filter(lengths(agreed) > 0) %>%
  mutate(reconciled_primary = map_chr(agreed, 1))  # first agreed label as primary

flagged_docs <- coding_sets %>%
  filter(lengths(agreed) == 0)

cat("Reconciled:", nrow(reconciled), "| Disputed:", nrow(flagged_docs), "\n")

# Build the agreed + additional columns
reconciled_sheet <- reconciled %>%
  rowwise() %>%
  mutate(
    agreed_others     = list(setdiff(agreed, reconciled_primary)),
    agreed_secondary  = if (length(agreed_others) >= 1) agreed_others[[1]] else NA_character_,
    agreed_tertiary   = if (length(agreed_others) >= 2) agreed_others[[2]] else NA_character_,
    fergus_additional = paste(setdiff(set_fergus, agreed), collapse = "; ") %>% na_if(""),
    marion_additional = paste(setdiff(set_marion, agreed), collapse = "; ") %>% na_if("")
  ) %>%
  ungroup() %>%
  select(ID, 
         agreed_primary = reconciled_primary,  # rename on the fly
         agreed_secondary, agreed_tertiary,
         fergus_additional, marion_additional)

# Disputed tab 
disputed_sheet <- flagged_docs %>%
  mutate(adjudicated_label = NA_character_)

# Going to add this back now, 
# Bind reconciled + disputed into one label lookup 
# For disputed, adjudicated_label will be NA until you fill them in manually
label_lookup <- bind_rows(
  reconciled_sheet %>% mutate(status = "reconciled"),
  disputed_sheet %>%
    select(ID, adjudicated_label) %>%
    rename(agreed_primary = adjudicated_label) %>%
    mutate(agreed_secondary = NA_character_,
           agreed_tertiary  = NA_character_,
           fergus_additional = NA_character_,
           marion_additional = NA_character_,
           status = "disputed")
)

# Join back to full original sheet 
# Use fergus_sheet as the base — document metadata is the same in both

colnames(marion_sheet)[which(names(marion_sheet) == "Label")] <- "Label.Marion"
colnames(marion_sheet)[which(names(marion_sheet) == "Secondary Label")] <- "Secondary Label.Marion"
colnames(marion_sheet)[which(names(marion_sheet) == "Tertiary Label")] <- "Tertiary Label.Marion"
colnames(marion_sheet)[which(names(marion_sheet) == "Notes")] <- "Notes.Marion"

final_sheet <- fergus_sheet %>%
  distinct(ID, .keep_all = TRUE) %>%
  left_join(label_lookup, by = "ID") %>%
  left_join(marion_sheet %>% select(ID, Label.Marion, `Secondary Label.Marion`, 
                                    `Tertiary Label.Marion`, Notes.Marion), 
            by = "ID") %>%
  relocate(Label.Marion, `Secondary Label.Marion`, `Tertiary Label.Marion`, Notes.Marion,
           .after = Notes) %>%
  mutate(status = case_when(
    ID %in% exclude_ids ~ "excluded",
    is.na(status)       ~ "no_label",
    TRUE                ~ status
  ))

library(writexl)
write_xlsx(final_sheet, "reconciled_sheet.xlsx")

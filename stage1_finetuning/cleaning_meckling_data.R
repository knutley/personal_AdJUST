# Cleaning up the Meckling and Allan Scraped Data 
# Author: Katelyn Nutley 
# Date: 17/04/2026

library(readr)
meckling_df <- read_csv("~/Downloads/all_comments_20260417_172737.csv")

# Okay, I think this looks really good, but what I'm going to do is get rid of the 
# miscellaneous comments that don't pertain to the coding scheme; 

# Numbers: 57, 76, 87, 111, 137, 142, 143, 217, 278, 312, 315, 454, 466, 472, 516, 525, 1052, 1053, 1054, 1055, 1056

rows_to_remove <- c(57, 76, 87, 111, 137, 142, 143, 217, 278, 312, 315, 454, 466, 472, 516, 525, 1052, 1053, 1054, 1055, 1056)
meckling_df1 <- meckling_df[-rows_to_remove, ]
table(meckling_df1$comment_text) # okay, this removed everything that didn't have some kind of label 

# ── Label lookups ─────────────────────────────────────────────────────────────

# Discourse labels: N = Neoclassical, K/S = Green Growth, M = Limits to Growth
# Tags with no N/K/S/M annotation are assigned based on which discourse 
# section they appear in within the Meckling & Allan coding scheme
tag_labels <- c(
  # Neoclassical (N)
  "G-WC" = "Neoclassical",
  "P-WF" = "Neoclassical",
  "P-DG" = "Neoclassical",
  "P-MF" = "Neoclassical",
  "C-MK" = "Neoclassical",
  "C-PR" = "Neoclassical",
  "C-IR" = "Neoclassical",
  "C-SX" = "Neoclassical",
  "C-TT" = "Neoclassical",
  "C-WW" = "Neoclassical",
  "C-PV" = "Neoclassical",
  "C-IV" = "Neoclassical",
  "G-WC G-SD" = "Neoclassical",
  "P-WF G-WC" = "Neoclassical",
  "P-WF G-SD" = "Neoclassical",
  "C=PV" = "Neoclassical",
  "P-CC G-WC" = "Neoclassical",
  "P-WF?" = "Neoclassical",
  "P-PV P-WF" = "Neoclassical",
  "P-WF P-PV" = "Neoclassical",
  "G-MN G-WC" = "Neoclassical",
  "P-WF P-CC" = "Neoclassical",
  # Green Growth (K and/or S)
  "G-SC" = "Green Growth",
  "P-WG" = "Green Growth",
  "P-IN" = "Green Growth",
  "C-GV" = "Green Growth",
  "C-SD" = "Green Growth",
  "C-RD" = "Green Growth",
  "C-IF" = "Green Growth",
  "C-TP" = "Green Growth",
  "C-CT" = "Green Growth",
  "G-GG" = "Green Growth",
  "G-GV C-CT" = "Green Growth",
  "P-WG G-GG" = "Green Growth",
  "G-SD G-EQ G-SC" = "Green Growth",
  "G-GG G-SC" = "Green Growth",
  "C-CT C-RD" = "Green Growth",
  "C-IF C-CT" = "Green Growth",
  "C-CT C-IF" = "Green Growth",
  "G-DC C-CT P-IN" = "Green Growth",
  "C-CT C-IF" = "Green Growth",
  # Limits to Growth (M)
  "G-CF" = "Limits to Growth",
  "P-RB" = "Limits to Growth",
  "P-SE" = "Limits to Growth",
  "P-PR" = "Limits to Growth",
  "P-CO" = "Limits to Growth",
  "C-CO" = "Limits to Growth",
  "C-MK-" = "Limits to Growth",
  "C-IR-" = "Limits to Growth",
  "C-AM" = "Limits to Growth",
  "C-CO, P-PR" = "Limits to Growth", 
  "P-CO P-PR" = "Limits to Growth",
  "P-CO P-PV" = "Limits to Growth",
  "P-PR P-CO" = "Limits to Growth",
  "C-MK G-MN" = "Limits to Growth",
  "P-CO P-PR" = "Limits to Growth",
  # Spans multiple discourses
  "G-DC" = "Green Growth; Neoclassical",
  "C-WW G-SC" = "Green Growth; Neoclassical",
  "C-GV C-PV" = "Green Growth; Neoclassical",
  "G-GG P-CO P-PR" = "Green Growth; Limits to Growth",
  "G-GG C-CO" = "Green Growth; Limits to Growth"
)
names(tag_labels) <- toupper(names(tag_labels))

goal_labels <- c(
  "G-SD" = "sustainable development",
  "G-GG" = "green growth",
  "G-DC" = "decoupling (N, K, S)",
  "G-MN" = "mainstreaming environment",
  "G-EQ" = "equity (e.g., equitable, social, inclusive)",
  "G-RD" = "resilient development"
)

problem_labels <- c(
  "P-EN" = "environmental problems generally",
  "P-CC" = "climate change",
  "P-PV" = "poverty leads to enviro degradation",
  "P-WF" = "enviro degradation harms welfare/health/satisfaction",
  "P-DG" = "enviro degradation harms productivity",
  "P-MF" = "market failure (e.g., markets provide no incentive to curb pollution, externalities)",
  "P-RB" = "rebound effect (efficiency gains leading to more consumption; Jevons Paradox)",
  "P-SE" = "scarcity/entropy as a hard constraint on growth",
  "P-WG" = "weak growth, recession",
  "P-IN" = "lack of environmental innovation",
  "P-PR" = "production is a primary cause of enviro degradation",
  "P-CO" = "consumption is a primary cause of enviro degradation",
  "P-MT" = "metrics (notably GDP)",
  "P-TX" = "trade-environment conflict / undermines environmental standards",
  "P-TR" = "trade is an essential aspect of complementarity approach to enviro problems",
  "P-IP" = "institutional perspective"
)

policy_labels <- c(
  "C-CO"  = "Curbing consumption and resource extraction",
  "C-WW"  = "Win-win policies/no-regrets policies (e.g., water management, efficiency, poverty reduction)",
  "C-MK-" = "Market-based policy is insufficient",
  "C-MK"  = "Market-based policy (e.g., carbon pricing, ecosystem services)",
  "C-PR"  = "Property rights: clarify rights to own and use resources",
  "C-IR-" = "Incentives and regulations are insufficient",
  "C-IR"  = "Combine incentives and regulations to value environment",
  "C-SX"  = "Eliminate harmful subsidies",
  "C-SD"  = "Subsidies to stimulate demand for cleantech (e.g., short-term stimulus)",
  "C-RD"  = "Subsidies for R&D",
  "C-IF"  = "Infrastructure",
  "C-PV"  = "Mobilizing private sector finance and investment",
  "C-GV"  = "Government intervention (e.g., state, purchasing)",
  "C-IV"  = "Investment and financial mechanisms generally",
  "C-TT"  = "Technology transfer to developing countries",
  "C-TP"  = "Technology for pollution reduction (end-of-pipe)",
  "C-CT"  = "Clean technology (e.g., renewables)",
  "C-AM"  = "Alternative metrics"
)

# ── Pattern: anchored so only rows where comment_text IS a tag get matched ────
# Longer tags first so C-MK- matches before C-MK, C-IR- before C-IR

all_tags <- c(tag_labels, goal_labels, problem_labels, policy_labels)
tag_pattern <- paste(unique(names(all_tags)[order(-nchar(names(all_tags)))]), collapse = "|")
anchored_pattern <- paste0("^\\s*(", tag_pattern, ")\\s*$")

# ── Match and assign ──────────────────────────────────────────────────────────

# Normalise whitespace once, use for both matching and assignment
normalised_text <- toupper(trimws(gsub("\\s+", " ", meckling_df1$comment_text)))

matches <- regexpr(anchored_pattern, normalised_text, ignore.case = TRUE)
meckling_df1$matched_tag <- ifelse(matches > 0, normalised_text, NA)

meckling_df1$discourse_label <- tag_labels[meckling_df1$matched_tag]
meckling_df1$goal_label      <- goal_labels[meckling_df1$matched_tag]
meckling_df1$problem_label   <- problem_labels[meckling_df1$matched_tag]
meckling_df1$policy_label    <- policy_labels[meckling_df1$matched_tag]

# ── Summaries ─────────────────────────────────────────────────────────────────

table(meckling_df1$matched_tag)
table(meckling_df1$discourse_label)
table(meckling_df1$goal_label)
table(meckling_df1$problem_label)
table(meckling_df1$policy_label)
table(is.na(meckling_df1$matched_tag))

# ── Spot checks ───────────────────────────────────────────────────────────────

# Inspect any unmatched rows
meckling_df1[is.na(meckling_df1$matched_tag), "comment_text"] # looks good, will do some of these by hand I think 
table(is.na(meckling_df1$matched_tag))

write_csv(meckling_df1, "cleaned_meckling_data.csv")


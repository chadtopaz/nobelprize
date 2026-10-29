# =============================================================================
# File: 14_supplementary_analyses.R
# Title: Supplementary Analyses: Laureate Prediction, Nominator Heterogeneity,
#        and Temporal Trend Tests
#
# Author: Chad M. Topaz
# Date: March 2026
#
# Purpose:
#   Three additional analyses strengthening the core findings:
#
#   1. LAUREATE ASSOCIATION: Restricted to nominees with an existing QID
#      and exactly one matching candidate under the documented procedure.
#      Outcome: award in the nominated prize through 2025. Exclude records
#      after the first same-prize award; retain award-year nominations.
#      This is a selected-subset association, not a causal or population claim.
#      No manual crosswalk is required or claimed.
#
#   2. NOMINATOR HETEROGENEITY: Characterizes individual variation in nominator
#      homophily rates. Tests whether nominator country size, Scandinavian
#      origin, decade, and discipline predict per-nominator same-country rates.
#
#   3. TEMPORAL TREND TESTS: Formal regression tests of the temporal trend in
#      continent-level homophily, including piecewise decomposition into
#      pre-1940s rise and post-1940s plateau.
#
# Inputs:
#   From Data/intermediate/:
#     - nominations.csv: All nominations with geographic fields
#     - nomination_people_qids.csv: Person-ID to QID mapping
#     - laureates.csv: Prize winners with QIDs
#   From Data/:
#     - results_temporal_homophily.csv: Decade-level homophily ratios
#
# Outputs:
#   Data/:
#     - results_laureate_prediction.csv: Laureate-association comparison and
#       logistic regression on the unambiguously linked nominee subset
#     - results_laureate_prediction_by_prize.csv: Descriptive counts, mean
#       same-country shares, and differences by prize (no separate regressions)
#     - results_laureate_analysis_sample.csv, results_laureate_sample_flow.csv,
#       results_laureate_record_exclusions.csv, results_laureate_linkage_used.csv:
#       analysis sample, sample flow, excluded records, linkage used
#     - results_laureate_outcome_audit.csv, results_laureate_outcome_changes.csv:
#       audit of the v2.1 outcome correction relative to the pooled coding
#     - results_nominator_heterogeneity.csv: Per-nominator homophily summary
#     - results_nominator_heterogeneity_by_country.csv: By nominator country
#     - results_temporal_trend_tests.csv: Regression coefficients for trends
#     - results_laureate_breadth.csv, results_laureate_breadth_summary.csv (v2.4):
#       distinct nominator countries per eventual laureate linked to the archive (§S6.10)
#
# Dependencies:
#   - tidyverse
# =============================================================================

library(tidyverse)

# --- Path helper functions ---
data_path <- function(f) file.path("Data", f)
int_path  <- function(f) file.path("Data", "intermediate", f)

# --- Global seed ---
set.seed(42)

# =============================================================================
# LOAD DATA
# =============================================================================

message("Loading data...")

nominations <- read_csv(int_path("nominations.csv"), show_col_types = FALSE)
laureates   <- read_csv(int_path("laureates.csv"), show_col_types = FALSE)
qid_map     <- read_csv(int_path("nomination_people_qids.csv"), show_col_types = FALSE)

laureate_qids <- laureates %>% pull(qid) %>% unique()

message(sprintf("  %d nomination records", nrow(nominations)))
message(sprintf("  %d laureate QIDs", length(laureate_qids)))

# Map nominee person_id to QID
pid_to_qid <- qid_map %>%
  filter(!is.na(qid), qid != "") %>%
  select(person_id, qid) %>%
  deframe()

# =============================================================================
# ANALYSIS 1: LAUREATE ASSOCIATION IN THE UNAMBIGUOUSLY LINKED SUBSET
# =============================================================================

# Preserve the original shared geography-complete records for analyses 2/3.
# Only the helper's separate laureate sample is restricted by identity/timing.
nom_data <- nominations %>%
  filter(
    !is.na(nominator_country_modern), nominator_country_modern != "NA",
    !is.na(nominee_country_modern), nominee_country_modern != "NA",
    !is.na(nominee_person_id)
  ) %>%
  mutate(
    same_country = as.integer(nominator_country_modern == nominee_country_modern),
    nominee_qid = unname(pid_to_qid[as.character(nominee_person_id)])
  )

source("Code/Analysis/helpers/laureate_matched_subset.R")
laureate_outputs <- run_matched_laureate_analysis(nominations, qid_map, laureates)
for (output_name in names(laureate_outputs)) {
  write_csv(laureate_outputs[[output_name]], data_path(output_name))
}
message("Saved the restricted laureate comparison and its sample/exclusion audit.")

# =============================================================================
# ANALYSIS 2: NOMINATOR HETEROGENEITY
# =============================================================================

message("\n=== Analysis 2: Nominator Heterogeneity ===")

# Per-nominator aggregation (across all nominations)
nominator_level <- nom_data %>%
  filter(!is.na(nominator_person_id)) %>%   # v2.4: drop the NA pseudo-nominator (481 records)
  group_by(nominator_person_id) %>%
  summarise(
    country         = first(nominator_country_modern),
    total_noms      = n(),
    same_country    = sum(same_country),
    same_frac       = same_country / total_noms,
    n_prizes        = n_distinct(prize),
    prizes          = paste(sort(unique(prize)), collapse = ";"),
    min_decade      = min(as.integer(year) %/% 10 * 10),
    max_decade      = max(as.integer(year) %/% 10 * 10),
    n_countries_nominated = n_distinct(nominee_country_modern),
    .groups = "drop"
  )

# Country size (number of nominators from each country)
country_sizes <- nominator_level %>%
  count(country, name = "country_n_nominators")

nominator_level <- nominator_level %>%
  left_join(country_sizes, by = "country") %>%
  mutate(
    scandinavian = country %in% c("Sweden", "Norway", "Denmark"),
    country_size_cat = case_when(
      country_n_nominators <= 50  ~ "small",
      country_n_nominators <= 200 ~ "medium",
      country_n_nominators <= 500 ~ "large",
      TRUE                        ~ "very_large"
    )
  )

# --- Summary stats for nominators with 2+ nominations ---
repeat_nominators <- nominator_level %>% filter(total_noms >= 2)
message(sprintf("  Nominators with 2+ noms: %d", nrow(repeat_nominators)))
message(sprintf("  Mean same-country rate: %.3f", mean(repeat_nominators$same_frac)))
message(sprintf("  Median: %.3f", median(repeat_nominators$same_frac)))
message(sprintf("  %% fully same-country: %.1f%%", 100 * mean(repeat_nominators$same_frac == 1)))
message(sprintf("  %% fully cross-country: %.1f%%", 100 * mean(repeat_nominators$same_frac == 0)))

# --- Scandinavian vs non-Scandinavian ---
scand_summary <- repeat_nominators %>%
  group_by(scandinavian) %>%
  summarise(
    n = n(),
    mean_same_frac = mean(same_frac),
    .groups = "drop"
  )
message("\nScandinavian vs non-Scandinavian:")
print(scand_summary)

# --- By country (top countries) ---
by_country <- repeat_nominators %>%
  group_by(country) %>%
  summarise(
    n_nominators    = n(),
    mean_same_frac  = mean(same_frac),
    median_same_frac = median(same_frac),
    .groups = "drop"
  ) %>%
  arrange(desc(n_nominators))

message("\nTop 10 nominator countries:")
print(by_country %>% head(10))

# --- By country size category ---
by_size <- repeat_nominators %>%
  group_by(country_size_cat) %>%
  summarise(
    n = n(),
    mean_same_frac = mean(same_frac),
    .groups = "drop"
  )
message("\nBy country size:")
print(by_size)

# --- Save results ---
write_csv(
  tibble(
    statistic = c(
      "n_repeat_nominators", "mean_same_frac", "median_same_frac",
      "pct_fully_same", "pct_fully_cross",
      "scand_mean_same_frac", "scand_n",
      "non_scand_mean_same_frac", "non_scand_n"
    ),
    value = c(
      nrow(repeat_nominators),
      mean(repeat_nominators$same_frac),
      median(repeat_nominators$same_frac),
      100 * mean(repeat_nominators$same_frac == 1),
      100 * mean(repeat_nominators$same_frac == 0),
      scand_summary$mean_same_frac[scand_summary$scandinavian == TRUE],
      scand_summary$n[scand_summary$scandinavian == TRUE],
      scand_summary$mean_same_frac[scand_summary$scandinavian == FALSE],
      scand_summary$n[scand_summary$scandinavian == FALSE]
    )
  ),
  data_path("results_nominator_heterogeneity.csv")
)

write_csv(by_country, data_path("results_nominator_heterogeneity_by_country.csv"))
message("  Saved results_nominator_heterogeneity.csv")
message("  Saved results_nominator_heterogeneity_by_country.csv")

# =============================================================================
# ANALYSIS 3: TEMPORAL TREND TESTS
# =============================================================================

message("\n=== Analysis 3: Temporal Trend Tests ===")

temporal <- read_csv(data_path("results_temporal_homophily.csv"), show_col_types = FALSE)

# --- Continent-level trend ---
cont <- temporal %>%
  filter(geo_level == "continent") %>%
  arrange(decade)

# Linear model
lm_full <- lm(homophily_ratio ~ decade, data = cont)
lm_sum  <- summary(lm_full)
message("Linear: H_continent ~ decade")
message(sprintf("  Slope: %.5f per year (%.4f per decade)", coef(lm_full)[2], coef(lm_full)[2] * 10))
message(sprintf("  t = %.2f, R² = %.3f", lm_sum$coefficients[2, 3], lm_sum$r.squared))

# Quadratic model
lm_quad <- lm(homophily_ratio ~ decade + I(decade^2), data = cont)
lm_quad_sum <- summary(lm_quad)
message(sprintf("\nQuadratic R²: %.3f", lm_quad_sum$r.squared))

# Piecewise: pre-1940s vs post-1940s
pre  <- cont %>% filter(decade <= 1940)
post <- cont %>% filter(decade >= 1940)

lm_pre  <- lm(homophily_ratio ~ decade, data = pre)
lm_post <- lm(homophily_ratio ~ decade, data = post)

pre_sum  <- summary(lm_pre)
post_sum <- summary(lm_post)

message(sprintf("\nPre-1940s: slope = %.4f/decade, t = %.2f",
                coef(lm_pre)[2] * 10, pre_sum$coefficients[2, 3]))
message(sprintf("Post-1940s: slope = %.4f/decade, t = %.2f",
                coef(lm_post)[2] * 10, post_sum$coefficients[2, 3]))

# --- Country-level trend ---
country <- temporal %>%
  filter(geo_level == "country") %>%
  arrange(decade)

lm_country <- lm(homophily_ratio ~ decade, data = country)
lm_country_sum <- summary(lm_country)
message(sprintf("\nCountry-level: slope = %.4f/decade, R² = %.3f",
                coef(lm_country)[2] * 10, lm_country_sum$r.squared))

# --- O - E trend ---
cont <- cont %>%
  mutate(O_minus_E_pp = (observed_rate - expected_rate) * 100)

lm_oe <- lm(O_minus_E_pp ~ decade, data = cont)
lm_oe_sum <- summary(lm_oe)
message(sprintf("\nO-E trend: slope = %.2f pp/decade, R² = %.3f",
                coef(lm_oe)[2] * 10, lm_oe_sum$r.squared))

# --- Save results ---
trend_results <- tibble(
  model = c(
    "continent_linear", "continent_linear",
    "continent_quadratic",
    "continent_pre1940", "continent_pre1940",
    "continent_post1940", "continent_post1940",
    "country_linear", "country_linear",
    "OminusE_linear"
  ),
  statistic = c(
    "slope_per_decade", "t_statistic",
    "R_squared",
    "slope_per_decade", "t_statistic",
    "slope_per_decade", "t_statistic",
    "slope_per_decade", "R_squared",
    "slope_pp_per_decade"
  ),
  value = c(
    coef(lm_full)[2] * 10, lm_sum$coefficients[2, 3],
    lm_quad_sum$r.squared,
    coef(lm_pre)[2] * 10, pre_sum$coefficients[2, 3],
    coef(lm_post)[2] * 10, post_sum$coefficients[2, 3],
    coef(lm_country)[2] * 10, lm_country_sum$r.squared,
    coef(lm_oe)[2] * 10
  )
)

write_csv(trend_results, data_path("results_temporal_trend_tests.csv"))
message("  Saved results_temporal_trend_tests.csv")

# =============================================================================
# ANALYSIS 4 (v2.4): NOMINATION BREADTH PER LINKED LAUREATE (supplement §S6.10)
# =============================================================================
# For every eventual laureate (any prize, awards through 2025) whose QID is linked
# to at least one archive nominee identifier by the automated linkage (including
# the API-ranked multi-matches), pool the nominations of all linked identifiers
# across prizes and years and count distinct nominator countries. Laureates with
# no nomination carrying a nominator country are excluded (expect 449 laureates).

message("\n=== Analysis 4: Nomination breadth per linked laureate ===")

laureate_breadth <- qid_map %>%
  filter(!is.na(qid), qid != "", qid %in% laureate_qids) %>%
  select(person_id, qid) %>%
  inner_join(
    nominations %>%
      filter(!is.na(nominator_country_modern), nominator_country_modern != "NA") %>%
      select(nominee_person_id, nominator_country_modern),
    by = c("person_id" = "nominee_person_id")
  ) %>%
  group_by(qid) %>%
  summarise(n_nominator_countries = n_distinct(nominator_country_modern), .groups = "drop")

laureate_breadth_summary <- tibble(
  statistic = c("n_laureates", "median", "mean", "q1", "q3"),
  value = c(nrow(laureate_breadth),
            median(laureate_breadth$n_nominator_countries),
            mean(laureate_breadth$n_nominator_countries),
            unname(quantile(laureate_breadth$n_nominator_countries, 0.25)),
            unname(quantile(laureate_breadth$n_nominator_countries, 0.75)))
)
print(laureate_breadth_summary)
write_csv(laureate_breadth, data_path("results_laureate_breadth.csv"))
write_csv(laureate_breadth_summary, data_path("results_laureate_breadth_summary.csv"))
message("  Saved results_laureate_breadth.csv and results_laureate_breadth_summary.csv")

# =============================================================================
# DONE
# =============================================================================

message("\n=== All supplementary analyses complete ===")
message("Output files:")
message("  Data/results_laureate_prediction.csv")
message("  Data/results_laureate_prediction_by_prize.csv")
message("  Data/results_laureate_*.csv (analysis sample, sample flow, exclusions, linkage, outcome audit)")
message("  Data/results_nominator_heterogeneity.csv")
message("  Data/results_nominator_heterogeneity_by_country.csv")
message("  Data/results_temporal_trend_tests.csv")
message("  Data/results_laureate_breadth.csv, Data/results_laureate_breadth_summary.csv")

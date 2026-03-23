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
#   1. LAUREATE PREDICTION: Tests whether same-country nominations are more or
#      less likely to identify eventual laureates. If geographic homophily
#      reflects informational advantage (nominators knowing compatriots' work
#      better), same-country nominations should be better predictors of laureate
#      status. We test this at the nominee level with a logistic regression
#      controlling for total nomination volume.
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
#     - results_laureate_prediction.csv: Nominee-level laureate prediction
#     - results_laureate_prediction_by_prize.csv: Same, by prize
#     - results_nominator_heterogeneity.csv: Per-nominator homophily summary
#     - results_nominator_heterogeneity_by_country.csv: By nominator country
#     - results_temporal_trend_tests.csv: Regression coefficients for trends
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
# ANALYSIS 1: LAUREATE PREDICTION
# =============================================================================

message("\n=== Analysis 1: Laureate Prediction ===")

# Prepare nomination-level data
nom_data <- nominations %>%
  filter(
    !is.na(nominator_country_modern), nominator_country_modern != "NA",
    !is.na(nominee_country_modern), nominee_country_modern != "NA",
    !is.na(nominee_person_id)
  ) %>%
  mutate(
    same_country = as.integer(nominator_country_modern == nominee_country_modern),
    nominee_qid  = pid_to_qid[as.character(nominee_person_id)],
    is_laureate  = as.integer(nominee_qid %in% laureate_qids)
  )

message(sprintf("  %d nominations with complete geography", nrow(nom_data)))

# --- Nominee-level aggregation ---
nominee_level <- nom_data %>%
  group_by(nominee_person_id, prize) %>%
  summarise(
    nominee_qid        = first(nominee_qid),
    total_nominations  = n(),
    same_country_noms  = sum(same_country),
    cross_country_noms = total_nominations - same_country_noms,
    same_country_frac  = same_country_noms / total_nominations,
    is_laureate        = first(is_laureate),
    .groups = "drop"
  )

# --- Overall comparison ---
overall_comparison <- nominee_level %>%
  group_by(is_laureate) %>%
  summarise(
    n_nominees         = n(),
    mean_same_frac     = mean(same_country_frac),
    median_same_frac   = median(same_country_frac),
    sd_same_frac       = sd(same_country_frac),
    .groups = "drop"
  )

message("Overall comparison:")
print(overall_comparison)

# --- Permutation test for difference in means ---
laureate_fracs     <- nominee_level %>% filter(is_laureate == 1) %>% pull(same_country_frac)
non_laureate_fracs <- nominee_level %>% filter(is_laureate == 0) %>% pull(same_country_frac)
observed_diff      <- mean(non_laureate_fracs) - mean(laureate_fracs)

N_PERM <- 10000
all_fracs <- c(laureate_fracs, non_laureate_fracs)
n_lau     <- length(laureate_fracs)
n_total   <- length(all_fracs)

perm_diffs <- replicate(N_PERM, {
  perm <- sample(all_fracs)
  mean(perm[(n_lau + 1):n_total]) - mean(perm[1:n_lau])
})
p_value <- (sum(perm_diffs >= observed_diff) + 1) / (N_PERM + 1)
message(sprintf("  Observed diff: %.4f, p = %.4f", observed_diff, p_value))

# --- Logistic regression controlling for total nominations ---
logit_data <- nominee_level %>%
  mutate(log_total_noms = log(total_nominations))

logit_model <- glm(
  is_laureate ~ same_country_frac + log_total_noms,
  data   = logit_data,
  family = binomial(link = "logit")
)

logit_summary <- summary(logit_model)
message("\nLogistic regression: is_laureate ~ same_country_frac + log(total_noms)")
print(logit_summary$coefficients)

# Extract key results
logit_coefs <- as.data.frame(logit_summary$coefficients)
logit_coefs$term <- rownames(logit_coefs)
logit_coefs$odds_ratio <- exp(logit_coefs$Estimate)

# --- By prize ---
by_prize <- nominee_level %>%
  group_by(prize, is_laureate) %>%
  summarise(
    n          = n(),
    mean_frac  = mean(same_country_frac),
    .groups    = "drop"
  ) %>%
  pivot_wider(
    names_from  = is_laureate,
    values_from = c(n, mean_frac),
    names_sep   = "_"
  ) %>%
  mutate(diff = mean_frac_0 - mean_frac_1)

message("\nBy prize:")
print(by_prize)

# --- By nomination count bucket ---
by_bucket <- nominee_level %>%
  mutate(bucket = case_when(
    total_nominations == 1          ~ "1",
    total_nominations <= 5          ~ "2-5",
    total_nominations <= 20         ~ "6-20",
    TRUE                            ~ "21+"
  )) %>%
  group_by(bucket, is_laureate) %>%
  summarise(
    n         = n(),
    mean_frac = mean(same_country_frac),
    .groups   = "drop"
  ) %>%
  pivot_wider(
    names_from  = is_laureate,
    values_from = c(n, mean_frac),
    names_sep   = "_"
  ) %>%
  mutate(diff = mean_frac_0 - mean_frac_1)

# --- Save results ---
laureate_pred_results <- tibble(
  statistic = c(
    "n_laureates", "n_non_laureates",
    "mean_same_frac_laureates", "mean_same_frac_non_laureates",
    "diff_non_minus_lau", "permutation_p_value",
    "logit_coef_same_frac", "logit_se_same_frac", "logit_z_same_frac",
    "logit_odds_ratio_same_frac",
    "logit_coef_log_noms", "logit_se_log_noms", "logit_z_log_noms"
  ),
  value = c(
    length(laureate_fracs), length(non_laureate_fracs),
    mean(laureate_fracs), mean(non_laureate_fracs),
    observed_diff, p_value,
    logit_coefs$Estimate[logit_coefs$term == "same_country_frac"],
    logit_coefs$`Std. Error`[logit_coefs$term == "same_country_frac"],
    logit_coefs$`z value`[logit_coefs$term == "same_country_frac"],
    logit_coefs$odds_ratio[logit_coefs$term == "same_country_frac"],
    logit_coefs$Estimate[logit_coefs$term == "log_total_noms"],
    logit_coefs$`Std. Error`[logit_coefs$term == "log_total_noms"],
    logit_coefs$`z value`[logit_coefs$term == "log_total_noms"]
  )
)

write_csv(laureate_pred_results, data_path("results_laureate_prediction.csv"))
write_csv(by_prize, data_path("results_laureate_prediction_by_prize.csv"))
message("  Saved results_laureate_prediction.csv")
message("  Saved results_laureate_prediction_by_prize.csv")

# =============================================================================
# ANALYSIS 2: NOMINATOR HETEROGENEITY
# =============================================================================

message("\n=== Analysis 2: Nominator Heterogeneity ===")

# Per-nominator aggregation (across all nominations)
nominator_level <- nom_data %>%
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
# DONE
# =============================================================================

message("\n=== All supplementary analyses complete ===")
message("Output files:")
message("  Data/results_laureate_prediction.csv")
message("  Data/results_laureate_prediction_by_prize.csv")
message("  Data/results_nominator_heterogeneity.csv")
message("  Data/results_nominator_heterogeneity_by_country.csv")
message("  Data/results_temporal_trend_tests.csv")

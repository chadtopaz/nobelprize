# =============================================================================
# File: 16_qid_representativeness.R
# Title: Representativeness of the QID-Matched Subset
#
# Author: Chad M. Topaz et al. (Nobel Prize project), QSS Revision 1
# Date: August 2026
#
# Purpose:
#   Referee points R2-5 and R3-5. Characterizes the 3,951-person QID-matched
#   subset against the full 14,474-person nomination archive:
#     1. Match-rate coverage tables by prize, decade, subregion, role
#        (+ direct test of R3's conjecture that unmatched names concentrate
#        in open-nomination Literature/Peace).
#     2. Matched-vs-unmatched comparison on archive-observable covariates.
#     3. Match-propensity model + inverse-probability-weighted re-estimate of
#        the birth-country homophily ratio on QID-matched nomination edges.
#     4. Regionally stratified random sample of unmatched names for manual
#        resolution (blank columns for resolvers).
#
#   Write-up context to preserve: the primary H = 4.85 does NOT use this
#   subset; it uses the archive's own professional-country field. The subset
#   feeds only the birth-country robustness check (H = 3.82, N = 8,119) and
#   institutional linkage.
#
# EXPECTED BENCHMARKS: 14,474 people; 4,570 matchable (name + birth year);
#   3,951 matched (86.5% of matchable, 27.3% overall). IPW step should
#   reproduce N ~= 8,119 edges and unweighted H ~= 3.82 before weighting.
#
# Inputs:  Data/intermediate/nomination_people_qids.csv,
#          Data/intermediate/nominations.csv,
#          Data/intermediate/demographics.csv
# Outputs: Data/results_qid_coverage_by_{prize,decade,subregion,role}.csv,
#          Data/results_qid_matched_vs_unmatched.csv,
#          Data/results_qid_match_propensity.csv,
#          Data/results_qid_H_ipw.csv,
#          Data/manual_validation_sample.csv
# Run from the repository root (NobelPrize.Rproj).
# =============================================================================

library(tidyverse)
library(broom)

int_path  <- function(f) file.path("Data", "intermediate", f)
data_path <- function(f) file.path("Data", f)
set.seed(1901)   # reproducible manual-validation sample

N_SAMPLE <- 200  # manual-validation sample size (as reported in §S7)

message("Loading data...")
pq    <- read_csv(int_path("nomination_people_qids.csv"), show_col_types = FALSE)
noms  <- read_csv(int_path("nominations.csv"),            show_col_types = FALSE)
demo  <- read_csv(int_path("demographics.csv"),           show_col_types = FALSE)

# ── Person-level archive observables (available regardless of match status) ─
person_obs <- bind_rows(
  noms %>% filter(!is.na(nominator_person_id)) %>%
    transmute(person_id = nominator_person_id, role_i = "nominator",
              prize, year, subregion = nominator_subregion,
              country = nominator_country_modern),
  noms %>%
    transmute(person_id = nominee_person_id, role_i = "nominee",
              prize, year, subregion = nominee_subregion,
              country = nominee_country_modern)
) %>%
  group_by(person_id) %>%
  summarise(
    n_records   = n(),
    role        = case_when(all(role_i == "nominator") ~ "nominator only",
                            all(role_i == "nominee")   ~ "nominee only",
                            TRUE                        ~ "both"),
    prize_main  = names(sort(table(prize), decreasing = TRUE))[1],
    decade_main = paste0(floor(median(year) / 10) * 10, "s"),
    subregion   = if (all(is.na(subregion))) NA_character_
                  else names(sort(table(subregion), decreasing = TRUE))[1],
    country     = if (all(is.na(country))) NA_character_
                  else names(sort(table(country), decreasing = TRUE))[1],
    .groups = "drop"
  )

df <- pq %>%
  mutate(matched   = !is.na(qid),
         matchable = !is.na(birth_year) & !is.na(name)) %>%
  left_join(person_obs, by = "person_id")
message("People: ", nrow(df), " (expect 14,474); matchable: ", sum(df$matchable),
        " (expect 4,570); matched: ", sum(df$matched), " (expect 3,951)")

# ── 1. Coverage tables ──────────────────────────────────────────────────────
cov_tab <- function(var) {
  df %>% filter(!is.na(.data[[var]])) %>%
    group_by(across(all_of(var))) %>%
    summarise(n = n(), n_matched = sum(matched),
              match_rate = round(mean(matched), 3), .groups = "drop") %>%
    arrange(desc(n))
}
for (v in c("prize_main", "decade_main", "subregion", "role")) {
  out <- cov_tab(v)
  write_csv(out, data_path(paste0("results_qid_coverage_by_",
                                  sub("_main", "", v), ".csv")))
}
message("R3 conjecture — do unmatched names concentrate in Literature/Peace?")
r3_test <- df %>%
  mutate(open_prize = prize_main %in% c("Literature", "Peace")) %>%
  count(open_prize, matched) %>%
  group_by(open_prize) %>% mutate(rate_matched = round(n / sum(n), 3))
print(r3_test)

# ── 2. Matched vs unmatched comparison ─────────────────────────────────────
comparison <- df %>%
  group_by(matched) %>%
  summarise(n = n(),
            median_records     = median(n_records, na.rm = TRUE),
            mean_records       = round(mean(n_records, na.rm = TRUE), 2),
            pct_nominator_only = round(mean(role == "nominator only", na.rm = TRUE), 3),
            pct_europe         = round(mean(str_detect(coalesce(subregion, ""), "Europe")), 3),
            pct_has_country    = round(mean(!is.na(country)), 3),
            .groups = "drop")
print(comparison)
write_csv(comparison, data_path("results_qid_matched_vs_unmatched.csv"))

# ── 3. Match-propensity model + IPW re-estimate of birth-country H ─────────
prop_dat <- df %>%
  filter(!is.na(subregion), !is.na(prize_main)) %>%
  mutate(subregion_c = fct_lump_min(factor(subregion), 100))
prop_mod <- glm(matched ~ subregion_c + decade_main + prize_main + role +
                  log1p(n_records),
                data = prop_dat, family = binomial())
write_csv(tidy(prop_mod), data_path("results_qid_match_propensity.csv"))
prop_dat$p_match <- predict(prop_mod, type = "response")

# QID birth-country nomination edges (mirrors the paper's robustness subset):
# records -> both endpoints matched to a QID with non-missing birth country.
bc <- demo %>% select(qid, birth_country_modern)
qid_of <- pq %>% select(person_id, qid)
qe <- noms %>%
  filter(!is.na(nominator_person_id)) %>%
  distinct(nominator_person_id, nominee_person_id, year, prize) %>%
  left_join(qid_of, by = c("nominator_person_id" = "person_id")) %>% rename(qid_s = qid) %>%
  left_join(qid_of, by = c("nominee_person_id"   = "person_id")) %>% rename(qid_r = qid) %>%
  left_join(bc, by = c("qid_s" = "qid")) %>% rename(bc_s = birth_country_modern) %>%
  left_join(bc, by = c("qid_r" = "qid")) %>% rename(bc_r = birth_country_modern) %>%
  filter(!is.na(bc_s), !is.na(bc_r))
message("QID birth-country edges: ", nrow(qe), " (expect ~8,119)")

H_of <- function(from, to, w = rep(1, length(from))) {
  O  <- weighted.mean(from == to, w)
  fs <- tapply(w, from, sum); fs <- fs / sum(fs)
  ts <- tapply(w, to,   sum); ts <- ts / sum(ts)
  cc <- intersect(names(fs), names(ts))
  E  <- sum(fs[cc] * ts[cc])
  c(O = O, E = E, H = O / E)
}
H_unw <- H_of(qe$bc_s, qe$bc_r)
message(sprintf("Unweighted birth-country H = %.2f (expect ~3.82)", H_unw["H"]))

qe_w <- qe %>%
  left_join(prop_dat %>% select(person_id, p_s = p_match),
            by = c("nominator_person_id" = "person_id")) %>%
  left_join(prop_dat %>% select(person_id, p_r = p_match),
            by = c("nominee_person_id" = "person_id")) %>%
  filter(!is.na(p_s), !is.na(p_r)) %>%
  mutate(w = 1 / (pmax(p_s, 0.02) * pmax(p_r, 0.02)))   # trimmed weights
H_ipw <- H_of(qe_w$bc_s, qe_w$bc_r, qe_w$w)
message(sprintf("IPW birth-country H = %.2f on %d edges", H_ipw["H"], nrow(qe_w)))
write_csv(tibble(spec = c("unweighted", "ipw"),
                 N = c(nrow(qe), nrow(qe_w)),
                 O = c(H_unw["O"], H_ipw["O"]),
                 E = c(H_unw["E"], H_ipw["E"]),
                 H = c(H_unw["H"], H_ipw["H"])),
          data_path("results_qid_H_ipw.csv"))

# Movers check [R2-3]: among matched nominees whose birth country differs from
# professional country, which geography do nominators match?
movers <- noms %>%
  filter(!is.na(nominator_country_modern), !is.na(nominee_country_modern)) %>%
  left_join(qid_of, by = c("nominee_person_id" = "person_id")) %>%
  left_join(bc, by = c("qid" = "qid")) %>%
  filter(!is.na(birth_country_modern),
         birth_country_modern != nominee_country_modern) %>%
  summarise(n_mover_records   = n(),
            match_professional = mean(nominator_country_modern == nominee_country_modern),
            match_birth        = mean(nominator_country_modern == birth_country_modern))
print(movers)
write_csv(movers, data_path("results_qid_movers.csv"))

# ── 4. Manual-validation sample (stratified by subregion) ──────────────────
pool <- df %>% filter(!matched, !is.na(name), !is.na(subregion))
k_per <- ceiling(N_SAMPLE / n_distinct(pool$subregion))
manual_sample <- pool %>%
  group_by(subregion) %>%
  group_modify(~ slice_sample(.x, n = min(k_per, nrow(.x)))) %>%  # safe when a stratum is small
  ungroup() %>%
  slice_sample(n = min(N_SAMPLE, nrow(.))) %>%
  select(person_id, name, prize_main, decade_main, country, subregion, n_records) %>%
  mutate(resolved_identity = "", resolved_birth_year = "",
         resolved_birth_country = "", resolved_prof_country = "",
         resolver = "", confidence = "", notes = "")
write_csv(manual_sample, data_path("manual_validation_sample.csv"))
message("Wrote manual_validation_sample.csv (n = ", nrow(manual_sample),
        ") for hand verification (protocol and results: §S7).")
message("Done.")
# =============================================================================

# =============================================================================
# File: 18_clogit_prominence.R
# Title: Edge-Level Conditional Logit with Nominee-Prominence Control
#
# Author: Chad M. Topaz et al. (Nobel Prize project), QSS Revision 1
# Date: August 2026
#
# Purpose:
#   Completes referee point R2-6/R2-4 (nominee prominence): a discrete-choice
#   companion to the gravity model. For each nomination record, the chosen
#   nominee is compared against sampled control candidates active in the same
#   prize-year (i.e., nominated by someone that prize-year). Conditional
#   logistic regression with record strata estimates the same-country effect
#   conditional on candidate prominence, so "nominators favor compatriots"
#   is separated from "nominators favor already-prominent candidates who
#   happen to be nearby."
#
# Methodological decisions:
#   - Risk set: distinct candidates nominated in the same prize-year,
#     excluding the chosen nominee; K = 20 controls sampled per record
#     (standard case-control sampling for conditional logit; consistent for
#     odds ratios). Records whose prize-year pool has < 2 candidates drop.
#   - Candidate covariates: same_country (nominator professional country vs
#     candidate professional country, modal within prize-year), shared_lang
#     (same coarse language map as script 15), log1p(prior nominations
#     received by the candidate in that prize before the focal year).
#   - Estimation: survival::clogit, strata(record_id); SEs clustered by
#     nominator via cluster(nominator_id).
#
# EXPECTED BEHAVIOR: same-country OR large (same order as gravity premia);
#   prominence coefficient positive; same-country OR should remain large
#   conditional on prominence. Runtime ~1-3 min.
#
# Inputs:  Data/intermediate/nominations.csv
# Outputs: Data/results_clogit_prominence.csv
# Run from the repository root (NobelPrize.Rproj).
# =============================================================================

library(tidyverse)
library(survival)

int_path  <- function(f) file.path("Data", "intermediate", f)
data_path <- function(f) file.path("Data", f)
set.seed(1901)
K_CONTROLS <- 20

lang_map <- c(
  "United States"="en","United Kingdom"="en","Ireland"="en","Canada"="en",
  "Australia"="en","New Zealand"="en","India"="en",
  "Germany"="de","Austria"="de","Switzerland"="de","France"="fr","Belgium"="fr",
  "Sweden"="sv","Norway"="no","Denmark"="da","Iceland"="is","Netherlands"="nl",
  "Italy"="it","Spain"="es","Portugal"="pt","Mexico"="es","Argentina"="es",
  "Chile"="es","Brazil"="pt","Russia"="ru","Poland"="pl","Czech Republic"="cs",
  "Czechia"="cs","Hungary"="hu","Finland"="fi","Greece"="el","Turkey"="tr",
  "Japan"="ja","China"="zh","Egypt"="ar","Israel"="he","Romania"="ro","Bulgaria"="bg")

message("Loading nominations...")
noms <- read_csv(int_path("nominations.csv"), show_col_types = FALSE)
d <- noms %>%
  filter(!is.na(nominator_continent), !is.na(nominee_continent),
         !is.na(nominee_person_id)) %>%
  transmute(prize, year,
            nominator_id = nominator_person_id,
            nominee_id   = nominee_person_id,
            s_ctry = nominator_country_modern,
            r_ctry = nominee_country_modern) %>%
  filter(!is.na(s_ctry)) %>%
  mutate(record_id = row_number())
message("Records with nominator geography: ", nrow(d))

# Candidate attributes per prize-year: modal professional country.
cand <- d %>%
  count(prize, year, nominee_id, r_ctry) %>%
  group_by(prize, year, nominee_id) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(prize, year, cand_id = nominee_id, cand_ctry = r_ctry)

# Prominence: cumulative nominations received in this prize BEFORE focal year.
prior <- d %>%
  count(prize, nominee_id, year, name = "n_y") %>%
  arrange(prize, nominee_id, year) %>%
  group_by(prize, nominee_id) %>%
  mutate(prior_noms = cumsum(n_y) - n_y) %>%   # strictly before focal year
  ungroup() %>%
  select(prize, year, cand_id = nominee_id, prior_noms)

cand <- cand %>%
  left_join(prior, by = c("prize", "year", "cand_id")) %>%
  mutate(prior_noms = replace_na(prior_noms, 0L))

# Build case-control data: for each record, the chosen candidate plus up to
# K_CONTROLS sampled same-prize-year alternatives.
message("Building case-control risk sets (K = ", K_CONTROLS, ")...")
pools <- cand %>% group_by(prize, year) %>% summarise(pool = list(cand_id), .groups = "drop")
d2 <- d %>% left_join(pools, by = c("prize", "year"))

cc <- d2 %>%
  mutate(controls = map2(pool, nominee_id, function(p, ch) {
    alts <- setdiff(p, ch)
    if (length(alts) == 0) return(integer(0))
    sample(alts, min(K_CONTROLS, length(alts)))
  })) %>%
  select(record_id, prize, year, nominator_id, s_ctry, nominee_id, controls)

long <- bind_rows(
  cc %>% transmute(record_id, prize, year, nominator_id, s_ctry,
                   cand_id = nominee_id, chosen = 1L),
  cc %>% select(-nominee_id) %>% unnest_longer(controls) %>%
    rename(cand_id = controls) %>%
    mutate(chosen = 0L) %>%
    select(record_id, prize, year, nominator_id, s_ctry, cand_id, chosen)
) %>%
  left_join(cand, by = c("prize", "year", "cand_id")) %>%
  filter(!is.na(cand_ctry)) %>%
  group_by(record_id) %>% filter(n() >= 2, sum(chosen) == 1) %>% ungroup() %>%
  mutate(same_country = as.integer(s_ctry == cand_ctry),
         shared_lang  = as.integer(!is.na(lang_map[s_ctry]) & !is.na(lang_map[cand_ctry]) &
                                   lang_map[s_ctry] == lang_map[cand_ctry]),
         log_prior    = log1p(prior_noms),
         cluster_nom  = coalesce(as.character(nominator_id), paste0("anon", record_id)))
message(nrow(long), " case-control rows across ", n_distinct(long$record_id), " records")

message("Fitting conditional logits...")
m1 <- clogit(chosen ~ same_country + strata(record_id),
             data = long, cluster = cluster_nom, method = "approximate")
m2 <- clogit(chosen ~ same_country + shared_lang + log_prior + strata(record_id),
             data = long, cluster = cluster_nom, method = "approximate")

tidy_cl <- function(m, label) {
  ct <- as.data.frame(summary(m)$coefficients) %>% rownames_to_column("term")
  names(ct) <- c("term", "est", "or", "se", "robust_se", "z", "p")
  ct %>% mutate(model = label, .before = 1)
}
out <- bind_rows(tidy_cl(m1, "bare"), tidy_cl(m2, "with_lang_prominence"))
print(out %>% select(model, term, or, robust_se, p))
message(sprintf("Same-country OR bare: %.2f;  conditional on language + prominence: %.2f",
                exp(coef(m1)["same_country"]), exp(coef(m2)["same_country"])))
write_csv(out, data_path("results_clogit_prominence.csv"))
message("Done. -> results_clogit_prominence.csv")
# =============================================================================

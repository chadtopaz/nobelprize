# =============================================================================
# File: 15_dyadic_model.R
# Title: Gravity Model of Nomination Flows (Destination-Attractiveness Conditioning)
#
# Author: Chad M. Topaz et al. (Nobel Prize project), QSS Revision 1
# Date: August 2026
#
# Purpose:
#   Referee points R1-5 (capacity/resources), R2-6 (structured edge-level
#   model), R1-3 / R2-4 (language mechanism probe). Models counts of
#   nominations from professional country a -> country b within prize x decade
#   strata, with sender-country x prize x decade and receiver-country x prize x
#   decade fixed effects plus a same-country term. Receiver FE absorb
#   everything about a country's attractiveness as a source of nominees
#   (resources, community size, candidate quality) nonparametrically, per
#   prize and period; sender FE absorb propensity to nominate. exp(beta_same)
#   is the multiplicative same-country premium conditional on country-level
#   destination attractiveness (the country-period imprint of capacity
#   together with visibility, prestige, and access; see §4.5).
#
# Methodological decisions:
#   - Analytic record set MATCHES 11_formal_analysis.R line ~444: nominations.csv
#     filtered to non-missing nominator/nominee continent (record level, no
#     dedup). This reproduces the paper's N = 25,844 and H = 4.848 exactly.
#   - Structural zeros included: within each prize x decade stratum the risk
#     set is {senders with >=1 nomination sent} x {receivers with >=1 received}
#     in that stratum. Positive-only counts would bias gravity estimates.
#   - Poisson pseudo-ML (fixest::fepois) with SEs clustered by sender country.
#   - shared_lang includes same-country pairs; the same_country term separates
#     them, so exp(b_lang) is the cross-country shared-language premium.
#
# Interpretation note: the gravity premium is a different estimand from the
#   descriptive H (within-stratum, conditional on FE, zeros included), and is
#   expected to be larger. Conditioning on destination attractiveness
#   strengthens, not weakens, the finding.
#
# EXPECTED BENCHMARKS (provisional cross-check on identical data, pyfixest
# 0.60; your fixest run is canonical — small numerical drift is fine, large
# discrepancies are not):
#   dyad cells ~ 31,122; nominations = 25,844
#   bare:       exp(b_same) ~ 14.9
#   with lang:  exp(b_same) ~  6.7 ; exp(b_lang) ~ 2.6
#   by prize (with lang), exp(b_same):
#     Chemistry ~5.1  Literature ~19.4  Peace ~6.5  Physics ~5.2  Phys/Med ~7.3
#   (Gradient persists conditional on language -> language explains only part
#    of the Literature-Physics gap.)
#
# Inputs:  Data/intermediate/nominations.csv
# Outputs: Data/results_gravity_main.csv, Data/results_gravity_by_prize.csv
# Run from the repository root (NobelPrize.Rproj).
# =============================================================================

library(tidyverse)
library(fixest)     # one-time: install.packages("fixest")

int_path  <- function(f) file.path("Data", "intermediate", f)
data_path <- function(f) file.path("Data", f)

message("Loading nominations...")
noms <- read_csv(int_path("nominations.csv"), show_col_types = FALSE)

# Analytic record set — mirrors 11_formal_analysis.R (primary H specification)
d <- noms %>%
  filter(!is.na(nominator_continent), !is.na(nominee_continent)) %>%
  transmute(prize, year,
            s = nominator_country_modern,
            r = nominee_country_modern,
            decade = paste0(floor(year / 10) * 10, "s"))
message("Analytic records: ", nrow(d), "  (expected 25,844)")
if (nrow(d) != 25844) warning("Record count differs from paper's 25,844 — investigate before writing up.")

# ── Shared primary language (mechanism probe, R1-3) ─────────────────────────
# Principal language of the national scientific/literary community, coarse.
# Coarse country-level proxy (principal language of the national scientific/
# literary community); §S8 documents the limitation. Uncovered countries are
# printed below and contribute shared_lang = 0.
lang_map <- c(
  "United States"="en","United Kingdom"="en","Ireland"="en","Canada"="en",
  "Australia"="en","New Zealand"="en","India"="en",
  "Germany"="de","Austria"="de","Switzerland"="de",
  "France"="fr","Belgium"="fr",
  "Sweden"="sv","Norway"="no","Denmark"="da","Iceland"="is",
  "Netherlands"="nl","Italy"="it","Spain"="es","Portugal"="pt",
  "Mexico"="es","Argentina"="es","Chile"="es","Brazil"="pt",
  "Russia"="ru","Poland"="pl","Czech Republic"="cs","Czechia"="cs",
  "Hungary"="hu","Finland"="fi","Greece"="el","Turkey"="tr",
  "Japan"="ja","China"="zh","Egypt"="ar","Israel"="he",
  "Romania"="ro","Bulgaria"="bg"
)
uncovered <- d %>% count(country = s) %>% filter(!country %in% names(lang_map)) %>% arrange(desc(n))
message("Countries lacking a language code (top 10 by volume; extend lang_map as needed):")
print(head(uncovered, 10))

# ── Dyadic counts with structural zeros, per prize x decade ────────────────
message("Building dyadic grid...")
dyads <- d %>%
  group_by(prize, decade) %>%
  group_modify(function(g, key) {
    cnt <- g %>% count(s, r, name = "n")
    crossing(s = unique(g$s), r = unique(g$r)) %>%
      left_join(cnt, by = c("s", "r")) %>%
      mutate(n = replace_na(n, 0L))
  }) %>%
  ungroup() %>%
  mutate(
    same_country = as.integer(s == r),
    shared_lang  = as.integer(!is.na(lang_map[s]) & !is.na(lang_map[r]) &
                              lang_map[s] == lang_map[r]),
    fe_s = paste(s, prize, decade, sep = "_"),
    fe_r = paste(r, prize, decade, sep = "_")
  )
message(nrow(dyads), " dyad cells (expected ~31,122); ",
        sum(dyads$n), " nominations (expected 25,844)")

# ── Estimation ──────────────────────────────────────────────────────────────
message("Fitting gravity models...")
m_bare <- fepois(n ~ same_country               | fe_s + fe_r, data = dyads, cluster = ~s)
m_lang <- fepois(n ~ same_country + shared_lang | fe_s + fe_r, data = dyads, cluster = ~s)

etable(m_bare, m_lang,
       dict = c(same_country = "Same country", shared_lang = "Shared language"))
message(sprintf("Same-country premium, bare:          x%.2f", exp(coef(m_bare)["same_country"])))
message(sprintf("Same-country premium, cond. on lang: x%.2f  (shared-language premium x%.2f)",
                exp(coef(m_lang)["same_country"]), exp(coef(m_lang)["shared_lang"])))

tidy_fit <- function(m, model_label) {
  as.data.frame(coeftable(m)) %>%
    rownames_to_column("term") %>%
    rename(est = Estimate, se = `Std. Error`, z = `z value`, p = `Pr(>|z|)`) %>%
    mutate(premium = exp(est), model = model_label, .before = 1)
}
main_out <- bind_rows(tidy_fit(m_bare, "bare"), tidy_fit(m_lang, "with_language")) %>%
  mutate(n_cells = nrow(dyads), n_nominations = sum(dyads$n))
write_csv(main_out, data_path("results_gravity_main.csv"))

# ── By prize (decomposes the discipline gradient, R1-3) ────────────────────
by_prize <- dyads %>%
  group_split(prize) %>%
  map_dfr(function(g) {
    fit <- tryCatch(fepois(n ~ same_country + shared_lang | fe_s + fe_r,
                           data = g, cluster = ~s),
                    error = function(e) NULL)
    if (is.null(fit)) return(tibble(prize = g$prize[1], term = NA, note = "fit failed"))
    tidy_fit(fit, "with_language") %>% mutate(prize = g$prize[1], .before = 1)
  })
print(by_prize %>% filter(term == "same_country") %>%
        select(prize, premium, se, p) %>% arrange(desc(premium)))
write_csv(by_prize, data_path("results_gravity_by_prize.csv"))

# ── Robustness variants (run after main results look sane) ─────────────────
# 1. Deduplicated-edge counts instead of records:
#      d_dedup <- noms %>% filter(!is.na(nominator_person_id)) %>%
#        distinct(nominator_person_id, nominee_person_id, year, prize, .keep_all = TRUE) %>%
#        filter(!is.na(nominator_continent), !is.na(nominee_continent)) %>% ...
# 2. Prize x year strata instead of prize x decade (slower, many more FE).
# 3. Negative binomial: fenegbin(...) with same formula.
# 4. Possible extension: log distance covariate (CEPII), colonial-tie indicator.
# 5. Conditional-logit companion at the edge level: implemented in
#    18_clogit_prominence.R.

message("Done. -> results_gravity_main.csv, results_gravity_by_prize.csv")
# =============================================================================

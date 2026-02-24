# =============================================================================
# 10_exploratory_analysis.R
# Exploratory analysis of demographic composition, edge-level homophily,
# and temporal trends across the Nobel Prize multilayer network.
#
# This script produces a text report to identify the strongest empirical
# patterns before committing to specific hypotheses.
#
# Output: Data/exploratory_analysis_report.txt
# =============================================================================

library(tidyverse)

data_path <- function(f) file.path("Data", f)
int_path  <- function(f) file.path("Data", "intermediate", f)

# --- Report setup ---
report_file <- data_path("exploratory_analysis_report.txt")
report_con  <- file(report_file, open = "wt")

rpt <- function(...) {
  msg <- paste0(...)
  message(msg)
  writeLines(msg, report_con)
}

# =============================================================================
# LOAD ALL DATA
# =============================================================================

demo   <- read_csv(int_path("demographics.csv"), show_col_types = FALSE)
gov    <- read_csv(int_path("governing_bodies.csv"), show_col_types = FALSE)
vet    <- read_csv(int_path("vetting_bodies.csv"), show_col_types = FALSE)
noms   <- read_csv(int_path("nominations.csv"), show_col_types = FALSE)
nom_pq <- read_csv(int_path("nomination_people_qids.csv"), show_col_types = FALSE)
laur   <- read_csv(int_path("laureates.csv"), show_col_types = FALSE)
nodes  <- read_csv(data_path("nodes.csv"), show_col_types = FALSE)
edges  <- read_csv(data_path("edges.csv"), show_col_types = FALSE)

# --- Prize-to-governing/vetting body mappings ---
prize_gov <- tribble(
  ~prize,                  ~body,
  "Chemistry",             "RSAS",
  "Physics",               "RSAS",
  "Physiology/Medicine",   "Karolinska Institutet",
  "Literature",            "Swedish Academy",
  "Peace",                 "Storting"
)

prize_vet <- tribble(
  ~prize,                  ~body,
  "Chemistry",             "Nobel Committee for Chemistry",
  "Physics",               "Nobel Committee for Physics",
  "Physiology/Medicine",   "Nobel Committee for Physiology/Medicine",
  "Literature",            "Nobel Committee for Literature",
  "Peace",                 "Norwegian Nobel Committee"
)

# =============================================================================
# HELPER: compute demographic summary for a group
# =============================================================================

geo_summary <- function(df, continent_col = "birth_continent",
                        subregion_col = "birth_subregion",
                        gender_col = "gender") {
  out <- list()

  # Gender
  if (!is.null(gender_col) && gender_col %in% names(df)) {
    g <- df[[gender_col]]
    g_known <- g[!is.na(g)]
    n_total <- length(g_known)
    if (n_total > 0) {
      out$pct_female <- 100 * sum(g_known == "female" | g_known == "F") / n_total
      out$pct_male   <- 100 * sum(g_known == "male" | g_known == "M") / n_total
      out$n_gender_known <- n_total
    }
  }

  # Continent
  if (continent_col %in% names(df)) {
    c_vals <- df[[continent_col]]
    c_known <- c_vals[!is.na(c_vals)]
    n_c <- length(c_known)
    if (n_c > 0) {
      ct <- table(c_known)
      out$pct_europe <- 100 * sum(ct[names(ct) == "Europe"]) / n_c
      out$pct_americas <- 100 * sum(ct[names(ct) == "Americas"]) / n_c
      out$pct_asia <- 100 * sum(ct[names(ct) == "Asia"]) / n_c
      out$pct_africa <- 100 * sum(ct[names(ct) == "Africa"]) / n_c
      out$pct_oceania <- 100 * sum(ct[names(ct) == "Oceania"]) / n_c
      out$n_geo_known <- n_c
      # Shannon entropy (effective number of continents)
      props <- as.numeric(ct) / n_c
      out$entropy_continent <- exp(-sum(props * log(props)))
    }
  }

  # Subregion
  if (subregion_col %in% names(df)) {
    s_vals <- df[[subregion_col]]
    s_known <- s_vals[!is.na(s_vals)]
    n_s <- length(s_known)
    if (n_s > 0) {
      st <- table(s_known)
      props_s <- as.numeric(st) / n_s
      out$entropy_subregion <- exp(-sum(props_s * log(props_s)))
      out$n_subregions <- length(st)
      # Top 3 subregions
      st_sorted <- sort(st, decreasing = TRUE)
      top3 <- head(st_sorted, 3)
      out$top_subregions <- paste(
        sprintf("%s (%.1f%%)", names(top3), 100 * as.numeric(top3) / n_s),
        collapse = ", "
      )
    }
  }

  out
}


# =============================================================================
# SECTION 1: LAYER-BY-LAYER DEMOGRAPHIC COMPOSITION (OVERALL)
# =============================================================================
rpt("")
rpt(strrep("=", 78))
rpt("  SECTION 1: LAYER-BY-LAYER DEMOGRAPHIC COMPOSITION (OVERALL)")
rpt(strrep("=", 78))
rpt("")

# --- Governing bodies: join with demographics ---
gov_demo <- gov %>%
  select(-name) %>%
  inner_join(demo, by = "qid")

rpt("--- GOVERNING BODIES (all, joined with demographics) ---")
s <- geo_summary(gov_demo)
rpt(sprintf("  N = %d members (with demographics)", nrow(gov_demo)))
rpt(sprintf("  Gender known: %d | Female: %.1f%% | Male: %.1f%%",
            s$n_gender_known, s$pct_female, s$pct_male))
rpt(sprintf("  Geo known: %d | Europe: %.1f%% | Americas: %.1f%% | Asia: %.1f%%",
            s$n_geo_known, s$pct_europe, s$pct_americas, s$pct_asia))
rpt(sprintf("  Effective continents: %.2f | Effective subregions: %.2f",
            s$entropy_continent, s$entropy_subregion))
rpt(sprintf("  Top subregions: %s", s$top_subregions))
rpt("")

# --- Vetting bodies: join with demographics ---
vet_demo <- vet %>%
  filter(!is.na(qid)) %>%
  select(-name) %>%
  inner_join(demo, by = "qid")

rpt("--- VETTING BODIES (all, joined with demographics) ---")
s <- geo_summary(vet_demo)
rpt(sprintf("  N = %d members (with demographics)", nrow(vet_demo)))
rpt(sprintf("  Gender known: %d | Female: %.1f%% | Male: %.1f%%",
            s$n_gender_known, s$pct_female, s$pct_male))
rpt(sprintf("  Geo known: %d | Europe: %.1f%% | Americas: %.1f%% | Asia: %.1f%%",
            s$n_geo_known, s$pct_europe, s$pct_americas, s$pct_asia))
rpt(sprintf("  Effective continents: %.2f | Effective subregions: %.2f",
            s$entropy_continent, s$entropy_subregion))
rpt(sprintf("  Top subregions: %s", s$top_subregions))
rpt("")

# --- Nominators (from nominations.csv, using standardized geo) ---
# Get unique nominators per prize-year with their demographics
nominator_unique <- noms %>%
  filter(!is.na(nominator_person_id)) %>%
  distinct(nominator_person_id, .keep_all = TRUE)

# Join gender from nomination_people_qids
nominator_demo <- nominator_unique %>%
  left_join(nom_pq %>% select(person_id, gender),
            by = c("nominator_person_id" = "person_id"))

rpt("--- NOMINATORS (unique individuals from nominations.csv) ---")
s <- geo_summary(nominator_demo,
                 continent_col = "nominator_continent",
                 subregion_col = "nominator_subregion",
                 gender_col = "gender")
rpt(sprintf("  N = %d unique nominators", nrow(nominator_demo)))
rpt(sprintf("  Gender known: %d | Female: %.1f%% | Male: %.1f%%",
            s$n_gender_known, s$pct_female, s$pct_male))
rpt(sprintf("  Geo known: %d | Europe: %.1f%% | Americas: %.1f%% | Asia: %.1f%%",
            s$n_geo_known, s$pct_europe, s$pct_americas, s$pct_asia))
rpt(sprintf("  Effective continents: %.2f | Effective subregions: %.2f",
            s$entropy_continent, s$entropy_subregion))
rpt(sprintf("  Top subregions: %s", s$top_subregions))
rpt("")

# --- Nominees (from nominations.csv) ---
nominee_unique <- noms %>%
  filter(!is.na(nominee_person_id)) %>%
  distinct(nominee_person_id, .keep_all = TRUE)

nominee_demo <- nominee_unique %>%
  left_join(nom_pq %>% select(person_id, gender),
            by = c("nominee_person_id" = "person_id"))

rpt("--- NOMINEES (unique individuals from nominations.csv) ---")
s <- geo_summary(nominee_demo,
                 continent_col = "nominee_continent",
                 subregion_col = "nominee_subregion",
                 gender_col = "gender")
rpt(sprintf("  N = %d unique nominees", nrow(nominee_demo)))
rpt(sprintf("  Gender known: %d | Female: %.1f%% | Male: %.1f%%",
            s$n_gender_known, s$pct_female, s$pct_male))
rpt(sprintf("  Geo known: %d | Europe: %.1f%% | Americas: %.1f%% | Asia: %.1f%%",
            s$n_geo_known, s$pct_europe, s$pct_americas, s$pct_asia))
rpt(sprintf("  Effective continents: %.2f | Effective subregions: %.2f",
            s$entropy_continent, s$entropy_subregion))
rpt(sprintf("  Top subregions: %s", s$top_subregions))
rpt("")

# --- Laureates (within nomination period, through ~1975) ---
# Use demo gender (more complete); drop laur gender to avoid collision
laur_nom_era <- laur %>%
  filter(year <= 1975) %>%
  select(-gender) %>%
  inner_join(demo, by = "qid")

rpt("--- LAUREATES (1901-1975, joined with demographics) ---")
s <- geo_summary(laur_nom_era)
rpt(sprintf("  N = %d laureate-prize records", nrow(laur_nom_era)))
rpt(sprintf("  Gender known: %d | Female: %.1f%% | Male: %.1f%%",
            s$n_gender_known, s$pct_female, s$pct_male))
rpt(sprintf("  Geo known: %d | Europe: %.1f%% | Americas: %.1f%% | Asia: %.1f%%",
            s$n_geo_known, s$pct_europe, s$pct_americas, s$pct_asia))
rpt(sprintf("  Effective continents: %.2f | Effective subregions: %.2f",
            s$entropy_continent, s$entropy_subregion))
rpt(sprintf("  Top subregions: %s", s$top_subregions))
rpt("")

# --- Laureates (all time) ---
laur_all <- laur %>%
  select(-gender) %>%
  inner_join(demo, by = "qid")

rpt("--- LAUREATES (all time 1901-2025, joined with demographics) ---")
s <- geo_summary(laur_all)
rpt(sprintf("  N = %d laureate-prize records", nrow(laur_all)))
rpt(sprintf("  Gender known: %d | Female: %.1f%% | Male: %.1f%%",
            s$n_gender_known, s$pct_female, s$pct_male))
rpt(sprintf("  Geo known: %d | Europe: %.1f%% | Americas: %.1f%% | Asia: %.1f%%",
            s$n_geo_known, s$pct_europe, s$pct_americas, s$pct_asia))
rpt(sprintf("  Effective continents: %.2f | Effective subregions: %.2f",
            s$entropy_continent, s$entropy_subregion))
rpt(sprintf("  Top subregions: %s", s$top_subregions))
rpt("")


# =============================================================================
# SECTION 2: LAYER-BY-LAYER COMPOSITION BY PRIZE
# =============================================================================
rpt("")
rpt(strrep("=", 78))
rpt("  SECTION 2: LAYER-BY-LAYER COMPOSITION BY PRIZE")
rpt(strrep("=", 78))
rpt("")

for (p in c("Chemistry", "Physics", "Physiology/Medicine", "Literature", "Peace")) {
  rpt(sprintf("=== %s ===", toupper(p)))
  rpt("")

  # Governing body for this prize
  gb <- prize_gov %>% filter(prize == p) %>% pull(body)
  gov_p <- gov_demo %>% filter(body == gb)
  s <- geo_summary(gov_p)
  rpt(sprintf("  Governing (%s): N=%d | Female=%.1f%% | Europe=%.1f%% | EffCont=%.2f | EffSub=%.2f",
              gb, nrow(gov_p), s$pct_female, s$pct_europe,
              s$entropy_continent, s$entropy_subregion))

  # Vetting body for this prize
  vb <- prize_vet %>% filter(prize == p) %>% pull(body)
  vet_p <- vet_demo %>% filter(body == vb)
  s <- geo_summary(vet_p)
  rpt(sprintf("  Vetting (%s): N=%d | Female=%.1f%% | Europe=%.1f%% | EffCont=%.2f | EffSub=%.2f",
              vb, nrow(vet_p), s$pct_female, s$pct_europe,
              s$entropy_continent, s$entropy_subregion))

  # Nominators for this prize
  nom_p <- noms %>%
    filter(prize == p | (p == "Physiology/Medicine" & prize == "Medicine"),
           !is.na(nominator_person_id)) %>%
    distinct(nominator_person_id, .keep_all = TRUE) %>%
    left_join(nom_pq %>% select(person_id, gender),
              by = c("nominator_person_id" = "person_id"))
  s <- geo_summary(nom_p,
                   continent_col = "nominator_continent",
                   subregion_col = "nominator_subregion",
                   gender_col = "gender")
  rpt(sprintf("  Nominators: N=%d | Female=%.1f%% | Europe=%.1f%% | EffCont=%.2f | EffSub=%.2f",
              nrow(nom_p),
              ifelse(is.null(s$pct_female), NA, s$pct_female),
              ifelse(is.null(s$pct_europe), NA, s$pct_europe),
              ifelse(is.null(s$entropy_continent), NA, s$entropy_continent),
              ifelse(is.null(s$entropy_subregion), NA, s$entropy_subregion)))

  # Nominees for this prize
  nee_p <- noms %>%
    filter(prize == p | (p == "Physiology/Medicine" & prize == "Medicine"),
           !is.na(nominee_person_id)) %>%
    distinct(nominee_person_id, .keep_all = TRUE) %>%
    left_join(nom_pq %>% select(person_id, gender),
              by = c("nominee_person_id" = "person_id"))
  s <- geo_summary(nee_p,
                   continent_col = "nominee_continent",
                   subregion_col = "nominee_subregion",
                   gender_col = "gender")
  rpt(sprintf("  Nominees: N=%d | Female=%.1f%% | Europe=%.1f%% | EffCont=%.2f | EffSub=%.2f",
              nrow(nee_p),
              ifelse(is.null(s$pct_female), NA, s$pct_female),
              ifelse(is.null(s$pct_europe), NA, s$pct_europe),
              ifelse(is.null(s$entropy_continent), NA, s$entropy_continent),
              ifelse(is.null(s$entropy_subregion), NA, s$entropy_subregion)))

  # Laureates for this prize (nomination era)
  laur_p <- laur %>%
    filter(prize == p | (p == "Physiology/Medicine" & prize == "Medicine")) %>%
    filter(year <= 1975) %>%
    select(-any_of(c("gender", "name"))) %>%
    inner_join(demo, by = "qid")
  s <- geo_summary(laur_p)
  rpt(sprintf("  Laureates (≤1975): N=%d | Female=%.1f%% | Europe=%.1f%% | EffCont=%.2f | EffSub=%.2f",
              nrow(laur_p),
              ifelse(is.null(s$pct_female), NA, s$pct_female),
              ifelse(is.null(s$pct_europe), NA, s$pct_europe),
              ifelse(is.null(s$entropy_continent), NA, s$entropy_continent),
              ifelse(is.null(s$entropy_subregion), NA, s$entropy_subregion)))

  rpt("")
}


# =============================================================================
# SECTION 3: EDGE-LEVEL GEOGRAPHIC HOMOPHILY
# =============================================================================
rpt("")
rpt(strrep("=", 78))
rpt("  SECTION 3: EDGE-LEVEL GEOGRAPHIC HOMOPHILY")
rpt(strrep("=", 78))
rpt("")
rpt("For each edge type, we measure the fraction of edges where both endpoints")
rpt("share the same continent or subregion. We compare to a null model: the")
rpt("expected match rate if edges were rewired randomly preserving layer sizes.")
rpt("")

# We need geographic info for edge endpoints.
# For edges involving QIDs: join with nodes.csv
# For nominator->nominee edges: use nominations.csv directly

# --- 3a. Nominator -> Nominee edges (richest data) ---
rpt("--- NOMINATOR → NOMINEE (from nominations.csv directly) ---")
rpt("")

nom_geo <- noms %>%
  filter(!is.na(nominator_continent), !is.na(nominee_continent))

n_total <- nrow(nom_geo)
n_same_continent <- sum(nom_geo$nominator_continent == nom_geo$nominee_continent)
n_same_subregion <- sum(nom_geo$nominator_subregion == nom_geo$nominee_subregion, na.rm = TRUE)
n_same_country   <- sum(nom_geo$nominator_country_modern == nom_geo$nominee_country_modern, na.rm = TRUE)

rpt(sprintf("  Total edges with geo for both endpoints: %s", format(n_total, big.mark = ",")))
rpt(sprintf("  Same continent: %s / %s (%.1f%%)",
            format(n_same_continent, big.mark = ","), format(n_total, big.mark = ","),
            100 * n_same_continent / n_total))
rpt(sprintf("  Same subregion: %s / %s (%.1f%%)",
            format(n_same_subregion, big.mark = ","), format(n_total, big.mark = ","),
            100 * n_same_subregion / n_total))
rpt(sprintf("  Same country:   %s / %s (%.1f%%)",
            format(n_same_country, big.mark = ","), format(n_total, big.mark = ","),
            100 * n_same_country / n_total))

# Null model: expected same-continent rate under random pairing
nominator_cont_dist <- table(nom_geo$nominator_continent) / n_total
nominee_cont_dist   <- table(nom_geo$nominee_continent) / n_total
# Expected same-continent rate = sum of products of marginal proportions
shared_continents <- intersect(names(nominator_cont_dist), names(nominee_cont_dist))
expected_same_cont <- sum(nominator_cont_dist[shared_continents] * nominee_cont_dist[shared_continents])
rpt(sprintf("  Expected same continent (null): %.1f%%", 100 * expected_same_cont))
rpt(sprintf("  Homophily ratio (observed/expected): %.2f",
            (n_same_continent / n_total) / expected_same_cont))

# Same for subregion
nominator_sub_dist <- table(nom_geo$nominator_subregion) / n_total
nominee_sub_dist   <- table(nom_geo$nominee_subregion) / n_total
shared_subs <- intersect(names(nominator_sub_dist), names(nominee_sub_dist))
expected_same_sub <- sum(nominator_sub_dist[shared_subs] * nominee_sub_dist[shared_subs])
rpt(sprintf("  Expected same subregion (null): %.1f%%", 100 * expected_same_sub))
rpt(sprintf("  Homophily ratio (observed/expected): %.2f",
            (n_same_subregion / n_total) / expected_same_sub))

# Same for country
nominator_cty_dist <- table(nom_geo$nominator_country_modern) / n_total
nominee_cty_dist   <- table(nom_geo$nominee_country_modern) / n_total
shared_ctys <- intersect(names(nominator_cty_dist), names(nominee_cty_dist))
expected_same_cty <- sum(nominator_cty_dist[shared_ctys] * nominee_cty_dist[shared_ctys])
rpt(sprintf("  Expected same country (null): %.1f%%", 100 * expected_same_cty))
rpt(sprintf("  Homophily ratio (observed/expected): %.2f",
            (n_same_country / n_total) / expected_same_cty))
rpt("")

# --- 3b. Nominator -> Nominee homophily BY PRIZE ---
rpt("--- NOMINATOR → NOMINEE HOMOPHILY BY PRIZE ---")
rpt("")

for (p in c("Chemistry", "Physics", "Physiology/Medicine", "Literature", "Peace")) {
  nom_p <- nom_geo %>%
    filter(prize == p | (p == "Physiology/Medicine" & prize == "Medicine"))
  n_p <- nrow(nom_p)
  if (n_p == 0) next

  same_cont <- sum(nom_p$nominator_continent == nom_p$nominee_continent)
  same_sub  <- sum(nom_p$nominator_subregion == nom_p$nominee_subregion, na.rm = TRUE)
  same_cty  <- sum(nom_p$nominator_country_modern == nom_p$nominee_country_modern, na.rm = TRUE)

  # Null models per prize
  nr_cont <- table(nom_p$nominator_continent) / n_p
  ne_cont <- table(nom_p$nominee_continent) / n_p
  sc <- intersect(names(nr_cont), names(ne_cont))
  exp_cont <- sum(nr_cont[sc] * ne_cont[sc])

  nr_cty <- table(nom_p$nominator_country_modern) / n_p
  ne_cty <- table(nom_p$nominee_country_modern) / n_p
  sc2 <- intersect(names(nr_cty), names(ne_cty))
  exp_cty <- sum(nr_cty[sc2] * ne_cty[sc2])

  rpt(sprintf("  %s (N=%s):", p, format(n_p, big.mark = ",")))
  rpt(sprintf("    Same continent: %.1f%% (expected %.1f%%, ratio %.2f)",
              100 * same_cont / n_p, 100 * exp_cont,
              (same_cont / n_p) / exp_cont))
  rpt(sprintf("    Same country:   %.1f%% (expected %.1f%%, ratio %.2f)",
              100 * same_cty / n_p, 100 * exp_cty,
              (same_cty / n_p) / exp_cty))
}
rpt("")

# --- 3c. Other edge types (using QID joins to nodes) ---
rpt("--- OTHER EDGE TYPES (QID-based, joined with nodes.csv) ---")
rpt("")

# Join edges with node geo for from and to
edges_geo <- edges %>%
  filter(!str_starts(from_qid, "NOM:"), !str_starts(to_qid, "NOM:")) %>%
  inner_join(nodes %>% select(qid, birth_continent, birth_subregion, birth_country_modern),
             by = c("from_qid" = "qid")) %>%
  rename(from_continent = birth_continent, from_subregion = birth_subregion,
         from_country = birth_country_modern) %>%
  inner_join(nodes %>% select(qid, birth_continent, birth_subregion, birth_country_modern),
             by = c("to_qid" = "qid")) %>%
  rename(to_continent = birth_continent, to_subregion = birth_subregion,
         to_country = birth_country_modern)

edge_types <- edges_geo %>%
  mutate(edge_type = paste0(from_layer, " → ", to_layer)) %>%
  filter(!is.na(from_continent), !is.na(to_continent))

for (et in unique(edge_types$edge_type)) {
  e_sub <- edge_types %>% filter(edge_type == et)
  n_e <- nrow(e_sub)
  if (n_e < 100) next

  same_cont <- sum(e_sub$from_continent == e_sub$to_continent)
  same_sub  <- sum(e_sub$from_subregion == e_sub$to_subregion, na.rm = TRUE)
  same_cty  <- sum(e_sub$from_country == e_sub$to_country, na.rm = TRUE)

  # Null model
  from_cont_dist <- table(e_sub$from_continent) / n_e
  to_cont_dist   <- table(e_sub$to_continent) / n_e
  sc <- intersect(names(from_cont_dist), names(to_cont_dist))
  exp_cont <- sum(from_cont_dist[sc] * to_cont_dist[sc])

  from_cty_dist <- table(e_sub$from_country) / n_e
  to_cty_dist   <- table(e_sub$to_country) / n_e
  sc2 <- intersect(names(from_cty_dist), names(to_cty_dist))
  exp_cty <- sum(from_cty_dist[sc2] * to_cty_dist[sc2])

  rpt(sprintf("  %s (N=%s):", et, format(n_e, big.mark = ",")))
  rpt(sprintf("    Same continent: %.1f%% (expected %.1f%%, ratio %.2f)",
              100 * same_cont / n_e, 100 * exp_cont,
              (same_cont / n_e) / exp_cont))
  rpt(sprintf("    Same country:   %.1f%% (expected %.1f%%, ratio %.2f)",
              100 * same_cty / n_e, 100 * exp_cty,
              (same_cty / n_e) / exp_cty))
}
rpt("")


# =============================================================================
# SECTION 4: TEMPORAL TRENDS (BY DECADE)
# =============================================================================
rpt("")
rpt(strrep("=", 78))
rpt("  SECTION 4: TEMPORAL TRENDS BY DECADE")
rpt(strrep("=", 78))
rpt("")

# --- 4a. Nomination-layer geographic diversity over time ---
rpt("--- NOMINEE GEOGRAPHIC DIVERSITY BY DECADE ---")
rpt("")

nominee_by_decade <- noms %>%
  filter(!is.na(nominee_continent)) %>%
  mutate(decade = 10 * (year %/% 10)) %>%
  group_by(decade)

decade_stats <- nominee_by_decade %>%
  summarise(
    n = n(),
    n_europe = sum(nominee_continent == "Europe"),
    pct_europe = 100 * n_europe / n,
    n_continents = n_distinct(nominee_continent),
    # Effective continents
    eff_cont = {
      ct <- table(nominee_continent)
      props <- as.numeric(ct) / sum(ct)
      exp(-sum(props * log(props)))
    },
    n_subregions = n_distinct(nominee_subregion),
    eff_sub = {
      st <- table(nominee_subregion)
      props <- as.numeric(st) / sum(st)
      exp(-sum(props * log(props)))
    },
    .groups = "drop"
  )

for (i in seq_len(nrow(decade_stats))) {
  r <- decade_stats[i, ]
  rpt(sprintf("  %ds: N=%s | Europe=%.1f%% | EffCont=%.2f | EffSub=%.2f | Subregions=%d",
              r$decade, format(r$n, big.mark = ","), r$pct_europe,
              r$eff_cont, r$eff_sub, r$n_subregions))
}
rpt("")

# --- 4b. Nominator geographic diversity by decade ---
rpt("--- NOMINATOR GEOGRAPHIC DIVERSITY BY DECADE ---")
rpt("")

nominator_by_decade <- noms %>%
  filter(!is.na(nominator_continent)) %>%
  mutate(decade = 10 * (year %/% 10)) %>%
  group_by(decade)

decade_stats_nr <- nominator_by_decade %>%
  summarise(
    n = n(),
    pct_europe = 100 * sum(nominator_continent == "Europe") / n,
    eff_cont = {
      ct <- table(nominator_continent)
      props <- as.numeric(ct) / sum(ct)
      exp(-sum(props * log(props)))
    },
    eff_sub = {
      st <- table(nominator_subregion)
      props <- as.numeric(st) / sum(st)
      exp(-sum(props * log(props)))
    },
    .groups = "drop"
  )

for (i in seq_len(nrow(decade_stats_nr))) {
  r <- decade_stats_nr[i, ]
  rpt(sprintf("  %ds: N=%s | Europe=%.1f%% | EffCont=%.2f | EffSub=%.2f",
              r$decade, format(r$n, big.mark = ","), r$pct_europe,
              r$eff_cont, r$eff_sub))
}
rpt("")

# --- 4c. Laureate geographic diversity by decade ---
rpt("--- LAUREATE GEOGRAPHIC DIVERSITY BY DECADE ---")
rpt("")

laur_decade <- laur %>%
  select(-any_of(c("gender", "name"))) %>%
    inner_join(demo, by = "qid") %>%
  filter(!is.na(birth_continent)) %>%
  mutate(decade = 10 * (year %/% 10)) %>%
  group_by(decade) %>%
  summarise(
    n = n(),
    pct_europe = 100 * sum(birth_continent == "Europe") / n,
    pct_female = 100 * sum(gender == "female", na.rm = TRUE) / sum(!is.na(gender)),
    eff_cont = {
      ct <- table(birth_continent)
      props <- as.numeric(ct) / sum(ct)
      exp(-sum(props * log(props)))
    },
    .groups = "drop"
  )

for (i in seq_len(nrow(laur_decade))) {
  r <- laur_decade[i, ]
  rpt(sprintf("  %ds: N=%d | Europe=%.1f%% | Female=%.1f%% | EffCont=%.2f",
              r$decade, r$n, r$pct_europe, r$pct_female, r$eff_cont))
}
rpt("")


# --- 4d. Homophily trends by decade ---
rpt("--- NOMINATION HOMOPHILY TRENDS BY DECADE ---")
rpt("")

homophily_decade <- noms %>%
  filter(!is.na(nominator_continent), !is.na(nominee_continent)) %>%
  mutate(decade = 10 * (year %/% 10)) %>%
  group_by(decade) %>%
  summarise(
    n = n(),
    same_continent = sum(nominator_continent == nominee_continent),
    pct_same_cont = 100 * same_continent / n,
    same_country = sum(nominator_country_modern == nominee_country_modern, na.rm = TRUE),
    pct_same_cty = 100 * same_country / n,
    # Null model for same continent
    exp_same_cont = {
      nr <- table(nominator_continent) / n()
      ne <- table(nominee_continent) / n()
      sc <- intersect(names(nr), names(ne))
      100 * sum(nr[sc] * ne[sc])
    },
    .groups = "drop"
  ) %>%
  mutate(homophily_ratio_cont = pct_same_cont / exp_same_cont)

for (i in seq_len(nrow(homophily_decade))) {
  r <- homophily_decade[i, ]
  rpt(sprintf("  %ds: N=%s | SameCont=%.1f%% (exp=%.1f%%, ratio=%.2f) | SameCty=%.1f%%",
              r$decade, format(r$n, big.mark = ","),
              r$pct_same_cont, r$exp_same_cont, r$homophily_ratio_cont,
              r$pct_same_cty))
}
rpt("")


# =============================================================================
# SECTION 5: CROSS-PRIZE COMPARISON — OPEN vs INVITATION-ONLY NOMINATIONS
# =============================================================================
rpt("")
rpt(strrep("=", 78))
rpt("  SECTION 5: OPEN vs INVITATION-ONLY NOMINATION PROCESSES")
rpt(strrep("=", 78))
rpt("")
rpt("Invitation-only: Chemistry, Physics, Physiology/Medicine")
rpt("Open (anyone eligible can nominate): Literature, Peace")
rpt("")

invite_noms <- noms %>%
  filter(prize %in% c("Chemistry", "Physics", "Medicine"),
         !is.na(nominee_continent))
open_noms <- noms %>%
  filter(prize %in% c("Literature", "Peace"),
         !is.na(nominee_continent))

rpt("--- INVITATION-ONLY PRIZES (nominees) ---")
s <- geo_summary(invite_noms, continent_col = "nominee_continent",
                 subregion_col = "nominee_subregion", gender_col = NULL)
rpt(sprintf("  N = %s nomination records", format(nrow(invite_noms), big.mark = ",")))
rpt(sprintf("  Europe: %.1f%% | Americas: %.1f%% | Asia: %.1f%%",
            s$pct_europe, s$pct_americas, s$pct_asia))
rpt(sprintf("  Effective continents: %.2f | Effective subregions: %.2f",
            s$entropy_continent, s$entropy_subregion))
rpt(sprintf("  Top subregions: %s", s$top_subregions))
rpt("")

rpt("--- OPEN-NOMINATION PRIZES (nominees) ---")
s <- geo_summary(open_noms, continent_col = "nominee_continent",
                 subregion_col = "nominee_subregion", gender_col = NULL)
rpt(sprintf("  N = %s nomination records", format(nrow(open_noms), big.mark = ",")))
rpt(sprintf("  Europe: %.1f%% | Americas: %.1f%% | Asia: %.1f%%",
            s$pct_europe, s$pct_americas, s$pct_asia))
rpt(sprintf("  Effective continents: %.2f | Effective subregions: %.2f",
            s$entropy_continent, s$entropy_subregion))
rpt(sprintf("  Top subregions: %s", s$top_subregions))
rpt("")

# Homophily comparison
rpt("--- HOMOPHILY: INVITATION vs OPEN ---")
for (label in c("Invitation-only", "Open")) {
  sub <- if (label == "Invitation-only") {
    noms %>% filter(prize %in% c("Chemistry", "Physics", "Medicine"),
                    !is.na(nominator_continent), !is.na(nominee_continent))
  } else {
    noms %>% filter(prize %in% c("Literature", "Peace"),
                    !is.na(nominator_continent), !is.na(nominee_continent))
  }
  n_s <- nrow(sub)
  same_cont <- sum(sub$nominator_continent == sub$nominee_continent)
  same_cty  <- sum(sub$nominator_country_modern == sub$nominee_country_modern, na.rm = TRUE)

  nr <- table(sub$nominator_continent) / n_s
  ne <- table(sub$nominee_continent) / n_s
  sc <- intersect(names(nr), names(ne))
  exp_cont <- sum(nr[sc] * ne[sc])

  rpt(sprintf("  %s (N=%s):", label, format(n_s, big.mark = ",")))
  rpt(sprintf("    Same continent: %.1f%% (expected %.1f%%, ratio %.2f)",
              100 * same_cont / n_s, 100 * exp_cont,
              (same_cont / n_s) / exp_cont))
  rpt(sprintf("    Same country:   %.1f%%", 100 * same_cty / n_s))
}
rpt("")


# =============================================================================
# SECTION 6: GENDER ANALYSIS
# =============================================================================
rpt("")
rpt(strrep("=", 78))
rpt("  SECTION 6: GENDER ACROSS LAYERS AND PRIZES")
rpt(strrep("=", 78))
rpt("")

# --- Gender by layer (overall) ---
rpt("--- GENDER BY LAYER (OVERALL % FEMALE) ---")
rpt("")

# Governing
gov_gender <- gov_demo %>% filter(!is.na(gender))
rpt(sprintf("  Governing bodies: %.1f%% female (%d / %d known)",
            100 * sum(gov_gender$gender == "female") / nrow(gov_gender),
            sum(gov_gender$gender == "female"), nrow(gov_gender)))

# Vetting
vet_gender <- vet_demo %>% filter(!is.na(gender))
rpt(sprintf("  Vetting bodies:   %.1f%% female (%d / %d known)",
            100 * sum(vet_gender$gender == "female") / nrow(vet_gender),
            sum(vet_gender$gender == "female"), nrow(vet_gender)))

# Nominators (from nom_pq)
nominator_ids <- noms %>%
  filter(!is.na(nominator_person_id)) %>%
  distinct(nominator_person_id) %>%
  left_join(nom_pq %>% select(person_id, gender),
            by = c("nominator_person_id" = "person_id")) %>%
  filter(!is.na(gender))
rpt(sprintf("  Nominators:       %.1f%% female (%d / %d known)",
            100 * sum(nominator_ids$gender == "F") / nrow(nominator_ids),
            sum(nominator_ids$gender == "F"), nrow(nominator_ids)))

# Nominees
nominee_ids <- noms %>%
  filter(!is.na(nominee_person_id)) %>%
  distinct(nominee_person_id) %>%
  left_join(nom_pq %>% select(person_id, gender),
            by = c("nominee_person_id" = "person_id")) %>%
  filter(!is.na(gender))
rpt(sprintf("  Nominees:         %.1f%% female (%d / %d known)",
            100 * sum(nominee_ids$gender == "F") / nrow(nominee_ids),
            sum(nominee_ids$gender == "F"), nrow(nominee_ids)))

# Laureates (nomination era)
laur_gender <- laur %>%
  filter(year <= 1975) %>%
  select(-any_of(c("gender", "name"))) %>%
    inner_join(demo, by = "qid") %>%
  filter(!is.na(gender))
rpt(sprintf("  Laureates (≤1975): %.1f%% female (%d / %d known)",
            100 * sum(laur_gender$gender == "female") / nrow(laur_gender),
            sum(laur_gender$gender == "female"), nrow(laur_gender)))
rpt("")

# --- Gender by layer by prize ---
rpt("--- GENDER BY LAYER BY PRIZE (% FEMALE) ---")
rpt("")
rpt(sprintf("  %-25s %-12s %-12s %-12s %-12s %-12s",
            "Prize", "Governing", "Vetting", "Nominators", "Nominees", "Laureates"))

for (p in c("Chemistry", "Physics", "Physiology/Medicine", "Literature", "Peace")) {
  # Governing
  gb <- prize_gov %>% filter(prize == p) %>% pull(body)
  gov_g <- gov_demo %>% filter(body == gb, !is.na(gender))
  pct_gov <- if (nrow(gov_g) > 0) 100 * sum(gov_g$gender == "female") / nrow(gov_g) else NA

  # Vetting
  vb <- prize_vet %>% filter(prize == p) %>% pull(body)
  vet_g <- vet_demo %>% filter(body == vb, !is.na(gender))
  pct_vet <- if (nrow(vet_g) > 0) 100 * sum(vet_g$gender == "female") / nrow(vet_g) else NA

  # Nominators
  nom_g <- noms %>%
    filter(prize == p | (p == "Physiology/Medicine" & prize == "Medicine")) %>%
    filter(!is.na(nominator_person_id)) %>%
    distinct(nominator_person_id) %>%
    left_join(nom_pq %>% select(person_id, gender),
              by = c("nominator_person_id" = "person_id")) %>%
    filter(!is.na(gender))
  pct_nom <- if (nrow(nom_g) > 0) 100 * sum(nom_g$gender == "F") / nrow(nom_g) else NA

  # Nominees
  nee_g <- noms %>%
    filter(prize == p | (p == "Physiology/Medicine" & prize == "Medicine")) %>%
    filter(!is.na(nominee_person_id)) %>%
    distinct(nominee_person_id) %>%
    left_join(nom_pq %>% select(person_id, gender),
              by = c("nominee_person_id" = "person_id")) %>%
    filter(!is.na(gender))
  pct_nee <- if (nrow(nee_g) > 0) 100 * sum(nee_g$gender == "F") / nrow(nee_g) else NA

  # Laureates
  laur_g <- laur %>%
    filter(prize == p | (p == "Physiology/Medicine" & prize == "Medicine")) %>%
    filter(year <= 1975) %>%
    select(-any_of(c("gender", "name"))) %>%
    inner_join(demo, by = "qid") %>%
    filter(!is.na(gender))
  pct_laur <- if (nrow(laur_g) > 0) 100 * sum(laur_g$gender == "female") / nrow(laur_g) else NA

  rpt(sprintf("  %-25s %-12s %-12s %-12s %-12s %-12s",
              p,
              ifelse(is.na(pct_gov), "N/A", sprintf("%.1f%%", pct_gov)),
              ifelse(is.na(pct_vet), "N/A", sprintf("%.1f%%", pct_vet)),
              ifelse(is.na(pct_nom), "N/A", sprintf("%.1f%%", pct_nom)),
              ifelse(is.na(pct_nee), "N/A", sprintf("%.1f%%", pct_nee)),
              ifelse(is.na(pct_laur), "N/A", sprintf("%.1f%%", pct_laur))))
}
rpt("")


# =============================================================================
# SECTION 7: DEGREE CENTRALITY AND DEMOGRAPHICS
# =============================================================================
rpt("")
rpt(strrep("=", 78))
rpt("  SECTION 7: CENTRALITY AND DEMOGRAPHICS")
rpt(strrep("=", 78))
rpt("")

# In-degree for nominees (number of times nominated, as edges)
nominee_edges <- edges %>%
  filter(to_layer == "nominee", !str_starts(to_qid, "NOM:"))

nominee_indegree <- nominee_edges %>%
  count(to_qid, name = "in_degree") %>%
  inner_join(nodes %>% select(qid, gender, birth_continent, birth_subregion),
             by = c("to_qid" = "qid"))

# Is the nominee a laureate?
laur_qids <- laur %>% distinct(qid) %>% pull(qid)
nominee_indegree <- nominee_indegree %>%
  mutate(is_laureate = to_qid %in% laur_qids)

rpt("--- NOMINEE IN-DEGREE BY LAUREATE STATUS ---")
rpt("")
ni_summary <- nominee_indegree %>%
  group_by(is_laureate) %>%
  summarise(
    n = n(),
    mean_degree = mean(in_degree),
    median_degree = median(in_degree),
    max_degree = max(in_degree),
    .groups = "drop"
  )
for (i in seq_len(nrow(ni_summary))) {
  r <- ni_summary[i, ]
  rpt(sprintf("  Laureate=%s: N=%d | Mean degree=%.1f | Median=%d | Max=%d",
              r$is_laureate, r$n, r$mean_degree, r$median_degree, r$max_degree))
}
rpt("")

rpt("--- NOMINEE IN-DEGREE BY CONTINENT ---")
rpt("")
ni_geo <- nominee_indegree %>%
  filter(!is.na(birth_continent)) %>%
  group_by(birth_continent) %>%
  summarise(
    n = n(),
    mean_degree = mean(in_degree),
    median_degree = median(in_degree),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_degree))
for (i in seq_len(nrow(ni_geo))) {
  r <- ni_geo[i, ]
  rpt(sprintf("  %-12s: N=%d | Mean degree=%.1f | Median=%d",
              r$birth_continent, r$n, r$mean_degree, r$median_degree))
}
rpt("")


# =============================================================================
# SECTION 8: SUBREGION-TO-SUBREGION NOMINATION FLOW MATRIX
# =============================================================================
rpt("")
rpt(strrep("=", 78))
rpt("  SECTION 8: SUBREGION-TO-SUBREGION NOMINATION FLOWS")
rpt(strrep("=", 78))
rpt("")

nom_flow <- noms %>%
  filter(!is.na(nominator_subregion), !is.na(nominee_subregion)) %>%
  count(nominator_subregion, nominee_subregion, name = "n_noms")

# --- 8a. Overall flow matrix ---
rpt("--- OVERALL FLOW MATRIX (top 30 flows) ---")
rpt("")
rpt(sprintf("  %-28s → %-28s  %7s  %5s", "Nominator subregion", "Nominee subregion", "Count", "Pct"))
rpt(sprintf("  %s", strrep("-", 78)))

total_flow <- sum(nom_flow$n_noms)
top_flows <- nom_flow %>% arrange(desc(n_noms)) %>% head(30)
for (i in seq_len(nrow(top_flows))) {
  r <- top_flows[i, ]
  rpt(sprintf("  %-28s → %-28s  %7s  %4.1f%%",
              r$nominator_subregion, r$nominee_subregion,
              format(r$n_noms, big.mark = ","),
              100 * r$n_noms / total_flow))
}
rpt("")

# --- 8b. Self-nomination rate by subregion ---
rpt("--- SELF-NOMINATION RATE BY SUBREGION ---")
rpt("(Fraction of nominations from a subregion that go to nominees in the same subregion)")
rpt("")

self_nom_rate <- nom_flow %>%
  group_by(nominator_subregion) %>%
  summarise(
    total_out = sum(n_noms),
    self_noms = sum(n_noms[nominee_subregion == nominator_subregion]),
    self_rate = self_noms / total_out,
    .groups = "drop"
  ) %>%
  arrange(desc(self_rate))

rpt(sprintf("  %-28s  %7s  %7s  %6s", "Subregion", "Total", "Self", "Rate"))
rpt(sprintf("  %s", strrep("-", 55)))
for (i in seq_len(nrow(self_nom_rate))) {
  r <- self_nom_rate[i, ]
  rpt(sprintf("  %-28s  %7s  %7s  %5.1f%%",
              r$nominator_subregion,
              format(r$total_out, big.mark = ","),
              format(r$self_noms, big.mark = ","),
              100 * r$self_rate))
}
rpt("")

# --- 8c. Self-nomination rate by subregion BY PRIZE ---
rpt("--- SELF-NOMINATION RATE BY SUBREGION × PRIZE ---")
rpt("(Top subregions only: those with ≥100 nominations in that prize)")
rpt("")

nom_flow_prize <- noms %>%
  filter(!is.na(nominator_subregion), !is.na(nominee_subregion)) %>%
  mutate(prize_clean = if_else(prize == "Medicine", "Physiology/Medicine", prize)) %>%
  count(prize_clean, nominator_subregion, nominee_subregion, name = "n_noms")

for (p in c("Chemistry", "Physics", "Physiology/Medicine", "Literature", "Peace")) {
  rpt(sprintf("  === %s ===", p))
  pf <- nom_flow_prize %>% filter(prize_clean == p)
  sr <- pf %>%
    group_by(nominator_subregion) %>%
    summarise(
      total_out = sum(n_noms),
      self_noms = sum(n_noms[nominee_subregion == nominator_subregion]),
      self_rate = self_noms / total_out,
      .groups = "drop"
    ) %>%
    filter(total_out >= 100) %>%
    arrange(desc(self_rate))
  for (i in seq_len(nrow(sr))) {
    r <- sr[i, ]
    rpt(sprintf("    %-25s  N=%5s  self=%.1f%%",
                r$nominator_subregion, format(r$total_out, big.mark = ","),
                100 * r$self_rate))
  }
  rpt("")
}


# =============================================================================
# SECTION 9: NOMINATION EQUITY (BALANCE OF FLOWS)
# =============================================================================
rpt("")
rpt(strrep("=", 78))
rpt("  SECTION 9: NOMINATION EQUITY (OUTGOING vs INCOMING)")
rpt(strrep("=", 78))
rpt("")
rpt("For each subregion: how many nominations does it SEND vs RECEIVE?")
rpt("Return ratio > 1 means region receives more than it sends.")
rpt("")

# Outgoing: count of nominations from each subregion
outgoing <- noms %>%
  filter(!is.na(nominator_subregion)) %>%
  count(nominator_subregion, name = "sent")

# Incoming: count of nominations to each subregion
incoming <- noms %>%
  filter(!is.na(nominee_subregion)) %>%
  count(nominee_subregion, name = "received")

equity <- full_join(outgoing, incoming,
                    by = c("nominator_subregion" = "nominee_subregion")) %>%
  rename(subregion = nominator_subregion) %>%
  replace_na(list(sent = 0, received = 0)) %>%
  mutate(
    net = received - sent,
    return_ratio = received / pmax(sent, 1)
  ) %>%
  arrange(desc(return_ratio))

rpt(sprintf("  %-28s  %7s  %7s  %7s  %6s",
            "Subregion", "Sent", "Received", "Net", "Ratio"))
rpt(sprintf("  %s", strrep("-", 65)))
for (i in seq_len(nrow(equity))) {
  r <- equity[i, ]
  rpt(sprintf("  %-28s  %7s  %7s  %+7s  %5.2f",
              r$subregion,
              format(r$sent, big.mark = ","),
              format(r$received, big.mark = ","),
              format(r$net, big.mark = ","),
              r$return_ratio))
}
rpt("")

# --- Equity by prize ---
rpt("--- NOMINATION EQUITY BY PRIZE (subregions with ≥50 nominations sent or received) ---")
rpt("")

for (p in c("Chemistry", "Physics", "Physiology/Medicine", "Literature", "Peace")) {
  rpt(sprintf("  === %s ===", p))
  noms_p <- noms %>%
    mutate(prize_clean = if_else(prize == "Medicine", "Physiology/Medicine", prize)) %>%
    filter(prize_clean == p)

  out_p <- noms_p %>%
    filter(!is.na(nominator_subregion)) %>%
    count(nominator_subregion, name = "sent")
  in_p <- noms_p %>%
    filter(!is.na(nominee_subregion)) %>%
    count(nominee_subregion, name = "received")

  eq_p <- full_join(out_p, in_p,
                    by = c("nominator_subregion" = "nominee_subregion")) %>%
    rename(subregion = nominator_subregion) %>%
    replace_na(list(sent = 0, received = 0)) %>%
    filter(sent >= 50 | received >= 50) %>%
    mutate(
      net = received - sent,
      return_ratio = received / pmax(sent, 1)
    ) %>%
    arrange(desc(return_ratio))

  for (j in seq_len(nrow(eq_p))) {
    r <- eq_p[j, ]
    rpt(sprintf("    %-25s  sent=%5s  recv=%5s  net=%+5d  ratio=%.2f",
                r$subregion,
                format(r$sent, big.mark = ","),
                format(r$received, big.mark = ","),
                r$net, r$return_ratio))
  }
  rpt("")
}


# =============================================================================
# SECTION 10: TOP NON-SELF NOMINATION TARGETS BY SUBREGION
# =============================================================================
rpt("")
rpt(strrep("=", 78))
rpt("  SECTION 10: TOP NON-SELF NOMINATION TARGETS")
rpt(strrep("=", 78))
rpt("")
rpt("When a subregion nominates OUTSIDE itself, where do nominations go?")
rpt("")

cross_noms <- nom_flow %>%
  filter(nominator_subregion != nominee_subregion) %>%
  group_by(nominator_subregion) %>%
  mutate(
    total_cross = sum(n_noms),
    pct = 100 * n_noms / total_cross
  ) %>%
  slice_max(pct, n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(nominator_subregion, desc(pct))

current_region <- ""
for (i in seq_len(nrow(cross_noms))) {
  r <- cross_noms[i, ]
  if (r$nominator_subregion != current_region) {
    current_region <- r$nominator_subregion
    rpt(sprintf("  %s (N=%s cross-regional nominations):",
                current_region, format(r$total_cross, big.mark = ",")))
  }
  rpt(sprintf("      → %-25s  %5s  (%4.1f%%)",
              r$nominee_subregion,
              format(r$n_noms, big.mark = ","),
              r$pct))
}
rpt("")


# =============================================================================
# SECTION 11: TEMPORAL HOMOPHILY BY PRIZE
# =============================================================================
rpt("")
rpt(strrep("=", 78))
rpt("  SECTION 11: TEMPORAL HOMOPHILY TRENDS BY PRIZE")
rpt(strrep("=", 78))
rpt("")

for (p in c("Chemistry", "Physics", "Physiology/Medicine", "Literature", "Peace")) {
  rpt(sprintf("  === %s ===", p))
  nom_p <- noms %>%
    mutate(prize_clean = if_else(prize == "Medicine", "Physiology/Medicine", prize)) %>%
    filter(prize_clean == p,
           !is.na(nominator_country_modern), !is.na(nominee_country_modern)) %>%
    mutate(decade = 10 * (year %/% 10))

  dec_hom <- nom_p %>%
    group_by(decade) %>%
    summarise(
      n = n(),
      same_cty = sum(nominator_country_modern == nominee_country_modern),
      pct_same_cty = 100 * same_cty / n,
      same_cont = sum(nominator_continent == nominee_continent),
      pct_same_cont = 100 * same_cont / n,
      exp_same_cont = {
        nr <- table(nominator_continent) / n()
        ne <- table(nominee_continent) / n()
        sc <- intersect(names(nr), names(ne))
        100 * sum(nr[sc] * ne[sc])
      },
      .groups = "drop"
    ) %>%
    mutate(ratio_cont = pct_same_cont / exp_same_cont)

  for (j in seq_len(nrow(dec_hom))) {
    r <- dec_hom[j, ]
    rpt(sprintf("    %ds: N=%5s | SameCty=%.1f%% | SameCont=%.1f%% (exp=%.1f%%, ratio=%.2f)",
                r$decade, format(r$n, big.mark = ","),
                r$pct_same_cty, r$pct_same_cont, r$exp_same_cont, r$ratio_cont))
  }
  rpt("")
}


# =============================================================================
# WRAP UP
# =============================================================================
rpt("")
rpt(strrep("=", 78))
rpt("  END OF EXPLORATORY ANALYSIS")
rpt(strrep("=", 78))

close(report_con)
message(sprintf("\nReport saved to %s", report_file))

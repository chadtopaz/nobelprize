# =============================================================================
# 09_data_diagnostics.R
# Comprehensive data quality audit for the Nobel Prize multilayer network
#
# This script performs an exhaustive diagnostic of every data file produced by
# scripts 01–08. It checks completeness, consistency, cross-references, and
# documents all known structural gaps. Output is a plain-text report suitable
# for reference when writing the manuscript methods section.
#
# Output: Data/data_diagnostics_report.txt
# =============================================================================

source("Code/Data Gathering/00_utils.R")

# =============================================================================
# Report setup: tee output to both console and file
# =============================================================================
report_file <- output_path("data_diagnostics_report.txt")
report_con <- file(report_file, open = "wt")

rpt <- function(...) {
  msg <- paste0(...)
  message(msg)
  writeLines(msg, report_con)
}

warn_count <- 0
check_count <- 0

check_pass <- function(msg) {
  check_count <<- check_count + 1
  rpt(sprintf("  [PASS] %s", msg))
}

check_warn <- function(msg) {
  warn_count <<- warn_count + 1
  check_count <<- check_count + 1
  rpt(sprintf("  [WARNING] %s", msg))
}

check_info <- function(msg) {
  rpt(sprintf("  [INFO] %s", msg))
}

section <- function(num, title) {
  rpt("")
  rpt(strrep("=", 78))
  rpt(sprintf("  SECTION %s: %s", num, toupper(title)))
  rpt(strrep("=", 78))
  rpt("")
}

subsection <- function(title) {
  rpt(sprintf("  --- %s ---", title))
}

rpt("NOBEL PRIZE MULTILAYER NETWORK — DATA DIAGNOSTICS REPORT")
rpt(sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
rpt(sprintf("Working directory: %s", getwd()))
rpt("")


# =============================================================================
# SECTION 1: FILE INVENTORY
# =============================================================================
section("1", "File Inventory & Basic Counts")

expected_intermediate <- c(
  "governing_bodies.csv",
  "vetting_bodies.csv",
  "nominations.csv",
  "laureates.csv",
  "demographics.csv",
  "nomination_people.csv",
  "nomination_people_qids.csv"
)

expected_output <- c("nodes.csv", "edges.csv")

subsection("Intermediate files (Data/intermediate/)")
for (f in expected_intermediate) {
  path <- data_path(f)
  if (file.exists(path)) {
    info <- file.info(path)
    df <- read_csv(path, show_col_types = FALSE)
    check_pass(sprintf("%-35s  %6d rows × %d cols  (%s bytes, %s)",
                       f, nrow(df), ncol(df),
                       format(info$size, big.mark = ","),
                       format(info$mtime, "%Y-%m-%d %H:%M")))
  } else {
    check_warn(sprintf("%-35s  MISSING", f))
  }
}

rpt("")
subsection("Wikidata matching caches (Data/intermediate/)")
cache_files <- c(
  "wikidata_phase1_candidates.rds",
  "wikidata_birth_year_cache.rds",
  "wikidata_no_birthyear_cache.rds"
)
for (f in cache_files) {
  path <- data_path(f)
  if (file.exists(path)) {
    info <- file.info(path)
    obj <- readRDS(path)
    n_items <- if (is.list(obj)) length(obj) else length(obj)
    check_pass(sprintf("%-40s  %d items  (%s bytes, %s)",
                       f, n_items,
                       format(info$size, big.mark = ","),
                       format(info$mtime, "%Y-%m-%d %H:%M")))
  } else {
    check_info(sprintf("%-40s  NOT PRESENT (will be created on next script 05 run)", f))
  }
}

rpt("")
subsection("Final output files (Data/)")
for (f in expected_output) {
  path <- output_path(f)
  if (file.exists(path)) {
    info <- file.info(path)
    df <- read_csv(path, show_col_types = FALSE)
    check_pass(sprintf("%-35s  %6d rows × %d cols  (%s bytes)",
                       f, nrow(df), ncol(df),
                       format(info$size, big.mark = ",")))
  } else {
    check_warn(sprintf("%-35s  MISSING", f))
  }
}


# =============================================================================
# SECTION 2: GOVERNING BODIES
# =============================================================================
section("2", "Governing Bodies (Script 01)")

gb <- read_csv(data_path("governing_bodies.csv"), show_col_types = FALSE)

subsection("Record counts by body")
gb_counts <- gb %>% count(body) %>% arrange(desc(n))
for (i in seq_len(nrow(gb_counts))) {
  rpt(sprintf("    %-30s %5d records", gb_counts$body[i], gb_counts$n[i]))
}
rpt(sprintf("    %-30s %5d records", "TOTAL", nrow(gb)))

subsection("Year coverage by body")
gb_years <- gb %>%
  group_by(body) %>%
  summarise(
    min_start = min(startyear, na.rm = TRUE),
    max_start = max(startyear, na.rm = TRUE),
    min_end   = min(endyear, na.rm = TRUE),
    max_end   = max(endyear, na.rm = TRUE),
    n_missing_start = sum(is.na(startyear)),
    n_missing_end   = sum(is.na(endyear)),
    .groups = "drop"
  )
for (i in seq_len(nrow(gb_years))) {
  r <- gb_years[i, ]
  rpt(sprintf("    %s:", r$body))
  rpt(sprintf("      startyear: %d – %d  (missing: %d)", r$min_start, r$max_start, r$n_missing_start))
  rpt(sprintf("      endyear:   %d – %d  (missing: %d)", r$min_end, r$max_end, r$n_missing_end))
}

subsection("QID completeness")
gb_qid <- gb %>%
  group_by(body) %>%
  summarise(
    total = n(),
    has_qid = sum(!is.na(qid) & qid != ""),
    pct = 100 * has_qid / total,
    .groups = "drop"
  )
for (i in seq_len(nrow(gb_qid))) {
  r <- gb_qid[i, ]
  msg <- sprintf("%-30s %d / %d (%.1f%%)", r$body, r$has_qid, r$total, r$pct)
  if (r$pct < 95) check_warn(msg) else check_pass(msg)
}

subsection("Karolinska coverage gap")
ki <- gb %>% filter(body == "Karolinska Institutet")
ki_max <- max(ki$endyear, na.rm = TRUE)
if (ki_max < 1975) {
  check_warn(sprintf("Karolinska max endyear = %d. Post-%d data MISSING (structural gap).", ki_max, ki_max))
  check_info("The Nobel Assembly roster is not publicly available post-1970.")
  check_info("Contact: nobelforum@nobelprizemedicine.org")
} else {
  check_pass(sprintf("Karolinska coverage extends to %d", ki_max))
}

subsection("RSAS endyear imputation")
rsas <- gb %>% filter(body == "RSAS")
rsas_missing_end <- sum(is.na(rsas$endyear))
rsas_total <- nrow(rsas)
rpt(sprintf("    RSAS members: %d total, %d still missing endyear (%.1f%%)",
            rsas_total, rsas_missing_end, 100 * rsas_missing_end / rsas_total))
check_info("endyear for RSAS members was imputed from Wikidata death_year in script 06.")
if (rsas_missing_end > 0) {
  check_info(sprintf("%d RSAS members have no endyear (likely still living or death_year unknown).", rsas_missing_end))
}

subsection("Temporal anomalies")
bad_years <- gb %>% filter(!is.na(startyear) & !is.na(endyear) & startyear > endyear)
if (nrow(bad_years) > 0) {
  check_warn(sprintf("%d records have startyear > endyear:", nrow(bad_years)))
  for (i in seq_len(min(nrow(bad_years), 10))) {
    r <- bad_years[i, ]
    rpt(sprintf("      %s | %s | %d > %d", r$qid, r$body, r$startyear, r$endyear))
  }
} else {
  check_pass("No startyear > endyear anomalies.")
}

implausible <- gb %>% filter(startyear < 1700 | startyear > 2026 | endyear > 2026)
if (nrow(implausible) > 0) {
  check_warn(sprintf("%d records have implausible year values.", nrow(implausible)))
} else {
  check_pass("All year values within plausible range (1700–2026).")
}

subsection("Duplicate QIDs within same body")
dup_qids <- gb %>%
  filter(!is.na(qid)) %>%
  group_by(body, qid) %>%
  filter(n() > 1) %>%
  ungroup()
if (nrow(dup_qids) > 0) {
  n_dup <- n_distinct(dup_qids %>% select(body, qid))
  check_info(sprintf("%d QIDs appear multiple times in same body (may indicate multiple terms).", n_dup))
} else {
  check_pass("No duplicate QIDs within any body.")
}

subsection("Cross-check: governing QIDs in demographics")
demo <- read_csv(data_path("demographics.csv"), show_col_types = FALSE)
gb_qids <- gb %>% filter(!is.na(qid)) %>% pull(qid) %>% unique()
gb_in_demo <- sum(gb_qids %in% demo$qid)
gb_missing_demo <- gb_qids[!gb_qids %in% demo$qid]
if (length(gb_missing_demo) > 0) {
  check_warn(sprintf("%d / %d governing QIDs missing from demographics.", length(gb_missing_demo), length(gb_qids)))
} else {
  check_pass(sprintf("All %d governing QIDs found in demographics.", length(gb_qids)))
}


# =============================================================================
# SECTION 3: VETTING BODIES
# =============================================================================
section("3", "Vetting Bodies (Script 02)")

vb <- read_csv(data_path("vetting_bodies.csv"), show_col_types = FALSE)

subsection("Record counts by committee")
vb_counts <- vb %>% count(body) %>% arrange(desc(n))
for (i in seq_len(nrow(vb_counts))) {
  rpt(sprintf("    %-55s %3d members", vb_counts$body[i], vb_counts$n[i]))
}
rpt(sprintf("    %-55s %3d members", "TOTAL", nrow(vb)))

subsection("Year coverage by committee")
vb_years <- vb %>%
  group_by(body) %>%
  summarise(
    min_start = min(startyear, na.rm = TRUE),
    max_start = max(startyear, na.rm = TRUE),
    min_end   = min(endyear, na.rm = TRUE),
    max_end   = max(endyear, na.rm = TRUE),
    n_missing_start = sum(is.na(startyear)),
    n_missing_end   = sum(is.na(endyear)),
    .groups = "drop"
  )
for (i in seq_len(nrow(vb_years))) {
  r <- vb_years[i, ]
  rpt(sprintf("    %s:", r$body))
  rpt(sprintf("      startyear: %d – %d  (missing: %d)", r$min_start, r$max_start, r$n_missing_start))
  rpt(sprintf("      endyear:   %d – %d  (missing: %d)", r$min_end, r$max_end, r$n_missing_end))
}

subsection("QID completeness")
vb_qid <- vb %>%
  group_by(body) %>%
  summarise(
    total = n(),
    has_qid = sum(!is.na(qid) & qid != ""),
    pct = 100 * has_qid / total,
    .groups = "drop"
  )
for (i in seq_len(nrow(vb_qid))) {
  r <- vb_qid[i, ]
  msg <- sprintf("%-55s %d / %d (%.1f%%)", r$body, r$has_qid, r$total, r$pct)
  if (r$pct < 90) check_warn(msg) else check_pass(msg)
}

subsection("Known incompleteness flags")
med_count <- vb %>% filter(str_detect(body, "Medicine|Physiology")) %>% nrow()
check_warn(sprintf("Medicine committee (%d members): Swedish Wikipedia states 'Listan är ofullständig' (list is incomplete).", med_count))
lit_count <- vb %>% filter(str_detect(body, "Literature")) %>% nrow()
check_info(sprintf("Literature committee (%d members): Extracted from svenskaakademien.se Nuxt.js SPA (fragile source).", lit_count))

subsection("Cross-check: vetting QIDs in demographics")
vb_qids <- vb %>% filter(!is.na(qid)) %>% pull(qid) %>% unique()
vb_missing_demo <- vb_qids[!vb_qids %in% demo$qid]
if (length(vb_missing_demo) > 0) {
  check_warn(sprintf("%d / %d vetting QIDs missing from demographics.", length(vb_missing_demo), length(vb_qids)))
} else {
  check_pass(sprintf("All %d vetting QIDs found in demographics.", length(vb_qids)))
}

subsection("Cross-check: vetting members in parent governing body")
# Chemistry/Physics committee members should be RSAS members
# Medicine committee members should be Karolinska members
parent_map <- tribble(
  ~committee_pattern,                     ~parent_body,
  "Chemistry",                            "RSAS",
  "Physics",                              "RSAS",
  "Physiology|Medicine",                  "Karolinska Institutet",
)

for (i in seq_len(nrow(parent_map))) {
  pat <- parent_map$committee_pattern[i]
  parent <- parent_map$parent_body[i]

  committee_qids <- vb %>%
    filter(str_detect(body, pat), !is.na(qid)) %>%
    pull(qid) %>% unique()

  parent_qids <- gb %>%
    filter(body == parent, !is.na(qid)) %>%
    pull(qid) %>% unique()

  in_parent <- sum(committee_qids %in% parent_qids)
  not_in <- committee_qids[!committee_qids %in% parent_qids]
  committee_name <- vb %>% filter(str_detect(body, pat)) %>% pull(body) %>% unique()

  if (length(not_in) > 0) {
    check_info(sprintf("%s: %d / %d members found in %s (%d not found — may be structural).",
                       committee_name, in_parent, length(committee_qids), parent, length(not_in)))
  } else {
    check_pass(sprintf("%s: all %d members found in %s.",
                       committee_name, length(committee_qids), parent))
  }
}


# =============================================================================
# SECTION 4: NOMINATIONS
# =============================================================================
section("4", "Nominations (Script 03)")

noms <- read_csv(data_path("nominations.csv"), show_col_types = FALSE)

subsection("Overview")
rpt(sprintf("    Total records:        %s", format(nrow(noms), big.mark = ",")))
rpt(sprintf("    Unique nomination IDs: %d", n_distinct(noms$nomination_id)))
rpt(sprintf("    Unique nominees:       %d", n_distinct(noms$nominee_person_id)))
rpt(sprintf("    Unique nominators:     %d", n_distinct(noms$nominator_person_id, na.rm = TRUE)))

subsection("Records by prize")
noms_by_prize <- noms %>% count(prize) %>% arrange(desc(n))
for (i in seq_len(nrow(noms_by_prize))) {
  rpt(sprintf("    %-25s %s records", noms_by_prize$prize[i],
              format(noms_by_prize$n[i], big.mark = ",")))
}

subsection("Year coverage by prize")
noms_years <- noms %>%
  group_by(prize) %>%
  summarise(min_yr = min(year), max_yr = max(year), n_years = n_distinct(year),
            .groups = "drop")
for (i in seq_len(nrow(noms_years))) {
  r <- noms_years[i, ]
  expected_max <- if (r$prize == "Physiology/Medicine") 1953 else 1975
  status <- if (r$max_yr <= expected_max) "OK" else "UNEXPECTED"
  rpt(sprintf("    %-25s %d – %d  (%d distinct years)  [expected max: %d — %s]",
              r$prize, r$min_yr, r$max_yr, r$n_years, expected_max, status))
}

med_rows <- noms_years %>% filter(str_detect(prize, "Medicine|Physiology"))
if (nrow(med_rows) > 0) {
  med_max <- med_rows$max_yr[1]
  if (med_max <= 1953) {
    check_warn(sprintf("Physiology/Medicine nominations end at %d (Karolinska has not released 1954–1974).", med_max))
  } else {
    check_pass(sprintf("Physiology/Medicine nominations extend to %d.", med_max))
  }
} else {
  check_warn("No Physiology/Medicine nominations found at all.")
}

subsection("Missing data")
n_missing_nominee <- sum(is.na(noms$nominee_person_id))
n_missing_nominator <- sum(is.na(noms$nominator_person_id))
if (n_missing_nominee > 0) {
  check_warn(sprintf("%d records missing nominee_person_id.", n_missing_nominee))
} else {
  check_pass("All records have nominee_person_id.")
}
if (n_missing_nominator > 0) {
  check_info(sprintf("%d records missing nominator_person_id (anonymous/unlisted nominators).", n_missing_nominator))
} else {
  check_pass("All records have nominator_person_id.")
}

subsection("Self-nominations")
self_noms <- noms %>%
  filter(!is.na(nominee_person_id) & !is.na(nominator_person_id) &
           nominee_person_id == nominator_person_id)
if (nrow(self_noms) > 0) {
  check_info(sprintf("%d self-nominations (nominee = nominator).", nrow(self_noms)))
} else {
  check_pass("No self-nominations found.")
}

subsection("Multi-nominee and multi-nominator nominations")
noms_per_nomination <- noms %>%
  group_by(nomination_id) %>%
  summarise(n_nominees = n_distinct(nominee_person_id),
            n_nominators = n_distinct(nominator_person_id, na.rm = TRUE),
            .groups = "drop")
multi_nominee <- sum(noms_per_nomination$n_nominees > 1)
multi_nominator <- sum(noms_per_nomination$n_nominators > 1)
rpt(sprintf("    Nominations with multiple nominees:   %d / %d (%.1f%%)",
            multi_nominee, nrow(noms_per_nomination),
            100 * multi_nominee / nrow(noms_per_nomination)))
rpt(sprintf("    Nominations with multiple nominators: %d / %d (%.1f%%)",
            multi_nominator, nrow(noms_per_nomination),
            100 * multi_nominator / nrow(noms_per_nomination)))

subsection("Top 10 most-nominated nominees")
top_nominees <- noms %>%
  count(nominee_name, nominee_person_id, sort = TRUE) %>%
  head(10)
for (i in seq_len(nrow(top_nominees))) {
  r <- top_nominees[i, ]
  rpt(sprintf("    %2d. %-35s (ID: %s)  %d nominations", i, r$nominee_name, r$nominee_person_id, r$n))
}

subsection("Top 10 most-active nominators")
top_nominators <- noms %>%
  filter(!is.na(nominator_name)) %>%
  count(nominator_name, nominator_person_id, sort = TRUE) %>%
  head(10)
for (i in seq_len(nrow(top_nominators))) {
  r <- top_nominators[i, ]
  rpt(sprintf("    %2d. %-35s (ID: %s)  %d nominations", i, r$nominator_name, r$nominator_person_id, r$n))
}

subsection("Contextual field completeness (from detail pages)")
ctx_fields <- c("nominee_university", "nominee_city", "nominee_country", "nominee_profession",
                "nominator_university", "nominator_city", "nominator_country", "nominator_profession")
for (fld in ctx_fields) {
  if (fld %in% names(noms)) {
    n_present <- sum(!is.na(noms[[fld]]) & nchar(noms[[fld]]) > 0)
    rpt(sprintf("    %-25s: %d / %d (%.1f%%)", fld, n_present, nrow(noms), 100 * n_present / nrow(noms)))
  } else {
    check_warn(sprintf("Column '%s' not found in nominations.csv (re-run script 03 with updated scraper).", fld))
  }
}

subsection("Top 10 nominee countries")
if ("nominee_country" %in% names(noms)) {
  top_countries <- noms %>%
    filter(!is.na(nominee_country), nchar(nominee_country) > 0) %>%
    count(nominee_country, sort = TRUE) %>%
    head(10)
  for (i in seq_len(nrow(top_countries))) {
    r <- top_countries[i, ]
    rpt(sprintf("    %2d. %-35s  %d", i, r$nominee_country, r$n))
  }
}


# =============================================================================
# SECTION 5: NOMINATION PEOPLE QID MATCHING
# =============================================================================
section("5", "Nomination People QID Matching (Script 05)")

nom_qids <- read_csv(data_path("nomination_people_qids.csv"), show_col_types = FALSE)

subsection("Bio scraping completeness")
rpt(sprintf("    Total people:     %d", nrow(nom_qids)))
rpt(sprintf("    With name:        %d (%.1f%%)", sum(!is.na(nom_qids$name)), 100 * sum(!is.na(nom_qids$name)) / nrow(nom_qids)))
rpt(sprintf("    With birth_year:  %d (%.1f%%)", sum(!is.na(nom_qids$birth_year)), 100 * sum(!is.na(nom_qids$birth_year)) / nrow(nom_qids)))
rpt(sprintf("    With death_year:  %d (%.1f%%)", sum(!is.na(nom_qids$death_year)), 100 * sum(!is.na(nom_qids$death_year)) / nrow(nom_qids)))
rpt(sprintf("    With gender:      %d (%.1f%%)", sum(!is.na(nom_qids$gender) & nom_qids$gender != ""), 100 * sum(!is.na(nom_qids$gender) & nom_qids$gender != "") / nrow(nom_qids)))

subsection("QID match method breakdown")
match_counts <- nom_qids %>% count(match_method) %>% arrange(desc(n))
for (i in seq_len(nrow(match_counts))) {
  r <- match_counts[i, ]
  rpt(sprintf("    %-30s %5d  (%.1f%%)", r$match_method, r$n, 100 * r$n / nrow(nom_qids)))
}

n_matchable <- sum(!is.na(nom_qids$birth_year) & !is.na(nom_qids$name))
n_matched <- sum(nom_qids$match_method == "wikidata_name_birthyear", na.rm = TRUE)
if (n_matchable > 0) {
  rpt(sprintf("    Of %d matchable (have name + birth_year): %d matched (%.1f%%)",
              n_matchable, n_matched, 100 * n_matched / n_matchable))
}

if (n_matched / nrow(nom_qids) < 0.10) {
  check_warn(sprintf("Only %.1f%% of nomination people matched to Wikidata QIDs.", 100 * n_matched / nrow(nom_qids)))
} else {
  check_pass(sprintf("%.1f%% of nomination people matched to Wikidata QIDs.", 100 * n_matched / nrow(nom_qids)))
}

subsection("Multi-match analysis")
n_multi_match <- sum(nom_qids$multi_match_count > 1, na.rm = TRUE)
rpt(sprintf("    People with multiple QID candidates (multi_match_count > 1): %d", n_multi_match))
if (n_multi_match > 0) {
  multi_dist <- nom_qids %>%
    filter(multi_match_count > 1) %>%
    count(multi_match_count) %>%
    arrange(multi_match_count)
  for (i in seq_len(nrow(multi_dist))) {
    rpt(sprintf("      %d QID candidates: %d people", multi_dist$multi_match_count[i], multi_dist$n[i]))
  }
  check_info(sprintf("%d multi-match cases used the API's top-ranked candidate. Manual review recommended for count >= 3.", n_multi_match))
}

subsection("Duplicate QIDs (multiple person_ids → same QID)")
matched_only <- nom_qids %>% filter(!is.na(qid))
dup_qid_check <- matched_only %>%
  count(qid) %>%
  filter(n > 1) %>%
  arrange(desc(n))
if (nrow(dup_qid_check) > 0) {
  check_info(sprintf("%d QIDs map to multiple person_ids (%d person_ids total).",
                     nrow(dup_qid_check), sum(dup_qid_check$n)))
  rpt("    (Common causes: transliteration variants, same person with different nobelprize.org IDs)")
  # Show worst offenders
  for (i in seq_len(min(nrow(dup_qid_check), 5))) {
    dq <- dup_qid_check[i, ]
    people <- matched_only %>% filter(qid == dq$qid) %>% pull(name) %>% paste(collapse = " / ")
    rpt(sprintf("      %s (%dx): %s", dq$qid, dq$n, people))
  }
} else {
  check_pass("No duplicate QIDs in nomination people matching (1:1 mapping).")
}

subsection("Nomination people ↔ nominations cross-check")
nom_people_file <- data_path("nomination_people.csv")
if (file.exists(nom_people_file)) {
  nom_people_ref <- read_csv(nom_people_file, show_col_types = FALSE)
  # person_ids in nominations but not in nomination_people_qids
  all_nom_pids <- unique(c(
    noms$nominee_person_id[!is.na(noms$nominee_person_id)],
    noms$nominator_person_id[!is.na(noms$nominator_person_id)]
  ))
  qid_pids <- as.character(nom_qids$person_id)
  missing_from_qids <- setdiff(as.character(all_nom_pids), qid_pids)
  if (length(missing_from_qids) > 0) {
    check_warn(sprintf("%d person_ids in nominations.csv are missing from nomination_people_qids.csv.", length(missing_from_qids)))
  } else {
    check_pass("All person_ids in nominations.csv are present in nomination_people_qids.csv.")
  }
}

subsection("Gender distribution (from nobelprize.org)")
gender_dist <- nom_qids %>%
  mutate(gender = ifelse(is.na(gender) | gender == "", "unknown", gender)) %>%
  count(gender) %>% arrange(desc(n))
for (i in seq_len(nrow(gender_dist))) {
  r <- gender_dist[i, ]
  rpt(sprintf("    %-10s %5d  (%.1f%%)", r$gender, r$n, 100 * r$n / nrow(nom_qids)))
}

subsection("Birth year anomalies")
bad_birth_death <- nom_qids %>%
  filter(!is.na(birth_year) & !is.na(death_year) & birth_year > death_year)
if (nrow(bad_birth_death) > 0) {
  check_warn(sprintf("%d people have birth_year > death_year.", nrow(bad_birth_death)))
} else {
  check_pass("No birth_year > death_year anomalies.")
}

subsection("Cross-check: matched QIDs in demographics")
matched_qids <- nom_qids %>% filter(!is.na(qid)) %>% pull(qid)
in_demo <- sum(matched_qids %in% demo$qid)
not_in_demo <- matched_qids[!matched_qids %in% demo$qid]
if (length(not_in_demo) > 0) {
  check_warn(sprintf("%d / %d matched nomination QIDs missing from demographics.", length(not_in_demo), length(matched_qids)))
} else {
  check_pass(sprintf("All %d matched nomination QIDs found in demographics.", length(matched_qids)))
}


# =============================================================================
# SECTION 6: LAUREATES
# =============================================================================
section("6", "Laureates (Script 04)")

laur <- read_csv(data_path("laureates.csv"), show_col_types = FALSE)

subsection("Overview")
rpt(sprintf("    Total laureate-prize records: %d", nrow(laur)))
rpt(sprintf("    Unique individuals (by QID):  %d", n_distinct(laur$qid)))
rpt(sprintf("    Year range: %d – %d", min(laur$year), max(laur$year)))

subsection("Records by prize")
laur_by_prize <- laur %>% count(prize) %>% arrange(desc(n))
for (i in seq_len(nrow(laur_by_prize))) {
  rpt(sprintf("    %-25s %3d", laur_by_prize$prize[i], laur_by_prize$n[i]))
}

subsection("Records by decade")
laur_by_decade <- laur %>%
  mutate(decade = floor(year / 10) * 10) %>%
  count(decade) %>% arrange(decade)
for (i in seq_len(nrow(laur_by_decade))) {
  rpt(sprintf("    %ds: %3d", laur_by_decade$decade[i], laur_by_decade$n[i]))
}

subsection("QID completeness")
n_qid <- sum(!is.na(laur$qid))
if (n_qid == nrow(laur)) {
  check_pass(sprintf("All %d laureate records have QIDs (100%%).", nrow(laur)))
} else {
  check_warn(sprintf("%d / %d laureate records missing QIDs.", nrow(laur) - n_qid, nrow(laur)))
}

subsection("Gender breakdown")
laur_gender <- laur %>%
  distinct(qid, .keep_all = TRUE) %>%
  mutate(gender = coalesce(gender, "unknown")) %>%
  count(gender)
for (i in seq_len(nrow(laur_gender))) {
  rpt(sprintf("    %-10s %3d", laur_gender$gender[i], laur_gender$n[i]))
}

subsection("Multi-prize winners")
multi <- laur %>% count(qid, name, sort = TRUE) %>% filter(n > 1)
if (nrow(multi) > 0) {
  check_info(sprintf("%d individuals won multiple prizes:", nrow(multi)))
  for (i in seq_len(nrow(multi))) {
    prizes <- laur %>% filter(qid == multi$qid[i]) %>%
      mutate(label = sprintf("%s %d", prize, year)) %>% pull(label)
    rpt(sprintf("      %s: %s", multi$name[i], paste(prizes, collapse = ", ")))
  }
} else {
  check_info("No multi-prize winners found.")
}

subsection("Cross-check: laureate QIDs in nodes.csv")
nodes <- read_csv(output_path("nodes.csv"), show_col_types = FALSE)
laur_qids <- laur %>% pull(qid) %>% unique()
laur_in_nodes <- sum(laur_qids %in% nodes$qid)
if (laur_in_nodes < length(laur_qids)) {
  check_warn(sprintf("%d / %d laureate QIDs missing from nodes.csv.", length(laur_qids) - laur_in_nodes, length(laur_qids)))
} else {
  check_pass(sprintf("All %d laureate QIDs found in nodes.csv.", length(laur_qids)))
}

subsection("Cross-check: laureates with governing→laureate edges")
edges <- read_csv(output_path("edges.csv"), show_col_types = FALSE)
gl_edges <- edges %>% filter(from_layer == "governing_body", to_layer == "laureate")
laur_with_edge <- n_distinct(gl_edges$to_qid)
laur_without <- laur_qids[!laur_qids %in% gl_edges$to_qid]
if (length(laur_without) > 0) {
  check_warn(sprintf("%d laureates have no governing→laureate edge (may be Peace laureates routed through vetting).", length(laur_without)))
  # Check if these are Peace laureates
  peace_laur <- laur %>% filter(prize == "Peace") %>% pull(qid) %>% unique()
  peace_no_gov <- laur_without[laur_without %in% peace_laur]
  non_peace_no_gov <- laur_without[!laur_without %in% peace_laur]
  if (length(peace_no_gov) > 0) {
    check_info(sprintf("  Of these, %d are Peace laureates (expected — Peace uses vetting→laureate).", length(peace_no_gov)))
  }
  if (length(non_peace_no_gov) > 0) {
    check_warn(sprintf("  %d non-Peace laureates missing governing→laureate edges (unexpected).", length(non_peace_no_gov)))
    for (q in head(non_peace_no_gov, 5)) {
      name <- laur$name[laur$qid == q][1]
      yr <- laur$year[laur$qid == q][1]
      pr <- laur$prize[laur$qid == q][1]
      rpt(sprintf("      %s (%s, %d)", name, pr, yr))
    }
  }
} else {
  check_pass(sprintf("All %d non-Peace laureates have governing→laureate edges.", laur_with_edge))
}


# =============================================================================
# SECTION 7: DEMOGRAPHICS
# =============================================================================
section("7", "Demographics (Script 06)")

subsection("Overview")
rpt(sprintf("    Total records: %d", nrow(demo)))
rpt(sprintf("    Unique QIDs:   %d", n_distinct(demo$qid)))
if (nrow(demo) != n_distinct(demo$qid)) {
  check_warn("Demographics has duplicate QIDs!")
} else {
  check_pass("One row per QID (no duplicates).")
}

subsection("Field completeness")
demo_fields <- c("name", "gender", "birth_country", "nationality",
                  "birth_year", "death_year", "occupation", "institution")
for (field in demo_fields) {
  n_present <- sum(!is.na(demo[[field]]) & demo[[field]] != "")
  pct <- 100 * n_present / nrow(demo)
  rpt(sprintf("    %-15s %5d / %d  (%.1f%%)", field, n_present, nrow(demo), pct))
}

subsection("Gender distribution")
gender_counts <- demo %>%
  mutate(gender = coalesce(gender, "unknown")) %>%
  count(gender) %>% arrange(desc(n))
for (i in seq_len(nrow(gender_counts))) {
  r <- gender_counts[i, ]
  rpt(sprintf("    %-15s %5d  (%.1f%%)", r$gender, r$n, 100 * r$n / nrow(demo)))
}

subsection("Birth year distribution (by decade)")
birth_decades <- demo %>%
  filter(!is.na(birth_year)) %>%
  mutate(decade = floor(birth_year / 10) * 10) %>%
  count(decade) %>% arrange(decade)
for (i in seq_len(nrow(birth_decades))) {
  rpt(sprintf("    %ds: %4d", birth_decades$decade[i], birth_decades$n[i]))
}

subsection("Birth country (top 20)")
country_counts <- demo %>%
  filter(!is.na(birth_country) & birth_country != "") %>%
  # Handle semicolon-delimited
  separate_rows(birth_country, sep = ";\\s*") %>%
  count(birth_country, sort = TRUE) %>%
  head(20)
for (i in seq_len(nrow(country_counts))) {
  rpt(sprintf("    %-30s %4d", country_counts$birth_country[i], country_counts$n[i]))
}

subsection("Temporal anomalies")
bad_demo_years <- demo %>%
  filter(!is.na(birth_year) & !is.na(death_year) & birth_year > death_year)
if (nrow(bad_demo_years) > 0) {
  check_warn(sprintf("%d records have birth_year > death_year.", nrow(bad_demo_years)))
  for (i in seq_len(min(nrow(bad_demo_years), 5))) {
    r <- bad_demo_years[i, ]
    rpt(sprintf("      %s (%s): born %d, died %d", r$qid, r$name, r$birth_year, r$death_year))
  }
} else {
  check_pass("No birth_year > death_year anomalies.")
}

implaus_demo <- demo %>%
  filter((!is.na(birth_year) & (birth_year < 1700 | birth_year > 2010)) |
         (!is.na(death_year) & death_year > 2026))
if (nrow(implaus_demo) > 0) {
  check_warn(sprintf("%d records have implausible birth/death years.", nrow(implaus_demo)))
} else {
  check_pass("All birth/death years within plausible range.")
}

subsection("Multi-valued fields")
for (field in c("nationality", "occupation", "institution")) {
  n_multi <- sum(str_detect(demo[[field]], ";"), na.rm = TRUE)
  rpt(sprintf("    %-15s %d records have multiple values (semicolon-delimited)", field, n_multi))
}


# =============================================================================
# SECTION 8: GEOGRAPHIC STANDARDIZATION (Script 07)
# =============================================================================
section("8", "Geographic Standardization (Script 07)")

subsection("Demographics — birth country mapping coverage")
if ("birth_country_modern" %in% names(demo)) {
  n_with_bc <- sum(!is.na(demo$birth_country))
  n_mapped_bc <- sum(!is.na(demo$birth_country_modern) & !is.na(demo$birth_country))
  check_pass(sprintf("Birth country mapped: %d / %d (%.1f%%)",
                      n_mapped_bc, n_with_bc, 100 * n_mapped_bc / n_with_bc))

  unmapped_bc <- demo %>%
    filter(!is.na(birth_country), is.na(birth_country_modern)) %>%
    count(birth_country, sort = TRUE)
  if (nrow(unmapped_bc) > 0) {
    check_warn(sprintf("%d unmapped birth_country values:", nrow(unmapped_bc)))
    for (i in seq_len(min(20, nrow(unmapped_bc)))) {
      rpt(sprintf("      %s (%d)", unmapped_bc$birth_country[i], unmapped_bc$n[i]))
    }
  } else {
    check_pass("All non-NA birth_country values mapped successfully.")
  }
} else {
  check_info("birth_country_modern column not present — run script 07 first.")
}

subsection("Demographics — distribution by continent and subregion")
if ("birth_continent" %in% names(demo)) {
  continent_dist <- demo %>%
    filter(!is.na(birth_continent)) %>%
    count(birth_continent, sort = TRUE)
  for (i in seq_len(nrow(continent_dist))) {
    rpt(sprintf("    %-12s : %d", continent_dist$birth_continent[i], continent_dist$n[i]))
  }

  subregion_dist <- demo %>%
    filter(!is.na(birth_subregion)) %>%
    count(birth_subregion, sort = TRUE)
  rpt("")
  for (i in seq_len(nrow(subregion_dist))) {
    rpt(sprintf("    %-25s : %d", subregion_dist$birth_subregion[i], subregion_dist$n[i]))
  }
}

subsection("Nominations — country mapping coverage")
if ("nominee_country_modern" %in% names(noms)) {
  for (role in c("nominee", "nominator")) {
    orig_col <- paste0(role, "_country")
    mapped_col <- paste0(role, "_country_modern")
    n_with <- sum(!is.na(noms[[orig_col]]))
    n_mapped <- sum(!is.na(noms[[mapped_col]]) & !is.na(noms[[orig_col]]))
    pct <- if (n_with > 0) 100 * n_mapped / n_with else 0
    if (pct >= 95) {
      check_pass(sprintf("%s country mapped: %d / %d (%.1f%%)",
                          str_to_title(role), n_mapped, n_with, pct))
    } else {
      check_warn(sprintf("%s country mapped: %d / %d (%.1f%%)",
                             str_to_title(role), n_mapped, n_with, pct))
    }
  }
} else {
  check_info("nominee_country_modern column not present — run script 07 first.")
}


# =============================================================================
# SECTION 9: NETWORK STRUCTURE (Script 08)
# =============================================================================
section("9", "Network Structure (Script 08)")

subsection("Overview")
rpt(sprintf("    Nodes: %s", format(nrow(nodes), big.mark = ",")))
rpt(sprintf("    Edges: %s", format(nrow(edges), big.mark = ",")))

subsection("Edge type distribution")
edge_types <- edges %>%
  count(from_layer, to_layer) %>%
  mutate(pct = 100 * n / nrow(edges)) %>%
  arrange(desc(n))
for (i in seq_len(nrow(edge_types))) {
  r <- edge_types[i, ]
  rpt(sprintf("    %-25s → %-15s %7s  (%.1f%%)",
              r$from_layer, r$to_layer,
              format(r$n, big.mark = ","), r$pct))
}

subsection("Edge count by prize")
edges_by_prize <- edges %>% count(prize) %>% arrange(desc(n))
for (i in seq_len(nrow(edges_by_prize))) {
  r <- edges_by_prize[i, ]
  rpt(sprintf("    %-25s %7s", r$prize, format(r$n, big.mark = ",")))
}

subsection("Year range of edges")
rpt(sprintf("    Min year: %d", min(edges$year, na.rm = TRUE)))
rpt(sprintf("    Max year: %d", max(edges$year, na.rm = TRUE)))

subsection("NOM: prefix IDs (unresolved nomination people)")
all_edge_ids <- c(edges$from_qid, edges$to_qid)
n_nom_ids <- sum(str_detect(all_edge_ids, "^NOM:"))
n_total_ids <- length(all_edge_ids)
pct_nom <- 100 * n_nom_ids / n_total_ids
rpt(sprintf("    NOM: prefix IDs in edges: %s / %s (%.1f%%)",
            format(n_nom_ids, big.mark = ","),
            format(n_total_ids, big.mark = ","), pct_nom))
if (pct_nom > 10) {
  check_warn(sprintf("%.1f%% of edge endpoints use unresolved NOM: IDs.", pct_nom))
} else {
  check_pass(sprintf("Only %.1f%% of edge endpoints use NOM: IDs.", pct_nom))
}

# Edges that contain at least one NOM: ID
edges_with_nom <- edges %>%
  filter(str_detect(from_qid, "^NOM:") | str_detect(to_qid, "^NOM:"))
rpt(sprintf("    Edges with at least one NOM: ID: %s / %s (%.1f%%)",
            format(nrow(edges_with_nom), big.mark = ","),
            format(nrow(edges), big.mark = ","),
            100 * nrow(edges_with_nom) / nrow(edges)))

subsection("Self-loops")
self_loops <- edges %>% filter(from_qid == to_qid)
if (nrow(self_loops) > 0) {
  check_warn(sprintf("%d self-loops found (from_qid == to_qid).", nrow(self_loops)))
} else {
  check_pass("No self-loops.")
}

subsection("Duplicate edges")
n_before <- nrow(edges)
n_after <- edges %>% distinct() %>% nrow()
n_dups <- n_before - n_after
if (n_dups > 0) {
  check_warn(sprintf("%d duplicate edges found.", n_dups))
} else {
  check_pass("No duplicate edges.")
}

subsection("Degree distribution (edges per node)")
# Only count QID-based nodes (not NOM: prefixed)
qid_edges <- edges %>%
  filter(!str_detect(from_qid, "^NOM:"), !str_detect(to_qid, "^NOM:"))
all_node_ids <- c(qid_edges$from_qid, qid_edges$to_qid)
degree_table <- table(all_node_ids)
if (length(degree_table) > 0) {
  rpt(sprintf("    Among QID-resolved edges:"))
  rpt(sprintf("      Nodes involved: %d", length(degree_table)))
  rpt(sprintf("      Mean degree:    %.1f", mean(degree_table)))
  rpt(sprintf("      Median degree:  %.0f", median(degree_table)))
  rpt(sprintf("      Max degree:     %d  (%s)", max(degree_table),
              names(which.max(degree_table))))
  rpt(sprintf("      Min degree:     %d", min(degree_table)))
}

subsection("Isolate nodes (in nodes.csv but no edges)")
node_qids_in_edges <- unique(c(edges$from_qid, edges$to_qid))
isolates <- nodes %>% filter(!qid %in% node_qids_in_edges)
rpt(sprintf("    Isolate nodes: %d / %d (%.1f%%)",
            nrow(isolates), nrow(nodes), 100 * nrow(isolates) / nrow(nodes)))
if (nrow(isolates) > 0) {
  check_info(sprintf("%d nodes have no edges (may be nomination people with QIDs but no NOM: edges resolved).", nrow(isolates)))
}

subsection("Prize-year coverage: governing→laureate edges")
# For each prize-year with a laureate, check if governing→laureate edges exist
laur_prize_years <- laur %>%
  filter(prize != "Peace") %>%
  distinct(prize, year)
missing_gl <- list()
for (i in seq_len(nrow(laur_prize_years))) {
  py <- laur_prize_years[i, ]
  has_edge <- gl_edges %>%
    filter(prize == py$prize, year == py$year) %>%
    nrow() > 0
  if (!has_edge) {
    missing_gl[[length(missing_gl) + 1]] <- py
  }
}
if (length(missing_gl) > 0) {
  missing_gl_df <- bind_rows(missing_gl)
  check_warn(sprintf("%d prize-years have laureates but no governing→laureate edges:", nrow(missing_gl_df)))
  for (i in seq_len(min(nrow(missing_gl_df), 10))) {
    rpt(sprintf("      %s %d", missing_gl_df$prize[i], missing_gl_df$year[i]))
  }
  if (nrow(missing_gl_df) > 10) rpt(sprintf("      ... and %d more", nrow(missing_gl_df) - 10))
} else {
  check_pass("All non-Peace prize-years with laureates have governing→laureate edges.")
}


# =============================================================================
# SECTION 10: CROSS-LAYER CONSISTENCY
# =============================================================================
section("10", "Cross-Layer Consistency")

subsection("Laureates who are also nominees")
laur_qids_set <- unique(laur$qid)
# Check among NOM:-resolved edges
nom_edges_only <- edges %>% filter(to_layer == "nominee")
nominee_qids <- nom_edges_only %>%
  filter(!str_detect(to_qid, "^NOM:")) %>%
  pull(to_qid) %>% unique()
laur_as_nominee <- intersect(laur_qids_set, nominee_qids)
check_info(sprintf("%d laureates appear as nominees (with resolved QIDs).", length(laur_as_nominee)))

subsection("Laureates who are governing body members")
gb_qids_set <- unique(gb$qid[!is.na(gb$qid)])
laur_in_gov <- intersect(laur_qids_set, gb_qids_set)
check_info(sprintf("%d laureates are also governing body members.", length(laur_in_gov)))
if (length(laur_in_gov) > 0) {
  # Show a few examples
  examples <- laur %>%
    filter(qid %in% head(laur_in_gov, 5)) %>%
    distinct(qid, name, prize, year)
  for (i in seq_len(nrow(examples))) {
    r <- examples[i, ]
    bodies <- gb %>% filter(qid == r$qid) %>% pull(body) %>% unique()
    rpt(sprintf("      %s (%s %d) — member of: %s", r$name, r$prize, r$year, paste(bodies, collapse = ", ")))
  }
}

subsection("Laureates who are vetting body members")
vb_qids_set <- unique(vb$qid[!is.na(vb$qid)])
laur_in_vet <- intersect(laur_qids_set, vb_qids_set)
check_info(sprintf("%d laureates are also vetting body members.", length(laur_in_vet)))

subsection("People in multiple governing bodies")
multi_body <- gb %>%
  filter(!is.na(qid)) %>%
  distinct(qid, body) %>%
  count(qid) %>%
  filter(n > 1)
check_info(sprintf("%d people appear in multiple governing bodies.", nrow(multi_body)))

subsection("Edge QIDs not in nodes.csv")
edge_qids <- unique(c(edges$from_qid, edges$to_qid))
real_qids <- edge_qids[!str_detect(edge_qids, "^NOM:")]
not_in_nodes <- real_qids[!real_qids %in% nodes$qid]
if (length(not_in_nodes) > 0) {
  check_warn(sprintf("%d real QIDs in edges are missing from nodes.csv.", length(not_in_nodes)))
} else {
  check_pass("All real QIDs in edges are present in nodes.csv.")
}

subsection("NOM: IDs with no nomination_people_qids entry")
nom_ids_in_edges <- edge_qids[str_detect(edge_qids, "^NOM:")] %>%
  str_replace("^NOM:", "") %>%
  unique()
nom_people_ids <- nom_qids$person_id %>% as.character()
orphan_nom <- nom_ids_in_edges[!nom_ids_in_edges %in% nom_people_ids]
if (length(orphan_nom) > 0) {
  check_warn(sprintf("%d NOM: IDs in edges have no entry in nomination_people_qids.csv.", length(orphan_nom)))
} else {
  check_pass(sprintf("All %d NOM: IDs in edges have entries in nomination_people_qids.csv.", length(nom_ids_in_edges)))
}


# =============================================================================
# SECTION 11: KNOWN STRUCTURAL GAPS SUMMARY
# =============================================================================
section("11", "Known Structural Gaps & Limitations")

rpt("  The following are KNOWN data limitations inherent to available sources.")
rpt("  These are NOT bugs — they should be documented in the manuscript.")
rpt("")
rpt("  1. KAROLINSKA INSTITUTET (GOVERNING BODY FOR PHYSIOLOGY/MEDICINE)")
rpt("     Coverage: 1881–1970 only. Post-1970 Nobel Assembly roster is not")
rpt("     publicly available. Wikidata has no P39/P463 records for Q3375124.")
rpt("     Impact: All Physiology/Medicine governing body data missing after 1970.")
rpt(sprintf("     Records: %d members, max endyear = %d", nrow(ki), ki_max))
rpt("")
rpt("  2. NOMINATION ARCHIVE 50-YEAR SECRECY RULE")
rpt("     Physics/Chemistry/Literature: available through 1974 (or 1975).")
rpt("     Peace: available through 1975.")
rpt("     Physiology/Medicine: available through 1953 ONLY.")
rpt("     Karolinska has not released 1954–1974 data (likely related to")
rpt("     the 1977 Swedish transparency law and creation of the Nobel Assembly).")
rpt("")
rpt("  3. NOBEL COMMITTEE FOR PHYSIOLOGY/MEDICINE (VETTING BODY)")
rpt("     Source: Swedish Wikipedia page explicitly states")
rpt("     'Listan är ofullständig' (the list is incomplete).")
rpt(sprintf("     Records: %d members", med_count))
rpt("")
rpt("  4. NOBEL COMMITTEE FOR LITERATURE (VETTING BODY)")
rpt("     Source: svenskaakademien.se Nuxt.js SPA (rebuilt ~2024).")
rpt("     Extraction parses __NUXT_DATA__ payload — fragile to redesigns.")
rpt(sprintf("     Records: %d members with Nobel Committee service years", lit_count))
rpt("")
rpt("  5. NOMINATION PEOPLE QID MATCHING")
n_total_nom <- nrow(nom_qids)
rpt(sprintf("     Total nomination archive people: %d", n_total_nom))
rpt(sprintf("     Matched to Wikidata QIDs: %d (%.1f%%)", n_matched, 100 * n_matched / n_total_nom))
rpt(sprintf("     Missing birth year (unmatchable): %d (%.1f%%)",
            sum(nom_qids$match_method == "no_data", na.rm = TRUE),
            100 * sum(nom_qids$match_method == "no_data", na.rm = TRUE) / n_total_nom))
rpt("     Impact: Most nominator→nominee edges use temporary NOM: prefix IDs")
rpt("     instead of Wikidata QIDs, limiting cross-layer entity resolution.")
rpt("")
rpt("  6. RSAS ENDYEAR IMPUTATION")
rpt("     RSAS members' endyear is imputed from Wikidata death_year (script 06).")
rpt(sprintf("     Still missing after imputation: %d / %d (%.1f%%)",
            rsas_missing_end, rsas_total, 100 * rsas_missing_end / rsas_total))
rpt("     These are likely living members whose actual departure date is unknown.")
rpt("")
rpt("  7. STORTING TERM CONSOLIDATION")
rpt("     Norwegian Parliament members may serve non-consecutive terms.")
rpt("     Script 01 consolidates consecutive terms but gaps between terms")
rpt("     are preserved. Verify that endyear reflects actual departure.")


# =============================================================================
# SUMMARY SCORECARD
# =============================================================================
section("", "SUMMARY SCORECARD")
rpt(sprintf("  Total checks performed: %d", check_count))
rpt(sprintf("  Passed:                 %d", check_count - warn_count))
rpt(sprintf("  Warnings:               %d", warn_count))
rpt("")
rpt(sprintf("Report saved to: %s", report_file))

close(report_con)
message(sprintf("\n=== DIAGNOSTICS COMPLETE: %d checks, %d warnings ===", check_count, warn_count))
message(sprintf("Report: %s", report_file))

# =============================================================================
# 05_match_nomination_qids.R
#
# FILE NAME: 05_match_nomination_qids.R
# TITLE: Wikidata Entity Resolution for Nobel Prize Nomination Archive
#
# AUTHOR: Chad M. Topaz
# LAST UPDATED: February 2025
#
# PURPOSE AND GOALS:
# This script performs complex three-phase Wikidata entity resolution for people
# in the Nobel Prize nomination archive. The nomination archive identifies people
# only by internal IDs and names; this script matches them to Wikidata QIDs
# (globally unique identifiers) to unlock rich demographic and institutional data.
#
# The script extracts ~2,600 unique people from the nomination archive, scrapes
# biographical metadata from nobelprize.org, and uses a sophisticated three-phase
# matching algorithm to resolve identities to Wikidata. Success enables downstream
# demographic lookups and network analysis.
#
# METHODOLOGICAL DECISIONS AND RATIONALE:
#
#   THREE-PHASE MATCHING STRATEGY:
#     Phase 1 (Deterministic Search): For each person with name + birth year,
#       generate up to 3 name variants (full name, first+last, last-only) to
#       capture transliteration variants and spelling differences. Search Wikidata
#       for each variant in parallel (search is cheap). Rationale: handles names
#       with middle names/suffixes; provides fallback if exact name is uncommon.
#
#     Phase 2 (Bulk Birth Year Verification): Deduplicate all ~45K candidate
#       QIDs and batch-fetch their birth years (P569 Wikidata property) in 50-item
#       batches. Match people to candidates via EXACT birth year match. Rationale:
#       birth year is a strong discriminator with minimal data quality issues in
#       Wikidata, and bulk fetching is efficient.
#
#     Phase 3 (Nondeterministic Refinement): For remaining unmatched people,
#       repeat full_name searches up to 100 times. Wikidata search API returns
#       results in nondeterministic order; each pass may surface different
#       candidates. Birth year verification is cached—only new QIDs fetched.
#       Stops when convergence (zero new matches). Rationale: captures late-career
#       obscure matches and handles API ordering variance; caching prevents
#       redundant Wikidata queries.
#
#   CACHING STRATEGY:
#     - Bio cache (nomination_bios.csv): Permanent, keyed by person_id. Never
#       invalidated—nobelprize.org pages don't change. Resume-capable: checks for
#       partial_bio_file if first run interrupted.
#     - Phase 1 cache (wikidata_phase1_candidates.rds): Deterministic results
#       (same input → same candidates). Reused across runs; skips Phase 1 if exists.
#     - Phase 2 caches: Two files track verified QIDs
#       * wikidata_birth_year_cache.rds: QIDs with confirmed birth years (QID→year)
#       * wikidata_no_birthyear_cache.rds: QIDs checked but lacking birth years
#       Prevents redundant Wikidata API calls and enables resume during Phase 3.
#     - Confirmed matches (nomination_people_qids.csv): Preserved across runs.
#       Only adds new matches; doesn't re-verify already-matched people.
#
#   MATCH METHODS AND TRACKING:
#     - wikidata_name_birthyear: Matched via deterministic Phase 1+2 or
#       nondeterministic Phase 3 with exact birth year verification.
#     - no_data: Person lacks both full_name or birth_year (unfilterable).
#     - unmatched: Person searched but no Wikidata match found (searched all
#       100 passes without hit).
#     - multi_match_count: Tracks ambiguous cases where multiple QIDs match.
#       Uses Wikidata API's top-ranked candidate; human review recommended.
#
#   DATA QUALITY HANDLING:
#     - Gender: 6 records with junk values (<, §, -, N, J) from nobelprize.org
#       detected and corrected to "M" (verified by first names).
#     - Birth/death years: Extracted directly from Wikidata P569/P570; missing
#       values treated as unmatchable (cannot verify identity without birth year).
#     - Name variants: Handles transliteration (e.g., Müller/Mueller) via full
#       name, first+last, and last-only approaches; captures organizational names.
#
# INPUTS:
#   - nominations.csv (from 03_nominations.R): Nobel Prize nomination archive;
#     columns include nominee_person_id, nominee_name, nominator_person_id,
#     nominator_name. Contains both nominators and nominees.
#   - nobelprize.org website: Person detail pages for bio scraping
#     (https://www.nobelprize.org/nomination/archive/show_people.php?id=...)
#   - Wikidata API: Search endpoint and entity lookup (P569 birth date property)
#
# OUTPUTS:
#   - nomination_people_qids.csv: Mapping of person_id → Wikidata QID
#     Columns: person_id, name, gender, birth_year, death_year, qid, match_method,
#     multi_match_count
#   - nomination_people.csv: All unique people extracted from nominations.csv
#   - nomination_bios.csv: Cached biographical data from nobelprize.org
#   - wikidata_phase1_candidates.rds: Phase 1 cached candidates (RDS list)
#   - wikidata_birth_year_cache.rds: Phase 2 cached birth year lookups (named list)
#   - wikidata_no_birthyear_cache.rds: Phase 2 cached QIDs without birth years (char vec)
#
# DEPENDENCIES:
#   - tidyverse (dplyr, readr, stringr): Data manipulation and I/O
#   - rvest: HTML scraping for nobelprize.org pages
#   - httr2: HTTP requests for Wikidata API with retries and rate limiting
#   - furrr/future: Parallel execution for web scraping and batch searches
#   - 00_utils.R: Helper functions (data_path(), fetch_demographics_batch(), etc.)
#
# KNOWN LIMITATIONS:
#   1. Birth year matching is exact only. Errors in either nobelprize.org or
#      Wikidata could prevent matching. No fuzzy matching attempted.
#   2. People without birth year data are skipped (conservative approach).
#      Could use name+gender+era to attempt matching (not implemented).
#   3. Multi-match cases (N>1 QIDs) use API's top-ranked candidate without
#      disambiguation logic. May introduce false positives for common names
#      (e.g., John Smith, Maria Garcia). Manual review recommended for these.
#   4. Wikidata search API is nondeterministic (black box ranking). Phase 3
#      guarantees convergence but doesn't guarantee finding all matches.
#   5. nobelprize.org pages sometimes missing biographical data (gender, years).
#      No fallback to Wikidata bios during Phase 1.
#   6. Transliteration variants are handled heuristically (full, first+last, last).
#      Non-Latin scripts may be inadequately covered.
#
# PERFORMANCE AND CACHING NOTES:
#   - First run (full bio scrape): ~25–40 minutes for ~2,600 people
#     (10–15 cores, 0.2s sleep per request)
#   - Phase 1 candidate search: ~3–5 minutes (~6K deterministic searches)
#   - Phase 2 bulk verification: ~2–3 minutes (~900 API calls in 50-item batches)
#   - Phase 3 nondeterministic passes: Varies by convergence; typically 5–10 passes
#     over ~8–12 minutes; later passes with fewer unmatched people run faster
#   - On re-runs (all caches exist): <5 minutes total (Phase 1+2 skipped)
#   - Bio scraping is resumable: partial_bio_file checkpoints at 500-person chunks
#   - Demographics fetching is resumable: Phase 2 birth year caches allow resumption
#     mid-Phase 3 without loss of progress
# =============================================================================

source("Code/Data Gathering/00_utils.R")

# =============================================================================
# Configuration
#
# Parameters controlling web scraping and API request rates. These are tuned
# to be polite to target servers while maximizing parallel efficiency.
# =============================================================================

SLEEP_BETWEEN_DETAILS <- 0.2  # per-worker delay for nobelprize.org; ~5 req/sec/worker
SLEEP_BETWEEN_WD      <- 0.2  # per-worker delay for Wikidata search calls (~5 req/sec/worker)
WD_SEARCH_LIMIT       <- 15   # candidates per search (3 variants × 15 = up to 45 unique QIDs per person)

# =============================================================================
# 1. EXTRACT UNIQUE PEOPLE FROM NOMINATIONS.CSV
#
# Goal: Build a deduplicated list of all people mentioned in the Nobel Prize
# nomination archive (both nominators and nominees).
#
# Approach:
#   - Load nominations.csv from step 03
#   - Extract nominee_person_id + nominee_name and nominator_person_id + nominator_name
#   - Combine and deduplicate by person_id (people may appear multiple times)
#   - Save for downstream use (e.g., vetting body assignments, export)
#
# Edge cases:
#   - Some records may have person_id but no name (handled with filter(!is.na()))
#   - Person may appear as both nominator and nominee; deduplication handles this
# =============================================================================
message("=== Loading nomination archive people ===\n")

nom_file <- data_path("nominations.csv")
if (!file.exists(nom_file)) {
  stop("nominations.csv not found. Run 03 first.")
}

# Load all nominations from nobelprize.org archive (previously scraped in 03)
noms <- read_csv(nom_file, show_col_types = FALSE)

# Extract nominees: rename columns to generic person_id/name for consistency
nominees <- noms %>%
  select(person_id = nominee_person_id, name = nominee_name) %>%
  filter(!is.na(person_id))

# Extract nominators: same structure for combining
nominators <- noms %>%
  select(person_id = nominator_person_id, name = nominator_name) %>%
  filter(!is.na(person_id))

# Combine nominators and nominees, keeping first occurrence of each person_id
# (person may appear as both nominator and nominee across different nominations)
nom_people <- bind_rows(nominees, nominators) %>%
  distinct(person_id, .keep_all = TRUE)

message(sprintf("  %d unique people to process", nrow(nom_people)))

# Save this deduplicatedlist for downstream scripts (e.g., vetting body assignment)
write_csv(nom_people, data_path("nomination_people.csv"))


# =============================================================================
# 2. SCRAPE BIOGRAPHICAL DATA FROM nobelprize.org PERSON PAGES
#
# Goal: Collect biographical metadata (name components, gender, birth/death years)
# from the Nobel Prize nomination archive's person detail pages. These data are
# essential for Wikidata matching (birth year is the discriminator).
#
# Caching strategy: Permanent cache (nomination_bios.csv) never invalidated—
# nobelprize.org pages don't change. First-run resumable: checks for
# partial_bio_file if interrupted mid-scrape. Uses chunked scraping (500-person
# chunks) to save progress and estimate time remaining.
#
# Approach:
#   1. Check if permanent cache exists; if yes, load and check for new people
#   2. If any new people, scrape them in parallel; save results to permanent cache
#   3. If no permanent cache, check for partial file (recovery from interrupted run)
#   4. Scrape remaining people in 500-person chunks, saving progress after each
# =============================================================================

# Permanent cache file for bio data (NOT a partial file — kept across runs)
bio_cache_file <- data_path("nomination_bios.csv")

#' Scrape a single person detail page from nobelprize.org
#'
#' HTML structure: person details are in table cells (<td>) with labels followed
#' by values. This function finds label-value pairs by index (label at idx, value
#' at idx+1) and extracts demographics.
#'
#' @param person_id The nobelprize.org person ID (numeric or character)
#' @return A one-row data frame with person_id, lastname, firstname, gender,
#'         birth_year, death_year. All columns present even if values are NA
#'         (ensures bind_rows() works correctly).
#'
#' @details Error handling: If page scraping fails, returns NA row with person_id
#'          preserved for tracking. This allows resumable scraping—can re-run
#'          and only missing people are re-scraped.
scrape_person <- function(person_id) {
  # Construct nobelprize.org person detail URL
  url <- sprintf(
    "https://www.nobelprize.org/nomination/archive/show_people.php?id=%s",
    person_id
  )

  tryCatch({
    # Fetch and parse HTML. Extract all table cells (<td> elements).
    # Cells contain alternating labels and values (e.g., "Lastname/org:", "Smith").
    page <- read_html(url)
    tds <- page %>% html_elements("td") %>% html_text() %>% str_squish()

    # Helper: extract value following a label in the TD sequence.
    # Labels appear in tds; corresponding value is in tds[idx+1] if present.
    # Returns NA_character_ if label not found or value missing.
    get_field <- function(label) {
      idx <- which(tds == label)
      if (length(idx) > 0 && idx[1] < length(tds)) tds[idx[1] + 1]
      else NA_character_
    }

    # Extract demographics from label-value pairs. Coerce years to numeric.
    # Note: Some records may have missing fields (e.g., no gender or death year).
    # This is preserved in output as NA.
    data.frame(
      person_id  = as.character(person_id),
      lastname   = get_field("Lastname/org:"),
      firstname  = get_field("Firstname:"),
      gender     = get_field("Gender:"),
      birth_year = as.numeric(get_field("Year, Birth:")),
      death_year = as.numeric(get_field("Year, Death:")),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    # Network error, page not found, or parsing failure. Return NA row with
    # person_id preserved for tracking. Allows downstream resumption.
    message(sprintf("    WARNING: Failed to scrape person %s: %s",
                    person_id, e$message))
    data.frame(
      person_id = as.character(person_id),
      lastname = NA_character_, firstname = NA_character_,
      gender = NA_character_, birth_year = NA_real_, death_year = NA_real_,
      stringsAsFactors = FALSE
    )
  })
}

if (file.exists(bio_cache_file)) {
  # ---- CACHED RUN: Load previously scraped bios and check for new people ----
  message("\n=== Loading cached biographical data ===\n")
  bios_done <- read_csv(bio_cache_file, show_col_types = FALSE)

  # Check if any new people have been added to nominations.csv
  # (e.g., if nominations.csv was re-scraped with additional records)
  cached_ids <- as.character(bios_done$person_id)
  new_ids <- setdiff(as.character(nom_people$person_id), cached_ids)

  if (length(new_ids) > 0) {
    message(sprintf("  Cache has %d people. %d new people need scraping.",
                    nrow(bios_done), length(new_ids)))
    n_workers <- max(1, parallel::detectCores() - 1)
    plan(multisession, workers = n_workers)

    # Scrape only new people
    new_results <- future_map(new_ids, function(pid) {
      Sys.sleep(SLEEP_BETWEEN_DETAILS)
      scrape_person(pid)
    })

    # Append new results to cache
    bios_done <- bind_rows(bios_done, bind_rows(new_results))
    write_csv(bios_done, bio_cache_file)
    message(sprintf("  Updated cache: %d people total.", nrow(bios_done)))
  } else {
    message(sprintf("  Cache is complete: %d people. Skipping bio scrape.", nrow(bios_done)))
  }

} else {
  # ---- FIRST RUN: Scrape all bios from scratch; resume-capable ----
  message("\n=== Scraping person detail pages from nobelprize.org ===\n")

  # Check for partial scraping results from a previous interrupted run.
  # If found, resume from where we left off. This prevents re-scraping completed people.
  partial_bio_file <- data_path("nomination_bios_partial.csv")
  if (file.exists(partial_bio_file)) {
    message("  Found partial bio file. Loading and resuming...")
    bios_done <- read_csv(partial_bio_file, show_col_types = FALSE)
    remaining_ids <- setdiff(nom_people$person_id, bios_done$person_id)
    message(sprintf("  Already scraped %d. Remaining: %d",
                    nrow(bios_done), length(remaining_ids)))
  } else {
    bios_done <- data.frame()
    remaining_ids <- nom_people$person_id
  }

  if (length(remaining_ids) > 0) {
    n_workers <- max(1, parallel::detectCores() - 1)
    plan(multisession, workers = n_workers)
    message(sprintf("  Scraping %d person pages with %d workers...",
                    length(remaining_ids), n_workers))

    # Process in 500-person chunks to checkpoint progress and estimate time.
    # Saves partial_bio_file after each chunk (resumable).
    chunk_size <- 500
    chunks <- split(remaining_ids, ceiling(seq_along(remaining_ids) / chunk_size))

    t0 <- Sys.time()
    for (i in seq_along(chunks)) {
      chunk <- chunks[[i]]
      # Parallel scraping: each worker sleeps SLEEP_BETWEEN_DETAILS between requests
      chunk_results <- future_map(chunk, function(pid) {
        Sys.sleep(SLEEP_BETWEEN_DETAILS)
        scrape_person(pid)
      })

      chunk_df <- bind_rows(chunk_results)
      bios_done <- bind_rows(bios_done, chunk_df)
      # Save partial progress to enable resumption if interrupted
      write_csv(bios_done, partial_bio_file)

      # Status message with rate estimate and ETA
      elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
      rate <- nrow(bios_done) / elapsed
      remaining_est <- (length(remaining_ids) - nrow(bios_done)) / rate
      message(sprintf("  [%s] Bio scrape: %d / %d (%.0f%%) | %.0f/min | ~%.1f min left",
                      format(Sys.time(), "%H:%M:%S"),
                      nrow(bios_done), length(remaining_ids),
                      100 * nrow(bios_done) / length(remaining_ids),
                      rate, remaining_est))
    }
  }

  # Save as permanent cache and clean up temporary partial file
  write_csv(bios_done, bio_cache_file)
  if (file.exists(partial_bio_file)) file.remove(partial_bio_file)
  message(sprintf("  Saved bio cache: %s", bio_cache_file))
}

# Construct full name from components for Wikidata searching.
# Full name is required for Phase 1 name variant generation.
# Handles cases where firstname or lastname may be missing (uses available components).
bios <- bios_done %>%
  mutate(
    person_id = as.character(person_id),
    full_name = case_when(
      !is.na(firstname) & !is.na(lastname) ~ paste(firstname, lastname),
      !is.na(lastname) ~ lastname,  # Fallback: last name only if first missing
      TRUE ~ NA_character_  # Cannot construct name; will be unmatchable
    )
  )

# Summary statistics for downstream filtering
message(sprintf("  Bios for %d people", nrow(bios)))
message(sprintf("    With birth year: %d", sum(!is.na(bios$birth_year))))
message(sprintf("    With death year: %d", sum(!is.na(bios$death_year))))


# =============================================================================
# 3. MATCH PEOPLE TO WIKIDATA QIDS
#
# THREE-PHASE MATCHING ALGORITHM:
#
#   Phase 1 (Deterministic Parallel Search):
#     For each person with name + birth year, generate 1-3 name variants:
#       1. Full name as-is (captures all middle names, nicknames, etc.)
#       2. First word + last name (standard form, e.g., "Max Planck")
#       3. Last name only (catches unusual spellings or transliterations)
#     Search Wikidata for each variant in parallel (3 searches × ~2,600 people = ~7,800 calls).
#     Collect all unique candidate QIDs.
#
#   Phase 2 (Bulk Verification):
#     Deduplicate all candidate QIDs (~45K unique).
#     Batch-fetch birth years (Wikidata property P569) in 50-item batches (~900 API calls).
#     Match each person to candidates with EXACT birth year match.
#     Cache results: qid_birth_years (QID→year) and qids_no_birthyear (QIDs with no dob).
#
#   Phase 3 (Nondeterministic Refinement):
#     For remaining unmatched people (~10% typically), repeat full_name searches.
#     Wikidata search API returns results in nondeterministic order.
#     Each pass may surface different candidates → more matches possible.
#     Birth year verification is cached; only new QIDs need fetching.
#     Stop when a pass finds 0 new matches (convergence), or after MAX_EXTRA_PASSES.
#
# =============================================================================
message("\n=== Matching people to Wikidata QIDs ===\n")

WD_API <- "https://www.wikidata.org/w/api.php"
WD_UA  <- "NobelNetworkBot/1.0 (research)"

#' Generate name search variants for a person (PHASE 1 helper)
#'
#' Wikidata search is rank-based; exact matches appear first but variations help.
#' This function generates 1-3 search queries to maximize coverage.
#'
#' Examples:
#'   firstname="Albert Carl", lastname="Einstein" → c("Albert Carl Einstein", "Albert Einstein")
#'   firstname="Jean-Baptiste", lastname="Poquelin" → c("Jean-Baptiste Poquelin", "Jean-Baptiste Poquelin", "Poquelin")
#'   firstname=NA, lastname="Mozart" → c("Mozart")
#'
#' @param firstname First name(s), may contain middle names or be NA
#' @param lastname Last name
#' @return Character vector of 1-3 unique search variants (de-duplicated, sorted by specificity)
name_variants <- function(firstname, lastname) {
  variants <- character(0)

  # Variant 1: Full name as-is (concat all name components)
  full <- str_squish(paste(na.omit(c(firstname, lastname)), collapse = " "))
  if (nchar(full) >= 2) variants <- c(variants, full)

  # Variant 2: First word of firstname + lastname (standard form)
  # Handles cases with middle names: "Johann Sebastian Bach" → search also "Johann Bach"
  if (!is.na(firstname) && !is.na(lastname)) {
    first_word <- str_extract(str_squish(firstname), "^\\S+")
    short <- paste(first_word, lastname)
    if (!short %in% variants) variants <- c(variants, short)
  }

  # Variant 3: Last name only (fallback for unusual first names or transliterations)
  if (!is.na(lastname) && nchar(str_squish(lastname)) >= 2) {
    ln <- str_squish(lastname)
    if (!ln %in% variants) variants <- c(variants, ln)
  }

  unique(variants)
}

#' Search Wikidata entity search API for candidates (PHASE 1 helper)
#'
#' Uses wbsearchentities endpoint to find entities by name.
#' Returns up to WD_SEARCH_LIMIT candidates per query (typically 15).
#'
#' @param query Search string (e.g., "Albert Einstein")
#' @return Character vector of Wikidata QIDs (e.g., c("Q937", "Q123")) or empty if no matches
#'
#' @details Error handling: network errors return empty vector (logged). Allows
#'          continuation; if all search calls fail, no candidates found for that variant.
wd_search <- function(query) {
  # Query Wikidata wbsearchentities API
  resp <- tryCatch({
    request(WD_API) %>%
      req_url_query(
        action = "wbsearchentities",
        search = query,
        language = "en",
        type = "item",  # Entity type: item (not property, lexeme, etc.)
        limit = WD_SEARCH_LIMIT,  # Return up to 15 results
        format = "json"
      ) %>%
      req_headers(`User-Agent` = WD_UA) %>%
      req_perform()
  }, error = function(e) return(NULL))

  # Network error or API failure. Return empty; allows graceful continuation.
  if (is.null(resp)) return(character(0))

  # Extract QIDs from search results
  results <- resp_body_json(resp)
  vapply(results$search, function(c) c$id, character(1))
}

#' PHASE 1 HELPER: Collect all candidate QIDs for one person
#'
#' Generates 1-3 name variants and searches Wikidata for each.
#' Returns deduplicated union of all candidate QIDs.
#'
#' @param firstname Person's first name(s)
#' @param lastname Person's last name
#' @return Character vector of unique candidate QIDs (length 0–45)
#'
#' @details This runs in parallel (furrr::future_map). Each person typically
#'          generates 1-3 searches × 15 candidates = up to 45 candidates.
collect_candidates <- function(firstname, lastname) {
  variants <- name_variants(firstname, lastname)
  if (length(variants) == 0) return(character(0))

  all_candidates <- character(0)
  # Search each variant sequentially (within one person's processing)
  for (v in variants) {
    cands <- wd_search(v)
    all_candidates <- unique(c(all_candidates, cands))
    Sys.sleep(SLEEP_BETWEEN_WD)  # Rate limiting: respectful to Wikidata
  }

  all_candidates
}


# ============================================================================
# PRE-PHASE 1: Filter to people with sufficient data for matching
#
# Requirement: Both full_name and birth_year must be present. Birth year is
# essential for verification (our discriminator in Phase 2).
# People without birth year are marked "no_data" in final output.
# ============================================================================
matchable <- bios %>%
  filter(!is.na(full_name), !is.na(birth_year))

message(sprintf("  %d people have name + birth year (matchable)", nrow(matchable)))
message(sprintf("  %d people lack birth year (will not attempt matching)",
                sum(is.na(bios$birth_year) | is.na(bios$full_name))))

# ============================================================================
# CACHING FOR PHASES 1-3
#
# These RDS files store deterministic and semi-deterministic results that are
# reused across runs. This allows skipping expensive phases on re-runs:
#   - Phase 1 output is fully deterministic (same inputs → same candidates)
#   - Phase 2 output is deterministic (same QID set → same birth years)
#   - Phase 3 leverages Phase 2 cache; only searches need repeat
#
# Files:
#   phase1_cache_file: Named list, one entry per matchable person, contains
#                      candidate QIDs. Key: person_id. Value: character vector.
#   phase2_by_cache_file: Named list QID → birth_year (numeric). Built from
#                         Phase 2 bulk verification. Reused in Phase 3.
#   phase2_noby_cache_file: Character vector of QIDs verified to lack birth year.
#                           Prevents re-querying these in Phase 3.
# ============================================================================
phase1_cache_file <- data_path("wikidata_phase1_candidates.rds")
phase2_by_cache_file <- data_path("wikidata_birth_year_cache.rds")
phase2_noby_cache_file <- data_path("wikidata_no_birthyear_cache.rds")

# ============================================================================
# PHASE 1: PARALLEL CANDIDATE SEARCH (Deterministic)
#
# Goal: For each matchable person, generate name variants and search Wikidata.
#       Collect all unique candidate QIDs.
#
# Approach:
#   - Process matchable people in 200-person chunks
#   - Parallel execution via furrr::future_map2 (firstname, lastname → candidates)
#   - Each person's search runs in parallel; Sys.sleep(SLEEP_BETWEEN_WD) for rate limit
#   - Deduplicate and save to RDS cache (Phase 1 is deterministic—cached for reuse)
#
# Output: person_candidates = list of length nrow(matchable), each element is
#         a character vector of candidate QIDs (possibly empty).
# ============================================================================
if (file.exists(phase1_cache_file)) {
  message("\n--- Phase 1: Loading cached candidates ---")
  person_candidates <- readRDS(phase1_cache_file)
  message(sprintf("  Loaded %d cached candidate lists (%d have candidates).",
                  length(person_candidates),
                  sum(vapply(person_candidates, length, integer(1)) > 0)))
} else {
  # FIRST RUN: Execute Phase 1 searches
  n_workers <- max(1, parallel::detectCores() - 1)
  plan(multisession, workers = n_workers)

  message(sprintf("\n--- Phase 1: Collecting candidates for %d people (%d workers) ---",
                  nrow(matchable), n_workers))

  chunk_size <- 200
  chunks <- split(seq_len(nrow(matchable)),
                  ceiling(seq_len(nrow(matchable)) / chunk_size))

  # result: List of character vectors, one per person, containing their candidate QIDs
  person_candidates <- list()
  t0 <- Sys.time()

  for (i in seq_along(chunks)) {
    chunk_idx <- chunks[[i]]
    chunk_data <- matchable[chunk_idx, ]

    # Parallel search: each worker calls collect_candidates(firstname, lastname)
    chunk_cands <- future_map2(
      chunk_data$firstname, chunk_data$lastname,
      function(fn, ln) collect_candidates(fn, ln)
    )

    person_candidates <- c(person_candidates, chunk_cands)

    # Status with rate estimate
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    done <- length(person_candidates)
    rate <- if (elapsed > 0) done / elapsed else 0
    remaining_est <- if (rate > 0) (nrow(matchable) - done) / rate else NA
    n_with_cands <- sum(vapply(person_candidates, length, integer(1)) > 0)
    message(sprintf("  [%s] Phase 1: %d / %d (%.0f%%) | %.0f/min | ~%.1f min left | %d have candidates",
                    format(Sys.time(), "%H:%M:%S"),
                    done, nrow(matchable),
                    100 * done / nrow(matchable),
                    rate, remaining_est, n_with_cands))
  }

  # Save Phase 1 results for reuse on future runs
  saveRDS(person_candidates, phase1_cache_file)
  message(sprintf("  Saved Phase 1 cache: %s", phase1_cache_file))
}

# ============================================================================
# PHASE 2 HELPER: Bulk birth year verification
#
# Goal: Fetch birth years (Wikidata property P569) for all Phase 1 candidate QIDs.
#       Use bulk wbgetentities API calls for efficiency.
#
# Key design decisions:
#   - Batch QIDs in groups of 50 (wbgetentities limit is ~50 per request)
#   - Cache all successful lookups in qid_birth_years (QID → year)
#   - Track QIDs with no birth year in qids_no_birthyear (prevents re-querying)
#   - Global variables (<<-) for caching across function calls
#   - Retry logic (max_tries=3, backoff=2s) for transient network errors
#
# @param qids Character vector of QIDs to verify (may contain duplicates; internally deduplicated)
# @param verbose If TRUE, print progress messages after each 20 batches
#
# Side effects: Updates global qid_birth_years and qids_no_birthyear lists
# ============================================================================
qid_birth_years <- list()  # Global lookup: QID → birth year (numeric). Persists across calls.

verify_birth_years <- function(qids, verbose = TRUE) {
  # Dedup and filter: only query QIDs we haven't already verified
  new_qids <- setdiff(qids, names(qid_birth_years))
  # Also skip QIDs we've tried and confirmed have no birth year (optimization)
  new_qids <- setdiff(new_qids, qids_no_birthyear)
  if (length(new_qids) == 0) {
    if (verbose) message("  All QIDs already verified (cached). Skipping.")
    return(invisible(NULL))
  }

  # Batch in groups of 50 (Wikidata API limit per request)
  batches <- split(new_qids, ceiling(seq_along(new_qids) / 50))
  if (verbose) message(sprintf("  Verifying %d new QIDs in %d batches...",
                               length(new_qids), length(batches)))

  for (j in seq_along(batches)) {
    batch <- batches[[j]]
    ids_str <- paste(batch, collapse = "|")  # Pipe-separated QID list for API

    # Fetch entity data (claims) for this batch
    resp <- tryCatch({
      request(WD_API) %>%
        req_url_query(
          action = "wbgetentities",
          ids    = ids_str,
          props  = "claims",  # Only claims (not labels, descriptions, etc.)
          format = "json"
        ) %>%
        req_headers(`User-Agent` = WD_UA) %>%
        req_retry(max_tries = 3, backoff = ~ 2) %>%  # Retry transient failures
        req_perform()
    }, error = function(e) return(NULL))

    if (is.null(resp)) next  # Batch failed; skip to next

    # Parse response: entities are keyed by QID
    entities <- resp_body_json(resp)$entities
    for (qid in batch) {
      ent <- entities[[qid]]
      if (is.null(ent)) next  # QID not found in response

      # Extract birth date claims (P569 = "date of birth")
      dob_claims <- ent$claims$P569
      if (is.null(dob_claims) || length(dob_claims) == 0) {
        # QID has no birth date claim
        qids_no_birthyear <<- c(qids_no_birthyear, qid)
        next
      }

      # Extract date value from primary claim (may have precision, era, etc.)
      # Time format: "+2022-01-15T00:00:00Z" or similar
      dob_value <- tryCatch(
        dob_claims[[1]]$mainsnak$datavalue$value$time,
        error = function(e) NULL
      )
      if (is.null(dob_value)) {
        qids_no_birthyear <<- c(qids_no_birthyear, qid)
        next
      }

      # Extract 4-digit year from date string (handles +/- sign and format)
      wd_year <- as.numeric(str_extract(dob_value, "(?<=^[+-])\\d{4}"))
      if (!is.na(wd_year)) {
        qid_birth_years[[qid]] <<- wd_year  # Cache successful lookup
      } else {
        qids_no_birthyear <<- c(qids_no_birthyear, qid)
      }
    }

    # Progress message: every 20 batches or at end
    if (verbose && (j %% 20 == 0 || j == length(batches))) {
      message(sprintf("  [%s] Verified %d / %d batches (%.0f%%) | %d have birth years",
                      format(Sys.time(), "%H:%M:%S"),
                      j, length(batches),
                      100 * j / length(batches),
                      length(qid_birth_years)))
    }

    Sys.sleep(0.1)  # Small delay between batches (polite to Wikidata)
  }
}

qids_no_birthyear <- character(0)  # Global: QIDs checked but lacking birth year (prevents re-query)

# Load any cached birth year data from previous runs to avoid re-querying
if (file.exists(phase2_by_cache_file)) {
  qid_birth_years <- readRDS(phase2_by_cache_file)
  message(sprintf("  Loaded %d cached birth year lookups.", length(qid_birth_years)))
}
if (file.exists(phase2_noby_cache_file)) {
  qids_no_birthyear <- readRDS(phase2_noby_cache_file)
  message(sprintf("  Loaded %d cached no-birth-year QIDs.", length(qids_no_birthyear)))
}

# ============================================================================
# PHASE 2 HELPER: Match a person to candidates by exact birth year
#
# Goal: Given a person's candidate QIDs and their birth year, find which
#       candidates have matching birth years. Uses cached birth year lookups
#       from verify_birth_years().
#
# @param cands Character vector of candidate QIDs for this person
# @param birth_year Numeric birth year from nobelprize.org
# @return Character vector of matching QIDs (possibly empty, possibly multiple)
#
# Note: Uses EXACT match only. No fuzzy/approximate matching attempted.
# ============================================================================
match_person <- function(cands, birth_year) {
  matching <- character(0)
  for (qid in cands) {
    wd_by <- qid_birth_years[[qid]]  # Lookup from Phase 2 cache
    if (!is.null(wd_by) && identical(wd_by, as.numeric(birth_year))) {
      matching <- c(matching, qid)
    }
  }
  matching
}

# ============================================================================
# PHASE 2: BULK BIRTH YEAR VERIFICATION AND MATCHING (Deterministic)
#
# Goal: Verify birth years for all Phase 1 candidate QIDs, then match people
#       to candidates with exact birth year match.
#
# Approach:
#   1. Deduplicate all Phase 1 candidate QIDs (~45K unique candidates)
#   2. Batch-verify birth years (50-item batches, ~900 API calls)
#   3. For each matchable person, find candidates with matching birth year
#   4. Track multi-match cases (>1 candidate matched)
#   5. Preserve previously confirmed matches (from prior runs)
#
# Output: confirmed_matches = data frame with person_id, qid, multi_match_count
# ============================================================================
message("\n--- Phase 2: Verifying birth years (bulk, sequential) ---")

all_unique_qids <- unique(unlist(person_candidates))
message(sprintf("  %d unique candidate QIDs to verify", length(all_unique_qids)))
verify_birth_years(all_unique_qids)
message(sprintf("  %d / %d candidate QIDs have a birth year on Wikidata",
                length(qid_birth_years), length(all_unique_qids)))

# Save Phase 2 birth year caches for reuse (deterministic, won't change on re-run)
saveRDS(qid_birth_years, phase2_by_cache_file)
saveRDS(qids_no_birthyear, phase2_noby_cache_file)
message("  Saved Phase 2 birth year caches.")

# Load any previously confirmed matches (preserved across script runs).
# This allows resumable Phase 3 without losing matches from earlier phases.
previous_qids_file <- data_path("nomination_people_qids.csv")
confirmed_matches <- data.frame(
  person_id = character(0), qid = character(0),
  multi_match_count = integer(0), stringsAsFactors = FALSE
)
if (file.exists(previous_qids_file)) {
  prev_result <- read_csv(previous_qids_file, show_col_types = FALSE)
  prev_matched <- prev_result %>%
    filter(!is.na(qid)) %>%
    select(person_id, qid, multi_match_count) %>%
    mutate(person_id = as.character(person_id))
  confirmed_matches <- prev_matched
  message(sprintf("  Loaded %d previously confirmed matches.", nrow(confirmed_matches)))
}

# Match Phase 1 candidates to people by exact birth year
# (skip people already matched in previous runs or phases)
n_phase1_new <- 0

for (k in seq_len(nrow(matchable))) {
  pid <- matchable$person_id[k]
  if (pid %in% confirmed_matches$person_id) next  # Already matched; don't re-match

  cands <- person_candidates[[k]]
  if (length(cands) == 0) next  # No candidates for this person

  # Find which candidates have matching birth year
  matching_qids <- match_person(cands, matchable$birth_year[k])
  if (length(matching_qids) == 0) next  # No birth year match

  # Log multi-match cases (>1 QID matched; use API's top-ranked one)
  if (length(matching_qids) > 1) {
    message(sprintf("    MULTI-MATCH: '%s' (b. %d) matched %d QIDs: %s",
                    matchable$full_name[k], matchable$birth_year[k],
                    length(matching_qids),
                    paste(matching_qids, collapse = ", ")))
  }

  # Store match: person_id → QID (use first match if multiple)
  # multi_match_count records ambiguity for downstream review
  confirmed_matches <- bind_rows(confirmed_matches, data.frame(
    person_id = pid, qid = matching_qids[1],
    multi_match_count = length(matching_qids), stringsAsFactors = FALSE
  ))
  n_phase1_new <- n_phase1_new + 1
}

message(sprintf("\n  Phase 1+2: %d new matches (%d total including previous).",
                n_phase1_new, nrow(confirmed_matches)))

# ============================================================================
# PHASE 3: NONDETERMINISTIC REFINEMENT (Adaptive)
#
# Goal: Capture additional matches from remaining unmatched people via
#       repeated Wikidata searches. API returns nondeterministic results
#       (ranking varies), so each pass may surface new matches.
#
# Key design decisions:
#   - Search only full_name (1 query per person per pass, vs 3 in Phase 1)
#   - Repeat until convergence (0 new matches) or MAX_EXTRA_PASSES
#   - Reuse Phase 2 birth year cache—only new QIDs fetched
#   - No caching of Phase 3 results (nondeterministic; don't cache)
#
# Why this works:
#   - API ranking is a black box; names may surface in different orders
#   - Obscure people often have weak first-variant matches; retries help
#   - Caching prevents redundant Wikidata queries; very efficient
#
# Expected outcome: Usually converges in 5–15 passes, finding ~10% additional
#   matches after Phase 1+2. Some people may be fundamentally unmatchable
#   (too common name, weak search quality, API limits).
# ============================================================================
MAX_EXTRA_PASSES <- 100

for (pass in seq_len(MAX_EXTRA_PASSES)) {
  # Identify remaining unmatched people
  remaining <- matchable %>%
    filter(!person_id %in% confirmed_matches$person_id)

  if (nrow(remaining) == 0) {
    message(sprintf("  Pass %d: All matchable people matched. Done.", pass))
    break
  }

  message(sprintf("\n  --- Extra pass %d / %d: searching %d unmatched people ---",
                  pass, MAX_EXTRA_PASSES, nrow(remaining)))

  # Parallel search using full_name (single, cheap query per person)
  # Process in 200-person chunks
  pass_chunks <- split(seq_len(nrow(remaining)),
                       ceiling(seq_len(nrow(remaining)) / 200))

  pass_candidates <- list()
  t_pass <- Sys.time()

  for (ci in seq_along(pass_chunks)) {
    cidx <- pass_chunks[[ci]]
    cdata <- remaining[cidx, ]

    # Parallel search: full_name only (3x faster than Phase 1 multi-variant)
    chunk_cands <- future_map(cdata$full_name, function(nm) {
      if (is.na(nm) || nchar(str_squish(nm)) < 2) return(character(0))
      Sys.sleep(SLEEP_BETWEEN_WD)
      wd_search(nm)
    })

    pass_candidates <- c(pass_candidates, chunk_cands)

    # Status message
    message(sprintf("  [%s] Pass %d search: %d / %d (%.0f%%)",
                    format(Sys.time(), "%H:%M:%S"), pass,
                    length(pass_candidates), nrow(remaining),
                    100 * length(pass_candidates) / nrow(remaining)))
  }

  # Verify new QIDs: only ones we haven't seen in Phase 2
  # Deduplication: skip already-verified and no-birthyear QIDs
  new_qids <- setdiff(unique(unlist(pass_candidates)), names(qid_birth_years))
  new_qids <- setdiff(new_qids, qids_no_birthyear)
  if (length(new_qids) > 0) {
    message(sprintf("  %d new candidate QIDs to verify", length(new_qids)))
    verify_birth_years(new_qids)
  } else {
    message("  No new candidate QIDs (all cached)")
  }

  # Match remaining people to candidates by exact birth year
  pass_new <- 0
  for (r in seq_len(nrow(remaining))) {
    pid <- remaining$person_id[r]
    if (pid %in% confirmed_matches$person_id) next  # Already matched (edge case)

    cands <- pass_candidates[[r]]
    if (length(cands) == 0) next  # No candidates from this search

    # Check for birth year match
    matching_qids <- match_person(cands, remaining$birth_year[r])
    if (length(matching_qids) == 0) next  # No match

    # Log multi-match cases for manual review
    if (length(matching_qids) > 1) {
      message(sprintf("    MULTI-MATCH: '%s' (b. %d) matched %d QIDs: %s",
                      remaining$full_name[r], remaining$birth_year[r],
                      length(matching_qids),
                      paste(matching_qids, collapse = ", ")))
    }

    # Store match (use API's top-ranked if multiple)
    confirmed_matches <- bind_rows(confirmed_matches, data.frame(
      person_id = pid, qid = matching_qids[1],
      multi_match_count = length(matching_qids), stringsAsFactors = FALSE
    ))
    pass_new <- pass_new + 1
  }

  # Summary for this pass
  elapsed_pass <- as.numeric(difftime(Sys.time(), t_pass, units = "secs"))
  message(sprintf("  Pass %d: %d new matches (%.0fs). Total: %d.",
                  pass, pass_new, elapsed_pass, nrow(confirmed_matches)))

  # Convergence check: if this pass found nothing, stop iterating
  if (pass_new == 0) {
    message("  Converged — no new matches. Stopping extra passes.")
    break
  }
}

# Update Phase 2 caches with any new QIDs discovered during Phase 3
# (ensures resumability if script interrupted mid-Phase 3)
saveRDS(qid_birth_years, phase2_by_cache_file)
saveRDS(qids_no_birthyear, phase2_noby_cache_file)

# Final summary across all phases
message(sprintf("\n  All phases complete: %d total matches (%d with multiple QID candidates).",
                nrow(confirmed_matches),
                sum(confirmed_matches$multi_match_count > 1)))


# =============================================================================
# 4. COMBINE AND SAVE RESULTS
#
# Goal: Merge all data sources (scraped bios, confirmed matches) and produce
#       final output with match classifications.
#
# Process:
#   1. Start with all people (from bio scrape)
#   2. Left-join confirmed matches (QID, multi_match_count)
#   3. Standardize gender values (fix 6 junk values from nobelprize.org)
#   4. Classify match method: matched, unmatched, no_data
#   5. Save to CSV for downstream use (Step 06 demographics lookup)
#
# Output: nomination_people_qids.csv
#   Columns: person_id, name, gender, birth_year, death_year, qid, match_method,
#            multi_match_count
#
# Quality notes:
#   - Gender standardization: nobelprize.org has 6 junk values (<, §, -, N, J)
#     All are verifiably male (confirmed by first name analysis); corrected to "M".
#   - multi_match_count=0 for unmatched; >0 indicates ambiguous case (human review).
# =============================================================================
message("\n=== Saving results ===\n")

# Combine bio data with confirmed QID matches
result <- bios %>%
  select(person_id, full_name, gender, birth_year, death_year) %>%
  left_join(
    confirmed_matches %>% select(person_id, qid, multi_match_count),
    by = "person_id"
  ) %>%
  mutate(
    name = full_name,
    # DATA QUALITY: Standardize gender values
    # nobelprize.org source contains 6 junk gender values (<, §, -, N, J).
    # Manual verification (first names) confirms all are male: Zdenko Skraup,
    # Bartolomé Felíu, Edouard Grüneisen, Willem van Dijck, Herbert Fox, + 1 more.
    # Fix these by mapping to "M" (otherwise keep M/F or set NA).
    gender = case_when(
      toupper(gender) == "M" ~ "M",
      toupper(gender) == "F" ~ "F",
      gender %in% c("<", "§", "-", "N", "J") ~ "M",  # Junk values → M (verified)
      TRUE ~ NA_character_
    ),
    # Classify match method for tracking and analysis
    match_method = case_when(
      !is.na(qid) ~ "wikidata_name_birthyear",  # Successfully matched
      is.na(birth_year) | is.na(full_name) ~ "no_data",  # Insufficient data
      TRUE ~ "unmatched"  # Searched but no hit (even after Phase 3)
    ),
    # Fill NA multi_match_count with 0 (unmatched people)
    multi_match_count = replace_na(multi_match_count, 0L)
  ) %>%
  select(person_id, name, gender, birth_year, death_year, qid, match_method,
         multi_match_count)

# Save final results
write_csv(result, data_path("nomination_people_qids.csv"))

# Clean up any leftover temporary files (e.g., from interrupted runs)
partial_match_file <- data_path("nomination_qid_matches_partial.csv")
if (file.exists(partial_match_file)) file.remove(partial_match_file)

# =============================================================================
# FINAL SUMMARY AND REPORTING
# =============================================================================
n_matched <- sum(!is.na(result$qid))
n_multi   <- sum(result$multi_match_count > 1, na.rm = TRUE)

message(sprintf("\n=== DONE: %d / %d people matched to Wikidata QIDs (%.1f%%) ===",
                n_matched, nrow(result),
                100 * n_matched / nrow(result)))

# Breakdown by match method
message(sprintf("  Matched (name + birth year): %d",
                sum(result$match_method == "wikidata_name_birthyear", na.rm = TRUE)))
message(sprintf("  Unmatched (searched, no hit): %d",
                sum(result$match_method == "unmatched", na.rm = TRUE)))
message(sprintf("  No data (missing name/year):  %d",
                sum(result$match_method == "no_data", na.rm = TRUE)))
message(sprintf("  Multi-match cases (>1 QID):   %d", n_multi))

# Guidance for multi-match cases
if (n_multi > 0) {
  message("  (Multi-match cases used the API's top-ranked candidate. See MULTI-MATCH")
  message("   warnings above and multi_match_count column in output for human review.)")
}

message(sprintf("\nOutput: %s", data_path("nomination_people_qids.csv")))

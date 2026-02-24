# =============================================================================
# 05_match_nomination_qids.R
# Match nomination archive people to Wikidata QIDs
#
# The Nobel Prize nomination archive identifies people by internal IDs and
# names only. This script:
#   1. Extracts unique people from nominations.csv
#   2. Scrapes biographical info (name, birth/death year, gender) from each
#      person's detail page on nobelprize.org (cached after first run)
#   3. Searches Wikidata for candidate matches using multiple name variants
#      (full name, first+last, last-only) and verifies by birth year
#   4. Saves a mapping from nobelprize.org person_id → Wikidata QID
#
# Matching strategy (3 phases):
#   Phase 1 (deterministic): For each person, generate up to 3 name variants
#     (full name, first+last, last-only) and search Wikidata for each. Collect
#     all unique candidate QIDs. Run in parallel (search only, no verification).
#   Phase 2 (bulk verification): Deduplicate all candidate QIDs and batch-fetch
#     birth years from Wikidata. Match people to candidates by exact birth year.
#   Phase 3 (nondeterministic): For remaining unmatched people, repeat full_name
#     searches up to MAX_EXTRA_PASSES times. The Wikidata search API returns
#     candidates in varying order, so each pass may surface new matches. Birth
#     year verification is cached — only new QIDs need fetching. Stops when a
#     pass finds 0 new matches.
#
# Output: nomination_people_qids.csv with columns:
#   person_id, name, gender, birth_year, death_year, qid, match_method,
#   multi_match_count
# =============================================================================

source("Code/Data Gathering/00_utils.R")

# =============================================================================
# Configuration
# =============================================================================

SLEEP_BETWEEN_DETAILS <- 0.2  # per-worker delay for nobelprize.org
SLEEP_BETWEEN_WD      <- 0.2  # per-worker delay for Wikidata search calls
WD_SEARCH_LIMIT       <- 15   # candidates per search (3 variants × 15 = up to 45 unique)

# =============================================================================
# 1. Extract unique people from nominations.csv
# =============================================================================
message("=== Loading nomination archive people ===\n")

nom_file <- data_path("nominations.csv")
if (!file.exists(nom_file)) {
  stop("nominations.csv not found. Run 03 first.")
}

noms <- read_csv(nom_file, show_col_types = FALSE)

# Build unique people list from both nominee and nominator columns
nominees <- noms %>%
  select(person_id = nominee_person_id, name = nominee_name) %>%
  filter(!is.na(person_id))

nominators <- noms %>%
  select(person_id = nominator_person_id, name = nominator_name) %>%
  filter(!is.na(person_id))

nom_people <- bind_rows(nominees, nominators) %>%
  distinct(person_id, .keep_all = TRUE)

message(sprintf("  %d unique people to process", nrow(nom_people)))

# Save this mapping for downstream scripts
write_csv(nom_people, data_path("nomination_people.csv"))


# =============================================================================
# 2. Scrape biographical data from nobelprize.org person pages
#    This only needs to run once — results are cached permanently.
# =============================================================================

# Permanent cache file for bio data (NOT a partial file — kept across runs)
bio_cache_file <- data_path("nomination_bios.csv")

#' Scrape a single person detail page
#'
#' @param person_id The nobelprize.org person ID
#' @return A one-row data frame with person_id, lastname, firstname, gender,
#'         birth_year, death_year
scrape_person <- function(person_id) {
  url <- sprintf(
    "https://www.nobelprize.org/nomination/archive/show_people.php?id=%s",
    person_id
  )

  tryCatch({
    page <- read_html(url)
    tds <- page %>% html_elements("td") %>% html_text() %>% str_squish()

    # Parse label-value pairs from consecutive TDs
    get_field <- function(label) {
      idx <- which(tds == label)
      if (length(idx) > 0 && idx[1] < length(tds)) tds[idx[1] + 1]
      else NA_character_
    }

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
  # ---- Load cached bios ----
  message("\n=== Loading cached biographical data ===\n")
  bios_done <- read_csv(bio_cache_file, show_col_types = FALSE)

  # Check if any new people need scraping (e.g., nominations.csv was re-scraped)
  cached_ids <- as.character(bios_done$person_id)
  new_ids <- setdiff(as.character(nom_people$person_id), cached_ids)

  if (length(new_ids) > 0) {
    message(sprintf("  Cache has %d people. %d new people need scraping.",
                    nrow(bios_done), length(new_ids)))
    n_workers <- max(1, parallel::detectCores() - 1)
    plan(multisession, workers = n_workers)

    new_results <- future_map(new_ids, function(pid) {
      Sys.sleep(SLEEP_BETWEEN_DETAILS)
      scrape_person(pid)
    })

    bios_done <- bind_rows(bios_done, bind_rows(new_results))
    write_csv(bios_done, bio_cache_file)
    message(sprintf("  Updated cache: %d people total.", nrow(bios_done)))
  } else {
    message(sprintf("  Cache is complete: %d people. Skipping bio scrape.", nrow(bios_done)))
  }

} else {
  # ---- First run: scrape all bios ----
  message("\n=== Scraping person detail pages from nobelprize.org ===\n")

  # Check for partial scraping results from an interrupted first run
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

    chunk_size <- 500
    chunks <- split(remaining_ids, ceiling(seq_along(remaining_ids) / chunk_size))

    t0 <- Sys.time()
    for (i in seq_along(chunks)) {
      chunk <- chunks[[i]]
      chunk_results <- future_map(chunk, function(pid) {
        Sys.sleep(SLEEP_BETWEEN_DETAILS)
        scrape_person(pid)
      })

      chunk_df <- bind_rows(chunk_results)
      bios_done <- bind_rows(bios_done, chunk_df)
      write_csv(bios_done, partial_bio_file)

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

  # Save permanent cache and clean up partial file
  write_csv(bios_done, bio_cache_file)
  if (file.exists(partial_bio_file)) file.remove(partial_bio_file)
  message(sprintf("  Saved bio cache: %s", bio_cache_file))
}

# Build full name for matching
bios <- bios_done %>%
  mutate(
    person_id = as.character(person_id),
    full_name = case_when(
      !is.na(firstname) & !is.na(lastname) ~ paste(firstname, lastname),
      !is.na(lastname) ~ lastname,
      TRUE ~ NA_character_
    )
  )

message(sprintf("  Bios for %d people", nrow(bios)))
message(sprintf("    With birth year: %d", sum(!is.na(bios$birth_year))))
message(sprintf("    With death year: %d", sum(!is.na(bios$death_year))))


# =============================================================================
# 3. Match against Wikidata using multiple name variants + birth year
#    Two-phase approach: parallel search, then sequential bulk verification.
# =============================================================================
message("\n=== Matching people to Wikidata QIDs ===\n")

WD_API <- "https://www.wikidata.org/w/api.php"
WD_UA  <- "NobelNetworkBot/1.0 (research)"

#' Generate name search variants for a person
#'
#' Given first and last names, generate up to 3 search strings:
#'   1. Full name as-is (e.g. "Max Karl Ernst Ludwig Planck")
#'   2. First + Last only (e.g. "Max Planck") — if different from #1
#'   3. Last name only (e.g. "Planck") — wide net for unusual spellings
#'
#' @param firstname First name(s)
#' @param lastname Last name
#' @return Character vector of 1-3 unique search variants
name_variants <- function(firstname, lastname) {
  variants <- character(0)

  full <- str_squish(paste(na.omit(c(firstname, lastname)), collapse = " "))
  if (nchar(full) >= 2) variants <- c(variants, full)

  if (!is.na(firstname) && !is.na(lastname)) {
    first_word <- str_extract(str_squish(firstname), "^\\S+")
    short <- paste(first_word, lastname)
    if (!short %in% variants) variants <- c(variants, short)
  }

  if (!is.na(lastname) && nchar(str_squish(lastname)) >= 2) {
    ln <- str_squish(lastname)
    if (!ln %in% variants) variants <- c(variants, ln)
  }

  unique(variants)
}

#' Search Wikidata for candidates matching a single query string
#'
#' @param query Search string
#' @return Character vector of candidate QIDs (may be empty)
wd_search <- function(query) {
  resp <- tryCatch({
    request(WD_API) %>%
      req_url_query(
        action = "wbsearchentities",
        search = query,
        language = "en",
        type = "item",
        limit = WD_SEARCH_LIMIT,
        format = "json"
      ) %>%
      req_headers(`User-Agent` = WD_UA) %>%
      req_perform()
  }, error = function(e) return(NULL))

  if (is.null(resp)) return(character(0))

  results <- resp_body_json(resp)
  vapply(results$search, function(c) c$id, character(1))
}

#' PHASE 1: Collect candidate QIDs for one person (search only, no verification)
#'
#' @param firstname Person's first name(s)
#' @param lastname Person's last name
#' @return Character vector of unique candidate QIDs
collect_candidates <- function(firstname, lastname) {
  variants <- name_variants(firstname, lastname)
  if (length(variants) == 0) return(character(0))

  all_candidates <- character(0)
  for (v in variants) {
    cands <- wd_search(v)
    all_candidates <- unique(c(all_candidates, cands))
    Sys.sleep(SLEEP_BETWEEN_WD)
  }

  all_candidates
}


# Filter to people with enough info to match
matchable <- bios %>%
  filter(!is.na(full_name), !is.na(birth_year))

message(sprintf("  %d people have name + birth year (matchable)", nrow(matchable)))
message(sprintf("  %d people lack birth year (will not attempt matching)",
                sum(is.na(bios$birth_year) | is.na(bios$full_name))))

# ======================================================================
# Cache files for Phase 1 candidates and Phase 2 birth year lookups.
# These are deterministic — same results every run. Caching lets us skip
# directly to Phase 3 nondeterministic passes on re-runs.
# ======================================================================
phase1_cache_file <- data_path("wikidata_phase1_candidates.rds")
phase2_by_cache_file <- data_path("wikidata_birth_year_cache.rds")
phase2_noby_cache_file <- data_path("wikidata_no_birthyear_cache.rds")

# ======================================================================
# PHASE 1: Parallel candidate search (search API only — lightweight)
# ======================================================================
if (file.exists(phase1_cache_file)) {
  message("\n--- Phase 1: Loading cached candidates ---")
  person_candidates <- readRDS(phase1_cache_file)
  message(sprintf("  Loaded %d cached candidate lists (%d have candidates).",
                  length(person_candidates),
                  sum(vapply(person_candidates, length, integer(1)) > 0)))
} else {
  n_workers <- max(1, parallel::detectCores() - 1)
  plan(multisession, workers = n_workers)

  message(sprintf("\n--- Phase 1: Collecting candidates for %d people (%d workers) ---",
                  nrow(matchable), n_workers))

  chunk_size <- 200
  chunks <- split(seq_len(nrow(matchable)),
                  ceiling(seq_len(nrow(matchable)) / chunk_size))

  # List of character vectors: one per person, containing their candidate QIDs
  person_candidates <- list()
  t0 <- Sys.time()

  for (i in seq_along(chunks)) {
    chunk_idx <- chunks[[i]]
    chunk_data <- matchable[chunk_idx, ]

    chunk_cands <- future_map2(
      chunk_data$firstname, chunk_data$lastname,
      function(fn, ln) collect_candidates(fn, ln)
    )

    person_candidates <- c(person_candidates, chunk_cands)

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

  # Save Phase 1 cache
  saveRDS(person_candidates, phase1_cache_file)
  message(sprintf("  Saved Phase 1 cache: %s", phase1_cache_file))
}

# ======================================================================
# Helper: bulk birth-year verification (sequential, deduplicated)
# Given a set of QIDs, fetch their birth years from Wikidata and add to
# the global qid_birth_years lookup table. Skips QIDs already verified.
# ======================================================================
qid_birth_years <- list()  # Global lookup: QID → birth year (numeric)

verify_birth_years <- function(qids, verbose = TRUE) {
  # Only verify QIDs we haven't seen before
  new_qids <- setdiff(qids, names(qid_birth_years))
  # Also skip QIDs we've tried and found no birth year for
  new_qids <- setdiff(new_qids, qids_no_birthyear)
  if (length(new_qids) == 0) {
    if (verbose) message("  All QIDs already verified (cached). Skipping.")
    return(invisible(NULL))
  }

  batches <- split(new_qids, ceiling(seq_along(new_qids) / 50))
  if (verbose) message(sprintf("  Verifying %d new QIDs in %d batches...",
                               length(new_qids), length(batches)))

  for (j in seq_along(batches)) {
    batch <- batches[[j]]
    ids_str <- paste(batch, collapse = "|")

    resp <- tryCatch({
      request(WD_API) %>%
        req_url_query(
          action = "wbgetentities",
          ids    = ids_str,
          props  = "claims",
          format = "json"
        ) %>%
        req_headers(`User-Agent` = WD_UA) %>%
        req_retry(max_tries = 3, backoff = ~ 2) %>%
        req_perform()
    }, error = function(e) return(NULL))

    if (is.null(resp)) next

    entities <- resp_body_json(resp)$entities
    for (qid in batch) {
      ent <- entities[[qid]]
      if (is.null(ent)) next

      dob_claims <- ent$claims$P569
      if (is.null(dob_claims) || length(dob_claims) == 0) {
        qids_no_birthyear <<- c(qids_no_birthyear, qid)
        next
      }

      dob_value <- tryCatch(
        dob_claims[[1]]$mainsnak$datavalue$value$time,
        error = function(e) NULL
      )
      if (is.null(dob_value)) {
        qids_no_birthyear <<- c(qids_no_birthyear, qid)
        next
      }

      wd_year <- as.numeric(str_extract(dob_value, "(?<=^[+-])\\d{4}"))
      if (!is.na(wd_year)) {
        qid_birth_years[[qid]] <<- wd_year
      } else {
        qids_no_birthyear <<- c(qids_no_birthyear, qid)
      }
    }

    if (verbose && (j %% 20 == 0 || j == length(batches))) {
      message(sprintf("  [%s] Verified %d / %d batches (%.0f%%) | %d have birth years",
                      format(Sys.time(), "%H:%M:%S"),
                      j, length(batches),
                      100 * j / length(batches),
                      length(qid_birth_years)))
    }

    Sys.sleep(0.1)
  }
}

qids_no_birthyear <- character(0)  # QIDs we've checked that have no birth year

# Load any cached birth year data from previous runs
if (file.exists(phase2_by_cache_file)) {
  qid_birth_years <- readRDS(phase2_by_cache_file)
  message(sprintf("  Loaded %d cached birth year lookups.", length(qid_birth_years)))
}
if (file.exists(phase2_noby_cache_file)) {
  qids_no_birthyear <- readRDS(phase2_noby_cache_file)
  message(sprintf("  Loaded %d cached no-birth-year QIDs.", length(qids_no_birthyear)))
}

# ======================================================================
# Helper: given a person's candidates and birth year, find matching QIDs
# ======================================================================
match_person <- function(cands, birth_year) {
  matching <- character(0)
  for (qid in cands) {
    wd_by <- qid_birth_years[[qid]]
    if (!is.null(wd_by) && identical(wd_by, as.numeric(birth_year))) {
      matching <- c(matching, qid)
    }
  }
  matching
}

# ======================================================================
# PHASE 2: Verify Phase 1 candidates and match
# ======================================================================
message("\n--- Phase 2: Verifying birth years (bulk, sequential) ---")

all_unique_qids <- unique(unlist(person_candidates))
message(sprintf("  %d unique candidate QIDs to verify", length(all_unique_qids)))
verify_birth_years(all_unique_qids)
message(sprintf("  %d / %d candidate QIDs have a birth year on Wikidata",
                length(qid_birth_years), length(all_unique_qids)))

# Save Phase 2 birth year caches
saveRDS(qid_birth_years, phase2_by_cache_file)
saveRDS(qids_no_birthyear, phase2_noby_cache_file)
message("  Saved Phase 2 birth year caches.")

# Load any previously confirmed matches (preserved across runs)
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

# Match Phase 1 candidates (only for people not already matched)
n_phase1_new <- 0

for (k in seq_len(nrow(matchable))) {
  pid <- matchable$person_id[k]
  if (pid %in% confirmed_matches$person_id) next

  cands <- person_candidates[[k]]
  if (length(cands) == 0) next

  matching_qids <- match_person(cands, matchable$birth_year[k])
  if (length(matching_qids) == 0) next

  if (length(matching_qids) > 1) {
    message(sprintf("    MULTI-MATCH: '%s' (b. %d) matched %d QIDs: %s",
                    matchable$full_name[k], matchable$birth_year[k],
                    length(matching_qids),
                    paste(matching_qids, collapse = ", ")))
  }

  confirmed_matches <- bind_rows(confirmed_matches, data.frame(
    person_id = pid, qid = matching_qids[1],
    multi_match_count = length(matching_qids), stringsAsFactors = FALSE
  ))
  n_phase1_new <- n_phase1_new + 1
}

message(sprintf("\n  Phase 1+2: %d new matches (%d total including previous).",
                n_phase1_new, nrow(confirmed_matches)))

# ======================================================================
# PHASE 3: Nondeterministic passes on remaining unmatched people
#
# The Wikidata search API returns candidates in nondeterministic order.
# Each pass may surface different candidates. We repeat full_name searches
# (the cheapest query) on unmatched people until a pass finds 0 new matches
# or we hit MAX_EXTRA_PASSES. Only the search is repeated — birth year
# verification is deduplicated via the qid_birth_years cache.
# ======================================================================
MAX_EXTRA_PASSES <- 100

for (pass in seq_len(MAX_EXTRA_PASSES)) {
  # Who's still unmatched?
  remaining <- matchable %>%
    filter(!person_id %in% confirmed_matches$person_id)

  if (nrow(remaining) == 0) {
    message(sprintf("  Pass %d: All matchable people matched. Done.", pass))
    break
  }

  message(sprintf("\n  --- Extra pass %d / %d: searching %d unmatched people ---",
                  pass, MAX_EXTRA_PASSES, nrow(remaining)))

  # Parallel search using full_name (single query per person, fast)
  pass_chunks <- split(seq_len(nrow(remaining)),
                       ceiling(seq_len(nrow(remaining)) / 200))

  pass_candidates <- list()
  t_pass <- Sys.time()

  for (ci in seq_along(pass_chunks)) {
    cidx <- pass_chunks[[ci]]
    cdata <- remaining[cidx, ]

    chunk_cands <- future_map(cdata$full_name, function(nm) {
      if (is.na(nm) || nchar(str_squish(nm)) < 2) return(character(0))
      Sys.sleep(SLEEP_BETWEEN_WD)
      wd_search(nm)
    })

    pass_candidates <- c(pass_candidates, chunk_cands)

    message(sprintf("  [%s] Pass %d search: %d / %d (%.0f%%)",
                    format(Sys.time(), "%H:%M:%S"), pass,
                    length(pass_candidates), nrow(remaining),
                    100 * length(pass_candidates) / nrow(remaining)))
  }

  # Verify any new QIDs we haven't seen before
  new_qids <- setdiff(unique(unlist(pass_candidates)), names(qid_birth_years))
  new_qids <- setdiff(new_qids, qids_no_birthyear)
  if (length(new_qids) > 0) {
    message(sprintf("  %d new candidate QIDs to verify", length(new_qids)))
    verify_birth_years(new_qids)
  } else {
    message("  No new candidate QIDs (all cached)")
  }

  # Match
  pass_new <- 0
  for (r in seq_len(nrow(remaining))) {
    pid <- remaining$person_id[r]
    if (pid %in% confirmed_matches$person_id) next

    cands <- pass_candidates[[r]]
    if (length(cands) == 0) next

    matching_qids <- match_person(cands, remaining$birth_year[r])
    if (length(matching_qids) == 0) next

    if (length(matching_qids) > 1) {
      message(sprintf("    MULTI-MATCH: '%s' (b. %d) matched %d QIDs: %s",
                      remaining$full_name[r], remaining$birth_year[r],
                      length(matching_qids),
                      paste(matching_qids, collapse = ", ")))
    }

    confirmed_matches <- bind_rows(confirmed_matches, data.frame(
      person_id = pid, qid = matching_qids[1],
      multi_match_count = length(matching_qids), stringsAsFactors = FALSE
    ))
    pass_new <- pass_new + 1
  }

  elapsed_pass <- as.numeric(difftime(Sys.time(), t_pass, units = "secs"))
  message(sprintf("  Pass %d: %d new matches (%.0fs). Total: %d.",
                  pass, pass_new, elapsed_pass, nrow(confirmed_matches)))

  if (pass_new == 0) {
    message("  Converged — no new matches. Stopping extra passes.")
    break
  }
}

# Update Phase 2 caches with any new QIDs discovered during Phase 3
saveRDS(qid_birth_years, phase2_by_cache_file)
saveRDS(qids_no_birthyear, phase2_noby_cache_file)

message(sprintf("\n  All phases complete: %d total matches (%d with multiple QID candidates).",
                nrow(confirmed_matches),
                sum(confirmed_matches$multi_match_count > 1)))


# =============================================================================
# 4. Combine and save results
# =============================================================================
message("\n=== Saving results ===\n")

# Join bios with confirmed QID matches
result <- bios %>%
  select(person_id, full_name, gender, birth_year, death_year) %>%
  left_join(
    confirmed_matches %>% select(person_id, qid, multi_match_count),
    by = "person_id"
  ) %>%
  mutate(
    name = full_name,
    # Standardize gender: keep M/F, fix known junk values from nobelprize.org
    # 6 records have junk gender in the source database (<, §, -, N, J).
    # All are verifiably male based on first names (Zdenko Skraup, Bartolome
    # Felíu, Edouard Grüneisen, Willem van Dijck, Herbert Fox, + 1 more).
    gender = case_when(
      toupper(gender) == "M" ~ "M",
      toupper(gender) == "F" ~ "F",
      gender %in% c("<", "§", "-", "N", "J") ~ "M",
      TRUE ~ NA_character_
    ),
    match_method = case_when(
      !is.na(qid) ~ "wikidata_name_birthyear",
      is.na(birth_year) | is.na(full_name) ~ "no_data",
      TRUE ~ "unmatched"
    ),
    multi_match_count = replace_na(multi_match_count, 0L)
  ) %>%
  select(person_id, name, gender, birth_year, death_year, qid, match_method,
         multi_match_count)

write_csv(result, data_path("nomination_people_qids.csv"))

# Clean up any leftover partial files
partial_match_file <- data_path("nomination_qid_matches_partial.csv")
if (file.exists(partial_match_file)) file.remove(partial_match_file)

# Summary
n_matched <- sum(!is.na(result$qid))
n_multi   <- sum(result$multi_match_count > 1, na.rm = TRUE)
message(sprintf("\n=== DONE: %d / %d people matched to Wikidata QIDs (%.1f%%) ===",
                n_matched, nrow(result),
                100 * n_matched / nrow(result)))
message(sprintf("  Matched (name + birth year): %d",
                sum(result$match_method == "wikidata_name_birthyear", na.rm = TRUE)))
message(sprintf("  Unmatched (searched, no hit): %d",
                sum(result$match_method == "unmatched", na.rm = TRUE)))
message(sprintf("  No data (missing name/year):  %d",
                sum(result$match_method == "no_data", na.rm = TRUE)))
message(sprintf("  Multi-match cases (>1 QID):   %d", n_multi))
if (n_multi > 0) {
  message("  (Multi-match cases used the API's top-ranked candidate. See MULTI-MATCH")
  message("   warnings above and multi_match_count column in output for review.)")
}
message(sprintf("\nOutput: %s", data_path("nomination_people_qids.csv")))

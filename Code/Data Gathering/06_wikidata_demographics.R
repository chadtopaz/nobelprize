# =============================================================================
# 06_wikidata_demographics.R
#
# FILE NAME: 06_wikidata_demographics.R
# TITLE: Unified Wikidata Demographics Lookup for All Unique People
#
# AUTHOR: Chad M. Topaz
# LAST UPDATED: February 2025
#
# PURPOSE AND GOALS:
# This script performs unified demographic lookups on Wikidata for all unique
# people in the Nobel Prize network study: governing bodies (01), vetting bodies
# (02), laureates (04), and nomination archive people with QID matches (05).
#
# The script aggregates QIDs from all data sources, efficiently fetches
# demographics via batch SPARQL queries with fallback to individual lookups,
# and produces a single normalized demographics.csv for network analysis.
# Includes resume capability for long-running queries and post-processing
# (e.g., filling RSAS endyear from death_year).
#
# METHODOLOGICAL DECISIONS AND RATIONALE:
#
#   UNIFIED DEMOGRAPHIC LOOKUP:
#     All unique people in the network have QIDs (governing, vetting, laureates
#     from API; nomination people from Step 05 matching). This script fetches
#     demographics once and caches—prevents redundant Wikidata queries.
#     Output (demographics.csv) is normalized and deduplicated for downstream
#     network construction and analysis.
#
#   BATCH SPARQL STRATEGY:
#     Uses Wikidata SPARQL endpoint (wikibase:) for efficient bulk queries.
#     Batches QIDs in groups of 200 (safe limit for VALUES clause). Falls back
#     to individual wbgetentities lookups if batch fails (transient network
#     errors). Includes small delays (2s) between batches for rate limiting.
#
#   RESUME CAPABILITY:
#     Checks for partial demographics file on startup. If found, loads completed
#     QIDs and fetches only remaining ones. Saves partial after each batch.
#     Allows resumption if script interrupted (e.g., network timeout, memory issue).
#
#   DEMOGRAPHIC PROPERTIES FETCHED (Wikidata):
#     - P569: date of birth (birth_year extracted)
#     - P570: date of death (death_year extracted)
#     - P21: sex or gender (M/F standardization)
#     - P19: place of birth → country (birth_country)
#     - P27: country of citizenship (nationality, first value)
#     - P106: occupations (multiple values; collapsed to comma-separated)
#     - P937: work location (institution; multiple values collapsed)
#
#   POST-PROCESSING:
#     Collapses multi-valued properties (occupations, institutions) with
#     semicolon-separation. Fills RSAS (Royal Swedish Academy of Sciences)
#     endyear from death_year for governing bodies (Step 01).
#
#   PARTIAL RESULTS HANDLING:
#     Some QIDs may fail to return full demographics. SPARQL queries handle
#     missing properties gracefully (returns available fields). Individual
#     fallback lookups fetch what's available for QIDs that fail in batch.
#
# INPUTS:
#   - governing_bodies.csv (from 01): Organizations/people with QIDs
#   - vetting_bodies.csv (from 02): Committee members with QIDs
#   - laureates.csv (from 04): Nobel laureates with QIDs (from API)
#   - nomination_people_qids.csv (from 05): Matched nomination archive people
#   - Wikidata SPARQL endpoint: Bulk demographic queries
#
# OUTPUTS:
#   - demographics.csv: Normalized demographics for all unique people
#     Columns: qid, name, gender, birth_country, nationality, birth_year, death_year,
#     occupation, institution
#   - governing_bodies.csv (updated): RSAS endyear filled from death_year
#   - demographics_partial.csv (temporary): Resume checkpoint (deleted after completion)
#
# DEPENDENCIES:
#   - tidyverse (dplyr, readr): Data manipulation
#   - httr2: HTTP requests to Wikidata SPARQL endpoint
#   - 00_utils.R: Helper functions (data_path(), fetch_demographics_batch(),
#     fetch_demographics_for_qid(), collapse_demographics())
#
# KNOWN LIMITATIONS:
#   1. SPARQL batch size limit (200 QIDs) is safe but conservative. Could
#      potentially go higher (~300–500), but 200 ensures reliability.
#   2. Some Wikidata properties may be incomplete or unreliable (esp. occupations,
#      institutions). No data quality scoring; all returned values used as-is.
#   3. Gender field typically sparse on Wikidata; many missing or NULL values.
#   4. Birth country / nationality may differ; returns both, caller chooses.
#   5. Fallback to individual lookups is inefficient; included for robustness
#      against transient errors. May cause slow sections on unreliable networks.
#   6. RSAS endyear fill uses death_year; assumes RSAS members are identified
#      by death. Some may have resigned earlier (data quality limitation).
#
# PERFORMANCE AND CACHING NOTES:
#   - Full demographic lookup: ~5–10 minutes for ~5,000 unique QIDs
#     (~25 batches of 200, SPARQL queries efficient, 2s sleep between)
#   - Batch SPARQL queries: ~200–300ms per batch (network-dependent)
#   - Individual fallback queries (if batch fails): ~100ms per QID (slower)
#   - Resume-capable: Can interrupt and restart; checks partial_demo_file
#   - No persistent cache: Demographics refetched each run (SPARQL efficient)
#   - RSAS fill: <1 minute (batch join on demographics)
# =============================================================================

source("Code/Data Gathering/00_utils.R")

# =============================================================================
# 1. COLLECT ALL UNIQUE QIDs FROM ALL DATA SOURCES
#
# Goal: Gather QIDs from all four sources (governing bodies, vetting bodies,
#       laureates, and matched nomination archive people). Deduplicate to avoid
#       redundant Wikidata queries. This is the input set for demographic lookup.
#
# Process:
#   - Load each intermediate file (01–04 and 05 output)
#   - Extract QIDs from qid column
#   - Filter out NA (people without QID matches)
#   - Deduplicate across sources (same person in multiple roles)
#
# Quality notes:
#   - Governing/vetting bodies from 01–02: QIDs from Wikipedia matching
#   - Laureates from 04: QIDs from Nobel Prize API
#   - Nomination people from 05: QIDs from Wikidata entity resolution
#   - All assumed valid and unique per source
# =============================================================================
message("=== Collecting unique QIDs from all intermediate files ===\n")

qid_sources <- list()

# GOVERNING BODIES (from Step 01: Organizations and governing body members)
if (file.exists(data_path("governing_bodies.csv"))) {
  gb <- read_csv(data_path("governing_bodies.csv"), show_col_types = FALSE)
  qid_sources$governing <- gb %>%
    filter(!is.na(qid)) %>%
    pull(qid) %>%
    unique()
  message(sprintf("  Governing bodies: %d unique QIDs", length(qid_sources$governing)))
} else {
  message("  WARNING: governing_bodies.csv not found. Run 01_governing_bodies.R first.")
}

# VETTING BODIES (from Step 02: Committee members and vetting body organizations)
if (file.exists(data_path("vetting_bodies.csv"))) {
  vb <- read_csv(data_path("vetting_bodies.csv"), show_col_types = FALSE)
  qid_sources$vetting <- vb %>%
    filter(!is.na(qid)) %>%
    pull(qid) %>%
    unique()
  message(sprintf("  Vetting bodies: %d unique QIDs", length(qid_sources$vetting)))
} else {
  message("  WARNING: vetting_bodies.csv not found. Run 02_vetting_bodies.R first.")
}

# LAUREATES (from Step 04: Nobel laureates; QIDs from Nobel Prize API, not Wikidata)
if (file.exists(data_path("laureates.csv"))) {
  laur <- read_csv(data_path("laureates.csv"), show_col_types = FALSE)
  qid_sources$laureates <- laur %>%
    filter(!is.na(qid)) %>%
    pull(qid) %>%
    unique()
  message(sprintf("  Laureates: %d unique QIDs", length(qid_sources$laureates)))
} else {
  message("  WARNING: laureates.csv not found. Run 04_laureates.R first.")
}

# Deduplicate: combine all QIDs from all sources and remove duplicates
# (same person may appear in multiple roles)
all_qids <- unique(unlist(qid_sources))
message(sprintf("\n  Total unique QIDs across all sources: %d", length(all_qids)))


# =============================================================================
# 2. FETCH DEMOGRAPHICS FROM WIKIDATA
#
# Goal: Look up demographic properties for all unique QIDs using Wikidata SPARQL.
#       Includes batch fetching with fallback to individual queries, and resume
#       capability for long-running operations.
#
# Strategy:
#   - Use Wikidata SPARQL endpoint (fast bulk queries, ~200–300ms per batch)
#   - Batch size: 200 QIDs per query (safe limit for VALUES clause)
#   - Check for partial results file; resume from last completed QID if found
#   - Fallback: If batch fails, try individual lookups via wbgetentities
#   - Save progress after each batch (resume-capable)
#   - Small delay (2s) between batches for rate limiting
#
# Properties fetched (Wikidata):
#   - P569: date of birth (birth_year extracted)
#   - P570: date of death (death_year extracted)
#   - P21: sex or gender (typically M/F)
#   - P19: place of birth (country extracted)
#   - P27: citizenship/nationality
#   - P106: occupations (professional roles)
#   - P937: work locations (institutions)
#
# Resilience:
#   - Transient Wikidata errors: return empty (logged as warning)
#   - Individual fallback: slower but handles some batch failures
#   - Partial file: allows resumption without loss of completed QIDs
# =============================================================================
message("\n=== Fetching demographics from Wikidata ===\n")

# Wikidata SPARQL query size limit. VALUES clause supports ~200 QIDs safely.
# Can go higher (300–500), but 200 is conservative and reliable.
BATCH_SIZE <- 200

# Split all QIDs into batches
batches <- split(all_qids, ceiling(seq_along(all_qids) / BATCH_SIZE))
message(sprintf("  Processing %d QIDs in %d batches of up to %d...",
                length(all_qids), length(batches), BATCH_SIZE))

# Check for partial results from interrupted run. If found, resume from checkpoint.
partial_demo_file <- data_path("demographics_partial.csv")
if (file.exists(partial_demo_file)) {
  message("  Found partial demographics file. Loading and resuming...")
  demo_partial <- read_csv(partial_demo_file, show_col_types = FALSE)
  completed_qids <- unique(demo_partial$qid)
  message(sprintf("  Already have demographics for %d QIDs.", length(completed_qids)))

  # Filter batches to only remaining QIDs (skip completed)
  remaining_qids <- setdiff(all_qids, completed_qids)
  batches <- split(remaining_qids, ceiling(seq_along(remaining_qids) / BATCH_SIZE))
  # Start with partial results; append new results to this list
  all_demo_raw <- list(demo_partial)
} else {
  # Fresh run: no previous partial file
  all_demo_raw <- list()
}

# BATCH DEMOGRAPHIC FETCH LOOP
# Process each batch with error recovery and resume capability
t0 <- Sys.time()
for (i in seq_along(batches)) {
  batch_qids <- batches[[i]]

  # Try batch SPARQL query first (fast, efficient)
  result <- tryCatch({
    fetch_demographics_batch(batch_qids)
  }, error = function(e) {
    # Batch failed (network error, timeout, etc.). Fall back to individual lookups.
    # Slower but provides robustness against transient failures.
    message(sprintf("  Batch %d failed: %s. Trying individual lookups...", i, e$message))

    # Fetch each QID individually; collect non-NULL results
    individual_results <- lapply(batch_qids, function(qid) {
      tryCatch({
        fetch_demographics_for_qid(qid)
      }, error = function(e2) {
        # Individual lookup also failed; skip this QID (logged)
        message(sprintf("  Failed for %s: %s", qid, e2$message))
        NULL
      })
    })
    # Combine results, filtering out NULL entries
    bind_rows(individual_results[!sapply(individual_results, is.null)])
  })

  # Save batch results (if any) to in-memory list and partial file
  if (!is.null(result) && nrow(result) > 0) {
    all_demo_raw <- c(all_demo_raw, list(result))

    # Write partial checkpoint: combine all results seen so far
    partial <- bind_rows(all_demo_raw)
    write_csv(partial, partial_demo_file)
  }

  # Status message with elapsed time
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  message(sprintf("  [%s] Demographics: batch %d / %d (%.0f%%) | %.1f min elapsed",
                  format(Sys.time(), "%H:%M:%S"),
                  i, length(batches),
                  100 * i / length(batches), elapsed))

  # Rate limiting: polite delay between batch requests to Wikidata
  # Prevents overwhelming the server and helps avoid throttling
  Sys.sleep(2)
}


# =============================================================================
# 3. COLLAPSE AND CLEAN DEMOGRAPHICS
#
# Goal: Normalize multi-valued properties, standardize formats, and deduplicate.
#
# Post-processing (in collapse_demographics() helper):
#   - Collapse occupations: multiple Wikidata values → semicolon-separated string
#   - Collapse institutions: multiple values → semicolon-separated string
#   - Standardize gender: normalize to M/F or NA
#   - Extract country from place of birth (complex Wikidata structure)
#   - Extract year from date values (handle precision, era, BC/AD)
#   - Deduplicate rows (same QID may appear multiple times from SPARQL)
#
# Result: Clean, normalized demographics table ready for downstream analysis
# =============================================================================
message("\n=== Collapsing and cleaning demographics ===")

demo_raw <- bind_rows(all_demo_raw)
demographics <- collapse_demographics(demo_raw)

message(sprintf("  Demographics retrieved for %d / %d QIDs",
                nrow(demographics), length(all_qids)))


# =============================================================================
# 4. INCLUDE NOMINATION ARCHIVE PEOPLE WITH QIDs (From Step 05)
#
# Goal: Add demographics for people matched to Wikidata in Step 05
#       (nomination archive QID resolution).
#
# Logic:
#   - Load nomination_people_qids.csv output from Step 05
#   - Filter to people with QID matches (!is.na(qid))
#   - Check if any new QIDs not already in all_qids (from steps 01–04)
#   - If new QIDs found, fetch their demographics in batches
#   - Re-collapse to include nomination archive people
#
# Note: Nomination people may also appear in governing/vetting bodies
#       (same QID, different roles). Deduplication handles this.
# =============================================================================
message("\n=== Adding nomination archive people with matched QIDs ===")

nom_qid_file <- data_path("nomination_people_qids.csv")
if (file.exists(nom_qid_file)) {
  nom_matched <- read_csv(nom_qid_file, show_col_types = FALSE) %>%
    filter(!is.na(qid))  # Only people with QID matches

  # Identify new QIDs (not already in Step 01–04 sources)
  new_qids <- setdiff(nom_matched$qid, all_qids)
  if (length(new_qids) > 0) {
    message(sprintf("  %d new QIDs from nomination archive to fetch demographics for",
                    length(new_qids)))

    # Fetch demographics for new nomination QIDs in batches
    new_batches <- split(new_qids, ceiling(seq_along(new_qids) / BATCH_SIZE))
    for (i in seq_along(new_batches)) {
      result <- tryCatch({
        fetch_demographics_batch(new_batches[[i]])
      }, error = function(e) {
        message(sprintf("  WARNING: Batch failed: %s", e$message))
        NULL
      })
      if (!is.null(result) && nrow(result) > 0) {
        all_demo_raw <- c(all_demo_raw, list(result))
      }
      Sys.sleep(2)  # Rate limiting
    }
    # Add new QIDs to master list (for final summary)
    all_qids <- c(all_qids, new_qids)
  }
  message(sprintf("  %d nomination archive people have Wikidata QIDs", nrow(nom_matched)))
} else {
  message("  WARNING: nomination_people_qids.csv not found. Run 05 first.")
  message("  Nomination archive people will not be included in demographics.")
}

# Re-collapse demographics: combine all results including nomination archive people
message("\n=== Re-collapsing demographics with nomination people ===")
demo_raw <- bind_rows(all_demo_raw)
demographics <- collapse_demographics(demo_raw)
message(sprintf("  Demographics now cover %d unique QIDs", nrow(demographics)))


# =============================================================================
# 5. FILL MISSING RSAS ENDYEAR FROM DEATH_YEAR
#
# Goal: Post-process governing_bodies.csv to infer end years for RSAS
#       (Royal Swedish Academy of Sciences) members with missing endyear.
#
# Logic:
#   - Royal Swedish Academy of Sciences (RSAS) members may not have recorded
#     endyear (cessation of membership). Assume member tenure ended at death.
#   - Look up death_year from demographics for RSAS members with missing endyear
#   - Fill endyear = death_year where applicable
#   - Update and re-save governing_bodies.csv
#
# Caveat: This assumes RSAS members left the academy at death. Some may have
#         resigned or retired earlier. This is a data quality limitation of
#         the source (governing_bodies).
# =============================================================================
message("\n=== Filling RSAS endyear from death_year ===")

if (file.exists(data_path("governing_bodies.csv"))) {
  gb <- read_csv(data_path("governing_bodies.csv"), show_col_types = FALSE)

  # Identify RSAS members with missing endyear
  rsas_mask <- gb$body == "RSAS" & is.na(gb$endyear)
  n_missing <- sum(rsas_mask, na.rm = TRUE)

  if (n_missing > 0) {
    # Left-join demographics to get death_year for RSAS members
    gb_updated <- gb %>%
      left_join(demographics %>% select(qid, death_year), by = "qid") %>%
      mutate(
        # Fill endyear: use death_year if RSAS member and endyear missing
        endyear = ifelse(body == "RSAS" & is.na(endyear) & !is.na(death_year),
                         death_year, endyear)
      ) %>%
      select(-death_year)  # Remove temporary death_year column

    # Count how many values were successfully filled
    n_filled <- sum(rsas_mask, na.rm = TRUE) -
      sum(gb_updated$body == "RSAS" & is.na(gb_updated$endyear), na.rm = TRUE)

    # Update the governing_bodies.csv file
    write_csv(gb_updated, data_path("governing_bodies.csv"))
    message(sprintf("  Filled %d / %d missing RSAS endyear values from death_year",
                    n_filled, n_missing))
  } else {
    message("  No missing RSAS endyear values to fill.")
  }
}


# =============================================================================
# FINAL SAVE AND CLEANUP
#
# Goal: Write final demographics.csv, clean up temporary files, and report.
#
# Output file: demographics.csv
#   Columns: qid, name, gender, birth_country, nationality, birth_year, death_year,
#            occupation, institution
#   Rows: One per unique QID with available demographics
#
# Cleanup: Remove demographics_partial.csv (temporary checkpoint file)
#
# Summary: Print final statistics (record count, gender breakdown)
# =============================================================================
write_csv(demographics, data_path("demographics.csv"))

# Remove temporary partial checkpoint file (no longer needed)
if (file.exists(partial_demo_file)) {
  file.remove(partial_demo_file)
}

# Final summary
message(sprintf("\n=== DONE: %d demographic records saved to %s ===",
                nrow(demographics), data_path("demographics.csv")))

# Gender distribution summary (useful for data quality assessment)
message(sprintf("  Gender breakdown:"))
demographics %>%
  count(gender) %>%
  mutate(msg = sprintf("    %s: %d", coalesce(gender, "unknown"), n)) %>%
  pull(msg) %>%
  walk(message)

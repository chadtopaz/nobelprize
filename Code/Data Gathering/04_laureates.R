# =============================================================================
# 04_laureates.R
# Gather all Nobel Prize laureates from the Nobel Prize API v2.1
# =============================================================================
#
# AUTHOR:       Chad M. Topaz
# LAST UPDATED: February 2025
#
# PURPOSE AND GOALS
# ================================================================================
# This script fetches comprehensive Nobel Prize laureate data from the official
# Nobel Prize API (https://api.nobelprize.org/2.1) and processes it into a
# structured dataset for network analysis. This dataset forms the "laureate layer"
# of the multilayer network in "Geographic Homophily in the Nobel Prize Selection
# Network," capturing all individuals and organizations who have received a Nobel
# Prize across the 5 original Nobel Prize categories.
#
# The API provides direct access to Wikidata QIDs (Wikidata identifiers), which
# are essential for linking laureates to external datasets and semantic knowledge
# bases. This avoids the fragile and time-consuming Wikipedia → Wikidata lookup
# pipeline used in historical nomination data processing. Instead, we directly
# extract and store QIDs from the API response.
#
# The script focuses exclusively on the 5 original Nobel Prizes (Physics,
# Chemistry, Physiology/Medicine, Literature, Peace) established by Nobel's will
# in 1901. The Economics Prize (officially "Sveriges Riksbank Prize in Economic
# Sciences in Memory of Alfred Nobel," established 1969) is intentionally excluded
# from this analysis to maintain historical consistency with the original 5 prizes.
#
# METHODOLOGICAL DECISIONS AND DATA SOURCE RATIONALE
# ================================================================================
# API Selection (Nobel Prize API v2.1):
#   - Official, authoritative source maintained by the Nobel Prize Foundation
#   - Provides complete laureate roster with rich structured metadata
#   - Includes Wikidata QIDs directly (no external lookup required)
#   - Uses pagination (offset/limit) for efficient data retrieval
#   - Stable schema with error handling for network issues
#   - Free public access, no authentication required
#
# Pagination Strategy:
#   - API uses offset/limit pagination (not cursor-based)
#   - Default page size is 25 laureates; we use this to minimize requests
#   - Loop until offset >= total count
#   - Between-page delays (0.5 sec) for server politeness
#   - Retry logic: on failure, sleep 5 sec and retry once per offset
#
# Nested JSON Structure Processing:
#   - Each laureate record contains a "nobelPrizes" array (1+ prizes per person)
#   - Each prize entry contains "affiliations" array (1+ affiliations per prize)
#   - We use jsonlite::fromJSON(..., flatten=TRUE) for initial flattening
#   - Further processing via bind_rows() and list traversal handles remaining nesting
#   - Edge case handling: persons without prizes are skipped
#
# Wikidata QID Extraction:
#   - Available directly in API response as "wikidata.id" field
#   - Eliminates need for expensive Wikipedia lookup or fuzzy name matching
#   - All laureates are linked to Wikidata, enabling semantic analysis
#   - QIDs form the backbone of cross-dataset integration (e.g., birthplace,
#     nationality, co-author networks)
#
# Prize Filtering:
#   - The 5 original prizes: Physics, Chemistry, Physiology or Medicine, Literature, Peace
#   - Economics excluded (added 1969, outside original Nobel vision)
#   - API category names are mapped to our canonical names via PRIZE_MAP
#   - Records with missing or unrecognized prize categories are skipped
#
# Organization vs. Individual Distinction:
#   - The API includes both individual laureates and organizational awardees
#   - Gender field is populated for individuals; often NA for organizations
#   - All records are retained in the output (not filtered by gender)
#   - Network analysis can optionally filter to individuals via gender field
#
# INPUTS (Data Sources and APIs)
# ================================================================================
# Primary Source: Nobel Prize API v2.1
#   - Base URL: https://api.nobelprize.org/2.1
#   - Endpoint: /laureates
#   - Query parameters: offset (pagination start), limit (page size, default 25)
#   - Authentication: None required (public API)
#   - Data freshness: Updated annually with new laureates
#
# API Response Schema (relevant fields):
#   - meta.count: Total number of laureates in database
#   - laureates: Array of laureate objects
#   - Per laureate:
#     * id: Internal laureate ID
#     * fullName.en: Full name in English
#     * knownName.en: Common/known name if different
#     * gender: "male", "female", or NA for organizations
#     * wikidata.id: QID (Wikidata identifier, e.g., "Q123")
#     * nobelPrizes: Array of prize records
#   - Per prize record:
#     * category.en: Prize name (e.g., "Physics", "Physiology or Medicine")
#     * awardYear: Year of award (numeric)
#     * portion: Fractional share (e.g., "1/2", "1/3")
#     * motivation.en: Award citation in English
#     * affiliations: Array of affiliation objects
#   - Per affiliation:
#     * name.en: Affiliation name (institution, organization)
#     * city.en: City name
#     * country.en: Country name
#
# Dependencies: 00_utils.R (provides data_path() function for output location)
#
# OUTPUTS (Files and Schema)
# ================================================================================
# Primary Output: laureates.csv (written to data_path("laureates.csv"))
#
# Schema (8 columns):
#   1. qid (character): Wikidata QID (e.g., "Q1234567") or NA for unlinked records
#   2. name (character): Laureate's full or known name in English
#   3. gender (character): "male", "female", or NA for organizations/unclear
#   4. year (numeric): Year of award
#   5. prize (character): Standardized prize name ("Physics", "Chemistry",
#                        "Physiology/Medicine", "Literature", or "Peace")
#   6. portion (character): Fractional share of prize (e.g., "1", "1/2", "1/3")
#   7. motivation (character): Award citation (motivation/justification)
#   8. affiliation (character): Primary affiliation at time of award (institution name)
#
# Row Count: One row per laureate-prize combination. Most individuals have 1 row,
#           but some (e.g., Marie Curie) have multiple rows (multiple prizes).
#           For 1901-present across 5 original prizes, typically 800-900 rows.
#
# DEPENDENCIES
# ================================================================================
# Required R Libraries:
#   - jsonlite (JSON parsing: fromJSON, flatten)
#   - dplyr (data manipulation: mutate, filter, arrange, bind_rows, count)
#   - purrr (functional programming: map, pull, walk)
#   - readr (CSV I/O: write_csv)
#
# Sourced Files:
#   - Code/Data Gathering/00_utils.R (provides data_path() function)
#
# KNOWN LIMITATIONS AND GAPS
# ================================================================================
# 1. Economics Prize Excluded:
#    The Nobel Prize in Economic Sciences ("Sveriges Riksbank Prize") is
#    intentionally excluded because it was established in 1969, well after the
#    original 5 prizes. This maintains historical consistency with our analysis
#    of the original Nobel vision.
#
# 2. Multiple Affiliations:
#    The API may list multiple affiliations per laureate-prize combination
#    (e.g., research university + award-presenting institution). We retain only
#    the first (primary) affiliation. A full analysis of affiliation networks
#    would require storing all affiliations, which is beyond the scope of this script.
#
# 3. Incomplete Motivations:
#    Some older records (particularly early 20th century) have sparse or
#    abbreviated motivations in English. This reflects historical documentation
#    practices rather than a data collection issue.
#
# 4. Name Variations:
#    Some laureates are known by different names across years or sources.
#    The API uses full formal names; historical common names may differ.
#    Fuzzy matching with external sources may be necessary for disambiguation.
#
# 5. Organizations vs. Individuals:
#    The Peace Prize includes organizational awardees (e.g., Doctors Without
#    Borders). These lack gender information and have organization-level QIDs.
#    Network analysis using this data should consider filtering by gender
#    when focusing on individual networks.
#
# 6. Missing Wikidata Links:
#    Most laureates have QIDs, but a small number (particularly organizations
#    or very early historical figures) may not. These appear as NA in the qid column.
#
# PERFORMANCE NOTES
# ================================================================================
# Execution Speed:
#   - Full API fetch (paginated, all laureates): ~1-2 minutes
#   - JSON processing and flattening: ~30-60 seconds
#   - Total wall-clock time: 2-3 minutes
#   - No parallelization needed (API is the bottleneck, not computation)
#
# Network Politeness:
#   - Between-page delays: 0.5 seconds
#   - No parallel requests to API
#   - If a request fails, we retry after 5-second backoff
#
# Data Volume:
#   - API contains ~900-1000 laureate records (as of 2025)
#   - 5 original prizes: ~1000-1100 laureate-prize rows
#   - CSV output: ~100-150 KB
#
# =============================================================================

source("Code/Data Gathering/00_utils.R")
library(jsonlite)      # JSON parsing: fromJSON with flatten option
library(dplyr)         # Data manipulation: mutate, filter, arrange, bind_rows, count
library(purrr)         # Functional programming: map, pull, walk
library(readr)         # CSV I/O: write_csv

# =============================================================================
# Configuration
# =============================================================================

# === API ENDPOINT ===
# Nobel Prize API v2.1 is the official API maintained by the Nobel Foundation.
# It provides authoritative data on all Nobel Prize recipients and awards.
# No authentication required; public access.
API_BASE <- "https://api.nobelprize.org/2.1"

# === PRIZE NAME STANDARDIZATION ===
# The API may return prize category names in different formats across versions
# and data sources. This map ensures consistent naming across our analysis:
#   - "Physics" → "Physics"
#   - "Chemistry" → "Chemistry"
#   - "Physiology or Medicine" → "Physiology/Medicine" (forward slash for consistency)
#   - "Literature" → "Literature"
#   - "Peace" → "Peace"
#
# Note: "Economic Sciences" is intentionally NOT included in this map.
# Unrecognized categories (including Economics) will be filtered out later.
PRIZE_MAP <- c(
  "Physics"                  = "Physics",
  "Chemistry"                = "Chemistry",
  "Physiology or Medicine"   = "Physiology/Medicine",
  "Literature"               = "Literature",
  "Peace"                    = "Peace"
)

# =============================================================================
# Fetch all laureates from the API with pagination
# =============================================================================
message("=== Fetching laureates from Nobel Prize API v2.1 ===\n")

# === PAGINATION LOOP ===
# The API uses offset/limit pagination. We fetch pages until offset >= total count.
# - offset: Starting index for this page
# - limit: Number of records per page (25 is API default; balances requests vs. payload)
# - total: Total count fetched from response$meta$count on first iteration
#
all_laureates_raw <- list()
offset <- 0
limit <- 25  # API default and reasonable page size
total <- NULL

repeat {
  # Build URL with pagination parameters
  url <- sprintf("%s/laureates?offset=%d&limit=%d", API_BASE, offset, limit)

  # === FETCH WITH RETRY LOGIC ===
  # Attempt to fetch and parse JSON. On failure:
  #   1. Log warning with offset (useful for debugging)
  #   2. Sleep 5 seconds (backoff for rate limiting or server issues)
  #   3. Retry once without the sleep
  # If retry also fails, stop processing (return NULL).
  response <- tryCatch({
    fromJSON(url, flatten = TRUE)
  }, error = function(e) {
    message(sprintf("  WARNING: API request failed at offset=%d: %s", offset, e$message))
    Sys.sleep(5)
    tryCatch(fromJSON(url, flatten = TRUE), error = function(e2) NULL)
  })

  if (is.null(response)) {
    message("  ERROR: Failed after retry. Stopping.")
    break
  }

  # On first successful response, extract total count from API metadata
  if (is.null(total)) {
    total <- response$meta$count
    message(sprintf("  Total laureates in API: %d", total))
  }

  # Accumulate laureates from this page into a list
  # (We'll combine all pages into a single data frame later)
  all_laureates_raw <- c(all_laureates_raw, list(response$laureates))
  offset <- offset + limit

  # Log progress every page with time and percentage
  message(sprintf("  [%s] Fetched %d / %d laureates (%.0f%%)",
                  format(Sys.time(), "%H:%M:%S"),
                  min(offset, total), total,
                  100 * min(offset, total) / total))

  # Check if we've fetched all records
  if (offset >= total) break

  # Brief delay between requests (server politeness)
  Sys.sleep(0.5)
}

# === COMBINE PAGINATED RESULTS ===
# Convert the list of page results into a single data frame
laureates_raw <- bind_rows(all_laureates_raw)
message(sprintf("  Fetched %d laureate records", nrow(laureates_raw)))


# =============================================================================
# Process laureate data and unnest nested JSON structures
# =============================================================================
message("\n=== Processing laureate records ===")

# === NESTED DATA STRUCTURE HANDLING ===
# The API returns deeply nested JSON:
#   - Each laureate has a "nobelPrizes" array (1+ prizes per person)
#   - Each prize has an "affiliations" array (1+ organizations)
# After fromJSON(..., flatten=TRUE), some nesting remains that requires
# manual recursion and bind_rows() to fully unnest.
#
# Some laureates have multiple Nobel Prizes (rare but important, e.g., Marie Curie).
# We create one output row per laureate-prize combination, not per person.
#
# This process_laureate() function handles:
#   - Extracting person-level fields (name, gender, QID)
#   - Iterating through each prize in the person's record
#   - Filtering to only the 5 original prizes (rejecting Economics)
#   - Extracting prize-specific fields (year, portion, motivation, affiliation)
#   - Error handling for missing/malformed data
#
process_laureate <- function(row_idx) {
  row <- laureates_raw[row_idx, ]

  # === EXTRACT PERSON-LEVEL FIELDS ===
  # These fields are shared across all prizes for this person.

  # Get Wikidata QID: direct link to Wikidata for semantic integration
  qid <- tryCatch({
    wikidata_id <- row$wikidata.id
    if (is.null(wikidata_id) || is.na(wikidata_id)) NA_character_
    else wikidata_id
  }, error = function(e) NA_character_)

  # Get name: prefer full formal name; fall back to known name
  name <- tryCatch({
    fn <- row$fullName.en
    if (is.null(fn) || is.na(fn)) {
      kn <- row$knownName.en
      if (is.null(kn) || is.na(kn)) NA_character_
      else kn
    } else fn
  }, error = function(e) NA_character_)

  # Get gender: indicates individual vs. organizational awardee
  # Organizations typically have NA for gender
  gender <- tryCatch({
    g <- row$gender
    if (is.null(g) || is.na(g)) NA_character_ else g
  }, error = function(e) NA_character_)

  # === PROCESS PRIZES FOR THIS LAUREATE ===
  # Extract the nobelPrizes array (may contain 1 or more prize records)
  prizes <- row$nobelPrizes
  if (is.null(prizes) || length(prizes) == 0) return(data.frame())

  # Handle different nesting levels: prizes might be a data frame or list
  # depending on flatten=TRUE interaction with the API response structure
  if (is.data.frame(prizes)) {
    prize_df <- prizes
  } else if (is.list(prizes)) {
    # If it's a list of lists/data frames, bind them into a single data frame
    prize_df <- tryCatch(bind_rows(prizes), error = function(e) data.frame())
  } else {
    return(data.frame())
  }

  if (nrow(prize_df) == 0) return(data.frame())

  # === PROCESS EACH PRIZE ===
  # For each prize entry in this laureate's record, extract fields and build an output row
  results <- lapply(seq_len(nrow(prize_df)), function(p_idx) {
    p <- prize_df[p_idx, ]

    # Extract prize category name
    category <- tryCatch({
      cat_en <- p$category.en
      if (is.null(cat_en) || is.na(cat_en)) NA_character_ else cat_en
    }, error = function(e) NA_character_)

    # === FILTER TO 5 ORIGINAL PRIZES ===
    # Skip Economics (added 1969) and any other non-standard prizes.
    # A category is in PRIZE_MAP if it's one of the 5 original prizes.
    if (is.na(category) || !(category %in% names(PRIZE_MAP))) return(NULL)

    # Extract prize-specific fields
    year <- tryCatch(as.numeric(p$awardYear), error = function(e) NA_real_)

    # Portion: fractional share (e.g., "1", "1/2", "1/3")
    portion <- tryCatch({
      por <- p$portion
      if (is.null(por)) NA_character_ else por
    }, error = function(e) NA_character_)

    # Motivation: the award citation/justification
    motivation <- tryCatch({
      mot <- p$motivation.en
      if (is.null(mot) || is.na(mot)) NA_character_ else mot
    }, error = function(e) NA_character_)

    # === EXTRACT PRIMARY AFFILIATION ===
    # The API may list multiple affiliations per prize (e.g., research institution,
    # award-presenting institution). We retain only the first (primary) one.
    # Affiliations are stored in a nested array that may be a data frame or list.
    affiliation <- tryCatch({
      affs <- p$affiliations
      if (is.null(affs) || length(affs) == 0) {
        NA_character_
      } else if (is.data.frame(affs)) {
        # If affs is already a data frame, extract the first name
        affs$name.en[1]
      } else if (is.list(affs) && length(affs) > 0) {
        # If affs is a list, extract the first element (may be a data frame)
        if (is.data.frame(affs[[1]])) affs[[1]]$name.en[1]
        else NA_character_
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)

    # Build output row for this laureate-prize combination
    data.frame(
      qid = qid,
      name = name,
      gender = gender,
      year = year,
      prize = PRIZE_MAP[category],   # Remap to standard prize name
      portion = portion,
      motivation = motivation,
      affiliation = affiliation,
      stringsAsFactors = FALSE
    )
  })

  # Combine all prize rows for this laureate, filtering out NULL entries
  # (NULL entries come from rejected non-standard prizes)
  bind_rows(results[!sapply(results, is.null)])
}

# === APPLY PROCESSING FUNCTION TO ALL LAUREATES ===
message("  Processing laureate records...")
laureates_list <- map(seq_len(nrow(laureates_raw)), process_laureate,
                      .progress = "Processing laureates")
laureates <- bind_rows(laureates_list)

# === FINAL FILTERING AND ORDERING ===
# Remove any rows with missing year or prize (shouldn't happen given our checks,
# but this is a safety measure). Then sort by prize, year, and name for readability.
laureates <- laureates %>%
  filter(!is.na(year), !is.na(prize)) %>%
  arrange(prize, year, name)

# === SUMMARY BY PRIZE ===
# Display counts by prize to verify distribution across all 5 original categories
message(sprintf("\n  Laureates by prize:"))
laureates %>%
  count(prize) %>%
  mutate(msg = sprintf("    %s: %d", prize, n)) %>%
  pull(msg) %>%
  walk(message)


# =============================================================================
# Save output and report final statistics
# =============================================================================

# === WRITE FINAL OUTPUT ===
# Save the processed laureate data to CSV for use in downstream network analysis
write_csv(laureates, data_path("laureates.csv"))

# === FINAL SUMMARY STATISTICS ===
# Report key metrics about the dataset:
#   - Total rows (laureate-prize combinations)
#   - Unique individuals (by QID)
#   - Year range covered
#   - Coverage of Wikidata links
message(sprintf("\n=== DONE: %d laureate-prize records saved to %s ===",
                nrow(laureates), data_path("laureates.csv")))
message(sprintf("  Unique individuals: %d", n_distinct(laureates$qid)))
message(sprintf("  Year range: %d–%d", min(laureates$year), max(laureates$year)))
message(sprintf("  Records with QIDs: %d / %d",
                sum(!is.na(laureates$qid)), nrow(laureates)))

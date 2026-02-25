# =============================================================================
# 03_nominators_nominees.R
# Scrape Nobel Prize Nomination Archive for nominator-nominee relationships
# =============================================================================
#
# AUTHOR:       Chad M. Topaz
# LAST UPDATED: February 2025
#
# PURPOSE AND GOALS
# ================================================================================
# This script harvests structural and contextual data on Nobel Prize nomination
# relationships from the publicly accessible Nobel Prize nomination archive
# (https://www.nobelprize.org/nomination/archive/). The archive data is released
# under a 50-year secrecy rule, making historical nomination records progressively
# available as time passes. This dataset is fundamental to the multilayer network
# analysis in "Geographic Homophily in the Nobel Prize Selection Network,"
# capturing the nominator→nominee directed relationships that form the nomination
# layer of the network.
#
# The script extracts both structural data (person identities, IDs) and contextual
# metadata (university affiliation, city, country, profession) as they appeared
# in the nomination form at the time of nomination. This temporal and contextual
# information is crucial for network analysis and understanding the geographic
# clustering patterns in the nomination process.
#
# METHODOLOGICAL DECISIONS AND DATA SOURCE RATIONALE
# ================================================================================
# Archive Coverage (as of 2025):
#   - Physics, Chemistry, Literature, Peace: Available through 1975 (50-year rule)
#   - Physiology/Medicine: Limited to 1953
#     * Karolinska Institute (custodian of Med/Phys records) has not released
#       additional years, likely due to the Swedish Public Access to Information
#       and Secrecy Act (Offentlighets- och sekretesslagen), reformed in 1977.
#       The 1977 reforms also led to the establishment of the Nobel Assembly.
#
# Two-Pass Architecture (Sequential Lists → Parallel Details):
#   - Pass 1: Sequentially fetch list pages (one URL per prize-year) to extract
#     nomination IDs via CSS selector parsing. These list pages are lightweight.
#   - Pass 2: In parallel, fetch detail pages (one URL per nomination ID) using
#     ncores-1 workers. Detail pages contain full nominee/nominator information
#     and contextual fields. Parallelization significantly reduces total wall-clock
#     time while respecting politeness by adding per-worker delays.
#
# DOM-Based Parsing for Robust Extraction:
#   - The nomination detail pages (show.php) use a table-based HTML structure
#     without semantic markup (no data attributes, JSON-LD, etc.).
#   - We walk all <tr> elements in document order, identifying section headers
#     ("Nominee:" vs. "Nominator:") via <b> bold tags, then collecting labeled
#     fields (University, City, Country, Profession) from <span class="rubr">
#     elements. This approach is robust to minor HTML formatting changes while
#     remaining sensitive to intentional structural changes.
#   - Person links (show_people.php) are used to extract nobelprize.org person IDs.
#     When person links are missing, we use the text value as the name.
#
# Cross-Nomination Expansion:
#   - When a single nomination has multiple nominees or multiple nominators,
#     we create all possible pairs (Cartesian product) to represent each
#     distinct nominator→nominee relationship. This is semantically correct
#     because each nominator's statement nominated all listed nominees.
#
# Resume Capability via Partial File:
#   - The script checks for a partial results file (nominations_partial.csv)
#     at startup and resumes from where it left off, tracking completion by
#     prize-year keys. This is essential for long-running scraping tasks,
#     allowing recovery from network interruptions or API changes.
#   - Schema validation ensures outdated partial files are discarded rather
#     than causing silent column mismatches with bind_rows().
#
# INPUTS (Data Sources and APIs)
# ================================================================================
# Primary Source: https://www.nobelprize.org/nomination/archive/
#   - List pages: nomination/archive/list.php?prize={code}&year={year}
#     * HTML parsed to extract nomination IDs
#     * No API key required; publicly accessible
#   - Detail pages: nomination/archive/show.php?id={nomination_id}
#     * HTML parsed to extract nominee/nominator information and contexts
#     * No API key required; publicly accessible
#
# Data Gathering Configuration:
#   - PRIZES map: Prize names to numeric codes (1=Physics, 2=Chemistry, etc.)
#   - YEAR_RANGES: Per-prize year ranges based on 50-year secrecy rule
#   - SLEEP_BETWEEN_PAGES: 0.5 second delay between list page requests
#   - SLEEP_BETWEEN_DETAILS: 0.2 second delay per worker for detail requests
#
# Dependencies: 00_utils.R (provides data_path() function for output location)
#
# OUTPUTS (Files and Schema)
# ================================================================================
# Primary Output: nominations.csv (written to data_path("nominations.csv"))
#
# Schema (14 columns):
#   1. nomination_id (character): Unique identifier from nobelprize.org
#   2. year (numeric): Year of nomination
#   3. prize (character): Standardized prize name ("Physics", "Chemistry",
#                        "Physiology/Medicine", "Literature", or "Peace")
#   4. nominee_name (character): Nominee's name as listed in the nomination archive
#   5. nominee_person_id (character): nobelprize.org person ID for nominee
#                                     (NA if not linkable via detail page)
#   6. nominee_university (character): Institution affiliation at time of nomination
#   7. nominee_city (character): City of nominee's institution
#   8. nominee_country (character): Country of nominee's institution (ISO code stripped)
#   9. nominee_profession (character): Stated profession/position at nomination time
#  10. nominator_name (character): Nominator's name
#  11. nominator_person_id (character): nobelprize.org person ID for nominator
#                                       (often NA; not all nominators have profiles)
#  12. nominator_university (character): Institution affiliation at time of nomination
#  13. nominator_city (character): City of nominator's institution
#  14. nominator_country (character): Country of nominator's institution (ISO code stripped)
#  15. nominator_profession (character): Stated profession/position at nomination time
#
# Row Count: One row per nominator-nominee pair (Cartesian product when nominations
#           have multiple nominees and/or multiple nominators). For 1901-1975 physics,
#           chemistry, literature, and peace, plus 1901-1953 physiology/medicine,
#           typically 2000-4000+ rows depending on nomination density.
#
# DEPENDENCIES
# ================================================================================
# Required R Libraries:
#   - httr (HTTP requests; implicit via read_html from rvest)
#   - rvest (HTML parsing: read_html, html_elements, html_attr, html_text)
#   - dplyr (data manipulation: mutate, select, bind_rows, distinct)
#   - stringr (string processing: str_extract, str_detect, str_trim, str_remove, etc.)
#   - furrr & future (parallelization: future_map, plan, multisession)
#   - readr (CSV I/O: read_csv, write_csv)
#
# Sourced Files:
#   - Code/Data Gathering/00_utils.R (provides data_path() function)
#
# KNOWN LIMITATIONS AND GAPS
# ================================================================================
# 1. Medizin/Physiology Coverage Gap:
#    Only data through 1953 for Physiology/Medicine. This is a structural limitation
#    of data availability, not a scraping issue. Post-1953 nominations for this
#    prize are unavailable.
#
# 2. Partial Linkage to nobelprize.org Profiles:
#    Not all nominators appear in nobelprize.org's person database. person_id will
#    be NA for these individuals. This is expected; nominators are not necessarily
#    public figures with formal profiles.
#
# 3. HTML Structure Sensitivity:
#    The DOM-based parsing assumes the specific <tr>/<td>/<span class="rubr">
#    structure of nomination detail pages. Significant changes to the website
#    structure would require parser updates.
#
# 4. Missing or Incomplete Contextual Data:
#    University, city, country, and profession fields may be NA or empty strings
#    for some nominees/nominators. This reflects missing or incomplete information
#    in the original nomination forms.
#
# 5. Encoding and Special Characters:
#    Names and text fields are scraped as-is from HTML. Some historical records
#    may have inconsistent encoding or unusual character representations.
#
# PERFORMANCE NOTES
# ================================================================================
# Parallelization:
#   - List pages (one per prize-year) are fetched sequentially to avoid overwhelming
#     the server with rapid requests. Typical overhead: ~375 requests for the full
#     historical range.
#   - Detail pages (one per nomination, typically 2000-4000 for full history) are
#     fetched in parallel using ncores-1 workers. This typically reduces total
#     execution time from ~30-40 minutes (sequential) to ~5-10 minutes (parallel
#     with 8 cores), depending on network latency and server responsiveness.
#   - Per-worker delays (SLEEP_BETWEEN_DETAILS = 0.2 sec) ensure politeness to
#     the nobelprize.org server even under parallelization.
#
# Resume Capability:
#   - Partial results are written after each prize-year is completed, allowing
#     recovery without re-scraping completed years.
#   - Schema validation prevents silent failures when the column structure changes
#     across script iterations.
#
# Typical Execution Timeline:
#   - Full scrape (all available nominations 1901-1975, all 5 prizes): ~5-10 minutes
#   - Incremental update (new year only): ~30 seconds
#   - Resume from crash mid-year: ~3-5 minutes (depends on progress point)
#
# =============================================================================

source("Code/Data Gathering/00_utils.R")
library(httr)

# Load required libraries for web scraping and data wrangling
library(rvest)      # HTML parsing (read_html, html_elements, etc.)
library(dplyr)      # Data manipulation (mutate, bind_rows, etc.)
library(stringr)    # String operations (str_extract, str_detect, etc.)
library(furrr)      # Parallel map functions (future_map)
library(future)     # Parallel backend (plan, multisession)
library(readr)      # CSV I/O (read_csv, write_csv)

# =============================================================================
# Configuration
# =============================================================================

# PRIZES: Map human-readable prize names to numeric codes used in nobelprize.org URLs.
# These codes are consistent across the nomination archive list.php interface.
# Economics (code 6) is intentionally excluded because it was not part of the
# original Nobel Prizes and the nomination archive does not extend back to 1901
# for Economics (first Nobel Prize in Economics was 1969).
PRIZES <- c(
  "Physics"    = 1,
  "Chemistry"  = 2,
  "Medicine"   = 3,
  "Literature" = 4,
  "Peace"      = 5
)

# Year ranges per prize (based on data availability as of 2025)
# The 50-year secrecy rule means new years become eligible annually.
# Update these ranges as the archive is extended.
YEAR_RANGES <- list(
  Physics    = 1901:1975,
  Chemistry  = 1901:1975,
  Medicine   = 1901:1953,   # Karolinska has not released beyond 1953
  Literature = 1901:1975,
  Peace      = 1901:1975
)

# Scraping settings
SLEEP_BETWEEN_PAGES <- 0.5   # seconds between list-page requests (be polite)
SLEEP_BETWEEN_DETAILS <- 0.2 # seconds between detail page requests per worker

# =============================================================================
# Helper functions
# =============================================================================

#' Get list of nomination IDs for a given prize and year
#'
#' Scrapes the list page at nobelprize.org/nomination/archive/list.php
#' to extract nomination IDs (the ?id= parameter in "Show" links).
#'
#' The list page displays a table of nominations for a given prize-year combination.
#' Each nomination in the table has a "Show" link with href="show.php?id=XXXXX".
#' We extract the numeric ID from these links using CSS selectors and regex.
#'
#' Error handling is graceful: if the list page fails to load (network error,
#' 404, or malformed HTML), we log a warning and return an empty vector,
#' allowing the main loop to continue with other prize-years.
#'
#' @param prize_code Numeric prize code (1=Physics, 2=Chemistry, etc.)
#' @param year Nomination year
#' @return A character vector of nomination IDs, or empty vector if page fails
get_nomination_ids <- function(prize_code, year) {
  url <- sprintf(
    "https://www.nobelprize.org/nomination/archive/list.php?prize=%d&year=%d",
    prize_code, year
  )

  tryCatch({
    page <- read_html(url)

    # CSS selector: a[href*='show.php?id='] matches all <a> tags with href
    # containing "show.php?id=". This targets only the "Show" links on the list.
    # html_attr("href") extracts the full href value (e.g., "show.php?id=12345").
    # str_extract("(?<=id=)\\d+") uses a positive lookbehind to extract only
    # the numeric ID portion after "id=".
    ids <- page %>%
      html_elements("a[href*='show.php?id=']") %>%
      html_attr("href") %>%
      str_extract("(?<=id=)\\d+")

    # Remove NA values that may arise if regex fails to match
    ids[!is.na(ids)]
  }, error = function(e) {
    # Log a warning (not fatal) and return empty vector to allow graceful recovery
    message(sprintf("    WARNING: Failed to get list for prize=%d, year=%d: %s",
                    prize_code, year, e$message))
    character(0)
  })
}


#' Scrape a single nomination detail page
#'
#' Extracts nominee and nominator information from the show.php detail page,
#' including contextual fields: university, city, country, and profession.
#' Each nomination may have multiple nominees and/or multiple nominators.
#'
#' Uses a purely DOM-based approach: walks through all <tr> elements in document
#' order, detects "Nominee:" and "Nominator:" section headers from <b> tags,
#' identifies person links (show_people.php), and collects labeled fields
#' (University, City, Country, Profession) from <span class="rubr"> labels.
#' Each person block runs from a Name row to the next section header or
#' next person Name row.
#'
#' The detail page has a consistent table-based structure (historically stable
#' across 100+ years of nominees/nominators). Key HTML patterns:
#'   - Section headers: <tr><td colspan="2"><b>Nominee:</b> or Nominator:</td></tr>
#'   - Year/Prize info: scattered across <td colspan="2"> and <td> text content
#'   - Person info: <tr> with <td><span class="rubr">Label:</span></td><td>Value</td>
#'   - Person links: <a href="show_people.php?id=XXXXX"> with person ID and name
#'
#' Edge cases handled:
#'   - Multiple nominees: all paired with all nominators (Cartesian product)
#'   - Multiple nominators: same pairing logic
#'   - Missing person links: use text value as name, ID = NA
#'   - Missing contextual fields: store as NA_character_ (not empty string)
#'   - Malformed HTML: graceful recovery with empty data frame return
#'
#' @param nomination_id The nomination ID from the archive
#' @return A data frame with one row per nominator-nominee pair, or empty data frame if scraping fails
scrape_nomination <- function(nomination_id) {
  url <- sprintf(
    "https://www.nobelprize.org/nomination/archive/show.php?id=%s",
    nomination_id
  )

  tryCatch({
    page <- read_html(url)

    # === EXTRACT YEAR AND PRIZE ===
    # The page header contains the year (as a 4-digit number in a <td>)
    # and the prize name (in a <td colspan="2"> with "Nomination for Nobel Prize in X").
    header <- page %>%
      html_elements("td[colspan='2']") %>%
      html_text()

    # Find the first cell containing a 4-digit year
    td_texts <- page %>% html_elements("td") %>% html_text()
    year <- td_texts[which(str_detect(td_texts, "^\\d{4}$"))][1] %>%
      as.numeric()

    # Extract prize name from header. Handle two cases:
    #   1. Standard: "Nomination for Nobel Prize in Physics" → extract "Physics"
    #   2. Peace: "Nomination for Nobel Peace Prize" → use special case (no "in")
    # This special handling is necessary because Nobel Peace Prize uses a different
    # header format than the other four disciplines.
    nom_header <- header[str_detect(header, "Nomination for Nobel")][1]
    if (!is.na(nom_header) && str_detect(nom_header, "Nobel Peace Prize")) {
      prize <- "Peace"
    } else {
      prize <- str_extract(nom_header, "(?<=in ).*$")
    }

    # === DOM-BASED NOMINEE/NOMINATOR CLASSIFICATION ===
    # The detail page is structured as an HTML table with rows containing either:
    #   1. Section header: <tr><td colspan="2"><b>Nominee:</b> or Nominator:</td></tr>
    #      These mark transitions between nominee and nominator sections.
    #   2. Labeled field: <tr><td><span class="rubr">Label:</span></td><td>Value</td></tr>
    #      Labels include: Name, University, City, Country, Profession
    #   3. Other rows: Context (awards, prizes received, etc.) - ignored
    #
    # Algorithm: Walk all <tr> in document order, tracking:
    #   - section: "nominee", "nominator", or "unknown" (initial state)
    #   - current_person: accumulated data for the person being parsed
    # A "Name:" label signals the start of a new person block.
    # Subsequent University/City/Country/Profession labels populate current_person.
    # When we see a section header or EOF, we save the current_person to the appropriate list.
    #
    all_trs <- page %>% html_elements("tr")

    section <- "unknown"         # Current section ("nominee", "nominator", or unknown)
    nominees <- list()           # List of nominee data frames
    nominators <- list()         # List of nominator data frames
    current_person <- NULL       # Current person being accumulated

    # Helper function: save current_person to the appropriate list (nominees or nominators).
    # The save_person() function is defined here because it uses <<- to modify
    # the outer scope (nominees, nominators lists), which is necessary for the
    # loop to accumulate results. This is a closure pattern in R.
    save_person <- function() {
      if (!is.null(current_person) && current_person$section != "unknown") {
        entry <- data.frame(
          name = current_person$name,
          person_id = current_person$person_id,
          # %||% is the "null coalesce" operator (from rlang): use left if not NULL, else right
          university = current_person$university %||% NA_character_,
          city = current_person$city %||% NA_character_,
          country = current_person$country %||% NA_character_,
          profession = current_person$profession %||% NA_character_,
          stringsAsFactors = FALSE
        )
        if (current_person$section == "nominee") {
          nominees[[length(nominees) + 1]] <<- entry
        } else if (current_person$section == "nominator") {
          nominators[[length(nominators) + 1]] <<- entry
        }
      }
    }

    for (tr in all_trs) {
      tds <- tr %>% html_elements("td")

      # === CHECK FOR SECTION HEADERS ===
      # Section headers are <b> tags containing "Nominee:" or "Nominator:".
      # When we find one, save the current person and transition to the new section.
      bold_texts <- tr %>% html_elements("b") %>% html_text()
      if (length(bold_texts) > 0) {
        if (any(str_detect(bold_texts, regex("Nominee", ignore_case = TRUE)))) {
          save_person()
          current_person <- NULL
          section <- "nominee"
          next
        }
        if (any(str_detect(bold_texts, regex("Nominator", ignore_case = TRUE)))) {
          save_person()
          current_person <- NULL
          section <- "nominator"
          next
        }
      }

      # Skip rows before the first section header or rows with insufficient columns
      if (section == "unknown" || length(tds) < 2) next

      # === CHECK FOR LABELED FIELDS ===
      # Labeled fields have the pattern: <td><span class="rubr">Label:</span></td><td>Value</td>
      # The <span class="rubr"> is the label; the next <td> is the value.
      rubr <- tds[[1]] %>% html_elements("span.rubr") %>% html_text() %>% str_trim()
      if (length(rubr) == 0) next

      label <- rubr[1]
      value <- html_text(tds[[2]]) %>% str_trim()

      # === HANDLE NAME FIELD (START OF NEW PERSON BLOCK) ===
      # The Name field marks the beginning of a new person's data.
      # We save the previous person, then initialize current_person.
      if (str_detect(label, "^Name:")) {
        save_person()

        # Try to extract a person link from the Name field.
        # If present, the link has href="show_people.php?id=XXXXX" with the Nobel ID.
        # We extract the numeric ID and the linked text (person's name).
        # If the link is missing, use the cell's text value as the name.
        link <- tr %>% html_elements("a[href*='show_people.php']")
        if (length(link) > 0) {
          # str_extract("\\d+$") gets the digits at the end of the href
          pid   <- html_attr(link[[1]], "href") %>% str_extract("\\d+$")
          pname <- html_text(link[[1]]) %>% str_trim()
        } else {
          pid   <- NA_character_
          pname <- value
        }

        # Initialize the current_person list with this name and ID.
        # Other fields (university, city, country, profession) are NULL initially
        # and will be populated by subsequent rows.
        current_person <- list(
          section = section, name = pname, person_id = pid,
          university = NULL, city = NULL, country = NULL, profession = NULL
        )
        next
      }

      # === COLLECT CONTEXTUAL FIELDS ===
      # For the current person being accumulated, extract University, City, Country, Profession.
      # Only update the field if the value is non-empty and we have a current person.
      if (!is.null(current_person) && !is.na(value) && nchar(value) > 0) {
        if (str_detect(label, "^University:"))  current_person$university  <- value
        if (str_detect(label, "^City:"))        current_person$city        <- value
        if (str_detect(label, "^Country:"))     current_person$country     <- value
        if (str_detect(label, "^Profession:"))  current_person$profession  <- value
      }
    }

    # Save the last person accumulated (loop has ended)
    save_person()

    # === BUILD DATA FRAMES FROM ACCUMULATED LISTS ===
    # Convert the lists of individual nominees and nominators into data frames.
    # If no data exists (empty lists), create an empty data frame with the correct schema.
    nominees_df <- if (length(nominees) > 0) bind_rows(nominees) else
      data.frame(name = character(0), person_id = character(0),
                 university = character(0), city = character(0),
                 country = character(0), profession = character(0),
                 stringsAsFactors = FALSE)

    # For nominators, we default to one row of NA values if no nominators were found.
    # This is semantically correct: if we couldn't parse nominators, we still
    # represent the absence of nominator data (NA) rather than zero rows.
    # This is important for the Cartesian product below.
    nominators_df <- if (length(nominators) > 0) bind_rows(nominators) else
      data.frame(name = NA_character_, person_id = NA_character_,
                 university = NA_character_, city = NA_character_,
                 country = NA_character_, profession = NA_character_,
                 stringsAsFactors = FALSE)

    # Early exit: if there are no nominees, return empty data frame.
    # This prevents creating meaningless rows with all-NA nominees.
    if (nrow(nominees_df) == 0) {
      return(data.frame())
    }

    # === CREATE CARTESIAN PRODUCT: ALL NOMINEE-NOMINATOR PAIRS ===
    # A single nomination may list multiple nominees and/or multiple nominators.
    # Semantically, each nominator nominated each of the nominees listed on that form.
    # We represent this as a Cartesian product (all pairs) with one row per pair.
    # This ensures the network accurately represents the nomination relationships.
    #
    # For example:
    #   - Nominees: [Alice, Bob], Nominators: [Carol, Dave]
    #   - Creates pairs: (Carol→Alice), (Carol→Bob), (Dave→Alice), (Dave→Bob)
    #
    expand.grid(
      nominee_idx = seq_len(nrow(nominees_df)),
      nominator_idx = seq_len(nrow(nominators_df)),
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        nomination_id = nomination_id,
        year = year,
        prize = prize,
        # Populate nominee fields using indices into nominees_df
        nominee_name = nominees_df$name[nominee_idx],
        nominee_person_id = nominees_df$person_id[nominee_idx],
        nominee_university = nominees_df$university[nominee_idx],
        nominee_city = nominees_df$city[nominee_idx],
        nominee_country = nominees_df$country[nominee_idx],
        nominee_profession = nominees_df$profession[nominee_idx],
        # Populate nominator fields using indices into nominators_df
        nominator_name = nominators_df$name[nominator_idx],
        nominator_person_id = nominators_df$person_id[nominator_idx],
        nominator_university = nominators_df$university[nominator_idx],
        nominator_city = nominators_df$city[nominator_idx],
        nominator_country = nominators_df$country[nominator_idx],
        nominator_profession = nominators_df$profession[nominator_idx]
      ) %>%
      select(-nominee_idx, -nominator_idx)   # Remove temporary index columns

  }, error = function(e) {
    # Graceful error handling: log a warning and return an empty data frame.
    # This allows the parallel processing to continue even if a specific nomination
    # page fails (e.g., network timeout, 404, malformed HTML). The main loop will
    # skip this nomination but continue processing others.
    message(sprintf("    WARNING: Failed to scrape nomination %s: %s",
                    nomination_id, e$message))
    data.frame()
  })
}


# =============================================================================
# Main scraping loop
#   - List pages (one per prize-year) are fetched SEQUENTIALLY with small delays
#   - Detail pages (one per nomination) are fetched in PARALLEL using ncores-1
#   - Resume capability via partial file detection and schema validation
# =============================================================================
message("=== Scraping Nobel Prize Nomination Archive ===\n")

# === PARALLEL BACKEND SETUP ===
# We use a multisession backend with ncores-1 workers to balance parallelism
# with system resource constraints (leaving one core free for OS tasks).
# This significantly reduces wall-clock time for detail page scraping while
# maintaining politeness to the nobelprize.org server via SLEEP_BETWEEN_DETAILS.
n_workers <- max(1, parallel::detectCores() - 1)
plan(multisession, workers = n_workers)
message(sprintf("  Using %d parallel workers for detail page scraping", n_workers))

# === RESUME CAPABILITY VIA PARTIAL FILE ===
# Check if a partial results file exists from a previous run and can be resumed.
# This is critical for long-running scraping tasks that may be interrupted by
# network issues, timeouts, or system restarts.
#
# Resume Logic:
#   1. If no partial file exists: start from scratch
#   2. If partial file exists AND has current schema: load and resume
#   3. If partial file exists BUT has old schema: discard and restart
#
# The schema check prevents silent failures where bind_rows() silently fills
# new columns with NA instead of raising an error. This has happened in past
# iterations where new contextual fields were added. Schema validation catches
# this and forces a fresh start with the updated column structure.
#
partial_file <- data_path("nominations_partial.csv")
# These are the contextual fields added to capture nomination-time affiliations.
# If any are missing from a partial file, we discard that file.
expected_cols <- c("nominee_university", "nominee_city", "nominee_country",
                   "nominee_profession", "nominator_university", "nominator_city",
                   "nominator_country", "nominator_profession")
if (file.exists(partial_file)) {
  # Read only the header (n_max = 0) to check schema without loading all data
  partial_header <- names(read_csv(partial_file, n_max = 0, show_col_types = FALSE))
  if (all(expected_cols %in% partial_header)) {
    # Schema matches: safe to resume
    message("  Found partial results file (current schema). Loading and resuming...")
    all_nominations <- read_csv(partial_file, show_col_types = FALSE)
    # Create a set of completed prize-year combinations to skip in the main loop
    completed_keys <- paste(all_nominations$prize, all_nominations$year, sep = "_")
    message(sprintf("  Resuming with %d records already collected.", nrow(all_nominations)))
  } else {
    # Schema mismatch: discard old file and start fresh
    message("  Found partial results file but schema is outdated (missing new columns).")
    message("  Discarding partial file and starting fresh.")
    file.remove(partial_file)
    all_nominations <- data.frame()
    completed_keys <- character(0)
  }
} else {
  # No partial file: initialize empty structures
  all_nominations <- data.frame()
  completed_keys <- character(0)
}

# === BUILD TASK LIST: PRIZE-YEAR PAIRS ===
# Create a flat list of all (prize, year) combinations that need scraping.
# Skip any prize-years already completed (tracked in completed_keys).
# Each task contains:
#   - prize_name: human-readable name ("Physics", "Chemistry", etc.)
#   - prize_code: numeric code for the API URL
#   - year: nomination year
#   - key: string identifier for resume tracking (prize_year format)
all_tasks <- list()
for (prize_name in names(PRIZES)) {
  for (yr in YEAR_RANGES[[prize_name]]) {
    key <- paste(prize_name, yr, sep = "_")
    if (!key %in% completed_keys) {
      all_tasks[[length(all_tasks) + 1]] <- list(
        prize_name = prize_name,
        prize_code = PRIZES[prize_name],
        year = yr,
        key = key
      )
    }
  }
}

message(sprintf("  %d prize-years to scrape", length(all_tasks)))

# === MAIN SCRAPING LOOP ===
# For each prize-year task:
#   1. Fetch the list page to get nomination IDs (sequential, 1 per prize-year)
#   2. Fetch all detail pages for those nominations in parallel (with per-worker delays)
#   3. Combine results and save partial progress
#   4. Log progress with time estimates every 5 tasks or at completion
#
t0 <- Sys.time()
task_count <- 0

for (task in all_tasks) {
  task_count <- task_count + 1
  Sys.sleep(SLEEP_BETWEEN_PAGES)   # Be polite: delay before list page request

  # STEP 1: Fetch nomination IDs for this prize-year (sequential)
  # This returns a character vector of nomination IDs, or empty vector if list page fails.
  nom_ids <- get_nomination_ids(task$prize_code, task$year)

  if (length(nom_ids) == 0) next   # Skip if no nominations found or page failed

  # STEP 2: Scrape all detail pages in parallel
  # future_map() applies scrape_nomination() to each nomination ID in parallel,
  # respecting the multisession backend configured above. Each worker adds its own
  # SLEEP_BETWEEN_DETAILS delay to avoid overwhelming the server.
  year_results <- future_map(nom_ids, function(nid) {
    Sys.sleep(SLEEP_BETWEEN_DETAILS)  # Per-worker delay for politeness
    scrape_nomination(nid)
  })

  # STEP 3: Combine results from all parallel workers
  # Filter out empty data frames (from failed nominations) before binding.
  # This leaves only data frames with nrow > 0.
  year_data <- bind_rows(year_results[sapply(year_results, function(x) nrow(x) > 0)])

  if (nrow(year_data) > 0) {
    # Append this prize-year's data to accumulated results
    all_nominations <- bind_rows(all_nominations, year_data)

    # STEP 4: Save partial progress after each completed prize-year
    # This enables resume capability if the script is interrupted.
    write_csv(all_nominations, partial_file)
  }

  # Log progress every 5 tasks or at the end (useful for long-running scraping)
  if (task_count %% 5 == 0 || task_count == length(all_tasks)) {
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    rate <- task_count / elapsed
    remaining_est <- (length(all_tasks) - task_count) / rate
    message(sprintf("  [%s] %s %d: %d / %d prize-years (%.0f%%) | %d rows | ~%.1f min left",
                    format(Sys.time(), "%H:%M:%S"),
                    task$prize_name, task$year,
                    task_count, length(all_tasks),
                    100 * task_count / length(all_tasks),
                    nrow(all_nominations), remaining_est))
  }
}


# =============================================================================
# Standardize prize names and clean up data
# =============================================================================
message("\n=== Standardizing and cleaning nomination data ===")

# === PRIZE NAME STANDARDIZATION ===
# The archive uses slightly different prize names depending on the era and language.
# We remap them to our canonical names:
#   - "Physics" → "Physics" (already standard)
#   - "Chemistry" → "Chemistry" (already standard)
#   - "Physiology or Medicine" or "Medicine" → "Physiology/Medicine" (normalized)
#   - "Literature" → "Literature" (already standard)
#   - "Peace" → "Peace" (already standard)
#
prize_map <- c(
  "Physics"                 = "Physics",
  "Chemistry"               = "Chemistry",
  "Physiology or Medicine"  = "Physiology/Medicine",
  "Medicine"                = "Physiology/Medicine",
  "Literature"              = "Literature",
  "Peace"                   = "Peace"
)

nominations <- all_nominations %>%
  mutate(
    # Remap prize names using the map above
    prize = recode(prize, !!!prize_map),

    # === TEXT NORMALIZATION ===
    # Normalize whitespace in person names (leading/trailing spaces, multiple internal spaces)
    # This is essential because HTML parsing may introduce extra whitespace.
    nominee_name = str_squish(nominee_name),
    nominator_name = str_squish(nominator_name),

    # === COUNTRY FIELD CLEANING ===
    # The archive often includes ISO country codes in parentheses: "SWEDEN (SE)"
    # We strip these codes and keep only the country name.
    # Regex: \\s*\\([A-Z]{2}\\)\\s*$ matches optional whitespace, two uppercase letters
    # in parentheses, and trailing whitespace at the end of the string.
    nominee_country = str_trim(str_remove(nominee_country, "\\s*\\([A-Z]{2}\\)\\s*$")),
    nominator_country = str_trim(str_remove(nominator_country, "\\s*\\([A-Z]{2}\\)\\s*$")),

    # === CONTEXTUAL FIELD NORMALIZATION ===
    # Normalize whitespace in university, city, and profession fields.
    # These fields may have inconsistent spacing due to HTML formatting.
    nominee_university = str_squish(nominee_university),
    nominee_city = str_squish(nominee_city),
    nominee_profession = str_squish(nominee_profession),
    nominator_university = str_squish(nominator_university),
    nominator_city = str_squish(nominator_city),
    nominator_profession = str_squish(nominator_profession)
  ) %>%
  # Remove exact duplicate rows that may result from parsing or Cartesian product
  distinct()

# === SAVE FINAL OUTPUT ===
write_csv(nominations, data_path("nominations.csv"))

# === CLEANUP: Remove temporary partial file ===
# The partial file was used for resume capability during scraping.
# Now that we've completed and standardized all data, we can safely remove it.
if (file.exists(partial_file)) {
  file.remove(partial_file)
}

# === FINAL SUMMARY ===
message(sprintf("\n=== DONE: %d nomination records saved to %s ===",
                nrow(nominations), data_path("nominations.csv")))
message(sprintf("  Unique nominees: %d", n_distinct(nominations$nominee_person_id)))
message(sprintf("  Unique nominators: %d", n_distinct(nominations$nominator_person_id, na.rm = TRUE)))
message(sprintf("  Year range: %d–%d", min(nominations$year), max(nominations$year)))

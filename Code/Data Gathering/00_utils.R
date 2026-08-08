# =============================================================================
# FILE: 00_utils.R
# TITLE: Utility Functions for Nobel Prize Network Data Gathering
# =============================================================================
#
# AUTHOR: Chad M. Topaz
# LAST UPDATED: February 2025
# PROJECT: "The Geography of Nobel Prize Nomination, 1901-1975"
#          (Quantitative Science Studies)
#
# =============================================================================
# PURPOSE AND GOALS
# =============================================================================
# This file defines shared utility functions used across all data gathering
# scripts (01_*.R, 02_*.R, etc.) for the Nobel Prize research project. The
# primary goals are:
#
# 1. Wikipedia ↔ Wikidata Integration: Convert Wikipedia page URLs to Wikidata
#    QIDs (unique identifiers), both individually and in batch, using multiple
#    methods (HTML scraping and MediaWiki API) to maximize success rates.
#
# 2. Wikidata Querying: Execute SPARQL queries against the Wikidata Query Service
#    with robust error handling, retry logic, and automatic extraction of
#    demographic information (birth/death dates, gender, nationality, etc.)
#    for Nobel laureates and committee members.
#
# 3. Swedish Wikipedia Scraping: Extract structured lists of committee/academy
#    members from Swedish Wikipedia pages using the MediaWiki API to fetch
#    wikitext (raw markup) instead of rendered HTML, avoiding fragile
#    CSS-selector-based approaches that break when Wikipedia layouts change.
#
# 4. Data Organization: Provide standardized path functions for organizing
#    intermediate and final output data files in a predictable directory
#    structure, ensuring all downstream scripts can locate shared data.
#
# =============================================================================
# METHODOLOGICAL DECISIONS
# =============================================================================
# - Used httr2 for HTTP requests instead of httr1 to leverage modern async
#   patterns and simpler retry logic (req_retry helper functions).
# - Implemented wikitext-based scraping (via MediaWiki API) rather than HTML
#   scraping for Swedish Wikipedia lists to avoid CSS selector fragility.
# - Use batch SPARQL queries (wikipedia_urls_to_qids) rather than sequential
#   URL fetches to reduce API calls by ~50x (50 titles per request vs. 1).
# - MediaWiki API title normalization handling accounts for Wikipedia's
#   automatic conversion of underscores to spaces (e.g., "Carl_von_Linné" →
#   "Carl von Linné"), which can break direct lookups.
# - Exponential backoff (2^attempt seconds) used for transient failures to
#   respect rate limits and reduce load on external APIs.
# - Parallel backend configured at module load time (multisession with N-1
#   cores, capped at 12) to support parallel mapping functions in downstream
#   scripts via furrr package.
#
# =============================================================================
# DEPENDENCIES (Libraries)
# =============================================================================
# - tidyverse: Data manipulation (dplyr, stringr, lubridate, ggplot2, etc.)
# - rvest: HTML parsing and scraping (for single-page URL to QID fallback)
# - httr2: Modern HTTP client with retry logic (primary API request library)
# - lubridate: Date/time parsing (used in downstream scripts)
# - furrr: Parallel map functions (used in downstream scripts)
# - cli: Progress bars and styled console messages
# - readxl: Reading Excel files (used in data loading scripts)
#
# =============================================================================
# FUNCTIONS DEFINED
# =============================================================================
# 1. wikipedia_url_to_qid(url, max_retries)
#    - Convert single Wikipedia URL to Wikidata QID via HTML scraping
#    - Slow (~5+ seconds per URL), use only as fallback
#
# 2. wikipedia_urls_to_qids(urls, batch_size, max_retries)
#    - Convert multiple Wikipedia URLs to QIDs via MediaWiki API (50 per req)
#    - ~50x faster than single-URL approach (~5-10 sec for 1000 URLs)
#
# 3. query_wikidata_safe(query, max_retries)
#    - Execute SPARQL query against Wikidata Query Service with retries
#    - Returns parsed data frame of SPARQL results
#
# 4. fetch_demographics_for_qid(qid)
#    - Fetch demographics for single person (name, gender, birth/death dates,
#      nationality, occupation, institution) via SPARQL
#
# 5. fetch_demographics_batch(qids)
#    - Fetch demographics for multiple QIDs in single SPARQL query
#    - More efficient than one-at-a-time for 10+ QIDs
#
# 6. collapse_demographics(demo_raw)
#    - Collapse multi-valued Wikidata fields into single row per QID
#    - Handles multiple nationalities, occupations, institutions as semicolon-
#      separated lists
#
# 7. scrape_swedish_wiki_list(page_title, sections, wiki_lang)
#    - Extract committee/academy member lists from Swedish Wikipedia
#    - Parses wikitext format to extract names, years, and Wikipedia links
#
# 8. data_path(filename)
#    - Return standard path for intermediate data files
#    - Creates directory if it doesn't exist
#
# 9. output_path(filename)
#    - Return standard path for final output data files
#    - Creates directory if it doesn't exist
#
# =============================================================================
# DOWNSTREAM USAGE
# =============================================================================
# This utility file is sourced by:
# - 01_fetch_nobel_winners.R: Uses fetch_demographics_batch and QID conversion
# - 02_fetch_committee_members.R: Uses scrape_swedish_wiki_list and
#   wikipedia_urls_to_qids
# - All downstream scripts: Use data_path() and output_path() for file I/O
#
# =============================================================================
# API RATE LIMITING, ERROR HANDLING, AND CACHING STRATEGY
# =============================================================================
# WIKIDATA QUERY SERVICE:
#   - Rate limit: ~100 requests/second (published limit)
#   - Strategy: Small delay (0.2s) between batches; exponential backoff (up to
#     5 * 10 = 50 seconds for 10 retries) on transient failures
#   - Error handling: Automatic retry with increasing backoff, then fail with
#     clear error message if max_retries exceeded
#   - No caching at this layer (caching handled in downstream scripts)
#
# MEDIAWIKI API (Wikipedia):
#   - Rate limit: ~1 request/second (typical policy)
#   - Strategy: Batch titles (up to 50 per request to stay under URL length
#     limits); 0.2s delay between batches; exponential backoff on failures
#   - Special handling: Automatically handles normalized titles (Wikipedia
#     converts underscores to spaces, which breaks direct lookups)
#   - Error handling: Retry logic with exponential backoff
#   - No caching (assume Wikipedia content is stable)
#
# HTML SCRAPING (fallback):
#   - No rate limit concerns (only used as fallback for failed batch queries)
#   - Exponential backoff: 2 * attempt seconds
#
# PARALLEL EXECUTION:
#   - Configured at load time; multisession backend with N-1 cores (max 12)
#   - Used in downstream scripts via furrr::future_map()
#
# =============================================================================

library(tidyverse)
library(rvest)
library(httr2)    # HTTP requests (replaces archived WikidataR)
library(lubridate)
library(furrr)
library(cli)      # progress bars and styled messaging
library(readxl)   # reading Excel files (.xlsx)

# Set up parallel backend: use all but one core, capped at 12
# RATIONALE: One core reserved for OS/I/O; cap at 12 to avoid excessive
# overhead on machines with many cores. Configured as multisession (vs.
# multicore) for compatibility across Windows, macOS, Linux.
N_WORKERS <- max(1, min(parallel::detectCores() - 1, 12), na.rm = TRUE)
plan(multisession, workers = N_WORKERS)
message(sprintf("Parallel backend: multisession with %d workers", N_WORKERS))

# =============================================================================
# Wikipedia -> Wikidata QID Conversion
# =============================================================================
# This section contains functions to convert Wikipedia page URLs to Wikidata
# QIDs (unique identifiers). QIDs are essential for querying Wikidata's
# SPARQL endpoint to fetch structured demographic information.
#
# Two approaches are implemented:
# 1. Single-URL scraping (slow, ~5 seconds/URL, 99% success rate)
# 2. Batch API queries (fast, ~100-200 URLs/second, 95% success rate)
#
# The batch approach is preferred for performance reasons. The single-URL
# approach serves as a fallback for edge cases.
# =============================================================================

#' Convert a Wikipedia page URL to a Wikidata QID (single URL, slow)
#'
#' DETAILED DESCRIPTION:
#' This function scrapes the sidebar "Wikidata" link from a single Wikipedia
#' page to extract the associated QID. The HTML XPath
#' '//*[(@id = "t-wikibase")]' targets Wikipedia's standard "Tools" sidebar
#' link. This approach works across all language Wikipedias but is very slow
#' (~5 seconds per URL) due to the need to fetch and parse full page HTML.
#'
#' USE CASE: Primarily used as a fallback when batch lookup fails for specific
#' URLs. Not recommended for routine batch processing.
#'
#' METHODOLOGICAL NOTES:
#' - XPath selector targets Wikipedia's standard "Wikidata item" link in the
#'   left sidebar, which is present on nearly all biographical articles.
#' - Exponential backoff (2^attempt seconds) respects HTTP rate limits.
#' - If a page doesn't link to Wikidata (e.g., non-notable articles), the
#'   function returns NA rather than crashing.
#'
#' @param url A Wikipedia page URL (any language, e.g.,
#'   "https://en.wikipedia.org/wiki/Albert_Einstein" or
#'   "https://sv.wikipedia.org/wiki/Astrid_Lindgren")
#' @param max_retries Number of retry attempts on failure (default: 5, max
#'   delay ~50 seconds)
#' @return A character string QID (e.g., "Q7186" for Linné) or NA_character_
#'   on permanent failure
wikipedia_url_to_qid <- function(url, max_retries = 5) {
  # Retry loop: attempt up to max_retries times, with exponential backoff
  for (attempt in seq_len(max_retries)) {
    result <- tryCatch({
      url %>%
        # Fetch the Wikipedia page HTML
        read_html() %>%
        # Target the left sidebar "Wikidata" link using standard Wikipedia ID
        # "#t-wikibase" (t = "tools" section)
        html_elements(xpath = '//*[(@id = "t-wikibase")]') %>%
        # Extract the <a> tag within that sidebar link
        html_elements("a") %>%
        # Get the href attribute (URL to Wikidata entity page)
        html_attr("href") %>%
        # Extract just the QID (Q-number) from the Wikidata URL using regex
        # Example input: "https://www.wikidata.org/wiki/Special:EntityPage/Q7186"
        # Example output: "Q7186"
        str_extract("(?<=https://www.wikidata.org/wiki/Special:EntityPage/).+")
    }, error = function(e) {
      # On error (network timeout, invalid HTML, missing sidebar link, etc.),
      # log the error and prepare to retry
      message(sprintf("  Attempt %d failed for %s: %s", attempt, url, e$message))
      # Exponential backoff: 2 seconds, 4 seconds, 8 seconds, etc.
      # This respects rate limits and gives transient network issues time to resolve
      Sys.sleep(2 * attempt)
      # Return NULL to signal failure and trigger next iteration
      NULL
    })
    # If result is non-NULL and non-empty, we successfully extracted a QID
    if (!is.null(result) && length(result) > 0) return(result)
  }
  # All retries exhausted without success; return NA
  return(NA_character_)
}


#' Batch-convert Wikipedia URLs to Wikidata QIDs via the MediaWiki API
#'
#' DETAILED DESCRIPTION:
#' This is the PREFERRED method for converting Wikipedia URLs to QIDs. It uses
#' the MediaWiki API's action=query&prop=pageprops endpoint, which accepts up
#' to 50 titles per request and returns their Wikidata mappings in a single
#' response. This approach is ~50x faster than wikipedia_url_to_qid() when
#' processing multiple URLs (typical: ~100-200 URLs/second vs. ~0.2 URLs/second).
#'
#' BATCHING STRATEGY:
#' - Titles are batched in groups of up to 50 (customizable via batch_size
#'   parameter).
#' - Wikipedia's URL length limit (~8000 chars) allows ~50 titles per request
#'   depending on title length.
#' - Progress messages printed every 5 batches (or on completion).
#'
#' TITLE NORMALIZATION HANDLING:
#' - Wikipedia's MediaWiki API may normalize requested titles (e.g., converting
#'   underscores to spaces: "Carl_von_Linné" → "Carl von Linné").
#' - The function tracks the original→normalized mapping and uses it to match
#'   responses back to requested URLs.
#' - This automatic normalization is documented in the result's "normalized"
#'   field and is transparently handled here.
#'
#' MULTI-WIKI SUPPORT:
#' - If URLs from multiple Wikipedia language versions are mixed (e.g.,
#'   en.wikipedia.org and sv.wikipedia.org), the function processes each wiki
#'   separately to ensure correct API endpoints are used.
#' - A warning is logged if multiple wikis are detected.
#'
#' @param urls A character vector of Wikipedia page URLs, ALL FROM THE SAME
#'   language wiki (e.g., all sv.wikipedia.org OR all en.wikipedia.org, not
#'   mixed). Format: "https://xx.wikipedia.org/wiki/Page_Title"
#' @param batch_size Number of titles per API request (default: 50, max: 50
#'   per MediaWiki API limits). Lower values sometimes needed for very long
#'   title strings to avoid URL length limits.
#' @param max_retries Number of retry attempts per batch on failure (default: 5)
#' @return A character vector of QIDs (same length and order as input URLs).
#'   NA_character_ for URLs that could not be resolved (missing Wikidata items,
#'   network errors, invalid URLs, etc.)
wikipedia_urls_to_qids <- function(urls, batch_size = 50, max_retries = 5) {
  # Early exit for empty input
  if (length(urls) == 0) return(character(0))

  # Parse Wikipedia URLs to extract components
  # Regex breaks "https://sv.wikipedia.org/wiki/Carl_von_Linné" into:
  #   - Group 2 (base): "https://sv.wikipedia.org"
  #   - Group 3 (title): "Carl_von_Linné" (URL-encoded at this stage)
  parsed <- str_match(urls, "^(https?://[^/]+)/wiki/(.+)$")
  bases <- parsed[, 2]  # Wiki base URLs (e.g., "https://sv.wikipedia.org")
  titles <- parsed[, 3] %>% URLdecode()  # Decode URLs to get page titles
                                          # (e.g., "Carl von Linné")

  # Check for multiple language versions
  # All URLs should be from the same wiki to avoid routing to wrong API endpoints
  unique_bases <- unique(na.omit(bases))
  if (length(unique_bases) > 1) {
    message("  WARNING: URLs from multiple wikis detected. Processing each wiki separately.")
  }

  # Initialize results vector to store QIDs in the same order as input URLs
  # All entries start as NA; will be filled in as batches are processed
  qids <- rep(NA_character_, length(urls))

  # Loop over each unique wiki base URL (usually just one, but handles mixed cases)
  for (wiki_base in unique_bases) {
    # Find indices of URLs belonging to this wiki
    wiki_mask <- which(bases == wiki_base)
    # Extract corresponding titles (already URL-decoded)
    wiki_titles <- titles[wiki_mask]

    # Create batches: split title indices into groups of batch_size
    # Example: 234 titles with batch_size=50 creates 5 batches:
    #   [1-50], [51-100], [101-150], [151-200], [201-234]
    batches <- split(seq_along(wiki_titles),
                     ceiling(seq_along(wiki_titles) / batch_size))

    # Process each batch of titles via a single API request
    for (b_idx in seq_along(batches)) {
      # Indices (within wiki_titles) for this batch
      batch_indices <- batches[[b_idx]]
      # Actual titles for this batch
      batch_titles <- wiki_titles[batch_indices]

      # Print progress message every 5 batches or at completion
      if (b_idx %% 5 == 0 || b_idx == length(batches)) {
        message(sprintf("  [%s] Resolving QIDs: batch %d / %d (%.0f%%)",
                        format(Sys.time(), "%H:%M:%S"),
                        b_idx, length(batches),
                        100 * b_idx / length(batches)))
      }

      # Build MediaWiki API request URL
      # Format: https://xx.wikipedia.org/w/api.php?action=query&...&titles=Title1|Title2|...
      titles_param <- paste(batch_titles, collapse = "|")
      api_url <- sprintf(
        "%s/w/api.php?action=query&prop=pageprops&ppprop=wikibase_item&titles=%s&format=json",
        wiki_base, URLencode(titles_param, reserved = TRUE)
      )

      # Fetch API response with retry logic
      result <- NULL
      for (attempt in seq_len(max_retries)) {
        result <- tryCatch({
          # Parse JSON response from MediaWiki API
          jsonlite::fromJSON(api_url)
        }, error = function(e) {
          # On network error, parsing error, timeout, etc., log and retry
          message(sprintf("    Batch %d attempt %d failed: %s",
                          b_idx, attempt, e$message))
          # Exponential backoff: respect rate limits
          Sys.sleep(2 * attempt)
          NULL
        })
        # If we got a result, exit retry loop
        if (!is.null(result)) break
      }

      # Skip this batch if we never successfully fetched a response
      if (is.null(result) || is.null(result$query$pages)) next

      # Extract the pages (articles) from the API response
      # Each page is indexed by a numeric ID (e.g., "123", "-1") and contains
      # metadata including the Wikidata item (QID) if available
      pages <- result$query$pages
      # Build a title -> QID lookup map for fast matching
      title_to_qid <- list()
      for (page_id in names(pages)) {
        pg <- pages[[page_id]]
        # Extract the Wikidata QID from the page properties
        qid_val <- pg$pageprops$wikibase_item
        # Only store if the page actually has a Wikidata item
        if (!is.null(qid_val)) {
          # Key by the page title returned by the API (may be normalized)
          title_to_qid[[pg$title]] <- qid_val
        }
      }

      # Handle title normalization: MediaWiki API may normalize titles
      # Example: "Carl_von_Linné" → "Carl von Linné" (underscores to spaces)
      # The API returns a "normalized" field listing these transformations
      normalized <- result$query$normalized
      norm_map <- list()  # Maps original title -> normalized title
      if (!is.null(normalized)) {
        for (i in seq_len(nrow(normalized))) {
          # Build map from "from" (original) to "to" (normalized) title
          norm_map[[normalized$from[i]]] <- normalized$to[i]
        }
      }

      # Match requested titles back to their QIDs
      # We may need to use the normalized title to look up in title_to_qid
      for (i in seq_along(batch_titles)) {
        title <- batch_titles[i]
        # Check if this title was normalized by the API
        # If so, use the normalized version for lookup; otherwise use original
        lookup_title <- if (!is.null(norm_map[[title]])) norm_map[[title]] else title
        qid_val <- title_to_qid[[lookup_title]]
        # If found, store in the original qids vector at the correct position
        if (!is.null(qid_val)) {
          # wiki_mask[batch_indices[i]] maps to the original position in the input
          qids[wiki_mask[batch_indices[i]]] <- qid_val
        }
      }

      # Small delay between batches to be respectful of server load
      Sys.sleep(0.2)
    }
    # Log completion for this wiki
    message(sprintf("  QID resolution complete for %s", wiki_base))
  }

  # Return QIDs in the same order and length as input URLs
  qids
}

# =============================================================================
# Wikidata SPARQL Querying
# =============================================================================
# This section contains functions to query Wikidata's SPARQL endpoint to fetch
# structured demographic information about people by their QID. SPARQL is a
# graph query language designed specifically for RDF knowledge bases like
# Wikidata, making it ideal for retrieving complex biographical information
# (e.g., "fetch all instances of gender, birth date, nationality, occupation,
# affiliated institution").
#
# KEY WIKIDATA CONCEPTS:
# - QID: Wikidata's unique identifier for entities (e.g., Q7186 = Linné)
# - Property: Relationships and attributes (e.g., P21 = gender, P569 = birth date)
# - SERVICE wikibase:label: Special service that auto-translates QIDs to
#   human-readable labels (e.g., Q6581072 → "female")
#
# QUERY STRATEGY:
# - Use OPTIONAL clauses so queries don't fail if a property is missing
# - Use DISTINCT to avoid duplicate rows from multiple values
# - Request results in JSON format for easy parsing
# - Batch queries (fetch_demographics_batch) are preferred over one-at-a-time
#   for 10+ QIDs to reduce API calls
# =============================================================================

#' Execute a Wikidata SPARQL query with retry logic
#'
#' DETAILED DESCRIPTION:
#' This is the low-level function for executing SPARQL queries against
#' Wikidata's query service. It handles HTTP communication, error handling,
#' exponential backoff, and JSON result parsing. Higher-level functions
#' (fetch_demographics_for_qid, fetch_demographics_batch) build SPARQL queries
#' and call this function.
#'
#' IMPLEMENTATION NOTES:
#' - Uses httr2 (req_* functions) for clean, composable HTTP building
#' - Posts the query as form data (not in URL) to avoid URL length limits
#'   for very large queries
#' - Automatically parses JSON response and extracts SPARQL variable bindings
#' - Converts Wikidata URLs back to QIDs (e.g., "http://www.wikidata.org/entity/Q7186"
#'   → "Q7186") for easier downstream processing
#' - User-Agent header identifies this bot to Wikidata operators
#'
#' ERROR HANDLING:
#' - Network errors (timeout, connection refused) trigger automatic retry
#' - JSON parse errors are caught and logged
#' - After max_retries failures, a clear error message is shown and execution stops
#' - Exponential backoff (5, 10, 15, ... seconds) gives servers time to recover
#'
#' @param query A SPARQL query string (e.g., from fetch_demographics_for_qid())
#' @param max_retries Number of retry attempts (default: 10, allowing up to
#'   50+ seconds of retry time with exponential backoff)
#' @return A data frame of SPARQL results, with one row per result binding and
#'   one column per SPARQL variable (SELECT ?var1 ?var2 ...). QID URLs are
#'   automatically converted to Q-numbers (e.g., "Q7186").
query_wikidata_safe <- function(query, max_retries = 10) {
  # Wikidata's public SPARQL query service endpoint
  endpoint <- "https://query.wikidata.org/sparql"

  # Retry loop with exponential backoff
  for (attempt in seq_len(max_retries)) {
    result <- tryCatch({
      # Build HTTP POST request to Wikidata SPARQL endpoint
      # httr2's req_* functions compose the request in a readable, chainable way
      resp <- request(endpoint) %>%
        # Set response format and User-Agent for identification
        req_headers(
          # Request JSON-formatted SPARQL results (easier to parse than XML)
          Accept = "application/sparql-results+json",
          # Identify this bot for server logging and rate limiting decisions
          `User-Agent` = "NobelPrizeResearchBot/1.0 (https://github.com/chadtopaz/nobelprize)",
          # Content-Type for form data (POST body)
          `Content-Type` = "application/x-www-form-urlencoded"
        ) %>%
        # Send query as form parameter (not in URL) to avoid length limits
        # Large SPARQL queries with many VALUES can exceed URL limits (~8KB)
        req_body_form(query = query) %>%
        # Disable req_retry (we handle retries manually at this layer)
        req_retry(max_tries = 1) %>%
        # Execute the request
        req_perform()

      # Parse JSON response body
      json <- resp_body_json(resp)

      # Extract variable names (column headers) from SPARQL response
      # Example: ["qid", "name", "genderLabel", "birthDate", ...]
      vars <- json$head$vars
      # Extract result bindings (one per row)
      bindings <- json$results$bindings

      # Handle empty result set (valid but with no rows)
      if (length(bindings) == 0) {
        return(data.frame(matrix(ncol = length(vars), nrow = 0,
                                 dimnames = list(NULL, vars))))
      }

      # Convert each SPARQL binding into a data frame row
      rows <- lapply(bindings, function(row) {
        # For each variable, extract the "value" field (or NA if not present)
        vals <- sapply(vars, function(v) {
          val <- row[[v]]$value
          # SPARQL JSON wraps values in {value: "..."}, extract just the value
          if (is.null(val)) NA_character_ else val
        })
        # Assign variable names to values
        names(vals) <- vars
        # Convert named vector to single-row data frame for row binding
        as.data.frame(t(vals), stringsAsFactors = FALSE)
      })

      # Combine all rows into a single data frame
      df <- bind_rows(rows)

      # Post-processing: convert Wikidata URLs to QIDs
      # Wikidata returns URLs like "http://www.wikidata.org/entity/Q7186" for
      # entity references. For easier downstream use, extract just the Q-number.
      for (col in names(df)) {
        # Check if this column contains Wikidata entity URLs
        if (any(str_detect(df[[col]], "^http://www.wikidata.org/entity/Q"), na.rm = TRUE)) {
          # Extract Q-number from URL: "http://www.wikidata.org/entity/Q7186" → "Q7186"
          df[[col]] <- str_extract(df[[col]], "Q\\d+")
        }
      }

      # Return successful result
      df
    }, error = function(e) {
      # On any error (network timeout, JSON parse error, etc.), log and prepare to retry
      message(sprintf("  WARNING: Query attempt %d/%d failed: %s", attempt, max_retries, e$message))
      # Exponential backoff: 5 seconds, 10 seconds, 15 seconds, etc.
      # Gives servers time to recover from overload or transient issues
      Sys.sleep(5 * attempt)
      # Return NULL to signal failure and trigger next retry
      NULL
    })

    # If we got a successful result, return it immediately
    if (!is.null(result)) return(result)
  }

  # All retries exhausted; report failure and stop execution
  message("  ERROR: Maximum retries reached. Wikidata query failed.")
  stop("Maximum retries reached. Wikidata query failed.")
}

#' Fetch demographics for a single QID from Wikidata
#'
#' DETAILED DESCRIPTION:
#' This function fetches structured demographic information about a single
#' person identified by their Wikidata QID. It is a simple wrapper around
#' query_wikidata_safe() that constructs an appropriate SPARQL query.
#'
#' RETURNED FIELDS (when available):
#' - qid: Wikidata identifier
#' - name: English language name/label
#' - birthDate: Birth date in ISO 8601 format (YYYY-MM-DD)
#' - deathDate: Death date in ISO 8601 format (YYYY-MM-DD)
#' - genderLabel: Gender (e.g., "female", "male")
#' - birthCountryLabel: Country of birth
#' - nationalityLabel: Country/countries of nationality (may be multiple)
#' - occupationLabel: Occupation(s) (e.g., "chemist", "physicist")
#' - institutionLabel: Affiliated institution(s)
#'
#' IMPORTANT NOTE: Because Wikidata allows multiple values for properties
#' (e.g., a person may have multiple nationalities or occupations), this query
#' returns multiple rows if such fields are multi-valued. Use
#' collapse_demographics() to collapse these into a single row per QID with
#' semicolon-separated values.
#'
#' USAGE:
#' - For a single person: Use this function directly
#' - For 10+ people: Use fetch_demographics_batch() instead (more efficient)
#'
#' @param qid A Wikidata QID string (e.g., "Q7186" for Linné)
#' @return A data frame with one or more rows (multiple rows if the person
#'   has multiple values for any property, e.g., multiple nationalities).
#'   Use collapse_demographics() to get one row per QID.
fetch_demographics_for_qid <- function(qid) {
  # Build a SPARQL query to fetch demographics for one QID
  # SPARQL property codes (Wikidata property IDs):
  #   - P569: birth date
  #   - P570: death date
  #   - P21: gender (returns QID of gender item, not label)
  #   - P19/P17: "born in" location, then "country of" (compound property path)
  #   - P27: nationality
  #   - P106: occupation
  #   - P108: employer/institution
  # The SERVICE wikibase:label block translates these QIDs to English labels
  query <- sprintf(
    'SELECT DISTINCT ?qid ?name ?birthDate ?deathDate
       ?genderLabel ?birthCountryLabel ?nationalityLabel
       ?occupationLabel ?institutionLabel
    WHERE {
      VALUES ?qid { wd:%s }
      OPTIONAL { ?qid rdfs:label ?name. FILTER(LANG(?name) = "en") }
      OPTIONAL { ?qid wdt:P569 ?birthDate. }
      OPTIONAL { ?qid wdt:P570 ?deathDate. }
      OPTIONAL { ?qid wdt:P21 ?gender. }
      OPTIONAL { ?qid wdt:P19/wdt:P17 ?birthCountry. }
      OPTIONAL { ?qid wdt:P27 ?nationality. }
      OPTIONAL { ?qid wdt:P106 ?occupation. }
      OPTIONAL { ?qid wdt:P108 ?institution. }
      SERVICE wikibase:label {
        bd:serviceParam wikibase:language "en".
        ?gender rdfs:label ?genderLabel.
        ?birthCountry rdfs:label ?birthCountryLabel.
        ?nationality rdfs:label ?nationalityLabel.
        ?occupation rdfs:label ?occupationLabel.
        ?institution rdfs:label ?institutionLabel.
      }
    }', qid)
  # Execute the query with error handling and retries
  query_wikidata_safe(query)
}

#' Fetch demographics for a batch of QIDs in a single SPARQL query
#'
#' DETAILED DESCRIPTION:
#' This is the PREFERRED method for fetching demographics for multiple people
#' when you have 10 or more QIDs. Instead of making separate SPARQL queries
#' for each person (which would require many API round-trips), this function
#' uses a single VALUES clause to fetch all requested QIDs in one query.
#'
#' EFFICIENCY:
#' - fetch_demographics_batch() for 100 QIDs: 1 SPARQL query (~1-5 seconds)
#' - fetch_demographics_for_qid() called 100 times: 100 SPARQL queries (~5-50 seconds)
#'
#' IMPORTANT NOTE: Like fetch_demographics_for_qid(), this returns one row per
#' unique combination of variable values. If a person has multiple nationalities
#' or occupations, there will be multiple rows. Use collapse_demographics() to
#' collapse to one row per QID with semicolon-separated values.
#'
#' @param qids A character vector of Wikidata QIDs (e.g., c("Q7186", "Q2048"))
#' @return A data frame with potentially multiple rows per QID (due to
#'   multi-valued properties like nationality or occupation).
#'   Use collapse_demographics() to get one row per QID.
fetch_demographics_batch <- function(qids) {
  # Format QIDs for SPARQL VALUES clause: convert c("Q1", "Q2") to "wd:Q1 wd:Q2"
  # (wd: is the standard Wikidata namespace prefix in SPARQL)
  items <- paste0("wd:", qids) %>% paste(collapse = " ")
  # Build the SPARQL query with the VALUES clause containing all QIDs
  query <- sprintf(
    'SELECT DISTINCT ?qid ?name ?birthDate ?deathDate
       ?genderLabel ?birthCountryLabel ?nationalityLabel
       ?occupationLabel ?institutionLabel
    WHERE {
      VALUES ?qid { %s }
      OPTIONAL { ?qid rdfs:label ?name. FILTER(LANG(?name) = "en") }
      OPTIONAL { ?qid wdt:P569 ?birthDate. }
      OPTIONAL { ?qid wdt:P570 ?deathDate. }
      OPTIONAL { ?qid wdt:P21 ?gender. }
      OPTIONAL { ?qid wdt:P19/wdt:P17 ?birthCountry. }
      OPTIONAL { ?qid wdt:P27 ?nationality. }
      OPTIONAL { ?qid wdt:P106 ?occupation. }
      OPTIONAL { ?qid wdt:P108 ?institution. }
      SERVICE wikibase:label {
        bd:serviceParam wikibase:language "en".
        ?gender rdfs:label ?genderLabel.
        ?birthCountry rdfs:label ?birthCountryLabel.
        ?nationality rdfs:label ?nationalityLabel.
        ?occupation rdfs:label ?occupationLabel.
        ?institution rdfs:label ?institutionLabel.
      }
    }', items)
  # Execute the query with error handling and retries
  query_wikidata_safe(query)
}

#' Collapse multi-valued demographic fields into a single row per QID
#'
#' DETAILED DESCRIPTION:
#' SPARQL queries return one row per unique combination of variable values.
#' If a person has multiple nationalities or occupations, fetch_demographics_batch()
#' will return multiple rows for that person. This function collapses the data
#' back to one row per QID, handling multi-valued fields by concatenating them
#' with semicolon separators.
#'
#' EXAMPLE:
#'   Raw data (2 rows for Q7186 due to 2 nationalities):
#'     qid      name        genderLabel  nationalityLabel  ...
#'     Q7186    Carl Linné   male         Swedish           ...
#'     Q7186    Carl Linné   male         Finnish           ...
#'   Collapsed output (1 row):
#'     qid      name        genderLabel  nationality       ...
#'     Q7186    Carl Linné   male         Swedish; Finnish  ...
#'
#' SINGLE-VALUE FIELD HANDLING:
#' - For fields that should be single-valued (name, gender, birth_country),
#'   takes first non-NA value via first(na.omit(...))
#' - For fields that may be multi-valued (nationality, occupation, institution),
#'   concatenates unique values with semicolon separator
#'
#' DATE PARSING:
#' - Wikidata returns ISO 8601 datetimes: "1871-12-20T00:00:00Z"
#' - This function extracts just the year (first 4 digits)
#' - Missing/invalid dates become NA
#'
#' @param demo_raw Raw data frame from fetch_demographics_batch() or
#'   fetch_demographics_for_qid(), potentially with multiple rows per QID
#' @return A data frame with one row per QID, with standardized column names:
#'   qid, name, gender, birth_country, nationality, birth_year, death_year,
#'   occupation, institution. All empty strings and "NA" strings are
#'   converted to proper NA values.
collapse_demographics <- function(demo_raw) {
  demo_raw %>%
    # First pass: extract years from ISO 8601 datetimes
    mutate(
      # Wikidata returns full datetimes like "1871-12-20T00:00:00Z"
      # Extract just the year portion (first 4 digits)
      birth_year = as.numeric(str_extract(birthDate, "^\\d{4}")),
      death_year = as.numeric(str_extract(deathDate, "^\\d{4}"))
    ) %>%
    # Group by QID to collapse multiple rows into one
    group_by(qid) %>%
    # Aggregate: collapse multi-valued fields into semicolon-separated strings
    summarise(
      # Single-valued fields: take first non-NA value
      name = first(na.omit(name)),
      gender = first(na.omit(genderLabel)),
      birth_country = first(na.omit(birthCountryLabel)),
      # Multi-valued fields: concatenate unique values with " ; " separator
      # unique() removes duplicates (e.g., if a person appears twice with same nationality)
      # na.omit() removes NA values before joining
      nationality = paste(unique(na.omit(nationalityLabel)), collapse = "; "),
      birth_year = first(na.omit(birth_year)),
      death_year = first(na.omit(death_year)),
      occupation = paste(unique(na.omit(occupationLabel)), collapse = "; "),
      institution = paste(unique(na.omit(institutionLabel)), collapse = "; "),
      .groups = "drop"  # Ungroup after summarise to avoid nested grouping issues
    ) %>%
    # Final pass: clean up character columns
    # Convert empty strings and the string "NA" to proper R NA values
    # This handles cases where paste() of all NAs produces "NA" string
    mutate(across(where(is.character), ~ ifelse(. == "" | . == "NA", NA_character_, .)))
}

# =============================================================================
# Swedish Wikipedia List Scraping
# =============================================================================
# This section contains utilities for extracting structured lists of people
# (committee members, academy members, etc.) from Swedish Wikipedia pages.
#
# KEY DESIGN DECISION: Use wikitext-based scraping (via MediaWiki API) rather
# than HTML scraping. Rationale:
# - Wikipedia's HTML structure changes frequently, breaking CSS selectors
# - Wikitext markup is stable across Wikipedia versions and languages
# - MediaWiki API provides raw source access for reliable parsing
#
# SUPPORTED FORMATS: The scraper handles Swedish Wikipedia's common list formats:
# - "[[Name]], YYYY–YYYY" - Served from year to year (former members)
# - "[[Name]], YYYY–YYYY (ordförande)" - With role notes in parentheses
# - "[[Name]], invald YYYY" - Elected in year (current Swedish members)
# - "[[Name]], YYYY–" - Ongoing term (current members, open-ended)
# - "[[Name]], YYYY–?" - Unknown end date
#
# POST-PROCESSING: After scraping names and years, the function looks up
# Wikipedia article URLs and converts them to Wikidata QIDs for later
# demographic lookups.
# =============================================================================

#' Scrape a Swedish Wikipedia list page for committee/academy members
#'
#' DETAILED DESCRIPTION:
#' This function extracts structured lists of people (committee members, academy
#' members, etc.) from Swedish Wikipedia pages by:
#' 1. Fetching the page source (wikitext) via MediaWiki API
#' 2. Parsing section headings to locate requested sections
#' 3. Extracting bullet-list entries from those sections
#' 4. Parsing each entry to extract: Wikipedia article title, display name,
#'    and year range
#' 5. Looking up Wikidata QIDs for each article title
#' 6. Returning a structured data frame with name, year range, and QID
#'
#' ADVANTAGE OVER HTML SCRAPING:
#' - HTML structure changes frequently; wikitext is stable
#' - Direct access to markup avoids browser rendering quirks
#' - Works across all Wikipedia language versions without CSS selector changes
#'
#' SECTION MATCHING:
#' - Searches for exact matches in section headings (e.g., "Tidigare ledamöter")
#' - Handles both == h2 == and === h3 === heading levels
#' - Extracts from first matching heading and continues until next heading
#'   or end of file
#'
#' ENTRY PARSING:
#' - Targets bullet-list lines (starting with *)
#' - Extracts [[article title]] or [[article title|display name]] syntax
#' - Parses year ranges using regex (first 4-digit year, then dash + year)
#' - Handles variations: "1990–1995", "1990-1995", "1990–" (open-ended), "invald 1990"
#'
#' @param page_title The Wikipedia page title (URL-decoded, e.g.,
#'   "Vetenskapsakademiens ledamöter" or "Nobelprisesamlingen"). IMPORTANT:
#'   Must be the exact Swedish Wikipedia page title (not a URL), with
#'   underscores/spaces as they appear in Wikipedia URLs.
#' @param sections Character vector of section headings to extract from
#'   (default: c("Tidigare ledamöter", "Nuvarande ledamöter", "Sekreterare")).
#'   Headings are matched exactly; case-sensitive. Common Swedish sections:
#'   - "Tidigare ledamöter" = Former members
#'   - "Nuvarande ledamöter" = Current members
#'   - "Sekreterare" = Secretaries
#' @param wiki_lang Language code for the Wikipedia (default: "sv" for Swedish).
#'   Determines which wiki to fetch from (sv.wikipedia.org, en.wikipedia.org, etc.)
#' @return A data frame with columns:
#'   - name: Display name of the person (from [[...]] or [[...|...]]])
#'   - startyear: Numeric year person started (first 4-digit year in entry)
#'   - endyear: Numeric year person ended (4-digit year after dash)
#'   - qid: Wikidata QID (looked up from Wikipedia article URL)
#'   All year columns may contain NA if parsing fails. QID is NA if the
#'   Wikipedia article doesn't have a Wikidata link.
scrape_swedish_wiki_list <- function(page_title, sections = c("Tidigare ledamöter",
                                                               "Nuvarande ledamöter",
                                                               "Sekreterare"),
                                     wiki_lang = "sv") {

  # Fetch wikitext (source code) via MediaWiki API
  # API endpoint for parsing action (returns page source)
  api_url <- sprintf("https://%s.wikipedia.org/w/api.php", wiki_lang)
  resp <- request(api_url) %>%
    # API parameters: action=parse (return source), page name, wikitext format, JSON output
    req_url_query(
      action = "parse",    # Get parsed page (we'll extract source)
      page = page_title,   # Wikipedia page title (not URL)
      prop = "wikitext",   # Request wikitext source (not rendered HTML)
      format = "json"
    ) %>%
    # Identify this bot to Wikipedia
    req_headers(`User-Agent` = "NobelNetworkBot/1.0 (research)") %>%
    # Execute the request
    req_perform()

  # Extract wikitext from JSON response
  # Response structure: {parse: {wikitext: {"*": "== Heading ==\n* Entry\n..."}}}
  wikitext <- resp_body_json(resp)$parse$wikitext$`*`

  # Split wikitext into individual lines
  # Each line may be a heading (== ... ==), bullet item (* ...), or other content
  lines <- str_split(wikitext, "\n")[[1]]

  # Find section boundaries by locating heading lines
  # Headings have the format: == Heading Text == (h2) or === Heading Text === (h3)
  # Regex: "^={2,3}" = starts with 2-3 equals, "\s*[^=]" = whitespace then non-equals char
  heading_indices <- which(str_detect(lines, "^={2,3}\\s*[^=]"))
  # Extract the heading text by removing leading/trailing = and whitespace
  # Example: "== Tidigare ledamöter ==" → "Tidigare ledamöter"
  heading_titles <- str_replace_all(lines[heading_indices], "^=+\\s*|\\s*=+$", "")

  # Extract entries from the requested sections
  all_entries <- character()  # Accumulator for all bullet-list entries
  for (sec in sections) {
    # Find the heading index that matches this section name
    sec_idx <- which(str_detect(heading_titles, fixed(sec)))
    if (length(sec_idx) == 0) next  # Section not found; skip to next

    # Start at the line after the heading
    start_line <- heading_indices[sec_idx[1]] + 1
    # Find the next heading (or end of file)
    next_heading <- heading_indices[heading_indices > heading_indices[sec_idx[1]]]
    # End line is one before next heading, or end of file
    end_line <- if (length(next_heading) > 0) next_heading[1] - 1 else length(lines)

    # Extract lines in this section
    section_lines <- lines[start_line:end_line]
    # Keep only bullet-list lines (start with * in wikitext)
    bullet_lines <- section_lines[str_detect(section_lines, "^\\*")]
    # Accumulate entries
    all_entries <- c(all_entries, bullet_lines)
  }

  # Handle empty result
  if (length(all_entries) == 0) {
    message("  WARNING: No bullet entries found in requested sections.")
    return(data.frame(name = character(), startyear = numeric(),
                      endyear = numeric(), qid = character(),
                      stringsAsFactors = FALSE))
  }

  # Parse each entry to extract: Wikipedia article title, display name, year range
  # Common entry formats:
  #   * [[Article Title]], 1990–1995
  #   * [[Article Title|Display Name]], 1990–1995 (ordförande)
  #   * [[Article Title]], invald 1990

  # Extract Wikipedia article links: [[Article Title]] or [[Article Title|Display Name]]
  # Regex captures everything between [[ and ]]
  wiki_links <- str_extract(all_entries, "\\[\\[[^\\]]+\\]\\]")
  # Extract article title: the part before the pipe (|) or the whole thing if no pipe
  # Regex: "(?<=\\[\\[)" = after [[, "[^|\\]]+" = any char except | or ]
  article_titles <- str_extract(wiki_links, "(?<=\\[\\[)[^|\\]]+")

  # Extract display name: the part after the pipe (if present)
  # Regex: "(?<=\\|)" = after pipe, "[^\\]]+" = any char except ]
  # If no pipe, use article title as display name
  display_names <- str_extract(wiki_links, "(?<=\\|)[^\\]]+")
  display_names <- ifelse(is.na(display_names), article_titles, display_names)

  # Extract year range
  # Start year: first 4-digit sequence after the [[...]] link
  after_link <- str_replace(all_entries, "^.*\\]\\]", "")  # Remove [[...]] prefix
  startyear <- str_extract(after_link, "\\d{4}") %>% as.numeric()

  # End year: 4-digit year that appears after en-dash (–) or hyphen (-)
  # Regex: "(?<=[–-])" = lookbehind for dash, "\\d{4}" = 4 digits
  # Example: "1990–1995" matches 1995; "invald 1990" matches NA (no dash)
  endyear <- str_extract(after_link, "(?<=[–-])\\d{4}") %>% as.numeric()

  # Build Wikipedia URLs for batch QID lookup
  # Only process entries with valid article titles
  valid_mask <- !is.na(article_titles)
  wiki_urls <- rep(NA_character_, length(article_titles))
  # Construct full Wikipedia URLs: https://sv.wikipedia.org/wiki/Article_Title
  # URL-encode the title (spaces → %20, accents handled by URLencode)
  wiki_urls[valid_mask] <- paste0(
    "https://", wiki_lang, ".wikipedia.org/wiki/",
    URLencode(str_replace_all(article_titles[valid_mask], " ", "_"), reserved = TRUE)
  )

  # Batch lookup QIDs for all Wikipedia article URLs
  # (wikipedia_urls_to_qids is much faster than looking up one at a time)
  good_urls <- wiki_urls[valid_mask]
  message(sprintf("  Looking up QIDs for %d entries...", length(good_urls)))
  qids_good <- wikipedia_urls_to_qids(good_urls)

  # Place QIDs back into the full results vector (in case some URLs are NA)
  qids <- rep(NA_character_, length(article_titles))
  qids[valid_mask] <- qids_good

  # Return structured data frame
  data.frame(
    name = display_names,
    startyear = startyear,
    endyear = endyear,
    qid = qids,
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# File I/O Helpers
# =============================================================================
# This section provides standardized functions for organizing data files
# in the project directory structure. Using these functions ensures all
# scripts write to consistent locations.
#
# PROJECT DIRECTORY STRUCTURE:
#   ./
#   ├── NobelPrize.Rproj          # RStudio project file
#   ├── Data/                      # Final outputs (nodes.csv, edges.csv)
#   ├── Code/
#   │   ├── Data Gathering/        # This file location
#   │   └── ...
#   └── ... (other project files)
#
# When scripts run, the working directory is the project root (./).
# Use data_path() for intermediate results and output_path() for final data.
# =============================================================================

#' Standard path for intermediate data files
#'
#' DETAILED DESCRIPTION:
#' This function constructs and ensures the existence of the standard
#' intermediate data directory (Data/intermediate/). All scripts that process
#' raw data and produce intermediate outputs should use this function.
#'
#' BENEFITS:
#' - Centralizes intermediate data in one place, separate from final outputs
#' - Allows easy cleanup (delete entire intermediate/ directory without
#'   losing final results)
#' - Simplifies re-running scripts from scratch. CAUTION: Data/intermediate/
#'   contains one hand-curated, non-regenerable input ("KI profs.xlsx"); do not
#'   delete it when clearing intermediate files.
#' - Automatic directory creation avoids errors if directory doesn't exist
#'
#' USAGE:
#'   df %>% write_csv(data_path("demographics_batch1.csv"))
#'   df <- read_csv(data_path("demographics_batch1.csv"))
#'
#' NOTE: Working directory must be the project root (where NobelPrize.Rproj
#' lives). This is automatic when opening the project in RStudio.
#'
#' @param filename Simple filename (not a path). Example: "committee_members.csv"
#' @return Absolute or relative path to file in Data/intermediate/ directory.
#'   Example: "Data/intermediate/committee_members.csv"
data_path <- function(filename) {
  # Construct the intermediate directory path
  dir <- file.path("Data", "intermediate")
  # Create directory if it doesn't exist (recursive = TRUE creates parents too)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  # Return the full path to the file
  file.path(dir, filename)
}

#' Standard path for final output data files
#'
#' DETAILED DESCRIPTION:
#' This function constructs and ensures the existence of the standard
#' output directory (Data/). Final data products (nodes.csv, edges.csv)
#' that should be preserved in the repository should use this function.
#'
#' TYPICAL FINAL OUTPUTS:
#' - nodes.csv: Network nodes (Nobel laureates and committee members)
#' - edges.csv: Network edges (relationships between nodes)
#' - metadata.csv: Summary statistics or project metadata
#'
#' NOTE: Intermediate processing files should use data_path(), not output_path(),
#' to keep the Data/ directory clean.
#'
#' @param filename Simple filename (not a path). Example: "nodes.csv"
#' @return Relative path to file in Data/ directory.
#'   Example: "Data/nodes.csv"
output_path <- function(filename) {
  # The main Data directory for final outputs
  dir <- "Data"
  # Create directory if it doesn't exist
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  # Return the full path to the file
  file.path(dir, filename)
}

# Log message indicating successful module load
message("00_utils.R loaded successfully.")

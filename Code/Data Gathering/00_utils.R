# =============================================================================
# 00_utils.R
# Shared utility functions for Nobel Prize data gathering
# =============================================================================

library(tidyverse)
library(rvest)
library(httr2)    # HTTP requests (replaces archived WikidataR)
library(lubridate)
library(furrr)
library(cli)      # progress bars and styled messaging
library(readxl)   # reading Excel files (.xlsx)

# Set up parallel backend: use all but one core, capped at 12
N_WORKERS <- min(parallel::detectCores() - 1, 12)
plan(multisession, workers = N_WORKERS)
message(sprintf("Parallel backend: multisession with %d workers", N_WORKERS))

# =============================================================================
# Wikipedia -> Wikidata QID conversion
# =============================================================================

#' Convert a Wikipedia page URL to a Wikidata QID (single URL, slow)
#'
#' Scrapes the sidebar link from the page. Use wikipedia_urls_to_qids() for
#' batch lookups, which is ~50x faster.
#'
#' @param url A Wikipedia page URL (any language)
#' @param max_retries Number of retry attempts on failure
#' @return A character string QID (e.g., "Q7186") or NA on failure
wikipedia_url_to_qid <- function(url, max_retries = 5) {
  for (attempt in seq_len(max_retries)) {
    result <- tryCatch({
      url %>%
        read_html() %>%
        html_elements(xpath = '//*[(@id = "t-wikibase")]') %>%
        html_elements("a") %>%
        html_attr("href") %>%
        str_extract("(?<=https://www.wikidata.org/wiki/Special:EntityPage/).+")
    }, error = function(e) {
      message(sprintf("  Attempt %d failed for %s: %s", attempt, url, e$message))
      Sys.sleep(2 * attempt)  # exponential backoff
      NULL
    })
    if (!is.null(result) && length(result) > 0) return(result)
  }
  return(NA_character_)
}


#' Batch-convert Wikipedia URLs to Wikidata QIDs via the MediaWiki API
#'
#' Uses the action=query&prop=pageprops API to resolve up to 50 titles per
#' request, which is dramatically faster than scraping individual pages.
#'
#' @param urls A character vector of Wikipedia page URLs (all from the same
#'   language wiki, e.g., all sv.wikipedia.org or all en.wikipedia.org)
#' @param batch_size Number of titles per API request (max 50)
#' @param max_retries Number of retry attempts per batch on failure
#' @return A character vector of QIDs (same length/order as input), NA for
#'   pages that could not be resolved
wikipedia_urls_to_qids <- function(urls, batch_size = 50, max_retries = 5) {
  if (length(urls) == 0) return(character(0))

  # Extract the wiki base URL and page titles from the URLs
  # e.g., "https://sv.wikipedia.org/wiki/Carl_von_Linné" ->
  #   base = "https://sv.wikipedia.org", title = "Carl_von_Linné"
  parsed <- str_match(urls, "^(https?://[^/]+)/wiki/(.+)$")
  bases <- parsed[, 2]
  titles <- parsed[, 3] %>% URLdecode()

  # All URLs should be from the same wiki
  unique_bases <- unique(na.omit(bases))
  if (length(unique_bases) > 1) {
    message("  WARNING: URLs from multiple wikis detected. Processing each wiki separately.")
  }

  # Initialize results
  qids <- rep(NA_character_, length(urls))

  for (wiki_base in unique_bases) {
    wiki_mask <- which(bases == wiki_base)
    wiki_titles <- titles[wiki_mask]

    # Process in batches
    batches <- split(seq_along(wiki_titles),
                     ceiling(seq_along(wiki_titles) / batch_size))

    for (b_idx in seq_along(batches)) {
      batch_indices <- batches[[b_idx]]
      batch_titles <- wiki_titles[batch_indices]

      if (b_idx %% 5 == 0 || b_idx == length(batches)) {
        message(sprintf("  [%s] Resolving QIDs: batch %d / %d (%.0f%%)",
                        format(Sys.time(), "%H:%M:%S"),
                        b_idx, length(batches),
                        100 * b_idx / length(batches)))
      }

      # Build API URL
      titles_param <- paste(batch_titles, collapse = "|")
      api_url <- sprintf(
        "%s/w/api.php?action=query&prop=pageprops&ppprop=wikibase_item&titles=%s&format=json",
        wiki_base, URLencode(titles_param, reserved = TRUE)
      )

      # Fetch with retries
      result <- NULL
      for (attempt in seq_len(max_retries)) {
        result <- tryCatch({
          jsonlite::fromJSON(api_url)
        }, error = function(e) {
          message(sprintf("    Batch %d attempt %d failed: %s",
                          b_idx, attempt, e$message))
          Sys.sleep(2 * attempt)
          NULL
        })
        if (!is.null(result)) break
      }

      if (is.null(result) || is.null(result$query$pages)) next

      # Build a title -> QID lookup from the response
      pages <- result$query$pages
      title_to_qid <- list()
      for (page_id in names(pages)) {
        pg <- pages[[page_id]]
        qid_val <- pg$pageprops$wikibase_item
        if (!is.null(qid_val)) {
          title_to_qid[[pg$title]] <- qid_val
        }
      }

      # Handle normalized titles (API may normalize "Carl_von_Linné" to "Carl von Linné")
      normalized <- result$query$normalized
      norm_map <- list()
      if (!is.null(normalized)) {
        for (i in seq_len(nrow(normalized))) {
          norm_map[[normalized$from[i]]] <- normalized$to[i]
        }
      }

      # Map batch titles back to QIDs
      for (i in seq_along(batch_titles)) {
        title <- batch_titles[i]
        # Check if this title was normalized
        lookup_title <- if (!is.null(norm_map[[title]])) norm_map[[title]] else title
        qid_val <- title_to_qid[[lookup_title]]
        if (!is.null(qid_val)) {
          qids[wiki_mask[batch_indices[i]]] <- qid_val
        }
      }

      Sys.sleep(0.2)  # small delay between batches to be polite
    }
    message(sprintf("  QID resolution complete for %s", wiki_base))
  }

  qids
}

# =============================================================================
# Wikidata SPARQL querying
# =============================================================================

#' Execute a Wikidata SPARQL query with retry logic
#'
#' Sends the query directly to the Wikidata Query Service endpoint.
#' No external Wikidata package needed.
#'
#' @param query A SPARQL query string
#' @param max_retries Number of retry attempts
#' @return A data frame of results
query_wikidata_safe <- function(query, max_retries = 10) {
  endpoint <- "https://query.wikidata.org/sparql"

  for (attempt in seq_len(max_retries)) {
    result <- tryCatch({
      # Use POST for SPARQL queries (GET can hit URL length limits for large queries)
      resp <- request(endpoint) %>%
        req_headers(
          Accept = "application/sparql-results+json",
          `User-Agent` = "NobelPrizeResearchBot/1.0 (https://github.com/chadtopaz/nobelprize)",
          `Content-Type` = "application/x-www-form-urlencoded"
        ) %>%
        req_body_form(query = query) %>%
        req_retry(max_tries = 1) %>%
        req_perform()

      json <- resp_body_json(resp)

      # Parse SPARQL JSON results into a data frame
      vars <- json$head$vars
      bindings <- json$results$bindings

      if (length(bindings) == 0) {
        return(data.frame(matrix(ncol = length(vars), nrow = 0,
                                 dimnames = list(NULL, vars))))
      }

      rows <- lapply(bindings, function(row) {
        vals <- sapply(vars, function(v) {
          val <- row[[v]]$value
          if (is.null(val)) NA_character_ else val
        })
        names(vals) <- vars
        as.data.frame(t(vals), stringsAsFactors = FALSE)
      })

      df <- bind_rows(rows)

      # Clean up QID columns: extract just the Q-number from full URIs
      for (col in names(df)) {
        if (any(str_detect(df[[col]], "^http://www.wikidata.org/entity/Q"), na.rm = TRUE)) {
          df[[col]] <- str_extract(df[[col]], "Q\\d+")
        }
      }

      df
    }, error = function(e) {
      message(sprintf("  WARNING: Query attempt %d/%d failed: %s", attempt, max_retries, e$message))
      Sys.sleep(5 * attempt)  # exponential backoff
      NULL
    })

    if (!is.null(result)) return(result)
  }
  message("  ERROR: Maximum retries reached. Wikidata query failed.")
  stop("Maximum retries reached. Wikidata query failed.")
}

#' Fetch demographics for a single QID from Wikidata
#' Returns: qid, name, gender, birth_country, nationality, birth_year, death_year,
#'          birth_date, death_date, occupation, institution
#'
#' @param qid A Wikidata QID string (e.g., "Q7186")
#' @return A data frame with one or more rows (multiple nationalities etc.)
fetch_demographics_for_qid <- function(qid) {
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
  query_wikidata_safe(query)
}

#' Fetch demographics for a batch of QIDs in a single SPARQL query
#' More efficient than one-at-a-time when you have many QIDs
#'
#' @param qids A character vector of QIDs
#' @return A data frame
fetch_demographics_batch <- function(qids) {
  items <- paste0("wd:", qids) %>% paste(collapse = " ")
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
  query_wikidata_safe(query)
}

#' Collapse multi-valued demographic fields into a single row per QID
#'
#' @param demo_raw Raw data frame from fetch_demographics_batch
#' @return A data frame with one row per QID
collapse_demographics <- function(demo_raw) {
  demo_raw %>%
    mutate(
      # Wikidata returns datetimes like "1871-12-20T00:00:00Z"
      birth_year = as.numeric(str_extract(birthDate, "^\\d{4}")),
      death_year = as.numeric(str_extract(deathDate, "^\\d{4}"))
    ) %>%
    group_by(qid) %>%
    summarise(
      name = first(na.omit(name)),
      gender = first(na.omit(genderLabel)),
      birth_country = first(na.omit(birthCountryLabel)),
      nationality = paste(unique(na.omit(nationalityLabel)), collapse = "; "),
      birth_year = first(na.omit(birth_year)),
      death_year = first(na.omit(death_year)),
      occupation = paste(unique(na.omit(occupationLabel)), collapse = "; "),
      institution = paste(unique(na.omit(institutionLabel)), collapse = "; "),
      .groups = "drop"
    ) %>%
    mutate(across(where(is.character), ~ ifelse(. == "" | . == "NA", NA_character_, .)))
}

# =============================================================================
# Scraping helpers
# =============================================================================

#' Scrape a Swedish Wikipedia list page for committee/academy members
#'
#' Uses the MediaWiki API to fetch wikitext (not rendered HTML) for
#' reliable parsing. This avoids fragile CSS selectors that break when
#' Wikipedia's HTML structure changes.
#'
#' Extracts entries from bullet-list sections matching `sections` parameter.
#' Handles several common Swedish Wikipedia formats:
#'   - "[[Name]], YYYY–YYYY"             (former members with full range)
#'   - "[[Name]], YYYY–YYYY (ordförande)" (former members with role notes)
#'   - "[[Name]], invald YYYY"           (current members, Swedish format)
#'   - "[[Name]], YYYY–"                 (current members, open-ended)
#'   - "[[Name]], YYYY–?"               (members with unknown end date)
#'
#' @param page_title The Wikipedia page title (URL-decoded, e.g. "Vetenskapsakademiens...")
#' @param sections Character vector of section headings to extract from
#'   (e.g., c("Tidigare ledamöter", "Nuvarande ledamöter", "Sekreterare"))
#' @param wiki_lang Language code for the Wikipedia (default "sv")
#' @return A data frame with name, startyear, endyear, qid columns
scrape_swedish_wiki_list <- function(page_title, sections = c("Tidigare ledamöter",
                                                               "Nuvarande ledamöter",
                                                               "Sekreterare"),
                                     wiki_lang = "sv") {

  # Fetch wikitext via MediaWiki API
  api_url <- sprintf("https://%s.wikipedia.org/w/api.php", wiki_lang)
  resp <- request(api_url) %>%
    req_url_query(
      action = "parse",
      page = page_title,
      prop = "wikitext",
      format = "json"
    ) %>%
    req_headers(`User-Agent` = "NobelNetworkBot/1.0 (research)") %>%
    req_perform()

  wikitext <- resp_body_json(resp)$parse$wikitext$`*`

  # Split wikitext into sections by == headings ==
  # Each section starts with == Title == or === Title ===
  lines <- str_split(wikitext, "\n")[[1]]

  # Find section boundaries (match both == h2 == and === h3 === headings)
  heading_indices <- which(str_detect(lines, "^={2,3}\\s*[^=]"))
  # Extract heading text by removing leading/trailing = and whitespace
  heading_titles <- str_replace_all(lines[heading_indices], "^=+\\s*|\\s*=+$", "")

  # Extract entries from the requested sections
  all_entries <- character()
  for (sec in sections) {
    sec_idx <- which(str_detect(heading_titles, fixed(sec)))
    if (length(sec_idx) == 0) next

    start_line <- heading_indices[sec_idx[1]] + 1
    # End at next heading or end of file
    next_heading <- heading_indices[heading_indices > heading_indices[sec_idx[1]]]
    end_line <- if (length(next_heading) > 0) next_heading[1] - 1 else length(lines)

    section_lines <- lines[start_line:end_line]
    # Keep only bullet-list lines (start with *)
    bullet_lines <- section_lines[str_detect(section_lines, "^\\*")]
    all_entries <- c(all_entries, bullet_lines)
  }

  if (length(all_entries) == 0) {
    message("  WARNING: No bullet entries found in requested sections.")
    return(data.frame(name = character(), startyear = numeric(),
                      endyear = numeric(), qid = character(),
                      stringsAsFactors = FALSE))
  }

  # Parse each entry: extract [[link|display]] or [[link]] and years
  # Entry format: *[[Article title|Display name]], YYYY–YYYY (role notes)
  # or: *[[Article title]], invald YYYY

  # Extract Wikipedia article titles from [[...]] links
  # Handle both [[Title|Name]] and [[Title]] forms
  wiki_links <- str_extract(all_entries, "\\[\\[[^\\]]+\\]\\]")
  article_titles <- str_extract(wiki_links, "(?<=\\[\\[)[^|\\]]+")

  # Extract display names (after | if present, otherwise article title)
  display_names <- str_extract(wiki_links, "(?<=\\|)[^\\]]+")
  display_names <- ifelse(is.na(display_names), article_titles, display_names)

  # Extract years
  # Start year: first 4-digit year in the entry (after the link)
  after_link <- str_replace(all_entries, "^.*\\]\\]", "")
  startyear <- str_extract(after_link, "\\d{4}") %>% as.numeric()

  # End year: 4-digit year after en-dash (–) or hyphen (-)
  endyear <- str_extract(after_link, "(?<=[–-])\\d{4}") %>% as.numeric()

  # Build Wikipedia URLs for QID lookup
  # Clean article titles: remove parenthetical disambiguators in URL
  valid_mask <- !is.na(article_titles)
  wiki_urls <- rep(NA_character_, length(article_titles))
  wiki_urls[valid_mask] <- paste0(
    "https://", wiki_lang, ".wikipedia.org/wiki/",
    URLencode(str_replace_all(article_titles[valid_mask], " ", "_"), reserved = TRUE)
  )

  # Look up QIDs
  good_urls <- wiki_urls[valid_mask]
  message(sprintf("  Looking up QIDs for %d entries...", length(good_urls)))
  qids_good <- wikipedia_urls_to_qids(good_urls)

  qids <- rep(NA_character_, length(article_titles))
  qids[valid_mask] <- qids_good

  data.frame(
    name = display_names,
    startyear = startyear,
    endyear = endyear,
    qid = qids,
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# File I/O helpers
# =============================================================================

#' Standard path for intermediate data files
#' All intermediate outputs go to Data/intermediate/
#' Note: working directory is the project root (where NobelPrize.Rproj lives)
data_path <- function(filename) {
  dir <- file.path("Data", "intermediate")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, filename)
}

#' Standard path for final output data files
#' Final nodes.csv and edges.csv go to Data/
output_path <- function(filename) {
  dir <- "Data"
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, filename)
}

message("00_utils.R loaded successfully.")

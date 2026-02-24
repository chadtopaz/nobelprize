# =============================================================================
# 03_nominators_nominees.R
# Scrape Nobel Prize Nomination Archive for nominator→nominee relationships
#
# The nomination archive is subject to a 50-year secrecy rule.
# As of 2025, Physics/Chemistry/Literature/Peace are available through 1975.
# Physiology/Medicine remains limited to 1953 (Karolinska has not released
# additional years, likely due to the 1977 Swedish transparency law change
# that also prompted the creation of the Nobel Assembly).
#
# Source: https://www.nobelprize.org/nomination/archive/
#
# Output: nominations.csv with columns:
#   nomination_id, year, prize,
#   nominee_name, nominee_person_id,
#   nominee_university, nominee_city, nominee_country, nominee_profession,
#   nominator_name, nominator_person_id,
#   nominator_university, nominator_city, nominator_country, nominator_profession
#
# The "id" fields are the nobelprize.org person IDs (not Wikidata QIDs).
# Wikidata QID lookup happens in step 05 for all unique people.
# University, city, country, and profession are per-nomination contextual
# data from the detail pages (not the person pages).
# =============================================================================

source("Code/Data Gathering/00_utils.R")
library(httr)

# =============================================================================
# Configuration
# =============================================================================

# Prize categories and their archive codes
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
#' @param prize_code Numeric prize code (1=Physics, 2=Chemistry, etc.)
#' @param year Nomination year
#' @return A character vector of nomination IDs
get_nomination_ids <- function(prize_code, year) {
  url <- sprintf(
    "https://www.nobelprize.org/nomination/archive/list.php?prize=%d&year=%d",
    prize_code, year
  )

  tryCatch({
    page <- read_html(url)

    # Extract "show.php?id=XXX" links
    ids <- page %>%
      html_elements("a[href*='show.php?id=']") %>%
      html_attr("href") %>%
      str_extract("(?<=id=)\\d+")

    ids[!is.na(ids)]
  }, error = function(e) {
    message(sprintf("    WARNING: Failed to get list for prize=%d, year=%d: %s",
                    prize_code, year, e$message))
    character(0)
  })
}


#' Scrape a single nomination detail page
#'
#' Extracts nominee and nominator information from the show.php page,
#' including contextual fields: university, city, country, and profession.
#' Each nomination may have multiple nominees and/or multiple nominators.
#'
#' Uses a purely DOM-based approach: walks through <tr> elements in document
#' order, detects "Nominee:" and "Nominator:" section headers from <b> tags,
#' identifies person links (show_people.php), and collects labeled fields
#' (University, City, Country, Profession) from <span class="rubr"> labels.
#' Each person block runs from a Name row to the next section header or
#' next person Name row.
#'
#' @param nomination_id The nomination ID
#' @return A data frame with one row per nominee-nominator pair
scrape_nomination <- function(nomination_id) {
  url <- sprintf(
    "https://www.nobelprize.org/nomination/archive/show.php?id=%s",
    nomination_id
  )

  tryCatch({
    page <- read_html(url)

    # Extract year and prize from the header
    header <- page %>%
      html_elements("td[colspan='2']") %>%
      html_text()

    td_texts <- page %>% html_elements("td") %>% html_text()
    year <- td_texts[which(str_detect(td_texts, "^\\d{4}$"))][1] %>%
      as.numeric()

    # Extract prize name from header
    # Most prizes: "Nomination for Nobel Prize in X" → extract after "in "
    # Peace: "Nomination for Nobel Peace Prize" → special case
    nom_header <- header[str_detect(header, "Nomination for Nobel")][1]
    if (!is.na(nom_header) && str_detect(nom_header, "Nobel Peace Prize")) {
      prize <- "Peace"
    } else {
      prize <- str_extract(nom_header, "(?<=in ).*$")
    }

    # ---- DOM-based nominee/nominator classification ----
    # Walk through all <tr> elements in document order. Each <tr> is either:
    #   1. A section header: <td colspan="2"><b>Nominee:</b></td>
    #   2. A labeled field: <td><span class="rubr">Label:</span></td><td>Value</td>
    #   3. Other content (prizes awarded, spacers, etc.)
    #
    # We track: which section we're in (nominee/nominator), and the current
    # person being accumulated. A "Name:" label starts a new person block;
    # University/City/Country/Profession labels add to the current person.
    all_trs <- page %>% html_elements("tr")

    section <- "unknown"
    nominees <- list()
    nominators <- list()
    current_person <- NULL

    # Helper: save current_person to the appropriate list
    save_person <- function() {
      if (!is.null(current_person) && current_person$section != "unknown") {
        entry <- data.frame(
          name = current_person$name,
          person_id = current_person$person_id,
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

      # Check for section headers: <b>Nominee:</b> or <b>Nominator:</b>
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

      if (section == "unknown" || length(tds) < 2) next

      # Check for labeled fields via <span class="rubr">
      rubr <- tds[[1]] %>% html_elements("span.rubr") %>% html_text() %>% str_trim()
      if (length(rubr) == 0) next

      label <- rubr[1]
      value <- html_text(tds[[2]]) %>% str_trim()

      # Name field: starts a new person block
      if (str_detect(label, "^Name:")) {
        save_person()

        # Extract person link if present
        link <- tr %>% html_elements("a[href*='show_people.php']")
        if (length(link) > 0) {
          pid   <- html_attr(link[[1]], "href") %>% str_extract("\\d+$")
          pname <- html_text(link[[1]]) %>% str_trim()
        } else {
          pid   <- NA_character_
          pname <- value
        }

        current_person <- list(
          section = section, name = pname, person_id = pid,
          university = NULL, city = NULL, country = NULL, profession = NULL
        )
        next
      }

      # Collect contextual fields for current person
      if (!is.null(current_person) && !is.na(value) && nchar(value) > 0) {
        if (str_detect(label, "^University:"))  current_person$university  <- value
        if (str_detect(label, "^City:"))        current_person$city        <- value
        if (str_detect(label, "^Country:"))     current_person$country     <- value
        if (str_detect(label, "^Profession:"))  current_person$profession  <- value
      }
    }

    # Save the last person
    save_person()

    # Build data frames from collected lists
    nominees_df <- if (length(nominees) > 0) bind_rows(nominees) else
      data.frame(name = character(0), person_id = character(0),
                 university = character(0), city = character(0),
                 country = character(0), profession = character(0),
                 stringsAsFactors = FALSE)

    nominators_df <- if (length(nominators) > 0) bind_rows(nominators) else
      data.frame(name = NA_character_, person_id = NA_character_,
                 university = NA_character_, city = NA_character_,
                 country = NA_character_, profession = NA_character_,
                 stringsAsFactors = FALSE)

    if (nrow(nominees_df) == 0) {
      return(data.frame())
    }

    # Create all nominee-nominator pairs
    expand.grid(
      nominee_idx = seq_len(nrow(nominees_df)),
      nominator_idx = seq_len(nrow(nominators_df)),
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        nomination_id = nomination_id,
        year = year,
        prize = prize,
        nominee_name = nominees_df$name[nominee_idx],
        nominee_person_id = nominees_df$person_id[nominee_idx],
        nominee_university = nominees_df$university[nominee_idx],
        nominee_city = nominees_df$city[nominee_idx],
        nominee_country = nominees_df$country[nominee_idx],
        nominee_profession = nominees_df$profession[nominee_idx],
        nominator_name = nominators_df$name[nominator_idx],
        nominator_person_id = nominators_df$person_id[nominator_idx],
        nominator_university = nominators_df$university[nominator_idx],
        nominator_city = nominators_df$city[nominator_idx],
        nominator_country = nominators_df$country[nominator_idx],
        nominator_profession = nominators_df$profession[nominator_idx]
      ) %>%
      select(-nominee_idx, -nominator_idx)

  }, error = function(e) {
    message(sprintf("    WARNING: Failed to scrape nomination %s: %s",
                    nomination_id, e$message))
    data.frame()
  })
}


# =============================================================================
# Main scraping loop
#   - List pages (one per prize-year) are fetched sequentially
#   - Detail pages (one per nomination) are fetched in parallel using ncores-1
# =============================================================================
message("=== Scraping Nobel Prize Nomination Archive ===\n")

# Set up parallel backend with ncores - 1 workers
n_workers <- max(1, parallel::detectCores() - 1)
plan(multisession, workers = n_workers)
message(sprintf("  Using %d parallel workers for detail page scraping", n_workers))

# Check if we have a partial result to resume from
# NOTE: If partial file exists from a previous schema (missing new columns),
#       we discard it to avoid column mismatch issues with bind_rows.
partial_file <- data_path("nominations_partial.csv")
expected_cols <- c("nominee_university", "nominee_city", "nominee_country",
                   "nominee_profession", "nominator_university", "nominator_city",
                   "nominator_country", "nominator_profession")
if (file.exists(partial_file)) {
  partial_header <- names(read_csv(partial_file, n_max = 0, show_col_types = FALSE))
  if (all(expected_cols %in% partial_header)) {
    message("  Found partial results file (current schema). Loading and resuming...")
    all_nominations <- read_csv(partial_file, show_col_types = FALSE)
    completed_keys <- paste(all_nominations$prize, all_nominations$year, sep = "_")
    message(sprintf("  Resuming with %d records already collected.", nrow(all_nominations)))
  } else {
    message("  Found partial results file but schema is outdated (missing new columns).")
    message("  Discarding partial file and starting fresh.")
    file.remove(partial_file)
    all_nominations <- data.frame()
    completed_keys <- character(0)
  }
} else {
  all_nominations <- data.frame()
  completed_keys <- character(0)
}

# Build a flat list of all (prize, year) pairs to scrape
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

t0 <- Sys.time()
task_count <- 0

for (task in all_tasks) {
  task_count <- task_count + 1
  Sys.sleep(SLEEP_BETWEEN_PAGES)

  # Step 1: Get nomination IDs for this prize-year (sequential, one list page)
  nom_ids <- get_nomination_ids(task$prize_code, task$year)

  if (length(nom_ids) == 0) next

  # Step 2: Scrape all detail pages in parallel
  year_results <- future_map(nom_ids, function(nid) {
    Sys.sleep(SLEEP_BETWEEN_DETAILS)  # small per-worker delay
    scrape_nomination(nid)
  })

  # Combine results, filtering out empty data frames
  year_data <- bind_rows(year_results[sapply(year_results, function(x) nrow(x) > 0)])

  if (nrow(year_data) > 0) {
    all_nominations <- bind_rows(all_nominations, year_data)

    # Save partial progress after each prize-year
    write_csv(all_nominations, partial_file)
  }

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
# Standardize prize names and clean up
# =============================================================================
message("\n=== Standardizing and cleaning nomination data ===")

# Map archive prize names to our standard names
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
    prize = recode(prize, !!!prize_map),
    # Normalize whitespace in names
    nominee_name = str_squish(nominee_name),
    nominator_name = str_squish(nominator_name),
    # Clean country fields: "SWEDEN (SE)" → "SWEDEN"
    # Keep the raw country name; strip trailing ISO code in parentheses
    nominee_country = str_trim(str_remove(nominee_country, "\\s*\\([A-Z]{2}\\)\\s*$")),
    nominator_country = str_trim(str_remove(nominator_country, "\\s*\\([A-Z]{2}\\)\\s*$")),
    # Normalize whitespace in contextual text fields
    nominee_university = str_squish(nominee_university),
    nominee_city = str_squish(nominee_city),
    nominee_profession = str_squish(nominee_profession),
    nominator_university = str_squish(nominator_university),
    nominator_city = str_squish(nominator_city),
    nominator_profession = str_squish(nominator_profession)
  ) %>%
  distinct()

# Save final output
write_csv(nominations, data_path("nominations.csv"))

# Clean up partial file
if (file.exists(partial_file)) {
  file.remove(partial_file)
}

message(sprintf("\n=== DONE: %d nomination records saved to %s ===",
                nrow(nominations), data_path("nominations.csv")))
message(sprintf("  Unique nominees: %d", n_distinct(nominations$nominee_person_id)))
message(sprintf("  Unique nominators: %d", n_distinct(nominations$nominator_person_id, na.rm = TRUE)))
message(sprintf("  Year range: %d–%d", min(nominations$year), max(nominations$year)))

# =============================================================================
# 04_laureates.R
# Gather all Nobel Prize laureates from the Nobel Prize API v2.1
#
# The API provides rich structured data including Wikidata QIDs, which lets us
# skip the fragile Wikipedia→Wikidata lookup for laureates.
#
# We focus on the 5 original Nobel Prizes (excluding Economics, added 1969).
#
# Output: laureates.csv with columns:
#   qid, name, year, prize, portion, motivation, affiliation
# =============================================================================

source("Code/Data Gathering/00_utils.R")
library(jsonlite)

# =============================================================================
# Configuration
# =============================================================================

API_BASE <- "https://api.nobelprize.org/2.1"

# Map API category names to our standard names
PRIZE_MAP <- c(
  "Physics"                  = "Physics",
  "Chemistry"                = "Chemistry",
  "Physiology or Medicine"   = "Physiology/Medicine",
  "Literature"               = "Literature",
  "Peace"                    = "Peace"
)

# =============================================================================
# Fetch all laureates from the API
# =============================================================================
message("=== Fetching laureates from Nobel Prize API v2.1 ===\n")

# The API paginates; we need to loop through all pages
all_laureates_raw <- list()
offset <- 0
limit <- 25  # API default page size
total <- NULL

repeat {
  url <- sprintf("%s/laureates?offset=%d&limit=%d", API_BASE, offset, limit)

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

  if (is.null(total)) {
    total <- response$meta$count
    message(sprintf("  Total laureates in API: %d", total))
  }

  all_laureates_raw <- c(all_laureates_raw, list(response$laureates))
  offset <- offset + limit

  message(sprintf("  [%s] Fetched %d / %d laureates (%.0f%%)",
                  format(Sys.time(), "%H:%M:%S"),
                  min(offset, total), total,
                  100 * min(offset, total) / total))

  if (offset >= total) break
  Sys.sleep(0.5)
}

# Combine into a single data frame
laureates_raw <- bind_rows(all_laureates_raw)
message(sprintf("  Fetched %d laureate records", nrow(laureates_raw)))


# =============================================================================
# Process laureate data
# =============================================================================
message("\n=== Processing laureate records ===")

# Each laureate may have multiple prizes (rare but possible, e.g., Marie Curie)
# We need to unnest the nobelPrizes column
# The API returns nested data; we'll process it carefully

process_laureate <- function(row_idx) {
  row <- laureates_raw[row_idx, ]

  # Get Wikidata QID
  qid <- tryCatch({
    wikidata_id <- row$wikidata.id
    if (is.null(wikidata_id) || is.na(wikidata_id)) NA_character_
    else wikidata_id
  }, error = function(e) NA_character_)

  # Get name
  name <- tryCatch({
    fn <- row$fullName.en
    if (is.null(fn) || is.na(fn)) {
      kn <- row$knownName.en
      if (is.null(kn) || is.na(kn)) NA_character_
      else kn
    } else fn
  }, error = function(e) NA_character_)

  # Get gender
  gender <- tryCatch({
    g <- row$gender
    if (is.null(g) || is.na(g)) NA_character_ else g
  }, error = function(e) NA_character_)

  # Process each prize
  prizes <- row$nobelPrizes
  if (is.null(prizes) || length(prizes) == 0) return(data.frame())

  # prizes may be a list or data frame depending on nesting
  if (is.data.frame(prizes)) {
    prize_df <- prizes
  } else if (is.list(prizes)) {
    prize_df <- tryCatch(bind_rows(prizes), error = function(e) data.frame())
  } else {
    return(data.frame())
  }

  if (nrow(prize_df) == 0) return(data.frame())

  results <- lapply(seq_len(nrow(prize_df)), function(p_idx) {
    p <- prize_df[p_idx, ]

    category <- tryCatch({
      cat_en <- p$category.en
      if (is.null(cat_en) || is.na(cat_en)) NA_character_ else cat_en
    }, error = function(e) NA_character_)

    # Skip Economics and any other non-standard prizes
    if (is.na(category) || !(category %in% names(PRIZE_MAP))) return(NULL)

    year <- tryCatch(as.numeric(p$awardYear), error = function(e) NA_real_)
    portion <- tryCatch({
      por <- p$portion
      if (is.null(por)) NA_character_ else por
    }, error = function(e) NA_character_)
    motivation <- tryCatch({
      mot <- p$motivation.en
      if (is.null(mot) || is.na(mot)) NA_character_ else mot
    }, error = function(e) NA_character_)

    # Get primary affiliation at time of award
    affiliation <- tryCatch({
      affs <- p$affiliations
      if (is.null(affs) || length(affs) == 0) {
        NA_character_
      } else if (is.data.frame(affs)) {
        affs$name.en[1]
      } else if (is.list(affs) && length(affs) > 0) {
        if (is.data.frame(affs[[1]])) affs[[1]]$name.en[1]
        else NA_character_
      } else {
        NA_character_
      }
    }, error = function(e) NA_character_)

    data.frame(
      qid = qid,
      name = name,
      gender = gender,
      year = year,
      prize = PRIZE_MAP[category],
      portion = portion,
      motivation = motivation,
      affiliation = affiliation,
      stringsAsFactors = FALSE
    )
  })

  bind_rows(results[!sapply(results, is.null)])
}

message("  Processing laureate records...")
laureates_list <- map(seq_len(nrow(laureates_raw)), process_laureate,
                      .progress = "Processing laureates")  # in-memory, no parallelism needed
laureates <- bind_rows(laureates_list)

# Filter to only individual laureates (exclude organizations)
# Organizations typically don't have Wikidata QIDs or have organizational QIDs
# We keep them for now but flag them
laureates <- laureates %>%
  filter(!is.na(year), !is.na(prize)) %>%
  arrange(prize, year, name)

message(sprintf("\n  Laureates by prize:"))
laureates %>%
  count(prize) %>%
  mutate(msg = sprintf("    %s: %d", prize, n)) %>%
  pull(msg) %>%
  walk(message)


# =============================================================================
# Save output
# =============================================================================
write_csv(laureates, data_path("laureates.csv"))

message(sprintf("\n=== DONE: %d laureate-prize records saved to %s ===",
                nrow(laureates), data_path("laureates.csv")))
message(sprintf("  Unique individuals: %d", n_distinct(laureates$qid)))
message(sprintf("  Year range: %d–%d", min(laureates$year), max(laureates$year)))
message(sprintf("  Records with QIDs: %d / %d",
                sum(!is.na(laureates$qid)), nrow(laureates)))

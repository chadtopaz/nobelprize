# =============================================================================
# 06_wikidata_demographics.R
# Unified Wikidata demographic lookup for ALL unique people in our data
#
# This script:
#   1. Loads all intermediate files from steps 01–04
#   2. Collects all unique Wikidata QIDs
#   3. Fetches demographics in efficient batches from Wikidata SPARQL
#   4. Outputs a single demographics table
#
# For nominators/nominees (step 03), many people won't have Wikidata QIDs
# because they were identified by name only on nobelprize.org. We attempt
# Wikidata matching where possible.
#
# Output: demographics.csv with columns:
#   qid, name, gender, birth_country, nationality, birth_year, death_year,
#   occupation, institution
# =============================================================================

source("Code/Data Gathering/00_utils.R")

# =============================================================================
# 1. Collect all unique QIDs from intermediate files
# =============================================================================
message("=== Collecting unique QIDs from all intermediate files ===\n")

qid_sources <- list()

# Governing bodies (01)
if (file.exists(data_path("governing_bodies.csv"))) {
  gb <- read_csv(data_path("governing_bodies.csv"), show_col_types = FALSE)
  qid_sources$governing <- gb %>% filter(!is.na(qid)) %>% pull(qid) %>% unique()
  message(sprintf("  Governing bodies: %d unique QIDs", length(qid_sources$governing)))
} else {
  message("  WARNING: governing_bodies.csv not found. Run 01_governing_bodies.R first.")
}

# Vetting bodies (02)
if (file.exists(data_path("vetting_bodies.csv"))) {
  vb <- read_csv(data_path("vetting_bodies.csv"), show_col_types = FALSE)
  qid_sources$vetting <- vb %>% filter(!is.na(qid)) %>% pull(qid) %>% unique()
  message(sprintf("  Vetting bodies: %d unique QIDs", length(qid_sources$vetting)))
} else {
  message("  WARNING: vetting_bodies.csv not found. Run 02_vetting_bodies.R first.")
}

# Laureates (04) — these already have QIDs from the Nobel Prize API
if (file.exists(data_path("laureates.csv"))) {
  laur <- read_csv(data_path("laureates.csv"), show_col_types = FALSE)
  qid_sources$laureates <- laur %>% filter(!is.na(qid)) %>% pull(qid) %>% unique()
  message(sprintf("  Laureates: %d unique QIDs", length(qid_sources$laureates)))
} else {
  message("  WARNING: laureates.csv not found. Run 04_laureates.R first.")
}

# Combine all QIDs
all_qids <- unique(unlist(qid_sources))
message(sprintf("\n  Total unique QIDs across all sources: %d", length(all_qids)))


# =============================================================================
# 2. Fetch demographics from Wikidata in batches
# =============================================================================
message("\n=== Fetching demographics from Wikidata ===\n")

# Wikidata SPARQL has query size limits, so we batch QIDs
BATCH_SIZE <- 200  # Safe batch size for SPARQL VALUES clause

# Split QIDs into batches
batches <- split(all_qids, ceiling(seq_along(all_qids) / BATCH_SIZE))
message(sprintf("  Processing %d QIDs in %d batches of up to %d...",
                length(all_qids), length(batches), BATCH_SIZE))

# Check for partial results
partial_demo_file <- data_path("demographics_partial.csv")
if (file.exists(partial_demo_file)) {
  message("  Found partial demographics file. Loading and resuming...")
  demo_partial <- read_csv(partial_demo_file, show_col_types = FALSE)
  completed_qids <- unique(demo_partial$qid)
  message(sprintf("  Already have demographics for %d QIDs.", length(completed_qids)))

  # Filter out already-completed QIDs
  remaining_qids <- setdiff(all_qids, completed_qids)
  batches <- split(remaining_qids, ceiling(seq_along(remaining_qids) / BATCH_SIZE))
  all_demo_raw <- list(demo_partial)
} else {
  all_demo_raw <- list()
}

t0 <- Sys.time()
for (i in seq_along(batches)) {
  batch_qids <- batches[[i]]

  result <- tryCatch({
    fetch_demographics_batch(batch_qids)
  }, error = function(e) {
    message(sprintf("  Batch %d failed: %s. Trying individual lookups...", i, e$message))

    # Fall back to individual queries for this batch
    individual_results <- lapply(batch_qids, function(qid) {
      tryCatch({
        fetch_demographics_for_qid(qid)
      }, error = function(e2) {
        message(sprintf("  Failed for %s: %s", qid, e2$message))
        NULL
      })
    })
    bind_rows(individual_results[!sapply(individual_results, is.null)])
  })

  if (!is.null(result) && nrow(result) > 0) {
    all_demo_raw <- c(all_demo_raw, list(result))

    # Save partial progress
    partial <- bind_rows(all_demo_raw)
    write_csv(partial, partial_demo_file)
  }

  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  message(sprintf("  [%s] Demographics: batch %d / %d (%.0f%%) | %.1f min elapsed",
                  format(Sys.time(), "%H:%M:%S"),
                  i, length(batches),
                  100 * i / length(batches), elapsed))

  # Small delay between batches to be polite to Wikidata
  Sys.sleep(2)
}


# =============================================================================
# 3. Collapse and clean demographics
# =============================================================================
message("\n=== Collapsing and cleaning demographics ===")

demo_raw <- bind_rows(all_demo_raw)
demographics <- collapse_demographics(demo_raw)

message(sprintf("  Demographics retrieved for %d / %d QIDs",
                nrow(demographics), length(all_qids)))


# =============================================================================
# 4. Include nomination archive people with QIDs (from step 05)
# =============================================================================
message("\n=== Adding nomination archive people with matched QIDs ===")

nom_qid_file <- data_path("nomination_people_qids.csv")
if (file.exists(nom_qid_file)) {
  nom_matched <- read_csv(nom_qid_file, show_col_types = FALSE) %>%
    filter(!is.na(qid))

  # Add matched nomination QIDs to the demographics lookup
  new_qids <- setdiff(nom_matched$qid, all_qids)
  if (length(new_qids) > 0) {
    message(sprintf("  %d new QIDs from nomination archive to fetch demographics for",
                    length(new_qids)))

    # Fetch demographics for these new QIDs
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
      Sys.sleep(2)
    }
    all_qids <- c(all_qids, new_qids)
  }
  message(sprintf("  %d nomination archive people have Wikidata QIDs", nrow(nom_matched)))
} else {
  message("  WARNING: nomination_people_qids.csv not found. Run 05 first.")
  message("  Nomination archive people will not be included in demographics.")
}

# Re-collapse demographics now that nomination people are included
message("\n=== Re-collapsing demographics with nomination people ===")
demo_raw <- bind_rows(all_demo_raw)
demographics <- collapse_demographics(demo_raw)
message(sprintf("  Demographics now cover %d unique QIDs", nrow(demographics)))


# =============================================================================
# 5. Fill in RSAS endyear from death_year
# =============================================================================
message("\n=== Filling RSAS endyear from death_year ===")

if (file.exists(data_path("governing_bodies.csv"))) {
  gb <- read_csv(data_path("governing_bodies.csv"), show_col_types = FALSE)

  rsas_mask <- gb$body == "RSAS" & is.na(gb$endyear)
  n_missing <- sum(rsas_mask, na.rm = TRUE)

  if (n_missing > 0) {
    # Merge death_year from demographics
    gb_updated <- gb %>%
      left_join(demographics %>% select(qid, death_year), by = "qid") %>%
      mutate(
        endyear = ifelse(body == "RSAS" & is.na(endyear) & !is.na(death_year),
                         death_year, endyear)
      ) %>%
      select(-death_year)

    n_filled <- sum(rsas_mask, na.rm = TRUE) -
      sum(gb_updated$body == "RSAS" & is.na(gb_updated$endyear), na.rm = TRUE)

    write_csv(gb_updated, data_path("governing_bodies.csv"))
    message(sprintf("  Filled %d / %d missing RSAS endyear values from death_year",
                    n_filled, n_missing))
  } else {
    message("  No missing RSAS endyear values to fill.")
  }
}


# =============================================================================
# Save final demographics
# =============================================================================
write_csv(demographics, data_path("demographics.csv"))

# Clean up partial file
if (file.exists(partial_demo_file)) {
  file.remove(partial_demo_file)
}

message(sprintf("\n=== DONE: %d demographic records saved to %s ===",
                nrow(demographics), data_path("demographics.csv")))
message(sprintf("  Gender breakdown:"))
demographics %>%
  count(gender) %>%
  mutate(msg = sprintf("    %s: %d", coalesce(gender, "unknown"), n)) %>%
  pull(msg) %>%
  walk(message)

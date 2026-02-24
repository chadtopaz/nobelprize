# =============================================================================
# debug_03_prizes.R
# Diagnostic: why are Medicine and Peace missing from nominations.csv?
# Run this interactively in the R console.
# =============================================================================

source("Code/Data Gathering/00_utils.R")
library(httr)

# Prize codes from script 03
PRIZES <- c(Physics = 1, Chemistry = 2, Medicine = 4, Literature = 6, Peace = 7)

# =============================================================================
# Test 1: Do the list pages return nomination IDs?
# =============================================================================
message("\n=== TEST 1: List page nomination IDs ===\n")

for (prize_name in names(PRIZES)) {
  code <- PRIZES[prize_name]
  # Test year 1901 (all prizes should have data)
  url <- sprintf(
    "https://www.nobelprize.org/nomination/archive/list.php?prize=%d&year=1901",
    code
  )

  page <- tryCatch(read_html(url), error = function(e) {
    message(sprintf("  %s: FAILED to load page: %s", prize_name, e$message))
    NULL
  })

  if (is.null(page)) next

  # Check for show.php links (what script 03 uses)
  show_links <- page %>% html_elements("a[href*='show.php?id=']")
  show_ids <- show_links %>% html_attr("href") %>% str_extract("(?<=id=)\\d+")

  # Also check for ANY links on the page
  all_links <- page %>% html_elements("a") %>% html_attr("href")

  # Get the page text to see what's there
  page_text <- page %>% html_text() %>% str_squish() %>% str_sub(1, 500)

  message(sprintf("  %s (code=%d, year=1901):", prize_name, code))
  message(sprintf("    show.php links found: %d", length(na.omit(show_ids))))
  message(sprintf("    Total links on page: %d", length(all_links)))

  # Show any links that contain "show" or "nomination" or "archive"
  archive_links <- all_links[str_detect(all_links, "show|nomination|archive", negate = FALSE)]
  if (length(archive_links) > 0) {
    message(sprintf("    Archive-related links: %s", paste(head(archive_links, 5), collapse = ", ")))
  }

  message(sprintf("    Page text preview: %s", str_sub(page_text, 1, 200)))
  message("")

  Sys.sleep(1)
}

# =============================================================================
# Test 2: For prizes that DO return IDs, check detail page structure
# =============================================================================
message("\n=== TEST 2: Detail page structure for a Physics nomination ===\n")

# Get one Physics nomination ID
phys_url <- "https://www.nobelprize.org/nomination/archive/list.php?prize=1&year=1901"
phys_page <- read_html(phys_url)
phys_ids <- phys_page %>%
  html_elements("a[href*='show.php?id=']") %>%
  html_attr("href") %>%
  str_extract("(?<=id=)\\d+") %>%
  na.omit()

if (length(phys_ids) > 0) {
  detail_url <- sprintf(
    "https://www.nobelprize.org/nomination/archive/show.php?id=%s",
    phys_ids[1]
  )
  detail_page <- read_html(detail_url)

  # Get the colspan=2 header (what script 03 uses for prize extraction)
  headers <- detail_page %>% html_elements("td[colspan='2']") %>% html_text()
  message(sprintf("  Physics detail page headers: %s",
                  paste(headers, collapse = " | ")))

  # Extract prize the way script 03 does
  prize_raw <- headers[str_detect(headers, "Nomination for Nobel Prize")] %>%
    str_extract("(?<=in ).*$") %>%
    .[1]
  message(sprintf("  Extracted prize: '%s'", prize_raw))
}

Sys.sleep(1)

# =============================================================================
# Test 3: Check if Medicine/Peace list pages use different URL structure
# =============================================================================
message("\n=== TEST 3: Alternative URL patterns ===\n")

# Try fetching Medicine list page with verbose output
med_url <- "https://www.nobelprize.org/nomination/archive/list.php?prize=4&year=1901"
message(sprintf("  Fetching: %s", med_url))
med_resp <- tryCatch({
  GET(med_url, user_agent("Mozilla/5.0 (research)"))
}, error = function(e) {
  message(sprintf("  HTTP error: %s", e$message))
  NULL
})

if (!is.null(med_resp)) {
  message(sprintf("  HTTP status: %d", status_code(med_resp)))
  med_page <- read_html(content(med_resp, "text", encoding = "UTF-8"))

  # Get ALL text on the page
  full_text <- med_page %>% html_text() %>% str_squish()
  message(sprintf("  Full page text (first 500 chars): %s", str_sub(full_text, 1, 500)))

  # Check all links
  all_hrefs <- med_page %>% html_elements("a") %>% html_attr("href")
  message(sprintf("  All links on page (%d total):", length(all_hrefs)))
  for (h in head(all_hrefs, 20)) {
    message(sprintf("    %s", h))
  }
}

Sys.sleep(1)

# Same for Peace
peace_url <- "https://www.nobelprize.org/nomination/archive/list.php?prize=7&year=1901"
message(sprintf("\n  Fetching: %s", peace_url))
peace_resp <- tryCatch({
  GET(peace_url, user_agent("Mozilla/5.0 (research)"))
}, error = function(e) {
  message(sprintf("  HTTP error: %s", e$message))
  NULL
})

if (!is.null(peace_resp)) {
  message(sprintf("  HTTP status: %d", status_code(peace_resp)))
  peace_page <- read_html(content(peace_resp, "text", encoding = "UTF-8"))

  full_text <- peace_page %>% html_text() %>% str_squish()
  message(sprintf("  Full page text (first 500 chars): %s", str_sub(full_text, 1, 500)))

  all_hrefs <- peace_page %>% html_elements("a") %>% html_attr("href")
  message(sprintf("  All links on page (%d total):", length(all_hrefs)))
  for (h in head(all_hrefs, 20)) {
    message(sprintf("    %s", h))
  }
}

# =============================================================================
# Test 4: Check the search/advanced form — maybe there's a different entry point
# =============================================================================
message("\n=== TEST 4: Checking main archive page structure ===\n")
archive_url <- "https://www.nobelprize.org/nomination/archive/"
archive_page <- tryCatch(read_html(archive_url), error = function(e) NULL)

if (!is.null(archive_page)) {
  forms <- archive_page %>% html_elements("form")
  message(sprintf("  Forms found: %d", length(forms)))

  for (i in seq_along(forms)) {
    action <- forms[[i]] %>% html_attr("action")
    method <- forms[[i]] %>% html_attr("method")
    inputs <- forms[[i]] %>% html_elements("input, select") %>%
      html_attr("name")
    message(sprintf("  Form %d: action=%s method=%s inputs=%s",
                    i, action, method, paste(inputs, collapse=", ")))
  }

  # Check for select elements (dropdowns) that might reveal prize codes
  selects <- archive_page %>% html_elements("select")
  for (sel in selects) {
    sel_name <- html_attr(sel, "name")
    options <- sel %>% html_elements("option")
    opt_vals <- options %>% html_attr("value")
    opt_texts <- options %>% html_text()
    message(sprintf("\n  Select '%s':", sel_name))
    for (j in seq_along(opt_vals)) {
      message(sprintf("    value=%s  text=%s", opt_vals[j], opt_texts[j]))
    }
  }
}

message("\n=== Diagnostics complete ===")

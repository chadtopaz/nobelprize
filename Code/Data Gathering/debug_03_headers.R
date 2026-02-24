# Quick check: what do Medicine and Peace detail page headers look like?
source("Code/Data Gathering/00_utils.R")

# Correct codes now
# Medicine = 3, Peace = 5

# Get a Medicine nomination
med_page <- read_html("https://www.nobelprize.org/nomination/archive/list.php?prize=3&year=1901")
med_ids <- med_page %>%
  html_elements("a[href*='show.php?id=']") %>%
  html_attr("href") %>% str_extract("(?<=id=)\\d+") %>% na.omit()
message(sprintf("Medicine IDs for 1901: %d found", length(med_ids)))

if (length(med_ids) > 0) {
  detail <- read_html(sprintf(
    "https://www.nobelprize.org/nomination/archive/show.php?id=%s", med_ids[1]))
  headers <- detail %>% html_elements("td[colspan='2']") %>% html_text()
  message(sprintf("Medicine headers: %s", paste(headers, collapse = " | ")))

  # Test prize extraction
  prize_raw <- headers[str_detect(headers, "Nomination for Nobel")] %>%
    str_extract("(?<=in ).*$") %>% .[1]
  message(sprintf("Prize extraction with 'in' pattern: '%s'", prize_raw))

  # What if it says "Nobel Peace Prize" style?
  prize_raw2 <- headers[str_detect(headers, "Nomination for Nobel")]
  message(sprintf("Full header text: '%s'", paste(prize_raw2, collapse = " | ")))
}

Sys.sleep(1)

# Get a Peace nomination
peace_page <- read_html("https://www.nobelprize.org/nomination/archive/list.php?prize=5&year=1901")
peace_ids <- peace_page %>%
  html_elements("a[href*='show.php?id=']") %>%
  html_attr("href") %>% str_extract("(?<=id=)\\d+") %>% na.omit()
message(sprintf("\nPeace IDs for 1901: %d found", length(peace_ids)))

if (length(peace_ids) > 0) {
  detail <- read_html(sprintf(
    "https://www.nobelprize.org/nomination/archive/show.php?id=%s", peace_ids[1]))
  headers <- detail %>% html_elements("td[colspan='2']") %>% html_text()
  message(sprintf("Peace headers: %s", paste(headers, collapse = " | ")))

  prize_raw <- headers[str_detect(headers, "Nomination for Nobel")] %>%
    str_extract("(?<=in ).*$") %>% .[1]
  message(sprintf("Prize extraction with 'in' pattern: '%s'", prize_raw))

  # Alternative: extract word after "Nobel" and before "Prize" or end
  prize_raw2 <- headers[str_detect(headers, "Nomination for Nobel")]
  message(sprintf("Full header text: '%s'", paste(prize_raw2, collapse = " | ")))
}

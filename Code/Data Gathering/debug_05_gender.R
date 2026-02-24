# Debug: check what's on the pages with junk gender values
source("Code/Data Gathering/00_utils.R")

qids <- read_csv(data_path("nomination_people_qids.csv"), show_col_types = FALSE)

# Find junk gender values
junk <- qids %>% filter(!is.na(gender), !gender %in% c("M", "F", "m", "f", ""))
message(sprintf("Junk gender values: %d records", nrow(junk)))
print(junk %>% count(gender))

# Look at the actual pages for a few of these
for (i in seq_len(min(nrow(junk), 5))) {
  pid <- junk$person_id[i]
  url <- sprintf("https://www.nobelprize.org/nomination/archive/show_people.php?id=%s", pid)
  message(sprintf("\n--- Person %s (gender='%s') ---", pid, junk$gender[i]))
  message(sprintf("URL: %s", url))

  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) { message("  FAILED to load"); next }

  tds <- page %>% html_elements("td") %>% html_text() %>% str_squish()
  message(sprintf("  All TD contents (%d cells):", length(tds)))
  for (j in seq_along(tds)) {
    message(sprintf("    [%2d] '%s'", j, tds[j]))
  }

  Sys.sleep(0.5)
}

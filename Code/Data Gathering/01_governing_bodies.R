# =============================================================================
# 01_governing_bodies.R
# Gather membership lists for all governing/selection bodies
#
# Output: governing_bodies.csv with columns:
#   qid, name, body, startyear, endyear
#
# Bodies:
#   - Royal Swedish Academy of Sciences (RSAS) — Chemistry & Physics
#   - Nobel Assembly at Karolinska Institutet — Physiology/Medicine
#   - Swedish Academy — Literature
#   - Storting — Peace
# =============================================================================

source("Code/Data Gathering/00_utils.R")

# =============================================================================
# 1. Royal Swedish Academy of Sciences (RSAS)
#    Source: Swedish Wikipedia list page
#    Members serve for life (startyear = induction, endyear = death)
#
#    Page structure:
#      <p><b>1740</b>           ← year header
#      <ul>
#        <li>39. <a href="/wiki/...">Name</a></li>   ← Swedish member
#        <li><i>1. <a href="...">Name</a></i></li>   ← foreign member (italic)
#      </ul>
#      <p><b>1741</b>
#      ...
#    The founding members (1739) appear under "Akademiens grundare" and
#    "Övriga" headers with no explicit year.
# =============================================================================
message("=== Gathering RSAS members ===")

RSAS_URL <- "https://sv.wikipedia.org/wiki/Lista_%C3%B6ver_ledam%C3%B6ter_av_Kungliga_Vetenskapsakademien"

page <- read_html(RSAS_URL)

# Strategy: iterate through all <p> and <ul> children of the main content div.
# When we see a <p><b>YEAR</b></p>, update the current year.
# When we see a <ul>, extract all <li> members with that year.
# Select all direct children of the content div (p, ul, and heading divs).
# We stop when we reach the "Källor" (Sources) section.
content_nodes <- page %>%
  html_elements(".mw-parser-output > p, .mw-parser-output > ul, .mw-parser-output > div.mw-heading")

current_year <- NA_real_
rsas_records <- list()
reached_sources <- FALSE

for (node in content_nodes) {
  tag <- html_name(node)

  # Check if we've reached the "Källor" (Sources) heading — stop parsing
  if (tag == "div") {
    heading_text <- node %>% html_elements("h2, h3") %>% html_text()
    if (any(str_detect(heading_text, "(?i)k.llor|referenser|sources"))) {
      reached_sources <- TRUE
      break
    }
    next
  }

  if (tag == "p") {
    # Check if this is a year header: <p><b>YYYY</b></p>
    bold_text <- node %>% html_elements("b") %>% html_text()
    if (length(bold_text) > 0) {
      yr <- str_extract(bold_text[1], "^\\d{4}$")
      if (!is.na(yr)) {
        current_year <- as.numeric(yr)
      } else if (bold_text[1] %in% c("Akademiens grundare", "\u00d6vriga")) {
        # Founding members (1739). "Övriga" = "Others" in the 1739 section
        current_year <- 1739
      }
    }
    next
  }

  if (tag == "ul" && !is.na(current_year)) {
    # Extract all list items with Wikipedia links
    items <- node %>% html_elements("li")
    for (item in items) {
      link <- item %>% html_elements("a") %>% .[1]  # first link in each <li>
      if (length(link) == 0) next

      href <- html_attr(link, "href")
      name <- html_text(link)

      # Skip redlinks, non-wiki links, and action=edit links
      if (is.na(href) ||
          str_detect(href, "redlink|action=edit") ||
          !str_detect(href, "^/wiki/")) next

      full_url <- paste0("https://sv.wikipedia.org", href)

      rsas_records[[length(rsas_records) + 1]] <- data.frame(
        name = name,
        year = current_year,
        link = full_url,
        stringsAsFactors = FALSE
      )
    }
  }
}

df_rsas <- bind_rows(rsas_records)
message(sprintf("  Parsed %d RSAS member entries from page", nrow(df_rsas)))

# Look up QIDs using batch API (much faster than scraping individual pages)
message(sprintf("  Looking up QIDs for %d RSAS members via batch API...", nrow(df_rsas)))
rsas_qids <- wikipedia_urls_to_qids(df_rsas$link)

# We need death years to determine end of service.
# We'll get these in the demographics step (05), but we need a rough filter now.
# For now, store startyear; endyear will be filled from death_year in step 05.
rsas <- data.frame(
  qid = rsas_qids,
  name = df_rsas$name,
  body = "RSAS",
  startyear = as.numeric(df_rsas$year),
  endyear = NA_real_,  # to be filled from Wikidata death_year
  stringsAsFactors = FALSE
) %>%
  filter(!is.na(qid), !is.na(startyear))

message(sprintf("  RSAS: %d members found", nrow(rsas)))


# =============================================================================
# 2. Swedish Academy
#    Source: Wikidata (members hold position P39 = member of Swedish Academy)
#    Members are appointed for life.
# =============================================================================
message("=== Gathering Swedish Academy members ===")

sa_query <- '
SELECT DISTINCT ?qid ?name ?startDate ?endDate WHERE {
  {
    SELECT DISTINCT ?qid ?statement0 WHERE {
      ?qid p:P39 ?statement0.
      ?statement0 (ps:P39/(wdt:P361*)) wd:Q207360.
      MINUS { ?statement0 (ps:P39/(wdt:P31*)) wd:Q97563901. }
    }
  }
  OPTIONAL { ?statement0 pq:P580 ?startDate. }
  OPTIONAL { ?statement0 pq:P582 ?endDate. }
  SERVICE wikibase:label {
    bd:serviceParam wikibase:language "en".
    ?qid rdfs:label ?name.
  }
}'

sa_raw <- query_wikidata_safe(sa_query)

swedish_academy <- sa_raw %>%
  mutate(
    # Wikidata returns datetimes like "1871-12-20T00:00:00Z" — extract just the year
    startyear = as.numeric(str_extract(startDate, "^\\d{4}")),
    endyear = as.numeric(str_extract(endDate, "^\\d{4}"))
  ) %>%
  select(qid, name, startyear, endyear) %>%
  mutate(body = "Swedish Academy") %>%
  filter(!is.na(startyear))

message(sprintf("  Swedish Academy: %d member records found", nrow(swedish_academy)))


# =============================================================================
# 3. Storting (Norwegian Parliament)
#    Source: Wikidata (position P39 = member of Storting, Q9045502)
# =============================================================================
message("=== Gathering Storting members ===")

storting_query <- '
SELECT DISTINCT ?qid ?name ?starttime ?endtime WHERE {
  {
    SELECT DISTINCT ?qid ?endtime ?starttime ?statement1 WHERE {
      ?qid p:P31 ?statement0.
      ?statement0 ps:P31 wd:Q5.
      ?qid p:P39 ?statement1.
      ?statement1 ps:P39 wd:Q9045502.
    }
  }
  OPTIONAL { ?statement1 pq:P580 ?starttime. }
  OPTIONAL { ?statement1 pq:P582 ?endtime. }
  SERVICE wikibase:label {
    bd:serviceParam wikibase:language "en".
    ?qid rdfs:label ?name.
  }
}'

storting_raw <- query_wikidata_safe(storting_query)

storting <- storting_raw %>%
  mutate(
    startyear = as.numeric(str_extract(starttime, "^\\d{4}")),
    endyear = as.numeric(str_extract(endtime, "^\\d{4}"))
  ) %>%
  select(qid, name, startyear, endyear) %>%
  mutate(body = "Storting") %>%
  filter(!is.na(startyear))

# Consolidate consecutive terms for the same person
storting <- storting %>%
  group_by(qid, name) %>%
  arrange(startyear) %>%
  mutate(
    new_seq = if_else(is.na(lag(endyear)) | (startyear - lag(endyear) > 1), 1, 0),
    seq_group = cumsum(new_seq)
  ) %>%
  group_by(qid, name, seq_group) %>%
  summarise(
    startyear = min(startyear),
    endyear = max(endyear),
    .groups = "drop"
  ) %>%
  select(-seq_group) %>%
  mutate(body = "Storting")

message(sprintf("  Storting: %d member-term records found", nrow(storting)))


# =============================================================================
# 4. Karolinska Institutet (Nobel Assembly)
#    Source: Data/KI profs.xlsx (manually compiled from Project Runeberg's
#    digitized statskalender editions, 1881–1970)
#
#    Prior to 1984, ALL full professors at KI constituted the selection body
#    for the Prize in Physiology or Medicine. In 1977 the Nobel Assembly
#    became a separate entity; in 1984 membership was fixed at 50.
#
#    The Excel file (Sheet2) has 1021 rows × 35 columns:
#      qid, link, title, dob, dod, then per-edition columns (1881–1970)
#      with "note" columns, and finally Min, Max.
#    140 rows have both Min (=startyear) and Max (=endyear) populated,
#    representing confirmed full professors with Wikidata QIDs.
#
#    KNOWN GAP: Coverage ends at 1970. Post-1970 Nobel Assembly membership
#    is not publicly available online:
#      - Wikidata has essentially no P39/P463 records for Q3375124
#      - KI's website does not publish a member roster
#      - Project Runeberg's statskalender stops ~1972
#    To extend coverage, contact the Nobel Office directly:
#      nobelforum@nobelprizemedicine.org / +46(0)8-52487800
#    or check KI annual reports for faculty appointment records.
# =============================================================================
message("=== Gathering Karolinska Institutet members ===")

ki_xlsx <- file.path("Data", "KI profs.xlsx")
if (file.exists(ki_xlsx)) {
  ki_raw <- read_excel(ki_xlsx, sheet = "Sheet2")
  message(sprintf("  Loaded KI profs.xlsx: %d rows", nrow(ki_raw)))

  # Keep only rows that have a QID and both Min/Max (confirmed professors)
  karolinska <- ki_raw %>%
    filter(!is.na(qid), qid != "", !is.na(Min), !is.na(Max)) %>%
    transmute(
      qid = qid,
      name = title,
      body = "Karolinska Institutet",
      startyear = as.numeric(Min),
      endyear = as.numeric(Max)
    )
} else {
  message("  WARNING: KI profs.xlsx not found. Karolinska data must be gathered manually.")
  karolinska <- data.frame(qid = character(), name = character(), body = character(),
                           startyear = numeric(), endyear = numeric())
}

message(sprintf("  Karolinska: %d member records found", nrow(karolinska)))


# =============================================================================
# Combine and save
# =============================================================================
# For RSAS and Swedish Academy, a person may appear under multiple years
# (e.g., class transfers). Keep only the earliest startyear per person per body.
# For Storting, multiple terms are legitimate and should be preserved.
governing_bodies <- bind_rows(rsas, swedish_academy, storting, karolinska) %>%
  select(qid, name, body, startyear, endyear) %>%
  group_by(body, qid) %>%
  arrange(startyear) %>%
  filter(
    # For Storting: keep all terms (multiple non-consecutive terms are real)
    body == "Storting" |
    # For other bodies: keep only the earliest record
    row_number() == 1
  ) %>%
  ungroup() %>%
  arrange(body, startyear, name)

write_csv(governing_bodies, data_path("governing_bodies.csv"))
message(sprintf("\n=== DONE: %d total governing body records saved to %s ===",
                nrow(governing_bodies), data_path("governing_bodies.csv")))

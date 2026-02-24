# =============================================================================
# 02_vetting_bodies.R
# Gather membership lists for all vetting bodies (Nobel Committees)
#
# Output: vetting_bodies.csv with columns:
#   qid, name, body, startyear, endyear
#
# Bodies:
#   - Nobel Committee for Chemistry (Swedish Wikipedia)
#   - Nobel Committee for Physics (Swedish Wikipedia)
#   - Nobel Committee for Physiology/Medicine (Swedish Wikipedia)
#   - Nobel Committee for Literature (svenskaakademien.se)
#   - Norwegian Nobel Committee (English Wikipedia)
# =============================================================================

source("Code/Data Gathering/00_utils.R")

# =============================================================================
# 1. Nobel Committee for Chemistry
#    Source: Swedish Wikipedia (via MediaWiki API for stable wikitext parsing)
# =============================================================================
message("=== Gathering Nobel Committee for Chemistry ===")

chem <- scrape_swedish_wiki_list(
  page_title = "Vetenskapsakademiens Nobelkommitté för kemi",
  sections = c("Tidigare ledamöter", "Nuvarande ledamöter", "Sekreterare")
) %>%
  mutate(body = "Nobel Committee for Chemistry") %>%
  filter(!is.na(qid), !is.na(startyear))

message(sprintf("  Chemistry Committee: %d members found", nrow(chem)))


# =============================================================================
# 2. Nobel Committee for Physics
#    Source: Swedish Wikipedia (via MediaWiki API for stable wikitext parsing)
# =============================================================================
message("=== Gathering Nobel Committee for Physics ===")

phys <- scrape_swedish_wiki_list(
  page_title = "Vetenskapsakademiens Nobelkommitté för fysik",
  sections = c("Tidigare ledamöter", "Nuvarande ledamöter", "Sekreterare")
) %>%
  mutate(body = "Nobel Committee for Physics") %>%
  filter(!is.na(qid), !is.na(startyear))

message(sprintf("  Physics Committee: %d members found", nrow(phys)))


# =============================================================================
# 3. Nobel Committee for Physiology/Medicine
#    Source: Swedish Wikipedia (via MediaWiki API for stable wikitext parsing)
#    Note: This page states the list is incomplete ("Listan är ofullständig").
# =============================================================================
message("=== Gathering Nobel Committee for Physiology/Medicine ===")

med <- scrape_swedish_wiki_list(
  page_title = "Karolinska Institutets Nobelkommitté",
  sections = c("Tidigare ledamöter", "Nuvarande ledamöter",
               "Sekreterare / Generalsekreterare")
) %>%
  mutate(body = "Nobel Committee for Physiology/Medicine") %>%
  filter(!is.na(startyear))

message(sprintf("  Physio/Med Committee: %d members found (%d with QIDs)",
                nrow(med), sum(!is.na(med$qid))))


# =============================================================================
# 4. Nobel Committee for Literature
#    Source: svenskaakademien.se member biographies + Wikidata
#
#    The Swedish Academy website (rebuilt ~2024 as a Nuxt.js SPA) embeds
#    member biographies in the __NUXT_DATA__ script tag, which contains
#    HTML-encoded bio text mentioning Nobel Committee service years.
#    Example: "Han var ledamot av Nobelkommittén 1969–86"
#
#    Strategy:
#    1. Query Wikidata for all Swedish Academy members (with seat numbers)
#    2. Construct bio page URLs: /de-aderton/stol-nr-{seat}-{name-slug}
#    3. Extract bio text from __NUXT_DATA__ payload (no JS execution needed)
#    4. Parse "Nobelkommittén" mentions for committee service years
#    5. Combine with QIDs from the Wikidata query
# =============================================================================
message("=== Gathering Nobel Committee for Literature ===")

# Step 1: Get all Swedish Academy members with seat numbers from Wikidata
sa_lit_query <- '
SELECT DISTINCT ?qid ?name ?startDate ?endDate ?positionLabel ?seatLabel WHERE {
  {
    SELECT DISTINCT ?qid ?statement0 WHERE {
      ?qid p:P39 ?statement0.
      ?statement0 (ps:P39/(wdt:P361*)) wd:Q207360.
      MINUS { ?statement0 (ps:P39/(wdt:P31*)) wd:Q97563901. }
    }
  }
  OPTIONAL { ?statement0 ps:P39 ?position. }
  OPTIONAL { ?statement0 pq:P580 ?startDate. }
  OPTIONAL { ?statement0 pq:P582 ?endDate. }
  SERVICE wikibase:label {
    bd:serviceParam wikibase:language "en,sv".
    ?qid rdfs:label ?name.
    ?position rdfs:label ?positionLabel.
    ?position rdfs:label ?seatLabel.
  }
}'
sa_wd <- query_wikidata_safe(sa_lit_query)

# Extract seat numbers from position labels (e.g., "member of the Swedish Academy at seat 12")
sa_wd$seatNo <- str_extract(sa_wd$positionLabel, "(?<=seat )\\d{1,2}") %>% as.numeric()

# Normalize dates
sa_wd$startDate <- str_extract(sa_wd$startDate, "^\\d{4}-\\d{2}-\\d{2}")
sa_wd$endDate <- str_extract(sa_wd$endDate, "^\\d{4}-\\d{2}-\\d{2}")

message(sprintf("  Wikidata: %d Swedish Academy member records", nrow(sa_wd)))

# Step 2: Construct bio page URLs
# URL pattern: /svenska-akademien/de-aderton/stol-nr-{seat}-{first-last}
# The name slug uses lowercase, hyphen-separated, ASCII-ified names
sa_members <- sa_wd %>%
  filter(!is.na(seatNo)) %>%
  mutate(
    # Create URL-safe name slug from the member name
    name_slug = name %>%
      tolower() %>%
      str_replace_all("\\s+", "-") %>%
      # Swedish character transliteration for URLs
      str_replace_all("\u00e9", "e") %>%    # é → e
      str_replace_all("\u00e8", "e") %>%    # è → e
      str_replace_all("\u00e4", "a") %>%    # ä → a
      str_replace_all("\u00e5", "a") %>%    # å → a
      str_replace_all("\u00f6", "o") %>%    # ö → o
      str_replace_all("\u00fc", "u") %>%    # ü → u
      str_replace_all("\u00c9", "e") %>%    # É → e
      str_replace_all("[^a-z0-9-]", ""),    # remove remaining non-ASCII
    bio_url = sprintf(
      "https://www.svenskaakademien.se/svenska-akademien/de-aderton/stol-nr-%d-%s",
      seatNo, name_slug
    )
  )

message(sprintf("  Constructed %d bio URLs", nrow(sa_members)))

# Step 3: Fetch bio text from __NUXT_DATA__ payloads
# The bio text is HTML-encoded in the Nuxt payload, containing phrases like:
# "ledamot av Nobelkommittén YYYY–YY" or "Nobelkommittén YYYY–YYYY"
fetch_nuxt_bio <- function(url) {
  tryCatch({
    resp <- request(url) %>%
      req_headers(`User-Agent` = "NobelNetworkBot/1.0 (research)") %>%
      req_perform()
    page_html <- resp_body_string(resp)

    # Extract __NUXT_DATA__ payload
    # First find the script tag, then extract its content
    nuxt_tag <- str_extract(page_html,
      'id="__NUXT_DATA__"[^>]*>.*?</script>')
    if (is.na(nuxt_tag)) return("")
    nuxt_match <- str_replace(nuxt_tag, '^[^>]*>', '') %>%
      str_replace('</script>$', '')
    if (nchar(nuxt_match) == 0) return("")

    # Decode HTML entities and strip HTML tags
    nuxt_match %>%
      str_replace_all("\\\\u003C", "<") %>%
      str_replace_all("\\\\u003E", ">") %>%
      str_replace_all("&amp;", "&") %>%
      str_replace_all("&auml;", "\u00e4") %>%
      str_replace_all("&ouml;", "\u00f6") %>%
      str_replace_all("&aring;", "\u00e5") %>%
      str_replace_all("&eacute;", "\u00e9") %>%
      str_replace_all("&nbsp;", " ") %>%
      str_replace_all("<[^>]+>", " ") %>%
      str_squish()
  }, error = function(e) {
    ""
  })
}

# Step 4: Extract committee years from bio text
detect_committee <- function(txt) {
  if (str_detect(txt, regex("Nobelkommitt", ignore_case = TRUE))) {
    # Find the sentence/phrase mentioning Nobelkommittén
    sent <- str_extract(txt,
      "[^.]*Nobelkommitt[^\\.]{0,100}")
    if (is.na(sent)) return(c(NA_real_, NA_real_))

    # Extract all 2-4 digit years
    years <- str_extract_all(sent, "\\d{2,4}")[[1]] %>% as.numeric()
    if (length(years) == 0) return(c(NA_real_, NA_real_))

    # Convert 2-digit years to 4-digit (e.g., 86 → 1986)
    years <- ifelse(years < 100,
                    years + floor(max(years[years >= 1000], 1900) / 100) * 100,
                    years)
    # Keep only plausible Nobel-era years
    years <- years[years >= 1900 & years <= 2030]
    if (length(years) == 0) return(c(NA_real_, NA_real_))

    c(min(years), max(years))
  } else {
    c(NA_real_, NA_real_)
  }
}

# Fetch bios in parallel
message(sprintf("  Fetching %d bio pages from svenskaakademien.se...", nrow(sa_members)))
bio_texts <- future_map_chr(sa_members$bio_url, fetch_nuxt_bio,
                             .progress = TRUE)

# Extract committee years
years_mat <- do.call(rbind, lapply(bio_texts, detect_committee))
sa_members$startComm <- years_mat[, 1]
sa_members$endComm <- years_mat[, 2]

# Filter to those who served on the Nobel Committee
lit <- sa_members %>%
  filter(!is.na(startComm)) %>%
  transmute(
    qid = qid,
    name = name,
    body = "Nobel Committee for Literature",
    startyear = startComm,
    endyear = endComm
  )

message(sprintf("  Literature Committee: %d members found", nrow(lit)))


# =============================================================================
# 5. Norwegian Nobel Committee
#    Source: English Wikipedia
#    Table columns: Member | Start | End | Tenure | Party | Chair | Deputy chair
# =============================================================================
message("=== Gathering Norwegian Nobel Committee ===")

nnc_url <- "https://en.wikipedia.org/wiki/List_of_members_of_the_Norwegian_Nobel_Committee"
page <- read_html(nnc_url)

# The page has multiple wikitables; the main member table is the largest
tables <- page %>% html_table(fill = TRUE)
table_nodes <- page %>% html_elements("table.wikitable")

if (length(tables) > 0) {
  # Find the largest table (the members table)
  largest_idx <- which.max(sapply(tables, nrow))
  nnc_table <- tables[[largest_idx]]
  target_table <- table_nodes[[largest_idx]]

  message(sprintf("  Found table with %d rows and %d columns",
                  nrow(nnc_table), ncol(nnc_table)))

  # Parse: columns are Member[ref], Start, End, Tenure, Party, Chair, Deputy chair
  nnc <- data.frame(
    name = as.character(nnc_table[[1]]),
    startyear = str_extract(as.character(nnc_table[[2]]), "\\d{4}") %>% as.numeric(),
    endyear = str_extract(as.character(nnc_table[[3]]), "\\d{4}") %>% as.numeric(),
    stringsAsFactors = FALSE
  )

  # Extract one Wikipedia link per table row (from this specific table only)
  table_rows <- target_table %>% html_elements("tr")
  row_links <- sapply(table_rows[-1], function(row) {
    links <- row %>% html_elements("a") %>% html_attr("href")
    wiki_links <- links[str_detect(links, "^/wiki/") & !str_detect(links, "redlink")]
    if (length(wiki_links) > 0) {
      paste0("https://en.wikipedia.org", wiki_links[1])
    } else {
      NA_character_
    }
  })

  # Align link count with table row count
  if (length(row_links) > nrow(nnc)) {
    row_links <- row_links[seq_len(nrow(nnc))]
  }

  # Look up QIDs for rows with valid links
  good_link_mask <- !is.na(row_links)
  good_links <- row_links[good_link_mask]

  message(sprintf("  Looking up QIDs for %d links (of %d rows)...",
                  length(good_links), nrow(nnc)))

  if (length(good_links) > 0) {
    nnc_qids_good <- wikipedia_urls_to_qids(good_links)
    nnc$qid <- NA_character_
    nnc$qid[good_link_mask] <- nnc_qids_good
  } else {
    nnc$qid <- NA_character_
  }

  message(sprintf("  NNC: %d rows with QIDs, %d without",
                  sum(!is.na(nnc$qid)), sum(is.na(nnc$qid))))

  nnc <- nnc %>%
    mutate(body = "Norwegian Nobel Committee") %>%
    select(qid, name, body, startyear, endyear) %>%
    filter(!is.na(startyear))
} else {
  message("  WARNING: No tables found on NNC Wikipedia page.")
  nnc <- data.frame(qid = character(), name = character(), body = character(),
                    startyear = numeric(), endyear = numeric())
}

message(sprintf("  Norwegian Nobel Committee: %d members found", nrow(nnc)))


# =============================================================================
# Combine and save
# =============================================================================
vetting_bodies <- bind_rows(chem, phys, med, lit, nnc) %>%
  select(qid, name, body, startyear, endyear) %>%
  arrange(body, startyear, name)

write_csv(vetting_bodies, data_path("vetting_bodies.csv"))
message(sprintf("\n=== DONE: %d total vetting body records saved to %s ===",
                nrow(vetting_bodies), data_path("vetting_bodies.csv")))

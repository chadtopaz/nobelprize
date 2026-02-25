# =============================================================================
# FILE: 02_vetting_bodies.R
# TITLE: Gathering Vetting Bodies (Nobel Prize Evaluation Committees)
# AUTHOR: Chad M. Topaz
# LAST UPDATED: February 2025
# =============================================================================
#
# PURPOSE AND GOALS:
# This script gathers membership rosters for all five Nobel Prize vetting committees.
# These bodies are distinct from the governing bodies; they evaluate nominations and
# prepare recommendations (typically 3-5 members per committee). The script sources
# members from heterogeneous platforms (Swedish/English Wikipedia, the Swedish Academy
# website SPA) and consolidates them into a unified CSV for network analysis. The output
# preserves each committee member's tenure period to enable temporal network studies
# of scientific evaluation networks across different Prize categories.
#
# METHODOLOGICAL DECISIONS AND DATA SOURCE RATIONALE:
# - Chemistry, Physics, Physiology/Medicine: Swedish Wikipedia "Nobelkommitté" pages
#   are scraped via MediaWiki API to extract stable wikitext (avoiding rendered HTML
#   variability). Lists are typically segmented into "Tidigare ledamöter" (former members),
#   "Nuvarande ledamöter" (current members), and sometimes "Sekreterare" (secretary).
#   This provides the most comprehensive historical coverage for these Swedish committees.
# - Literature: The Swedish Academy website (svenskaakademien.se, rebuilt as Nuxt.js SPA)
#   embeds biographical data in __NUXT_DATA__ JSON payloads. Bio text mentions Nobel
#   Committee service years (e.g., "ledamot av Nobelkommittén 1969–86"). We parse these
#   embedded payloads rather than relying on rendered DOM (more reliable than JS-dependent parsing).
# - Norwegian Nobel Committee: English Wikipedia maintains a comprehensive table of
#   Peace Prize committee members with tenure dates. This is scraped and linked to Wikidata QIDs.
#
# INPUTS:
#   - Chemistry/Physics/PhysMed: Swedish Wikipedia via MediaWiki API
#     (scrape_swedish_wiki_list utility function from 00_utils.R)
#   - Literature: https://www.svenskaakademien.se (Nuxt.js SPA with __NUXT_DATA__ payloads)
#   - Norwegian Nobel Committee: https://en.wikipedia.org/wiki/List_of_members_of_the_Norwegian_Nobel_Committee
#   - Wikidata SPARQL endpoint (for Swedish Academy member QIDs and seat numbers)
#   - 00_utils.R (utility functions: scrape_swedish_wiki_list, query_wikidata_safe, etc.)
#
# OUTPUTS:
#   - FILE: Data/vetting_bodies.csv
#   - COLUMNS: qid (Wikidata ID), name, body (committee name), startyear, endyear (numeric, NA allowed)
#   - FORMAT: CSV, one row per person-committee combination
#   - COUNTS: ~400-500 total records across five committees (expected)
#
# DEPENDENCIES:
#   - Library: rvest (HTML parsing), dplyr (data manipulation), stringr (regex),
#     httr2 (HTTP requests), furrr (parallel mapping)
#   - Sourced: Code/Data Gathering/00_utils.R
#
# KNOWN LIMITATIONS AND DATA QUALITY NOTES:
#   - Swedish Wikipedia Medicine Committee: The list is marked "Listan är ofullständig"
#     (list is incomplete), indicating gaps in historical records. Users should verify
#     critical time periods through original Nobel Office documentation.
#   - Literature Committee dates: Parsed from bio text containing phrases like
#     "ledamot av Nobelkommittén 1969–86". If the text is ambiguous or missing,
#     the member is excluded (startyear required for inclusion).
#   - Norwegian Nobel Committee: English Wikipedia table may have transcription errors
#     in tenure dates; verify critical dates against official sources.
#   - QID coverage: Some Wikipedia entries lack QID linkage. Members without QIDs are
#     retained in output but with qid=NA. They won't link to demographic data in later steps.
#   - Literature Committee partial dates: Some members show only start year or ambiguous
#     decade references. The year extraction algorithm keeps all detected 2-4 digit numbers
#     and filters for plausible Nobel era ranges (1900-2030).
#
# =============================================================================

source("Code/Data Gathering/00_utils.R")

# =============================================================================
# 1. Nobel Committee for Chemistry
#    Source: Swedish Wikipedia via MediaWiki API wikitext parsing
#    URL: https://sv.wikipedia.org/wiki/Vetenskapsakademiens_Nobelkommitt%C3%A9_f%C3%B6r_kemi
#
#    PARSING STRATEGY:
#    The scrape_swedish_wiki_list() utility function (from 00_utils.R) uses the
#    MediaWiki API to retrieve stable wikitext rather than rendered HTML. This
#    provides robust parsing of member lists segmented into sections:
#      - "Tidigare ledamöter": Former members with tenure dates
#      - "Nuvarande ledamöter": Current members (may lack end dates)
#      - "Sekreterare": Committee secretaries (permanent staff)
#    The function parses wiki-style person-date patterns like "Name (start – end year)"
#    and cross-references with Wikidata for QID linkage.
# =============================================================================
message("=== Gathering Nobel Committee for Chemistry ===")

# Scrape the Chemistry Committee member list from Swedish Wikipedia
# Returns a dataframe with columns: qid, name, startyear, endyear
chem <- scrape_swedish_wiki_list(
  page_title = "Vetenskapsakademiens Nobelkommitté för kemi",
  sections = c("Tidigare ledamöter", "Nuvarande ledamöter", "Sekreterare")
) %>%
  mutate(body = "Nobel Committee for Chemistry") %>%
  # Retain only members with both a QID and a start year
  # (members without QIDs won't link to demographic data; missing startyear means unreliable record)
  filter(!is.na(qid), !is.na(startyear))

message(sprintf("  Chemistry Committee: %d members found", nrow(chem)))


# =============================================================================
# 2. Nobel Committee for Physics
#    Source: Swedish Wikipedia via MediaWiki API wikitext parsing
#    URL: https://sv.wikipedia.org/wiki/Vetenskapsakademiens_Nobelkommitt%C3%A9_f%C3%B6r_fysik
#
#    Same parsing methodology as Chemistry Committee above.
# =============================================================================
message("=== Gathering Nobel Committee for Physics ===")

# Scrape the Physics Committee member list from Swedish Wikipedia
phys <- scrape_swedish_wiki_list(
  page_title = "Vetenskapsakademiens Nobelkommitté för fysik",
  sections = c("Tidigare ledamöter", "Nuvarande ledamöter", "Sekreterare")
) %>%
  mutate(body = "Nobel Committee for Physics") %>%
  # Retain only members with both a QID and a start year
  filter(!is.na(qid), !is.na(startyear))

message(sprintf("  Physics Committee: %d members found", nrow(phys)))


# =============================================================================
# 3. Nobel Committee for Physiology/Medicine
#    Source: Swedish Wikipedia via MediaWiki API wikitext parsing
#    URL: https://sv.wikipedia.org/wiki/Karolinska_Institutets_Nobelkommitt%C3%A9
#
#    DATA QUALITY WARNING:
#    The Swedish Wikipedia page explicitly states "Listan är ofullständig" (the list
#    is incomplete). This is a known limitation on Wikipedia itself; historical records
#    for early committee members (pre-1960s) are sparse and may have gaps. Users should
#    cross-reference critical time periods with official Nobel Office publications.
#
#    FILTERING NOTE:
#    Unlike Chemistry and Physics committees, we retain members even without QID linkage
#    here (filter only on !is.na(startyear)). This is because the committee membership
#    is already sparse, and dropping QID-less members would further reduce coverage.
#    Members without QIDs are included but marked with qid=NA.
# =============================================================================
message("=== Gathering Nobel Committee for Physiology/Medicine ===")

# Scrape the Medicine Committee member list from Swedish Wikipedia
med <- scrape_swedish_wiki_list(
  page_title = "Karolinska Institutets Nobelkommitté",
  sections = c("Tidigare ledamöter", "Nuvarande ledamöter",
               "Sekreterare / Generalsekreterare")
) %>%
  mutate(body = "Nobel Committee for Physiology/Medicine") %>%
  # Retain members with valid startyear, but allow qid=NA (see note above)
  filter(!is.na(startyear))

message(sprintf("  Physio/Med Committee: %d members found (%d with QIDs)",
                nrow(med), sum(!is.na(med$qid))))


# =============================================================================
# 4. Nobel Committee for Literature
#    Source: Swedish Academy website (svenskaakademien.se) + Wikidata
#
#    OVERVIEW:
#    The Swedish Academy website (https://www.svenskaakademien.se) was rebuilt
#    around 2024 as a Nuxt.js single-page application (SPA). Member biographical
#    data is NOT rendered by JavaScript but instead embedded as JSON in a script
#    tag "__NUXT_DATA__" in the initial HTML. This allows us to extract bio text
#    without executing JavaScript, parsing directly from the HTTP response.
#
#    The bio pages mention Nobel Committee service, e.g.:
#      "Han var ledamot av Nobelkommittén 1969–86"
#      (He was a member of the Nobel Committee 1969-86)
#
#    STRATEGY:
#    1. Query Wikidata to get all Swedish Academy members with seat numbers (P39 property)
#    2. Construct bio page URLs using the pattern: /svenska-akademien/de-aderton/stol-nr-{N}-{name-slug}
#    3. Fetch each bio page and extract the __NUXT_DATA__ payload (HTML-encoded)
#    4. Decode HTML entities and strip tags, then parse for "Nobelkommittén" mentions
#    5. Extract years from the surrounding text and apply validity filters
#    6. Combine committee years with QIDs from Wikidata, creating final committee roster
# =============================================================================
message("=== Gathering Nobel Committee for Literature ===")

# STEP 1: Retrieve Swedish Academy membership from Wikidata
# Query all Swedish Academy members with their academic seat numbers (property P462)
# These seat numbers are essential for constructing the correct bio page URLs.
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

# Extract seat numbers from Wikidata position labels
# Format: "member of the Swedish Academy at seat 12" -> seatNo = 12
# Seats are numbered 1-18 (the original "18 members" of the Swedish Academy)
sa_wd$seatNo <- str_extract(sa_wd$positionLabel, "(?<=seat )\\d{1,2}") %>% as.numeric()

# Normalize ISO date strings to YYYY-MM-DD format (extract date part only)
sa_wd$startDate <- str_extract(sa_wd$startDate, "^\\d{4}-\\d{2}-\\d{2}")
sa_wd$endDate <- str_extract(sa_wd$endDate, "^\\d{4}-\\d{2}-\\d{2}")

message(sprintf("  Wikidata: %d Swedish Academy member records", nrow(sa_wd)))

# STEP 2: Construct bio page URLs
# The Swedish Academy website uses a predictable URL structure:
#   https://www.svenskaakademien.se/svenska-akademien/de-aderton/stol-nr-{seat}-{name-slug}
# We construct name slugs by lowercasing, replacing spaces with hyphens, and
# transliterating Swedish characters (ä, å, ö, é) to ASCII equivalents.
# This allows us to find each member's biography page without needing a directory listing.

sa_members <- sa_wd %>%
  filter(!is.na(seatNo)) %>%
  mutate(
    # Create URL-safe name slug from the member name
    # Example: "Pähr Löwdin" -> "pahr-lowdin"
    name_slug = name %>%
      tolower() %>%                           # Convert to lowercase
      str_replace_all("\\s+", "-") %>%        # Replace spaces with hyphens
      # Transliterate common Swedish characters to ASCII
      str_replace_all("\u00e9", "e") %>%      # é → e
      str_replace_all("\u00e8", "e") %>%      # è → e
      str_replace_all("\u00e4", "a") %>%      # ä → a
      str_replace_all("\u00e5", "a") %>%      # å → a
      str_replace_all("\u00f6", "o") %>%      # ö → o
      str_replace_all("\u00fc", "u") %>%      # ü → u
      str_replace_all("\u00c9", "e") %>%      # É → e
      str_replace_all("[^a-z0-9-]", ""),      # Remove any remaining non-ASCII characters
    bio_url = sprintf(
      "https://www.svenskaakademien.se/svenska-akademien/de-aderton/stol-nr-%d-%s",
      seatNo, name_slug
    )
  )

message(sprintf("  Constructed %d bio URLs", nrow(sa_members)))

# STEP 3: Fetch and extract bio text from __NUXT_DATA__ payloads
# The Swedish Academy website (Nuxt.js SPA) embeds member bio data in a JSON payload
# called __NUXT_DATA__ within a <script> tag. This approach avoids the need for
# JavaScript execution and provides reliable access to the bio text.
#
# The bio text is HTML-encoded (entities like &auml;, \\u003C, etc.).
# We decode these entities and strip HTML tags to extract clean text for parsing.
#
# TARGET PHRASES in bio text:
#   "ledamot av Nobelkommittén YYYY–YYYY"  (member of Nobel Committee YYYY–YYYY)
#   "Nobelkommittén YYYY–YY"                (Nobel Committee YYYY–YY)
#
# ERROR HANDLING:
#   - Network errors: tryCatch returns empty string ""
#   - Missing __NUXT_DATA__: returns empty string ""
#   - Malformed payloads: returns what was extracted (best effort)

fetch_nuxt_bio <- function(url) {
  tryCatch({
    # Perform HTTP GET request with User-Agent header
    # Many websites require a User-Agent to avoid bot blocking
    resp <- request(url) %>%
      req_headers(`User-Agent` = "NobelNetworkBot/1.0 (research)") %>%
      req_perform()
    page_html <- resp_body_string(resp)

    # EXTRACT __NUXT_DATA__ PAYLOAD:
    # The payload is in: <script id="__NUXT_DATA__" ...>...JSON...</script>
    # Regex: 'id="__NUXT_DATA__"[^>]*>.*?</script>' captures from opening tag to closing tag
    nuxt_tag <- str_extract(page_html,
      'id="__NUXT_DATA__"[^>]*>.*?</script>')
    if (is.na(nuxt_tag)) return("")

    # Remove the script tag markers to extract the payload content
    nuxt_match <- str_replace(nuxt_tag, '^[^>]*>', '') %>%
      str_replace('</script>$', '')
    if (nchar(nuxt_match) == 0) return("")

    # DECODE HTML ENTITIES AND TAGS:
    # The payload contains HTML-encoded entities that need to be decoded:
    #   \\u003C = "<", \\u003E = ">"  (JavaScript escape sequences)
    #   &amp; = "&", &auml; = "ä", etc. (HTML entities)
    #   &nbsp; = " " (non-breaking space)
    # Then strip remaining HTML tags <...> to get plain text
    nuxt_match %>%
      str_replace_all("\\\\u003C", "<") %>%
      str_replace_all("\\\\u003E", ">") %>%
      str_replace_all("&amp;", "&") %>%
      str_replace_all("&auml;", "\u00e4") %>%      # ä
      str_replace_all("&ouml;", "\u00f6") %>%      # ö
      str_replace_all("&aring;", "\u00e5") %>%     # å
      str_replace_all("&eacute;", "\u00e9") %>%    # é
      str_replace_all("&nbsp;", " ") %>%
      str_replace_all("<[^>]+>", " ") %>%          # Remove all HTML tags
      str_squish()                                  # Normalize whitespace
  }, error = function(e) {
    ""  # Return empty string on any network/parsing error
  })
}

# STEP 4: Parse Nobel Committee service years from bio text
# This function searches for mentions of "Nobelkommittén" (Nobel Committee) in the
# bio text and extracts years from the surrounding context.
#
# PARSING STRATEGY:
#   1. Check if "Nobelkommittén" appears in text (case-insensitive)
#   2. Extract a 100-character window around the mention (to get surrounding context)
#   3. Search this window for all 2-4 digit numbers (candidate years)
#   4. Convert 2-digit years to 4-digit using context (e.g., 86 → 1986 based on adjacent 4-digit year)
#   5. Filter years to plausible Nobel era range [1900, 2030]
#   6. Return min and max years as [startyear, endyear]
#
# EDGE CASES:
#   - No "Nobelkommittén" mention: return [NA, NA] (member didn't serve on committee)
#   - Partial date mentions: ambiguous years are filtered post-extraction
#   - Multiple year mentions: we keep min/max across all extracted years

detect_committee <- function(txt) {
  if (str_detect(txt, regex("Nobelkommitt", ignore_case = TRUE))) {
    # Extract a sentence fragment mentioning Nobelkommittén
    # Captures up to 100 characters after "Nobelkommitt" for context
    sent <- str_extract(txt,
      "[^.]*Nobelkommitt[^\\.]{0,100}")
    if (is.na(sent)) return(c(NA_real_, NA_real_))

    # YEAR EXTRACTION:
    # Extract all 2-4 digit sequences (candidate years: 86, 1986, etc.)
    years <- str_extract_all(sent, "\\d{2,4}")[[1]] %>% as.numeric()
    if (length(years) == 0) return(c(NA_real_, NA_real_))

    # YEAR EXPANSION:
    # Convert 2-digit years to 4-digit (e.g., 86 → 1986)
    # Heuristic: use the century of the largest 4-digit year found, with fallback to 1900
    years <- ifelse(years < 100,
                    years + floor(max(years[years >= 1000], 1900) / 100) * 100,
                    years)

    # YEAR FILTERING:
    # Keep only years in the plausible Nobel era range
    # (Nobel Prize started 1901; we allow up to 2030 for future members)
    years <- years[years >= 1900 & years <= 2030]
    if (length(years) == 0) return(c(NA_real_, NA_real_))

    # Return start and end years (min and max of extracted years)
    c(min(years), max(years))
  } else {
    # No Nobelkommittén mention found
    c(NA_real_, NA_real_)
  }
}

# STEP 5: Fetch all bio pages in parallel
# Using furrr::future_map_chr() for parallel HTTP requests (much faster than sequential fetching)
# Progress bar shows completion percentage
message(sprintf("  Fetching %d bio pages from svenskaakademien.se...", nrow(sa_members)))
bio_texts <- future_map_chr(sa_members$bio_url, fetch_nuxt_bio,
                             .progress = TRUE)

# Extract committee years from all bio texts
# years_mat is a 2-column matrix: [startComm, endComm] for each member
years_mat <- do.call(rbind, lapply(bio_texts, detect_committee))
sa_members$startComm <- years_mat[, 1]
sa_members$endComm <- years_mat[, 2]

# STEP 6: Filter to committee members and format output
# Keep only members who have a valid startyear (indicating confirmed committee service)
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
# 5. Norwegian Nobel Committee (Peace Prize)
#    Source: English Wikipedia
#    URL: https://en.wikipedia.org/wiki/List_of_members_of_the_Norwegian_Nobel_Committee
#
#    WIKIPEDIA TABLE STRUCTURE:
#    The page contains multiple wikitable elements. The main member roster is
#    the largest table with these columns:
#      1. Member (name + potential citation reference)
#      2. Start (year joined)
#      3. End (year left/retired)
#      4. Tenure (calculated duration)
#      5. Party (political affiliation)
#      6. Chair (if served as chair)
#      7. Deputy chair (if served as deputy)
#
#    We extract columns 1-3 and cross-reference names to Wikipedia links for QID lookup.
# =============================================================================
message("=== Gathering Norwegian Nobel Committee ===")

nnc_url <- "https://en.wikipedia.org/wiki/List_of_members_of_the_Norwegian_Nobel_Committee"
page <- read_html(nnc_url)

# Extract all wiki tables from the page (HTML parsing)
tables <- page %>% html_table(fill = TRUE)
table_nodes <- page %>% html_elements("table.wikitable")

if (length(tables) > 0) {
  # Find the largest table (most likely to be the main member roster)
  # Smaller tables on Wikipedia are typically infoboxes, sidebars, or summary tables
  largest_idx <- which.max(sapply(tables, nrow))
  nnc_table <- tables[[largest_idx]]
  target_table <- table_nodes[[largest_idx]]

  message(sprintf("  Found table with %d rows and %d columns",
                  nrow(nnc_table), ncol(nnc_table)))

  # PARSE TABLE COLUMNS:
  # Column 1: Member name (as rendered text, may contain HTML-formatted references)
  # Column 2: Start year (extracted via regex for 4-digit year)
  # Column 3: End year (extracted via regex for 4-digit year)
  nnc <- data.frame(
    name = as.character(nnc_table[[1]]),
    startyear = str_extract(as.character(nnc_table[[2]]), "\\d{4}") %>% as.numeric(),
    endyear = str_extract(as.character(nnc_table[[3]]), "\\d{4}") %>% as.numeric(),
    stringsAsFactors = FALSE
  )

  # EXTRACT WIKIPEDIA LINKS FOR QID LOOKUP:
  # For each table row, find the first Wikipedia article link (excluding redlinks)
  # Redlinks are broken links to non-existent Wikipedia articles [class="new"]
  # We extract the href and construct the full Wikipedia URL for QID lookup
  table_rows <- target_table %>% html_elements("tr")
  row_links <- sapply(table_rows[-1], function(row) {
    links <- row %>% html_elements("a") %>% html_attr("href")
    # Filter for valid Wikipedia article links (start with /wiki/, not redlinks)
    wiki_links <- links[str_detect(links, "^/wiki/") & !str_detect(links, "redlink")]
    if (length(wiki_links) > 0) {
      paste0("https://en.wikipedia.org", wiki_links[1])
    } else {
      NA_character_
    }
  })

  # ALIGNMENT SAFETY CHECK:
  # Ensure the number of extracted links doesn't exceed table rows
  # (sometimes HTML parsing finds extra links; we slice to match table size)
  if (length(row_links) > nrow(nnc)) {
    row_links <- row_links[seq_len(nrow(nnc))]
  }

  # QID LOOKUP VIA BATCH API:
  # Create a boolean mask for rows with valid Wikipedia links
  # Use the batch API (wikipedia_urls_to_qids) to look up Wikidata QIDs efficiently
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

  # FORMAT OUTPUT:
  # Standardize column names and order; filter to rows with valid startyear
  nnc <- nnc %>%
    mutate(body = "Norwegian Nobel Committee") %>%
    select(qid, name, body, startyear, endyear) %>%
    filter(!is.na(startyear))
} else {
  # Graceful fallback if no tables found (e.g., page layout changed)
  message("  WARNING: No tables found on NNC Wikipedia page.")
  nnc <- data.frame(qid = character(), name = character(), body = character(),
                    startyear = numeric(), endyear = numeric())
}

message(sprintf("  Norwegian Nobel Committee: %d members found", nrow(nnc)))


# =============================================================================
# CONSOLIDATION AND OUTPUT
# =============================================================================
# Combine all five committee datasets into a single unified roster.
# The output is standardized by column selection and sorted for readability.
# No deduplication is performed at this stage (each committee maintains
# its own independent records; cross-committee duplicates are expected
# for some members who served on multiple committees).
#
# OUTPUT FORMAT:
#   qid: Wikidata Q-identifier (may be NA for members without Wikidata linkage)
#   name: Full name as found in source
#   body: Committee name (one of five standard names)
#   startyear: Year member joined committee (numeric, required for inclusion)
#   endyear: Year member left committee (numeric, may be NA for current members)
# =============================================================================
vetting_bodies <- bind_rows(chem, phys, med, lit, nnc) %>%
  select(qid, name, body, startyear, endyear) %>%
  arrange(body, startyear, name)

# WRITE OUTPUT:
# Save the consolidated dataframe to Data/vetting_bodies.csv in standard CSV format.
# This file serves as input to network analysis and demographic merging in subsequent scripts.
write_csv(vetting_bodies, data_path("vetting_bodies.csv"))
message(sprintf("\n=== DONE: %d total vetting body records saved to %s ===",
                nrow(vetting_bodies), data_path("vetting_bodies.csv")))

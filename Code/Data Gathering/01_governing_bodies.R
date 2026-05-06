# =============================================================================
# FILE: 01_governing_bodies.R
# TITLE: Gathering Governing Bodies (Nobel Prize Selection Bodies)
# AUTHOR: Chad M. Topaz
# LAST UPDATED: February 2025
# =============================================================================
#
# PURPOSE AND GOALS:
# This script gathers membership rosters for all four governing bodies that
# officially select Nobel Prize recipients. These are distinct from the vetting
# committees and represent the highest decision-making bodies for each category.
# The script sources members from heterogeneous data sources (Wikipedia scraping,
# Wikidata SPARQL queries, and manually curated Excel data) and consolidates them
# into a unified CSV for network analysis. The output preserves each person's
# tenure period (startyear and endyear) to enable temporal network analysis of
# the selection institutions.
#
# METHODOLOGICAL DECISIONS AND DATA SOURCE RATIONALE:
# - RSAS (Chemistry/Physics): Swedish Wikipedia HTML scraping for historical member lists
#   was chosen because it maintains the complete chronological record by election year.
#   Members serve for life; death years are incorporated from Wikidata in step 05.
# - Swedish Academy (Literature): Wikidata SPARQL queries (P39/member position) provide
#   reliably structured data with standardized dates and QID linkage.
# - Storting (Peace): Similarly sourced from Wikidata for consistency. Consecutive terms
#   are consolidated to reduce redundancy while preserving legitimate non-consecutive
#   service periods.
# - Karolinska Institutet (Physiology/Medicine): Manual compilation from Project Runeberg's
#   digitized statskalender editions provides the only available historical roster.
#   Post-1970 data remains unavailable and requires contacting the Nobel Office directly.
#
# INPUTS:
#   - RSAS: https://sv.wikipedia.org/wiki/Lista_%C3%B6ver_ledam%C3%B6ter_av_Kungliga_Vetenskapsakademien
#   - Swedish Academy: Wikidata SPARQL endpoint (query targeting P39=Swedish Academy member)
#   - Storting: Wikidata SPARQL endpoint (query targeting P39=Storting member, Q9045502)
#   - Karolinska: Data/intermediate/KI profs.xlsx (Sheet2, manually compiled from Project Runeberg)
#   - 00_utils.R (utility functions: wikipedia_urls_to_qids, query_wikidata_safe, data_path)
#
# OUTPUTS:
#   - FILE: Data/governing_bodies.csv
#   - COLUMNS: qid (Wikidata ID), name, body (institution name), startyear, endyear (numeric, NA allowed)
#   - FORMAT: CSV, one row per person-body combination
#   - COUNTS: ~1500 total records across four bodies (expected)
#
# DEPENDENCIES:
#   - Library: rvest (HTML parsing), dplyr (data manipulation), stringr (regex), readxl (Excel)
#   - Sourced: Code/Data Gathering/00_utils.R
#
# KNOWN LIMITATIONS AND DATA QUALITY NOTES:
#   - KI post-1970 gap: Nobel Assembly membership post-1970 unavailable online. Current data
#     covers 1881–1970 only (~140 confirmed professors). To extend: contact Nobel Office
#     (nobelforum@nobelprizemedicine.org, +46(0)8-52487800) or consult KI annual reports.
#   - RSAS endyear: Populated from death_year in script 05 (demographic query). Until then,
#     all RSAS members show endyear=NA, indicating lifetime appointment status.
#   - Storting consolidation: Term merging assumes gaps of ≤1 year are interruptions within
#     a single service period (e.g., recess years). Longer gaps create new service records.
#   - Wikipedia coverage bias: Swedish Academy and RSAS lists may have minor gaps for
#     founding members (1739/1809) due to historical documentation inconsistencies.
#   - Wikidata incompleteness: Some members may lack QID linkage, reducing linkage to
#     demographic/affiliation data in later steps. QID-less rows are filtered out.
#
# =============================================================================

source("Code/Data Gathering/00_utils.R")

# =============================================================================
# 1. Royal Swedish Academy of Sciences (RSAS)
#    Source: Swedish Wikipedia list page (sv.wikipedia.org)
#    Members serve for life (startyear = induction, endyear = death year)
#
#    PAGE STRUCTURE (HTML):
#      <p><b>1740</b>           ← year header (elected members in that year)
#      <ul>
#        <li>39. <a href="/wiki/...">Name</a></li>   ← Swedish domestic member
#        <li><i>1. <a href="...">Name</a></i></li>   ← foreign member (italic styling)
#      </ul>
#      <p><b>1741</b>
#      ...
#    Special case: founding members (1739) appear under section headers
#    "Akademiens grundare" and "Övriga" (Others) with no explicit year marker.
#    These are assigned year 1739 as the founding year.
#
#    DATA QUALITY NOTES:
#    - Members are elected in specific years; we capture the election year as startyear.
#    - Lifetime appointments mean endyear is typically the member's death year.
#    - Death years are sourced separately in script 05 from Wikidata demographic data.
#    - Some members may appear multiple times across years if they transferred classes.
#    - Only the earliest startyear is retained per person per body (deduplication).
# =============================================================================
message("=== Gathering RSAS members ===")

RSAS_URL <- "https://sv.wikipedia.org/wiki/Lista_%C3%B6ver_ledam%C3%B6ter_av_Kungliga_Vetenskapsakademien"

# Fetch the Wikipedia page and parse its HTML structure
page <- read_html(RSAS_URL)

# PARSING STRATEGY:
# The Swedish Wikipedia page uses a nested structure of <p> (year headers),
# <ul> (member lists), and <div class="mw-heading"> (section headers).
# We iterate through all direct children of the content container
# (".mw-parser-output") to maintain structural context across the document.
# When a <p> with <b>YEAR</b> is encountered, we update the context year.
# When a <ul> is encountered, all <li> members within are assigned that year.
# Parsing stops when the "Källor" (Sources) heading is reached to exclude references.
content_nodes <- page %>%
  html_elements(".mw-parser-output > p, .mw-parser-output > ul, .mw-parser-output > div.mw-heading")

current_year <- NA_real_  # Tracks the election year for members in the current <ul> block
rsas_records <- list()    # Accumulator list for member records
reached_sources <- FALSE  # Flag to stop parsing when references section is reached

for (node in content_nodes) {
  tag <- html_name(node)

  # HANDLE SECTION HEADERS (mw-heading divs)
  # These typically contain h2 or h3 tags with section titles like "Referenser" (References)
  # or "Externa länkar" (External links). We use this to identify the end of the content.
  if (tag == "div") {
    heading_text <- node %>% html_elements("h2, h3") %>% html_text()
    if (any(str_detect(heading_text, "(?i)k.llor|referenser|sources"))) {
      # "Källor", "Referenser", or "Sources" heading indicates end of member lists
      reached_sources <- TRUE
      break
    }
    next
  }

  # HANDLE YEAR HEADER PARAGRAPHS (<p><b>YYYY</b></p>)
  # These paragraphs contain bold text with the election year or founding section names.
  # The year is stored in current_year to be applied to subsequent <ul> blocks.
  if (tag == "p") {
    bold_text <- node %>% html_elements("b") %>% html_text()
    if (length(bold_text) > 0) {
      # Try to extract a 4-digit year (e.g., "1740" from <b>1740</b>)
      yr <- str_extract(bold_text[1], "^\\d{4}$")
      if (!is.na(yr)) {
        current_year <- as.numeric(yr)
      } else if (bold_text[1] %in% c("Akademiens grundare", "Övriga")) {
        # Special case: Founding members are listed under these headers (no explicit year)
        # Assign them to the founding year 1739
        current_year <- 1739
      }
    }
    next
  }

  # HANDLE MEMBER LISTS (<ul> blocks)
  # Each <ul> contains <li> elements with member names and Wikipedia links.
  # We extract the first <a> in each <li> (which should be the person's name).
  # The current_year context is applied to all members in this list.
  if (tag == "ul" && !is.na(current_year)) {
    items <- node %>% html_elements("li")
    for (item in items) {
      # Extract the first hyperlink in the <li> (the member's name)
      link <- item %>% html_elements("a") %>% .[1]
      if (length(link) == 0) next

      href <- html_attr(link, "href")
      name <- html_text(link)

      # DATA QUALITY FILTERING:
      # Skip links that are:
      #   - Missing href (malformed HTML)
      #   - Redlinks: [class="new"] for non-existent Wikipedia articles
      #   - Edit links: action=edit query parameters for edit pages
      #   - External links: URLs not starting with /wiki/
      # Only valid Wikipedia article links are retained.
      if (is.na(href) ||
          str_detect(href, "redlink|action=edit") ||
          !str_detect(href, "^/wiki/")) next

      # Construct full Wikipedia URL from relative path
      full_url <- paste0("https://sv.wikipedia.org", href)

      # Store the record: name, year of election, and Wikipedia URL for QID lookup
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

# QID LOOKUP VIA BATCH API:
# The wikipedia_urls_to_qids() function queries Wikidata via the MediaWiki API
# in batches to retrieve Wikidata Q-identifiers (QIDs) for each Wikipedia article URL.
# Batch lookups are significantly faster than scraping individual Wikipedia pages.
# This is a utility function sourced from 00_utils.R.
message(sprintf("  Looking up QIDs for %d RSAS members via batch API...", nrow(df_rsas)))
rsas_qids <- wikipedia_urls_to_qids(df_rsas$link)

# ENDYEAR POPULATION STRATEGY:
# RSAS members serve for life, so endyear is typically their death year.
# Death years are obtained from Wikidata in script 05 (demographics step) where
# we query property P570 (death date) for each QID. For now, endyear is set to NA
# to indicate "lifetime appointment" status. The subsequent script will populate
# these values using death_year from Wikidata queries. This two-step approach
# allows demographic queries to batch-fetch death dates efficiently.
rsas <- data.frame(
  qid = rsas_qids,
  name = df_rsas$name,
  body = "RSAS",
  startyear = as.numeric(df_rsas$year),
  endyear = NA_real_,  # Will be populated from death_year in script 05
  stringsAsFactors = FALSE
) %>%
  filter(!is.na(qid), !is.na(startyear))

message(sprintf("  RSAS: %d members found", nrow(rsas)))


# =============================================================================
# 2. Swedish Academy (Svenska Akademien)
#    Source: Wikidata SPARQL endpoint
#    Property: P39 = "position held" (Swedish Academy membership)
#    Members are appointed for life unless they resign.
#
#    QUERY EXPLANATION:
#    - P39 represents position/office held by a person
#    - Q207360 is the QID for "Swedish Academy"
#    - P361 is "part of" relation (handles subclass hierarchies)
#    - P31 filters out deputy roles (Q97563901)
#    - P580 = "start time" (membership start date, optional)
#    - P582 = "end time" (membership end date, optional, only if resigned/removed)
# =============================================================================
message("=== Gathering Swedish Academy members ===")

# SPARQL query to retrieve all Swedish Academy members with their tenure dates
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

# Execute the Wikidata SPARQL query with error handling
# query_wikidata_safe() is a utility function from 00_utils.R that handles
# timeouts, rate limits, and malformed responses gracefully.
sa_raw <- query_wikidata_safe(sa_query)

# PROCESS WIKIDATA RESULTS:
# Wikidata returns ISO 8601 formatted datetimes (e.g., "1871-12-20T00:00:00Z").
# We extract just the year portion for this analysis. Members with missing
# start dates are filtered out (they don't have reliable membership data).
swedish_academy <- sa_raw %>%
  mutate(
    # Extract 4-digit year from ISO datetime: "1871-12-20T00:00:00Z" -> "1871"
    startyear = as.numeric(str_extract(startDate, "^\\d{4}")),
    endyear = as.numeric(str_extract(endDate, "^\\d{4}"))
  ) %>%
  select(qid, name, startyear, endyear) %>%
  mutate(body = "Swedish Academy") %>%
  filter(!is.na(startyear))

message(sprintf("  Swedish Academy: %d member records found", nrow(swedish_academy)))


# =============================================================================
# 3. Storting (Norwegian Parliament, Peace Prize selector)
#    Source: Wikidata SPARQL endpoint
#    Property: P39 = "position held" (Storting membership)
#    Q9045502 = "Member of the Storting"
#
#    SPECIAL HANDLING: Term Consolidation
#    The Norwegian Storting operates on a fixed term cycle. Storting members
#    typically serve non-consecutive terms (elected for 4-year periods with
#    intervening gaps). To avoid inflating the network with duplicate edges,
#    we consolidate consecutive service periods (with gaps ≤1 year) into single
#    service records. Gaps >1 year are considered genuine interruptions and
#    create separate records. This improves interpretability while preserving
#    non-consecutive service legitimacy.
# =============================================================================
message("=== Gathering Storting members ===")

# SPARQL query to retrieve all Storting members with their tenure dates
# P31 filters for humans (wd:Q5) to exclude organizational entities
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

# Execute the Wikidata SPARQL query
storting_raw <- query_wikidata_safe(storting_query)

# Parse dates and set up for consolidation
storting <- storting_raw %>%
  mutate(
    # Extract 4-digit years from ISO datetimes
    startyear = as.numeric(str_extract(starttime, "^\\d{4}")),
    endyear = as.numeric(str_extract(endtime, "^\\d{4}"))
  ) %>%
  select(qid, name, startyear, endyear) %>%
  mutate(body = "Storting") %>%
  filter(!is.na(startyear))

# CONSOLIDATION ALGORITHM:
# For each person, we identify consecutive service blocks by checking the gap
# between the endyear of one term and the startyear of the next term.
# - If gap ≤1 year: terms are merged (assume brief recess or administrative pause)
# - If gap >1 year: terms remain separate (genuine interruption in service)
# This reduces noise while accurately representing service history.
storting <- storting %>%
  group_by(qid, name) %>%
  arrange(startyear) %>%
  mutate(
    # new_seq = 1 if this term starts a new service block (gap from prior term is >1 year)
    new_seq = if_else(is.na(lag(endyear)) | (startyear - lag(endyear) > 1), 1, 0),
    # seq_group: cumulative group ID for consecutive term blocks
    seq_group = cumsum(new_seq)
  ) %>%
  # Merge all terms within the same seq_group: earliest start, latest end
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
# 4. Karolinska Institutet (Nobel Assembly for Physiology/Medicine)
#    Source: Data/intermediate/KI profs.xlsx (manually compiled from Project Runeberg)
#
#    HISTORICAL CONTEXT:
#    The Nobel Prize in Physiology or Medicine is awarded by the Nobel Assembly
#    at Karolinska Institutet. Prior to 1984, the selection body consisted of
#    ALL full professors ("professor ordinarius") at KI. In 1977, the Nobel
#    Assembly became a formally constituted separate body. In 1984, membership
#    was fixed at 50 members. This file covers the period 1881–1970 using
#    digitized statskalender (official faculty records) from Project Runeberg.
#
#    DATA SOURCE AND STRUCTURE:
#    The Excel file (Data/intermediate/KI profs.xlsx, Sheet2) contains:
#      - 1021 rows: unique professor records
#      - 35 columns: qid, link, title, dob, dod, then per-edition columns
#        (one per statskalender edition 1881–1970), followed by "note" columns,
#        and finally Min/Max columns (earliest/latest year mentioned in record)
#      - Only 140 rows have both Min and Max populated AND a valid Wikidata QID
#        These rows represent confirmed full professors with tenure dates.
#
#    METHODOLOGY:
#    Wikidata Q3375124 (Karolinska Institutet) and Q3375126 (Nobel Assembly at KI)
#    have essentially no P39 (position held) or P463 (member of) records for
#    post-1970 members. Hence we rely entirely on this manual Excel compilation.
#
#    KNOWN DATA GAP:
#    Coverage ends at 1970. Post-1970 Nobel Assembly membership is NOT available
#    through public online sources:
#      - Wikidata lacks P39/P463 records for Q3375124
#      - Karolinska Institutet's website does not publish historical member rosters
#      - Project Runeberg's statskalender digitization stops around 1972
#    To extend this dataset, contact the Nobel Office directly:
#      Email: nobelforum@nobelprizemedicine.org
#      Phone: +46 (0)8 5248 7800
#    Alternatively, consult KI annual reports and faculty handbooks for
#    appointment and retirement records (1970–present).
# =============================================================================
message("=== Gathering Karolinska Institutet members ===")

ki_xlsx <- file.path("Data", "intermediate", "KI profs.xlsx")
if (file.exists(ki_xlsx)) {
  # Load Sheet2 (the main data sheet with confirmed professors)
  ki_raw <- read_excel(ki_xlsx, sheet = "Sheet2")
  message(sprintf("  Loaded KI profs.xlsx: %d rows", nrow(ki_raw)))

  # DATA QUALITY FILTERING:
  # We keep only rows with:
  #   1. Non-empty QID (must have Wikidata linkage for network analysis)
  #   2. Non-empty Min/Max columns (confirmed tenure date boundaries)
  # These 140 rows represent the most reliable professor records with both
  # historical coverage and definitive Wikidata mappings.
  karolinska <- ki_raw %>%
    filter(!is.na(qid), qid != "", !is.na(Min), !is.na(Max)) %>%
    transmute(
      qid = qid,
      name = title,
      body = "Karolinska Institutet",
      startyear = as.numeric(Min),    # Earliest year professor appears in statskalender
      endyear = as.numeric(Max)       # Latest year professor appears in statskalender
    )
} else {
  # Graceful fallback: if the Excel file is missing, create an empty dataframe
  # with the correct schema. This allows the pipeline to continue (albeit with
  # incomplete Physiology/Medicine data) rather than crashing. A warning
  # alerts the user to manually obtain the file.
  message("  WARNING: KI profs.xlsx not found. Karolinska data must be gathered manually.")
  karolinska <- data.frame(qid = character(), name = character(), body = character(),
                           startyear = numeric(), endyear = numeric())
}

message(sprintf("  Karolinska: %d member records found", nrow(karolinska)))


# =============================================================================
# CONSOLIDATION AND DEDUPLICATION
# =============================================================================
# Combine all four governing body datasets and apply deduplication rules:
# - For RSAS and Swedish Academy: a person may appear in multiple years due to
#   class transfers or reclassifications (e.g., moving from domestic to foreign
#   membership, or between scientific classes). We retain ONLY the earliest
#   record per person per body to represent first appointment/entry.
# - For Storting: multiple non-consecutive terms are legitimate parliamentary
#   service and should all be preserved (they're already consolidated above).
# - For Karolinska: Each professor is listed once with first-to-last tenure years.
#
# The output is sorted by body name, then startyear, then alphabetically by name.
# =============================================================================
governing_bodies <- bind_rows(rsas, swedish_academy, storting, karolinska) %>%
  select(qid, name, body, startyear, endyear) %>%
  group_by(body, qid) %>%
  arrange(startyear) %>%
  filter(
    # For Storting: keep all consolidated terms (multiple terms per person is expected)
    body == "Storting" |
    # For other bodies: keep only the earliest record per person
    # (deduplicates within-body class transfers or reclassifications)
    row_number() == 1
  ) %>%
  ungroup() %>%
  arrange(body, startyear, name)

# WRITE OUTPUT:
# The consolidated dataframe is saved to Data/governing_bodies.csv in CSV format.
# This file serves as input to subsequent network analysis and demographic scripts.
# The file is deterministic and reproducible given fixed input sources.
write_csv(governing_bodies, data_path("governing_bodies.csv"))
message(sprintf("\n=== DONE: %d total governing body records saved to %s ===",
                nrow(governing_bodies), data_path("governing_bodies.csv")))

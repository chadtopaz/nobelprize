# =============================================================================
# 08_build_nodes_edges.R
#
# FILE TITLE:
#   Construct Multilayer Network from Nobel Prize Institutional Records
#
# AUTHOR:
#   Chad M. Topaz
#
# LAST UPDATED:
#   February 2025
#
# PURPOSE & GOALS:
#   This script constructs the final multilayer network representation of the Nobel
#   Prize selection and award process. It reads all intermediate data files (governing
#   bodies, vetting committees, nominations, laureates) and outputs two files:
#   (1) nodes.csv: a single table of unique individuals with demographics (gender,
#       birth year, occupation, etc.), and (2) edges.csv: directed relationships between
#       individuals, categorized by type (institutional affiliation, nomination activity,
#       award decision), prize domain, and year. The multilayer structure captures
#       five distinct edge types that together model the flow of decision-making from
#       governance → vetting → nomination → laureate selection. The network can be
#       analyzed as a single temporal network (all edges across all years), filtered
#       by prize or edge type, or decomposed by layer for prize-specific analysis.
#
# METHODOLOGICAL DECISIONS:
#   - Nodes: All unique individuals appearing in any network layer are included in
#     a single nodes.csv file. Demographic data comes primarily from Wikidata (via
#     demographics.csv); individuals missing from demographics are added with minimal
#     data (qid, name only). This ensures network completeness while preserving
#     available biographical information.
#   - Edges: Complete bipartite graphs for institutional layers (governing → vetting
#     per year per prize, vetting → nominator per year per prize). For each year,
#     all active members on one side are connected to all active members on the
#     other side (full Cartesian product via expand.grid). This captures the
#     ongoing institutional mandate and oversight structure year by year.
#   - Selective edges for nomination layer (nominator → nominee): Only actual nominator/
#     nominee pairs from the archive are included (no artificial density added).
#   - Prize-scoped edges: All institutional and laureate edges are filtered by prize
#     domain (e.g., governing_body → vetting only for prizes served by that governing body).
#   - Year field: For governing → vetting edges, year is the calendar year of the
#     active overlap (one edge set per year both members serve). For vetting →
#     nominator edges, year is the nomination year. For nomination edges, year is
#     the award year. For laureate edges, year is the award year.
#   - QID resolution: QIDs are used as node identifiers. Nomination archive persons
#     are resolved to Wikidata QIDs via step 05 mapping; unresolved persons are assigned
#     temporary "NOM:" prefix IDs to maintain network connectivity (optional: manual
#     reconciliation can later replace these with QIDs).
#
# INPUTS:
#   1. Data/intermediate/governing_bodies.csv
#      - Columns: qid, name, body (RSAS, Karolinska Institutet, Swedish Academy, Storting),
#                 startyear, endyear
#      - Scope: ~2000 records (governance body members across all prizes)
#   2. Data/intermediate/vetting_bodies.csv
#      - Columns: qid, name, body (Nobel Committee for Chemistry/Physics/etc.),
#                 startyear, endyear
#      - Scope: ~1000 records (committee members across all prizes)
#   3. Data/intermediate/nominations.csv
#      - Columns: year, prize, nominator_person_id, nominee_person_id, ...
#      - Scope: ~30,000 records (all five prizes: Chemistry, Physics,
#        Physiology/Medicine, Literature, Peace)
#   4. Data/intermediate/laureates.csv
#      - Columns: year, prize, qid, name
#      - Scope: ~800 records (all prize categories and years)
#   5. Data/intermediate/demographics.csv
#      - Columns: qid, name, gender, birth_year, death_year, occupation, institution, ...
#      - Scope: ~2300 records (primary biographical source)
#   6. Data/intermediate/nomination_people_qids.csv (optional)
#      - Columns: person_id, qid, name
#      - Scope: ~5000 records (mapping of nobelprize.org person_id to Wikidata QID)
#
# OUTPUTS:
#   1. Data/nodes.csv
#      - Columns: qid, name, gender, birth_year, death_year, occupation, institution,
#                 birth_country, nationality, [geographic fields], [any other demographic columns]
#      - Scope: unique entity-resolved individuals (8,134 nodes in the shipped file)
#      - Structure: one row per unique QID or NOM: ID; QIDs take priority in deduplication
#   2. Data/edges.csv
#      - Columns: from_qid, to_qid, year, prize, from_layer, to_layer
#      - Scope: all directed relationships (514,111 edges in the shipped file)
#      - Structure: one row per edge; edges are deduplicated before output
#      - Edge types (5 total):
#        * governing_body → vetting_body: institutional oversight (all prizes)
#        * vetting_body → nominator: invitation/oversight (Chem, Phys, Med only)
#        * nominator → nominee: actual nomination (Chem, Phys, Med, Lit, Peace)
#        * governing_body → laureate: award decision (Chem, Phys, Med, Lit)
#        * vetting_body → laureate: award decision (Peace only)
#
# DEPENDENCIES:
#   - Code/Data Gathering/00_utils.R (sourced: provides data_path, output_path helpers)
#   - tidyverse (dplyr, tidyr, readr, stringr) [implicit via utils]
#
# KNOWN LIMITATIONS:
#   - Missing QID mappings: ~300-400 nomination archive persons remain unmapped to Wikidata QIDs
#     (assigned NOM: prefix IDs). These are valid network participants but lack rich metadata.
#   - Complete bipartite assumption: Vetting → nominator edges assume all active committee
#     members knew/invited all active nominators in a year. In practice, committees may
#     have solicited nominations from specific nominators; this detail is not captured.
#   - Temporal edge collapse: Multiple institutional roles by same person in same year are
#     collapsed to one edge (distinct() deduplication). Temporal granularity below the year
#     level is not preserved.
#   - Survivorship bias: Committee members who left before 1901 are filtered out;
#     pre-1901 activity is not represented, even if historically relevant.
#
# NETWORK ANALYSIS NOTES:
#   - Prize-specific networks: Filter by prize column to analyze individual prize networks
#     (e.g., Physics-only network). Chemistry/Physics/Medicine can be aggregated.
#   - Temporal analysis: Use year field to construct dynamic network (snapshots by decade,
#     or cumulative over time). Note: awards are annual; institutional rosters are annual.
#   - Layer decomposition: Filter by from_layer and to_layer to study individual layers
#     (e.g., nomination dynamics only: nominator → nominee edges; or award outcomes only:
#     governing/vetting → laureate edges).
#   - Centrality: Calculate degree, betweenness, closeness for individuals within selected
#     layers and prize domains. Governance roles (governing → vetting) often show high
#     centrality due to complete bipartite structure.
#
# =============================================================================

source("Code/Data Gathering/00_utils.R")

# =============================================================================
# 1. Load all intermediate data files
# =============================================================================
# SECTION PURPOSE:
#   Load all necessary intermediate data files (output of prior scripts). Files
#   are loaded conditionally; if a file doesn't exist, a warning is printed and
#   NULL is returned. Downstream code handles NULL gracefully (skipping edges
#   that depend on missing data).
#
# LOADING STRATEGY:
#   - All files from Data/intermediate/ directory (via data_path() helper)
#   - If missing, print WARNING (not error) and continue; allows partial rebuilds
#   - Count and report each successfully loaded file for validation
#
# DATA STRUCTURE:
#   - Governing bodies: committee/academy members by institution and year
#   - Vetting bodies: committee members by committee and year
#   - Nominations: nominator→nominee pairs with prize and year
#   - Laureates: award winners by prize and year
#   - Demographics: biographical data (Wikidata), enriched with geographic info
#
# =============================================================================
message("=== Loading intermediate data files ===\n")

# Helper function: load a CSV file if it exists, return NULL otherwise
# Prevents script crash on missing intermediate files; allows partial builds
load_if_exists <- function(filename, label) {
  path <- data_path(filename)
  if (file.exists(path)) {
    df <- read_csv(path, show_col_types = FALSE)
    message(sprintf("  %s: %d records", label, nrow(df)))
    df
  } else {
    message(sprintf("  WARNING: %s not found (%s)", label, path))
    NULL
  }
}

# Load all five intermediate files (NULL if missing)
governing   <- load_if_exists("governing_bodies.csv", "Governing bodies")
vetting     <- load_if_exists("vetting_bodies.csv",   "Vetting bodies")
nominations <- load_if_exists("nominations.csv",      "Nominations")
laureates   <- load_if_exists("laureates.csv",        "Laureates")
demographics <- load_if_exists("demographics.csv",    "Demographics")

# --- Filter governing body to Nobel era (1901+) ---
# RATIONALE:
#   The Royal Swedish Academy of Sciences (RSAS), Norwegian Parliament (Storting),
#   and other governing bodies maintain historical rosters extending back centuries
#   (RSAS back to 1739, Storting back to 1814 or earlier). However, Nobel Prizes
#   only began in 1901. Members whose service ended before 1901 had no influence on
#   Nobel decisions and should be excluded from network analysis.
#
# FILTERING RULE:
#   Keep governing members whose service overlaps with Nobel era:
#     - endyear >= 1901 (departed during/after Nobel era), OR
#     - endyear is NA (still serving or unknown departure; assume relevant to 1901+)
#   Remove members with endyear < 1901 (left before Nobels began; irrelevant to network)
#
# IMPACT:
#   Reduces pre-Nobel members (e.g., 18th-century Swedish Academy members) but
#   preserves network connectivity for relevant decision-makers.
#
if (!is.null(governing)) {
  n_before <- nrow(governing)
  governing <- governing %>%
    filter(is.na(endyear) | endyear >= 1901)
  message(sprintf("  Filtered to Nobel era (endyear >= 1901): %d → %d governing records (%d pre-Nobel removed)",
                  n_before, nrow(governing), n_before - nrow(governing)))
}


# =============================================================================
# 2. Define prize-body mappings (governance and vetting structures)
# =============================================================================
# SECTION PURPOSE:
#   Define which governing and vetting bodies are responsible for each prize
#   category. These lookup tables are used throughout edge construction to
#   enforce prize-specific filtering (e.g., only connect RSAS members to
#   Chemistry vetting committee, not to committees for other prizes).
#
# STRUCTURE & RATIONALE:
#   - Each prize is assigned exactly one governing body (the institution that
#     formally awards the prize) and one vetting body (the committee that
#     recommends award decisions).
#   - These mappings follow the official Nobel Prize Foundation structure:
#     * Chemistry & Physics: Royal Swedish Academy of Sciences (RSAS) governance,
#       Nobel Committees for Chemistry/Physics vetting
#     * Physiology/Medicine: Karolinska Institutet governance,
#       Nobel Committee for Physiology/Medicine vetting
#     * Literature: Swedish Academy governance,
#       Nobel Committee for Literature vetting
#     * Peace: Norwegian Parliament (Storting) governance,
#       Norwegian Nobel Committee vetting
#   - Vetting bodies recommend; governing bodies decide (and the Prize is formally
#     awarded by the responsible institution, sometimes in consultation with a
#     monarch or other ceremonial figure). For network purposes, we model both
#     governance and vetting as distinct layers.
#   - Prize-scoped edge filtering uses these tables to ensure institutional edges
#     connect appropriate bodies to appropriate committees.
#
# EDGE TYPE 1: governing_body → vetting_body
#   Uses governing_prize_map to connect RSAS to Chemistry vetting, etc.
#   Each prize has one governing body and one vetting body.
#
# EDGE TYPES 4 & 5: governing/vetting → laureate
#   Uses these maps to assign laureate edges to appropriate bodies.
#   Most prizes: governing_body → laureate
#   Peace only: vetting_body → laureate (Norwegian Nobel Committee explicitly awards)
#
# =============================================================================

# Which governing body selects for which prize
# (the institution formally responsible for prize award)
governing_prize_map <- tribble(
  ~body,                ~prize,
  "RSAS",               "Chemistry",
  "RSAS",               "Physics",
  "Karolinska Institutet", "Physiology/Medicine",
  "Swedish Academy",    "Literature",
  "Storting",           "Peace"
)

# Which vetting body serves which prize
# (the committee that recommends winners and/or nominators)
vetting_prize_map <- tribble(
  ~body,                                    ~prize,
  "Nobel Committee for Chemistry",          "Chemistry",
  "Nobel Committee for Physics",            "Physics",
  "Nobel Committee for Physiology/Medicine", "Physiology/Medicine",
  "Nobel Committee for Literature",         "Literature",
  "Norwegian Nobel Committee",              "Peace"
)


# =============================================================================
# 3. Build nodes table (unique individuals in the network)
# =============================================================================
# SECTION PURPOSE:
#   Construct the nodes.csv file by collecting all unique individuals appearing
#   in any institutional or award role across all network layers. Primary biographical
#   data comes from demographics.csv (Wikidata); individuals missing from demographics
#   are supplemented with names from institutional records (governing, vetting, laureates).
#
# DESIGN RATIONALE:
#   - Single table of all individuals: Rather than separate person tables per layer,
#     maintain one unified nodes table with demographic attributes. This enables
#     cross-layer analysis (e.g., "committee members who later won prizes").
#   - QID as primary identifier: All individuals are keyed by Wikidata QID (uniform
#     global identifier). This enables linking to Wikidata for additional metadata
#     (occupations, affiliations, publications) beyond the Nobel dataset.
#   - Fallback to nomination mappings: Nomination archive persons are resolved to QIDs
#     via step 05 mapping (nomination_people_qids.csv). If a person appears in
#     nominations but not in demographics, they're added with NOM: prefix IDs.
#   - Missing demographic data: Some individuals (especially unmapped nomination persons)
#     will have sparse demographic data (QID + name only; missing gender, birth_year, etc.).
#     This is acceptable for network analysis (edges still valid) but limits demographic
#     properties for visualization/analysis.
#
# PROCESS:
#   1. Collect all unique QIDs from all institutional tables (govern, vetting, laureates)
#   2. Load optional nomination QID mapping (step 05 output)
#   3. Start with demographics table, filter to network participants
#   4. Add missing QIDs (those in govern/vetting/laureates but not in demographics)
#      by pulling names from institutional records (left_join)
#   5. Handle degenerate case (no demographics file): build nodes from names only
#
# =============================================================================
message("\n=== Building nodes ===\n")

# --- Load nomination QID mapping (optional) ---
# Step 05 mapped nobelprize.org person_id to Wikidata QID for ~5000 nomination persons.
# If available, use to resolve nomination archive persons to QIDs; improves metadata richness.
# If unavailable, downstream code will assign temporary NOM: prefix IDs (still valid).
nom_qid_map_file <- data_path("nomination_people_qids.csv")
nom_people_qids <- if (file.exists(nom_qid_map_file)) {
  read_csv(nom_qid_map_file, show_col_types = FALSE) %>%
    filter(!is.na(qid))
} else NULL

# --- Collect all unique QIDs from all sources ---
# These are all the individuals who participate in the network in ANY role.
# Later, we'll ensure each one is present in the nodes table (with at least qid & name).
all_qids <- c(
  if (!is.null(governing))       governing$qid,
  if (!is.null(vetting))         vetting$qid,
  if (!is.null(laureates))       laureates$qid,
  if (!is.null(nom_people_qids)) nom_people_qids$qid
) %>%
  na.omit() %>%  # Remove NA values
  unique()      # Deduplicate QIDs (may appear in multiple sources)

message(sprintf("  Total unique QIDs: %d", length(all_qids)))

# --- Build nodes starting from demographics (primary source) ---
# Demographics (from Wikidata) is the richest data source; use as base for nodes.
# Then supplement with individuals missing from demographics.
if (!is.null(demographics)) {
  # Filter demographics to only network participants
  # (excludes Wikidata persons not involved in Nobel network)
  nodes <- demographics %>%
    filter(qid %in% all_qids) %>%
    # Select key demographic columns (include geographic columns if available)
    select(qid, name, gender, birth_country, nationality,
           birth_year, death_year, occupation, institution,
           # Geographic columns (added by 07_standardize_geography.R)
           any_of(c("birth_country_modern", "birth_subregion", "birth_continent",
                     "nationality_modern")))

  # --- Add missing QIDs ---
  # Some individuals appear in govern/vetting/laureates but not in demographics.csv.
  # This can happen if their Wikidata entry is incomplete or missing, or if
  # records use different name variants. We add them with minimal data (qid + name).
  missing_qids <- setdiff(all_qids, nodes$qid)
  if (length(missing_qids) > 0) {
    message(sprintf("  %d QIDs missing from demographics — adding with name only",
                    length(missing_qids)))

    # Try to get names from governing/vetting/laureates records
    # (these sources have names from authoritative Nobel Foundation records)
    name_lookup <- bind_rows(
      if (!is.null(governing))  governing %>% select(qid, name),
      if (!is.null(vetting))    vetting %>% select(qid, name),
      if (!is.null(laureates))  laureates %>% select(qid, name)
    ) %>%
      # Filter to only missing QIDs
      filter(qid %in% missing_qids) %>%
      # Deduplicate (keep first name if person appears in multiple sources)
      distinct(qid, .keep_all = TRUE)

    # Create a row for each missing QID (left_join to name_lookup)
    missing_nodes <- data.frame(
      qid = missing_qids,
      stringsAsFactors = FALSE
    ) %>%
      left_join(name_lookup, by = "qid")

    # Append missing nodes to the main nodes table
    nodes <- bind_rows(nodes, missing_nodes)
  }
} else {
  # --- Degenerate case: no demographics file ---
  # If demographics.csv is missing (e.g., due to failed upstream step),
  # build nodes from names in institutional records only.
  # This is a fallback; ideally demographics should be available.
  message("  WARNING: No demographics available. Building nodes from names only.")
  name_lookup <- bind_rows(
    if (!is.null(governing))  governing %>% select(qid, name),
    if (!is.null(vetting))    vetting %>% select(qid, name),
    if (!is.null(laureates))  laureates %>% select(qid, name)
  ) %>%
    filter(!is.na(qid)) %>%
    distinct(qid, .keep_all = TRUE)

  nodes <- name_lookup
}

# --- Report node count ---
message(sprintf("  Nodes: %d individuals", nrow(nodes)))


# =============================================================================
# 4. Build edges (directed relationships in the network)
# =============================================================================
# SECTION PURPOSE:
#   Construct the edges.csv file by systematically building five types of
#   directed relationships (edges) between network individuals. Each edge
#   represents a relationship mediated by an institution or activity (nomination,
#   award decision) in a specific prize domain and year.
#
# EDGE TYPES (5 total):
#   1. governing_body → vetting_body: Institutional oversight
#      Connects governing body members to vetting committee members, same prize
#   2. vetting_body → nominator: Committee invitation (Chem/Phys/Med only)
#      Connects active committee members to active nominators in same year
#   3. nominator → nominee: Nomination activity
#      Connects nominators to nominees (actual nomination records)
#   4. governing_body → laureate: Award decision (Chem/Phys/Med/Lit)
#      Connects governing body members to laureates in same award year
#   5. vetting_body → laureate: Award decision (Peace only)
#      Connects Norwegian Nobel Committee to Peace laureates
#
# DESIGN RATIONALE:
#   - Complete bipartite for institutional layers (gov-vet, vet-nom):
#     For each year, all active members in one layer connected to all active
#     members in the other layer for the same prize. This captures ongoing
#     institutional oversight relationships year by year.
#   - Selective edges for nomination layer (nom-nominee):
#     Only actual nominator-nominee pairs from archive. No artificial density added.
#   - Prize-scoped filtering:
#     Each edge connects individuals only within appropriate prize domain
#     (e.g., vetting_body → nominator edges only for Chem/Phys/Med).
#   - Year assignment:
#     For gov → vet edges: year = calendar year of active overlap
#     For vet → nom edges: year = nomination year
#     For nomination edges: year = award/nomination year
#     For laureate edges: year = award year
#
# =============================================================================
message("\n=== Building edges ===\n")

# Initialize empty data frame to accumulate edges
edges <- data.frame()

# =========================================================================
# EDGE TYPE 1: governing_body → vetting_body (all prizes)
# =========================================================================
# RATIONALE:
#   Governing bodies provide institutional oversight of vetting committees.
#   Each prize has one governing body and one vetting committee. We model
#   the ongoing institutional relationship by creating a complete bipartite
#   graph for each year: every active governing member connects to every
#   active vetting member in the same year.
#
# LOGIC:
#   For each prize and each year Y in the range [1901, max_endyear]:
#   - Find all governing body members active in year Y
#     (startyear <= Y AND (endyear >= Y OR endyear is NA))
#   - Find all vetting body members active in year Y (same filter)
#   - Create complete bipartite edges via expand.grid
#   - Set year = Y
#
# STRUCTURE:
#   Complete bipartite by year: all active governors × all active vetters.
#   This mirrors the construction of Edge Type 2 (vetting → nominator),
#   which also uses year-by-year expand.grid.
#
# SCOPE:
#   All 5 prizes have this relationship (gov → vet for each prize).
#
# =========================================================================
message("  Building governing → vetting edges...")

if (!is.null(governing) && !is.null(vetting)) {
  # Determine the year range for edge construction.
  # Start at 1901 (Nobel era); end at the latest service year across both tables,
  # capped at 1975 to match the nomination data period.
  max_year <- min(1975, max(
    max(governing$endyear, na.rm = TRUE),
    max(vetting$endyear, na.rm = TRUE)
  ))

  # Iterate over each prize
  for (i in seq_len(nrow(vetting_prize_map))) {
    prize <- vetting_prize_map$prize[i]
    vb_name <- vetting_prize_map$body[i]
    gb_name <- governing_prize_map$body[governing_prize_map$prize == prize]

    # Filter to members for this prize
    vb <- vetting %>% filter(body == vb_name)
    gb <- governing %>% filter(body == gb_name)

    # Skip if either table is empty (no data for this prize)
    if (nrow(vb) == 0 || nrow(gb) == 0) next

    # --- Process each year: complete bipartite of active members ---
    for (yr in 1901:max_year) {
      # Find governing body members active in this year
      active_gb <- gb %>%
        filter(
          !is.na(qid),
          startyear <= yr,
          (is.na(endyear) | endyear >= yr)
        )

      # Find vetting body members active in this year
      active_vb <- vb %>%
        filter(
          !is.na(qid),
          !is.na(startyear),
          startyear <= yr,
          (is.na(endyear) | endyear >= yr)
        )

      if (nrow(active_gb) == 0 || nrow(active_vb) == 0) next

      # Create complete bipartite edges: all active governors × all active vetters
      new_edges <- expand.grid(
        from_qid = active_gb$qid,
        to_qid = active_vb$qid,
        stringsAsFactors = FALSE
      ) %>%
        mutate(year = yr, prize = prize,
               from_layer = "governing_body", to_layer = "vetting_body")

      edges <- bind_rows(edges, new_edges)
    }
  }
  message(sprintf("    → %d governing → vetting edges",
                  sum(edges$to_layer == "vetting_body", na.rm = TRUE)))
}


# =========================================================================
# EDGE TYPE 2: vetting_body → nominator (Chem/Phys/Med only)
# =========================================================================
# RATIONALE:
#   Nobel committees for Chemistry, Physics, and Physiology/Medicine actively
#   solicit nominations from expert nominators. The vetting committees curate
#   the nomination process: they identify, invite, and sometimes directly solicit
#   nominations from eligible nominators.
#
# SCOPE:
#   Only Chemistry, Physics, and Physiology/Medicine have formal nomination
#   committees and nomination archives (other prizes have different selection processes).
#   Hence, these edge types exist only for these three prizes.
#
# LOGIC:
#   For each year Y in the nominations data:
#   - Find all active vetting committee members in year Y
#   - Find all nominators active in year Y (people who submitted nominations)
#   - Create a complete bipartite graph: each active committee member connects
#     to each active nominator (all pairs via expand.grid)
#   - Set year = Y, prize = the prize being voted on
#
# ASSUMPTION (COMPLETE BIPARTITE):
#   We assume all active committee members invited/allowed nominations from all
#   active nominators in a given year. This is an oversimplification: committees
#   likely targeted specific experts and may not have solicited all nominators.
#   However, lacking detailed nomination invitation records, the complete bipartite
#   assumption is a reasonable proxy for "committee oversight of nomination process".
#
# QID RESOLUTION:
#   Nominators in the nomination archive are identified by nobelprize.org person_id.
#   These must be resolved to Wikidata QIDs (or assigned NOM: prefix IDs) to match
#   the node table. This is done via the nomination_people_qids.csv mapping (step 05).
#   If a person_id has no QID mapping, it's assigned NOM:person_id as a fallback.
#
# =========================================================================
message("  Building vetting → nominator edges...")

# --- Load QID mapping for nomination archive persons ---
# Step 05 created nomination_people_qids.csv mapping nobelprize.org person_id
# to Wikidata QID. This enables linking nominators to the nodes table (which uses QIDs).
nom_qid_map <- NULL
nom_qid_file <- data_path("nomination_people_qids.csv")
if (file.exists(nom_qid_file)) {
  nom_qid_map <- read_csv(nom_qid_file, show_col_types = FALSE) %>%
    filter(!is.na(qid)) %>%
    select(person_id, qid, name)
  message(sprintf("    Loaded QID mapping: %d of nomination people have QIDs",
                  nrow(nom_qid_map)))
} else {
  message("    WARNING: nomination_people_qids.csv not found. Using NOM: prefix IDs.")
}

# --- Helper function: resolve person_id to QID (or NOM: prefix) ---
# DESIGN:
#   Given a nobelprize.org person_id, return the corresponding Wikidata QID
#   (from step 05 mapping). If no mapping exists, return NOM:person_id.
#   This ensures all nominators have identifiers suitable for nodes table lookup.
#
# RATIONALE:
#   Splitting the mapping lookup into a function allows vectorized application.
#   The fallback NOM: prefix IDs are valid network identifiers; they just lack
#   rich Wikidata metadata (gender, birth year, etc.).
#
resolve_nom_id <- function(pid) {
  if (!is.null(nom_qid_map)) {
    match <- nom_qid_map$qid[nom_qid_map$person_id == as.character(pid)]
    if (length(match) > 0 && !is.na(match[1])) return(match[1])
  }
  paste0("NOM:", pid)
}

if (!is.null(nominations) && !is.null(vetting)) {
  # --- Build vectorized QID lookup for speed ---
  # Instead of calling resolve_nom_id() once per person_id (slow),
  # create a named vector and use vector lookup (fast).
  # This is critical for performance with 30,000+ nomination records.
  if (!is.null(nom_qid_map)) {
    qid_lookup <- setNames(nom_qid_map$qid, nom_qid_map$person_id)
  } else {
    qid_lookup <- character(0)
  }

  # Vectorized version of resolve_nom_id: resolve multiple person_ids at once
  resolve_ids <- function(person_ids) {
    resolved <- qid_lookup[as.character(person_ids)]
    ifelse(is.na(resolved), paste0("NOM:", person_ids), resolved)
  }

  # ===================================================================
  # EDGE TYPE 2: vetting_body → nominator (Chem/Phys/Med only)
  # ===================================================================
  # Build institutional oversight edges from vetting committees to nominators.
  # Uses complete bipartite graph (expand.grid) for each prize-year.
  # Only Chemistry, Physics, Physiology/Medicine have nominators.
  vn_prizes <- c("Chemistry", "Physics", "Physiology/Medicine")

  for (prize in vn_prizes) {
    # Get vetting committee name for this prize
    vb_name <- vetting_prize_map$body[vetting_prize_map$prize == prize]
    vb <- vetting %>% filter(body == vb_name)
    # Get nominations for this prize
    prize_noms <- nominations %>% filter(prize == !!prize)

    # Skip if missing data
    if (nrow(vb) == 0 || nrow(prize_noms) == 0) next

    # Process each year separately
    for (yr in unique(prize_noms$year)) {
      # --- Get nominators active in this year ---
      yr_nominators <- prize_noms %>%
        filter(year == yr) %>%
        pull(nominator_person_id) %>%
        unique() %>%       # Each nominator only once per year
        na.omit()          # Remove NA person_ids

      if (length(yr_nominators) == 0) next

      # --- Get vetting committee members active in this year ---
      active_vb <- vb %>%
        filter(!is.na(qid),              # Must have a QID
               startyear <= yr,          # Started in or before this year
               is.na(endyear) | endyear >= yr)  # Still serving in this year

      if (nrow(active_vb) == 0) next

      # --- Resolve nominators to QIDs ---
      resolved_nominators <- resolve_ids(yr_nominators)

      # --- Create complete bipartite edges ---
      # expand.grid creates all combinations: each active vetting member
      # paired with each active nominator.
      # This models: "committee invited/allowed nominations from all active nominators"
      new_edges <- expand.grid(
        from_qid = active_vb$qid,
        to_qid = resolved_nominators,
        stringsAsFactors = FALSE
      ) %>%
        # Add metadata columns
        mutate(year = yr, prize = prize,
               from_layer = "vetting_body", to_layer = "nominator")

      edges <- bind_rows(edges, new_edges)
    }
  }

  message(sprintf("    → %d vetting → nominator edges",
                  sum(edges$to_layer == "nominator", na.rm = TRUE)))


  # ===================================================================
  # EDGE TYPE 3: nominator → nominee (actual nomination records)
  # ===================================================================
  # RATIONALE:
  #   The nomination archive explicitly records nominator-nominee pairs.
  #   Unlike the institutional layers (which are complete bipartite),
  #   we include only ACTUAL nominations (selective edges).
  #   This captures the nomination activity itself.
  #
  # LOGIC:
  #   Filter nominations to rows with both nominator_person_id and
  #   nominee_person_id (excludes incomplete records). Resolve both
  #   to QIDs. Group by (from_qid, to_qid, year, prize) to deduplicate
  #   (if same nominator nominated same person in same year for same prize,
  #   count as one edge).
  #
  # COVERAGE:
  #   All five prizes have archive data: Chemistry, Physics,
  #   Physiology/Medicine, Literature, and Peace. Phys/Med records
  #   extend only through 1953; the other four extend through 1975.
  #
  nom_edges <- nominations %>%
    # Keep only complete nomination records
    filter(!is.na(nominator_person_id), !is.na(nominee_person_id)) %>%
    # Resolve person_ids to QIDs and keep relevant columns
    transmute(
      from_qid = resolve_ids(nominator_person_id),
      to_qid = resolve_ids(nominee_person_id),
      year = year,
      prize = prize,
      from_layer = "nominator",
      to_layer = "nominee"
    ) %>%
    # Deduplicate: if same nominator nominated same person in same year
    # for same prize, count as one edge (not multiple)
    distinct()

  edges <- bind_rows(edges, nom_edges)

  # Report coverage: how many person_ids resolved to QIDs vs NOM: prefix IDs
  n_with_qid <- sum(!str_detect(c(nom_edges$from_qid, nom_edges$to_qid), "^NOM:"))
  n_total_ids <- length(c(nom_edges$from_qid, nom_edges$to_qid))
  message(sprintf("    → %d nominator → nominee edges (%.1f%% of IDs resolved to QIDs)",
                  nrow(nom_edges), 100 * n_with_qid / n_total_ids))
}


# =========================================================================
# EDGE TYPE 4: governing_body → laureate (Chem/Phys/Med/Lit)
# =========================================================================
# RATIONALE:
#   For Chemistry, Physics, Physiology/Medicine, and Literature,
#   the governing bodies formally award the prize (decision authority).
#   This edge captures the institutional decision to award.
#
# SCOPE:
#   Not Peace (Peace uses vetting_body → laureate; see edge type 5).
#
# LOGIC:
#   For each laureate with award year Y:
#   - Find all governing body members active in year Y
#     (started by Y, still serving at Y or unknown departure)
#   - Create one edge per governing member → laureate
#   - Set year = award year (when decision was made)
#
# NOTE ON "AWARD DECISION":
#   This models the institutional authority behind the award. In practice,
#   the full governing body rarely votes; usually a smaller committee/subgroup
#   decides. The model treats all active governing members as collectively
#   responsible (represents institutional authority, not individual voting).
#
# =========================================================================
message("  Building governing → laureate edges...")

if (!is.null(laureates) && !is.null(governing)) {
  # Prizes with governing_body → laureate edges
  gov_laureate_prizes <- c("Chemistry", "Physics", "Physiology/Medicine", "Literature")

  for (prize in gov_laureate_prizes) {
    # Get governing body for this prize
    gb_name <- governing_prize_map$body[governing_prize_map$prize == prize]
    gb <- governing %>% filter(body == gb_name)
    # Get laureates for this prize (filter out those missing QID)
    laur <- laureates %>% filter(prize == !!prize, !is.na(qid))

    # Skip if missing data
    if (nrow(laur) == 0 || nrow(gb) == 0) next

    # Process each laureate separately
    for (l_idx in seq_len(nrow(laur))) {
      l_year <- laur$year[l_idx]
      l_qid <- laur$qid[l_idx]

      # --- Find governing body members active in award year ---
      active_gb <- gb %>%
        filter(
          !is.na(qid),                          # Must have a QID
          startyear <= l_year,                  # Started by award year
          (is.na(endyear) | endyear >= l_year) # Still serving at award year
        )

      # Create edges from all active governing members to this laureate
      if (nrow(active_gb) > 0) {
        new_edges <- data.frame(
          from_qid = active_gb$qid,
          to_qid = l_qid,
          year = l_year,
          prize = prize,
          from_layer = "governing_body",
          to_layer = "laureate",
          stringsAsFactors = FALSE
        )
        edges <- bind_rows(edges, new_edges)
      }
    }
  }
  message(sprintf("    → %d governing → laureate edges",
                  sum(edges$from_layer == "governing_body" &
                        edges$to_layer == "laureate", na.rm = TRUE)))
}


# =========================================================================
# EDGE TYPE 5: vetting_body → laureate (Peace only)
# =========================================================================
# RATIONALE:
#   The Norwegian Nobel Committee serves a unique dual role for the Peace Prize:
#   it both vets candidates (reviews, deliberates) AND officially awards the prize
#   (decision authority). Unlike other prizes where governance and vetting are
#   separated, Peace Prize authority is concentrated in the committee.
#
# SCOPE:
#   Peace only. Chemistry/Physics/Medicine/Literature use governing_body → laureate.
#
# INSTITUTIONAL CONTEXT:
#   The Norwegian Nobel Committee is nominated by the Norwegian Parliament (Storting)
#   but acts with substantial independent authority on Peace Prize selection.
#   For network purposes, we model the committee as directly responsible for awards
#   (vetting_body → laureate), unlike other prizes which flow through governing bodies.
#
# LOGIC:
#   For each Peace laureate with award year Y:
#   - Find all Norwegian Nobel Committee members active in year Y
#   - Create one edge per committee member → laureate
#   - Set year = award year
#
# =========================================================================
message("  Building vetting → laureate edges (Peace)...")

if (!is.null(laureates) && !is.null(vetting)) {
  # Get Peace laureates (must have a QID)
  peace_laur <- laureates %>% filter(prize == "Peace", !is.na(qid))
  # Get Norwegian Nobel Committee members
  nnc <- vetting %>% filter(body == "Norwegian Nobel Committee")

  # Only proceed if both exist
  if (nrow(peace_laur) > 0 && nrow(nnc) > 0) {
    # Process each Peace laureate
    for (l_idx in seq_len(nrow(peace_laur))) {
      l_year <- peace_laur$year[l_idx]
      l_qid <- peace_laur$qid[l_idx]

      # --- Find committee members active in award year ---
      active_nnc <- nnc %>%
        filter(
          !is.na(qid),                          # Must have a QID
          startyear <= l_year,                  # Started by award year
          (is.na(endyear) | endyear >= l_year) # Still serving at award year
        )

      # Create edges from all active committee members to this laureate
      if (nrow(active_nnc) > 0) {
        new_edges <- data.frame(
          from_qid = active_nnc$qid,
          to_qid = l_qid,
          year = l_year,
          prize = "Peace",
          from_layer = "vetting_body",
          to_layer = "laureate",
          stringsAsFactors = FALSE
        )
        edges <- bind_rows(edges, new_edges)
      }
    }
  }
  message(sprintf("    → %d vetting → laureate edges (Peace)",
                  sum(edges$from_layer == "vetting_body" &
                        edges$to_layer == "laureate", na.rm = TRUE)))
}


# =============================================================================
# 5. Finalize and output network files
# =============================================================================
# SECTION PURPOSE:
#   Deduplicate and organize edge data, generate summary statistics,
#   and output the final nodes.csv and edges.csv files for network analysis.
#
# FINAL PROCESSING:
#   - Remove duplicate edges (may occur if same pair appears in multiple data sources)
#   - Sort by prize, year, edge type for readability
#   - Generate summaries by edge type and prize
#   - Output to Data/ directory
#
# =============================================================================
message("\n=== Finalizing ===\n")

# --- Deduplicate and sort edges ---
# distinct() removes duplicate rows (same from_qid, to_qid, year, prize, layers)
# This can occur if expand.grid creates some pairs multiple times, or if
# edges are added from multiple sources accidentally.
edges <- edges %>%
  distinct() %>%                               # Remove exact duplicates
  arrange(prize, year, from_layer, to_layer)  # Sort for readability

# --- Summary report: edges by type ---
message("Edge count by type:")
edges %>%
  count(from_layer, to_layer) %>%
  mutate(msg = sprintf("  %s → %s: %d", from_layer, to_layer, n)) %>%
  pull(msg) %>%
  walk(message)

# --- Summary report: edges by prize ---
message(sprintf("\nEdge count by prize:"))
edges %>%
  count(prize) %>%
  mutate(msg = sprintf("  %s: %d", prize, n)) %>%
  pull(msg) %>%
  walk(message)

# --- Write output files ---
# nodes.csv: one row per individual (demographics + network role info)
write_csv(nodes, output_path("nodes.csv"))
# edges.csv: one row per directed relationship (metadata: prize, year, type)
write_csv(edges, output_path("edges.csv"))

# --- Final status message ---
message(sprintf("\n=== DONE ==="))
message(sprintf("  nodes.csv: %d individuals → %s", nrow(nodes), output_path("nodes.csv")))
message(sprintf("  edges.csv: %d edges → %s", nrow(edges), output_path("edges.csv")))

# --- Alert user about NOM: prefix IDs ---
# NOM: prefix IDs indicate people from the nomination archive not yet matched to Wikidata.
# These are valid network participants but lack biographical metadata.
# Optional: manual reconciliation could add Wikidata QIDs (improves later analysis).
n_nom_ids <- sum(grepl("^NOM:", nodes$qid))
if (n_nom_ids > 0) {
  message(sprintf("\nNOTE: %d nodes use NOM: prefix IDs (no Wikidata QID match).", n_nom_ids))
  message("  These are valid identifiers from nobelprize.org. Optional: manual QID")
  message("  reconciliation could replace some with Wikidata QIDs for richer metadata.")
}

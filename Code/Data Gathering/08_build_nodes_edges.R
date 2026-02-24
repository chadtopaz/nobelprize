# =============================================================================
# 08_build_nodes_edges.R
# Assemble the final multilayer network: nodes.csv and edges.csv
#
# This script reads all intermediate files and builds:
#   - nodes.csv: one row per unique individual, with demographics
#   - edges.csv: one row per directed relationship, with year/prize/layer info
#
# Edge types (from → to):
#   1. governing_body → vetting_body (all prizes)
#   2. vetting_body → nominator (Chemistry, Physics, Physiology/Medicine only)
#   3. nominator → nominee (all prizes with nomination data)
#   4. governing_body → laureate (Chemistry, Physics, Physiology/Medicine, Lit)
#   5. vetting_body → laureate (Peace only)
#
# See manuscript Section 2.1 (Network Model) for full description.
# =============================================================================

source("Code/Data Gathering/00_utils.R")

# =============================================================================
# 1. Load all intermediate data
# =============================================================================
message("=== Loading intermediate data files ===\n")

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

governing   <- load_if_exists("governing_bodies.csv", "Governing bodies")
vetting     <- load_if_exists("vetting_bodies.csv",   "Vetting bodies")
nominations <- load_if_exists("nominations.csv",      "Nominations")
laureates   <- load_if_exists("laureates.csv",        "Laureates")
demographics <- load_if_exists("demographics.csv",    "Demographics")

# Filter governing body members to the Nobel era (1901+).
# RSAS, Storting, and Swedish Academy rosters extend back to the 1700s,
# but members who left before 1901 never participated in Nobel decisions.
# Keep members whose service overlaps with 1901+:
#   - endyear >= 1901, OR
#   - endyear is NA (still serving / unknown departure)
if (!is.null(governing)) {
  n_before <- nrow(governing)
  governing <- governing %>%
    filter(is.na(endyear) | endyear >= 1901)
  message(sprintf("  Filtered to Nobel era (endyear >= 1901): %d → %d governing records (%d pre-Nobel removed)",
                  n_before, nrow(governing), n_before - nrow(governing)))
}


# =============================================================================
# 2. Map bodies to prizes
# =============================================================================

# Which governing body selects for which prize
governing_prize_map <- tribble(
  ~body,                ~prize,
  "RSAS",               "Chemistry",
  "RSAS",               "Physics",
  "Karolinska Institutet", "Physiology/Medicine",
  "Swedish Academy",    "Literature",
  "Storting",           "Peace"
)

# Which vetting body serves which prize
vetting_prize_map <- tribble(
  ~body,                                    ~prize,
  "Nobel Committee for Chemistry",          "Chemistry",
  "Nobel Committee for Physics",            "Physics",
  "Nobel Committee for Physiology/Medicine", "Physiology/Medicine",
  "Nobel Committee for Literature",         "Literature",
  "Norwegian Nobel Committee",              "Peace"
)


# =============================================================================
# 3. Build nodes
# =============================================================================
message("\n=== Building nodes ===\n")

# Load nomination QID mapping (from step 05)
nom_qid_map_file <- data_path("nomination_people_qids.csv")
nom_people_qids <- if (file.exists(nom_qid_map_file)) {
  read_csv(nom_qid_map_file, show_col_types = FALSE) %>%
    filter(!is.na(qid))
} else NULL

# Collect all QIDs from all sources (including matched nomination people)
all_qids <- c(
  if (!is.null(governing))       governing$qid,
  if (!is.null(vetting))         vetting$qid,
  if (!is.null(laureates))       laureates$qid,
  if (!is.null(nom_people_qids)) nom_people_qids$qid
) %>%
  na.omit() %>%
  unique()

message(sprintf("  Total unique QIDs: %d", length(all_qids)))

# Start with demographics as the base for nodes
if (!is.null(demographics)) {
  nodes <- demographics %>%
    filter(qid %in% all_qids) %>%
    select(qid, name, gender, birth_country, nationality,
           birth_year, death_year, occupation, institution,
           any_of(c("birth_country_modern", "birth_subregion", "birth_continent",
                     "nationality_modern")))

  # Add any QIDs that are missing from demographics
  missing_qids <- setdiff(all_qids, nodes$qid)
  if (length(missing_qids) > 0) {
    message(sprintf("  %d QIDs missing from demographics — adding with name only",
                    length(missing_qids)))

    # Try to get names from other sources
    name_lookup <- bind_rows(
      if (!is.null(governing))  governing %>% select(qid, name),
      if (!is.null(vetting))    vetting %>% select(qid, name),
      if (!is.null(laureates))  laureates %>% select(qid, name)
    ) %>%
      filter(qid %in% missing_qids) %>%
      distinct(qid, .keep_all = TRUE)

    missing_nodes <- data.frame(
      qid = missing_qids,
      stringsAsFactors = FALSE
    ) %>%
      left_join(name_lookup, by = "qid")

    nodes <- bind_rows(nodes, missing_nodes)
  }
} else {
  # No demographics; build nodes from names only
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

message(sprintf("  Nodes: %d individuals", nrow(nodes)))


# =============================================================================
# 4. Build edges
# =============================================================================
message("\n=== Building edges ===\n")

edges <- data.frame()

# -------------------------------------------------------------------------
# Edge type 1: governing_body → vetting_body
# For each prize, members of the governing body who served during a year
# when a vetting body member also served create an edge.
# -------------------------------------------------------------------------
message("  Building governing → vetting edges...")

if (!is.null(governing) && !is.null(vetting)) {
  for (i in seq_len(nrow(vetting_prize_map))) {
    prize <- vetting_prize_map$prize[i]
    vb_name <- vetting_prize_map$body[i]
    gb_name <- governing_prize_map$body[governing_prize_map$prize == prize]

    vb <- vetting %>% filter(body == vb_name)
    gb <- governing %>% filter(body == gb_name)

    if (nrow(vb) == 0 || nrow(gb) == 0) next

    # For each vetting body member, find governing body members who
    # were active during the vetting member's service years
    for (v_idx in seq_len(nrow(vb))) {
      v_start <- vb$startyear[v_idx]
      v_end <- ifelse(is.na(vb$endyear[v_idx]),
                      as.numeric(format(Sys.Date(), "%Y")),
                      vb$endyear[v_idx])
      v_qid <- vb$qid[v_idx]

      if (is.na(v_qid) || is.na(v_start)) next

      # Find years where both were active
      # Edge year = first year of vetting body member's service
      active_gb <- gb %>%
        filter(
          !is.na(qid),
          startyear <= v_start,
          (is.na(endyear) | endyear >= v_start)
        )

      if (nrow(active_gb) > 0) {
        new_edges <- data.frame(
          from_qid = active_gb$qid,
          to_qid = v_qid,
          year = v_start,
          prize = prize,
          from_layer = "governing_body",
          to_layer = "vetting_body",
          stringsAsFactors = FALSE
        )
        edges <- bind_rows(edges, new_edges)
      }
    }
  }
  message(sprintf("    → %d governing → vetting edges",
                  sum(edges$to_layer == "vetting_body", na.rm = TRUE)))
}


# -------------------------------------------------------------------------
# Edge type 2: vetting_body → nominator
# For Chemistry, Physics, Physiology/Medicine only (these committees
# appoint nominators). We need nomination data to know who was nominated.
# -------------------------------------------------------------------------
message("  Building vetting → nominator edges...")

# Load QID mapping from step 05 (nomination archive people → Wikidata QIDs)
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

# Helper: resolve a nobelprize.org person_id to a QID (or NOM: fallback)
resolve_nom_id <- function(pid) {
  if (!is.null(nom_qid_map)) {
    match <- nom_qid_map$qid[nom_qid_map$person_id == as.character(pid)]
    if (length(match) > 0 && !is.na(match[1])) return(match[1])
  }
  paste0("NOM:", pid)
}

if (!is.null(nominations) && !is.null(vetting)) {
  # Build a vectorized lookup for speed
  if (!is.null(nom_qid_map)) {
    qid_lookup <- setNames(nom_qid_map$qid, nom_qid_map$person_id)
  } else {
    qid_lookup <- character(0)
  }

  resolve_ids <- function(person_ids) {
    resolved <- qid_lookup[as.character(person_ids)]
    ifelse(is.na(resolved), paste0("NOM:", person_ids), resolved)
  }

  # --- Edge type 2: vetting_body → nominator ---
  # For Chem/Phys/Med, vetting committees invite nominators.
  # We connect active vetting members to nominators in the same prize-year.
  vn_prizes <- c("Chemistry", "Physics", "Physiology/Medicine")

  for (prize in vn_prizes) {
    vb_name <- vetting_prize_map$body[vetting_prize_map$prize == prize]
    vb <- vetting %>% filter(body == vb_name)
    prize_noms <- nominations %>% filter(prize == !!prize)

    if (nrow(vb) == 0 || nrow(prize_noms) == 0) next

    for (yr in unique(prize_noms$year)) {
      # Nominators active this year
      yr_nominators <- prize_noms %>%
        filter(year == yr) %>%
        pull(nominator_person_id) %>%
        unique() %>%
        na.omit()

      if (length(yr_nominators) == 0) next

      # Vetting body members active this year
      active_vb <- vb %>%
        filter(!is.na(qid), startyear <= yr,
               is.na(endyear) | endyear >= yr)

      if (nrow(active_vb) == 0) next

      resolved_nominators <- resolve_ids(yr_nominators)

      new_edges <- expand.grid(
        from_qid = active_vb$qid,
        to_qid = resolved_nominators,
        stringsAsFactors = FALSE
      ) %>%
        mutate(year = yr, prize = prize,
               from_layer = "vetting_body", to_layer = "nominator")

      edges <- bind_rows(edges, new_edges)
    }
  }

  message(sprintf("    → %d vetting → nominator edges",
                  sum(edges$to_layer == "nominator", na.rm = TRUE)))


  # --- Edge type 3: nominator → nominee ---
  nom_edges <- nominations %>%
    filter(!is.na(nominator_person_id), !is.na(nominee_person_id)) %>%
    transmute(
      from_qid = resolve_ids(nominator_person_id),
      to_qid = resolve_ids(nominee_person_id),
      year = year,
      prize = prize,
      from_layer = "nominator",
      to_layer = "nominee"
    ) %>%
    distinct()

  edges <- bind_rows(edges, nom_edges)

  n_with_qid <- sum(!str_detect(c(nom_edges$from_qid, nom_edges$to_qid), "^NOM:"))
  n_total_ids <- length(c(nom_edges$from_qid, nom_edges$to_qid))
  message(sprintf("    → %d nominator → nominee edges (%.1f%% of IDs resolved to QIDs)",
                  nrow(nom_edges), 100 * n_with_qid / n_total_ids))
}


# -------------------------------------------------------------------------
# Edge type 4: governing_body → laureate
# For Chemistry, Physics, Physiology/Medicine, Literature
# The governing body formally awards the prize.
# -------------------------------------------------------------------------
message("  Building governing → laureate edges...")

if (!is.null(laureates) && !is.null(governing)) {
  gov_laureate_prizes <- c("Chemistry", "Physics", "Physiology/Medicine", "Literature")

  for (prize in gov_laureate_prizes) {
    gb_name <- governing_prize_map$body[governing_prize_map$prize == prize]
    gb <- governing %>% filter(body == gb_name)
    laur <- laureates %>% filter(prize == !!prize, !is.na(qid))

    if (nrow(laur) == 0 || nrow(gb) == 0) next

    for (l_idx in seq_len(nrow(laur))) {
      l_year <- laur$year[l_idx]
      l_qid <- laur$qid[l_idx]

      # Governing body members active in the award year
      active_gb <- gb %>%
        filter(
          !is.na(qid),
          startyear <= l_year,
          (is.na(endyear) | endyear >= l_year)
        )

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


# -------------------------------------------------------------------------
# Edge type 5: vetting_body → laureate (Peace only)
# The Norwegian Nobel Committee both vets and awards the Peace Prize.
# -------------------------------------------------------------------------
message("  Building vetting → laureate edges (Peace)...")

if (!is.null(laureates) && !is.null(vetting)) {
  peace_laur <- laureates %>% filter(prize == "Peace", !is.na(qid))
  nnc <- vetting %>% filter(body == "Norwegian Nobel Committee")

  if (nrow(peace_laur) > 0 && nrow(nnc) > 0) {
    for (l_idx in seq_len(nrow(peace_laur))) {
      l_year <- peace_laur$year[l_idx]
      l_qid <- peace_laur$qid[l_idx]

      active_nnc <- nnc %>%
        filter(
          !is.na(qid),
          startyear <= l_year,
          (is.na(endyear) | endyear >= l_year)
        )

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
# 5. Deduplicate and save
# =============================================================================
message("\n=== Finalizing ===\n")

edges <- edges %>%
  distinct() %>%
  arrange(prize, year, from_layer, to_layer)

# Summary
message("Edge count by type:")
edges %>%
  count(from_layer, to_layer) %>%
  mutate(msg = sprintf("  %s → %s: %d", from_layer, to_layer, n)) %>%
  pull(msg) %>%
  walk(message)

message(sprintf("\nEdge count by prize:"))
edges %>%
  count(prize) %>%
  mutate(msg = sprintf("  %s: %d", prize, n)) %>%
  pull(msg) %>%
  walk(message)

# Save
write_csv(nodes, output_path("nodes.csv"))
write_csv(edges, output_path("edges.csv"))

message(sprintf("\n=== DONE ==="))
message(sprintf("  nodes.csv: %d individuals → %s", nrow(nodes), output_path("nodes.csv")))
message(sprintf("  edges.csv: %d edges → %s", nrow(edges), output_path("edges.csv")))
n_nom_ids <- sum(grepl("^NOM:", nodes$qid))
if (n_nom_ids > 0) {
  message(sprintf("\nNOTE: %d nodes use NOM: prefix IDs (no Wikidata QID match).", n_nom_ids))
  message("  These are valid identifiers from nobelprize.org. Optional: manual QID")
  message("  reconciliation could replace some with Wikidata QIDs for richer metadata.")
}

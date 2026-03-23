# =============================================================================
# File: 13_multilayer_quantities.R
# Title: Multilayer Network Quantities for the Nobel Prize Selection Network
#
# Author: Chad M. Topaz
# Date: March 2026
#
# Purpose:
#   Computes two genuinely multilayer quantities that require the full
#   multi-edge-type structure:
#
#   1. SUPRA-ADJACENCY ASSORTATIVITY: Newman assortativity coefficient (r) on
#      the full network (all edge types combined), compared to r for each
#      edge type separately. Shows which layer drives system-wide geographic
#      structure.
#
#   2. LAYER-CONTRAST PERMUTATION TEST: Formal test of whether nomination-edge
#      homophily significantly exceeds institutional-edge homophily, with a
#      p-value for the between-layer contrast. This is a genuinely multilayer
#      hypothesis test — it tests a relationship between layers, not a property
#      of any single layer.
#
# Inputs:
#   - Data/edges.csv: All directed edges with layer information
#   - Data/nodes.csv: All individuals with geographic attributes
#
# Outputs:
#   - Data/results_multilayer_quantities.csv: Summary results
#   - Console output with interpretable summary
#
# Dependencies:
#   - tidyverse, igraph
# =============================================================================

library(tidyverse)
library(igraph)
library(furrr)
library(progressr)

# --- Configuration ---
N_PERM <- 10000
GLOBAL_SEED <- 42
set.seed(GLOBAL_SEED)

# --- Parallel backend setup ---
n_cores <- max(1, availableCores() - 1)
plan(multisession, workers = n_cores)
message(sprintf("Parallel backend: %d workers", n_cores))
handlers(global = TRUE)

# --- Load data ---
message("Loading data...")
edges <- read_csv("Data/edges.csv", show_col_types = FALSE)
nodes <- read_csv("Data/nodes.csv", show_col_types = FALSE)

# --- Assign geography ---
# Use birth_country_modern from nodes.csv for ALL nodes (consistent definition).
# This differs from the main analysis which uses professional country for
# nomination edges; we note this and provide both where possible.
geo_lookup <- nodes %>%
  select(qid, birth_country_modern, birth_subregion, birth_continent) %>%
  distinct()

message(sprintf("Nodes with birth_country_modern: %d / %d (%.1f%%)",
                sum(!is.na(geo_lookup$birth_country_modern)),
                nrow(geo_lookup),
                100 * mean(!is.na(geo_lookup$birth_country_modern))))

# --- Merge geography onto edges ---
edges_geo <- edges %>%
  left_join(geo_lookup, by = c("from_qid" = "qid")) %>%
  rename(from_country = birth_country_modern,
         from_subregion = birth_subregion,
         from_continent = birth_continent) %>%
  left_join(geo_lookup, by = c("to_qid" = "qid")) %>%
  rename(to_country = birth_country_modern,
         to_subregion = birth_subregion,
         to_continent = birth_continent)

# Report coverage
message(sprintf("Edges with both endpoints having country data: %d / %d (%.1f%%)",
                sum(!is.na(edges_geo$from_country) & !is.na(edges_geo$to_country)),
                nrow(edges_geo),
                100 * mean(!is.na(edges_geo$from_country) & !is.na(edges_geo$to_country))))

# =============================================================================
# PART 1: SUPRA-ADJACENCY ASSORTATIVITY
# =============================================================================
# Compute Newman assortativity on geography for:
# (a) Full network (all edges)
# (b) Nomination edges only
# (c) Institutional edges only
# (d) Each specific edge type
# =============================================================================

message("\n=== PART 1: Supra-Adjacency Assortativity ===\n")

compute_assortativity <- function(edge_df, geo_col_from, geo_col_to, label = "") {
  # Filter to edges where both endpoints have geography
  df <- edge_df %>%
    filter(!is.na(!!sym(geo_col_from)), !is.na(!!sym(geo_col_to)))

  if (nrow(df) == 0) return(tibble(label = label, n_edges = 0, r = NA_real_))

  # Build igraph object
  # Need unique node IDs with geography
  all_nodes <- bind_rows(
    df %>% select(qid = from_qid, geo = !!sym(geo_col_from)),
    df %>% select(qid = to_qid, geo = !!sym(geo_col_to))
  ) %>% distinct(qid, .keep_all = TRUE)

  g <- graph_from_data_frame(
    df %>% select(from_qid, to_qid),
    directed = TRUE,
    vertices = all_nodes %>% select(name = qid, geo)
  )

  # Newman nominal assortativity
  geo_factor <- as.integer(factor(V(g)$geo))
  r <- assortativity_nominal(g, types = geo_factor, directed = TRUE)

  tibble(label = label, n_edges = nrow(df), n_nodes = vcount(g), r = r)
}

# Define edge type categories
edge_types <- edges_geo %>%
  mutate(edge_type = paste0(from_layer, "->", to_layer)) %>%
  mutate(edge_class = if_else(from_layer == "nominator" & to_layer == "nominee",
                               "nomination", "institutional"))

# Compute assortativity at country level for each grouping
results_r <- bind_rows(
  # Full network
  compute_assortativity(edge_types, "from_country", "to_country", "Full network (all edges)"),
  # Nomination only
  compute_assortativity(edge_types %>% filter(edge_class == "nomination"),
                        "from_country", "to_country", "Nomination edges only"),
  # Institutional only
  compute_assortativity(edge_types %>% filter(edge_class == "institutional"),
                        "from_country", "to_country", "Institutional edges only"),
  # Each specific edge type
  edge_types %>%
    group_split(edge_type) %>%
    map_dfr(~ compute_assortativity(.x, "from_country", "to_country",
                                     label = unique(.x$edge_type)))
)

message("Newman assortativity (r) by edge grouping — country level:")
results_r %>%
  mutate(r = round(r, 4)) %>%
  print(n = 20)

# Also compute at continent level
results_r_continent <- bind_rows(
  compute_assortativity(edge_types, "from_continent", "to_continent", "Full network (all edges)"),
  compute_assortativity(edge_types %>% filter(edge_class == "nomination"),
                        "from_continent", "to_continent", "Nomination edges only"),
  compute_assortativity(edge_types %>% filter(edge_class == "institutional"),
                        "from_continent", "to_continent", "Institutional edges only")
)

message("\nNewman assortativity (r) by edge grouping — continent level:")
results_r_continent %>%
  mutate(r = round(r, 4)) %>%
  print(n = 20)


# =============================================================================
# PART 2: LAYER-CONTRAST PERMUTATION TEST
# =============================================================================
# Test H0: H_nomination = H_institutional
# vs  H1: H_nomination > H_institutional
#
# Procedure:
#   1. Compute observed contrast: Delta = H_nom - H_inst
#   2. For each permutation: randomly reassign edge-type labels (nomination vs
#      institutional) to edges, recompute H for each class, compute Delta_perm
#   3. p-value = fraction of permuted Delta >= observed Delta
#
# This tests whether the DIFFERENCE between layers is larger than expected
# if edge types were interchangeable — a genuinely multilayer hypothesis.
# =============================================================================

message("\n=== PART 2: Layer-Contrast Permutation Test ===\n")

compute_H <- function(from_geo, to_geo) {
  # Compute homophily ratio H = O/E at country level
  valid <- !is.na(from_geo) & !is.na(to_geo)
  from_geo <- from_geo[valid]
  to_geo <- to_geo[valid]
  n <- length(from_geo)
  if (n == 0) return(NA_real_)

  # Observed same-country rate
  O <- mean(from_geo == to_geo)

  # Expected under independence of marginals
  from_tab <- table(from_geo) / n
  to_tab <- table(to_geo) / n
  shared <- intersect(names(from_tab), names(to_tab))
  E <- sum(from_tab[shared] * to_tab[shared])

  if (E == 0) return(NA_real_)
  O / E
}

# Compute observed H for each class
nom_edges <- edge_types %>% filter(edge_class == "nomination")
inst_edges <- edge_types %>% filter(edge_class == "institutional")

H_nom_obs <- compute_H(nom_edges$from_country, nom_edges$to_country)
H_inst_obs <- compute_H(inst_edges$from_country, inst_edges$to_country)
Delta_obs <- H_nom_obs - H_inst_obs

message(sprintf("Observed H_nomination  = %.4f", H_nom_obs))
message(sprintf("Observed H_institutional = %.4f", H_inst_obs))
message(sprintf("Observed Delta (H_nom - H_inst) = %.4f", Delta_obs))

# Permutation test: shuffle edge-class labels (parallelized)
message(sprintf("\nRunning %d permutations across %d workers...", N_PERM, n_cores))

# Prepare combined data for permutation
combined <- edge_types %>%
  filter(!is.na(from_country), !is.na(to_country)) %>%
  select(from_country, to_country, edge_class)

n_total <- nrow(combined)
n_nom <- sum(combined$edge_class == "nomination")

# Pre-extract vectors for speed
from_geo_all <- combined$from_country
to_geo_all <- combined$to_country

# Parallel permutation using furrr — chunked to minimize serialization overhead.
# Each worker receives one chunk of N_PERM/n_cores iterations and loops internally,
# so the large vectors (from_geo_all, to_geo_all) are serialized only once per worker.
chunk_ids <- split(seq_len(N_PERM), cut(seq_len(N_PERM), n_cores, labels = FALSE))

set.seed(GLOBAL_SEED)
Delta_perm <- unlist(future_map(chunk_ids, function(ids) {
  # Each worker runs its chunk of permutations in a tight loop
  deltas <- numeric(length(ids))
  for (j in seq_along(ids)) {
    perm_idx <- sample.int(n_total, n_nom)
    H_nom_p <- compute_H(from_geo_all[perm_idx], to_geo_all[perm_idx])
    H_inst_p <- compute_H(from_geo_all[-perm_idx], to_geo_all[-perm_idx])
    deltas[j] <- H_nom_p - H_inst_p
  }
  deltas
}, .options = furrr_options(seed = GLOBAL_SEED)))

# P-value (one-sided: is observed Delta larger than permuted?)
p_value <- (sum(Delta_perm >= Delta_obs) + 1) / (N_PERM + 1)

message(sprintf("\nLayer-contrast test results:"))
message(sprintf("  Observed Delta = %.4f", Delta_obs))
message(sprintf("  Permutation mean(Delta) = %.4f", mean(Delta_perm)))
message(sprintf("  Permutation SD(Delta) = %.4f", sd(Delta_perm)))
message(sprintf("  Permutation 95%% interval = [%.4f, %.4f]",
                quantile(Delta_perm, 0.025), quantile(Delta_perm, 0.975)))
message(sprintf("  p-value = %.6f", p_value))

# =============================================================================
# SAVE RESULTS
# =============================================================================

results_summary <- bind_rows(
  results_r %>% mutate(geo_scale = "country", quantity = "assortativity_r"),
  results_r_continent %>% mutate(geo_scale = "continent", quantity = "assortativity_r"),
  tibble(
    label = "Layer contrast (H_nom - H_inst)",
    n_edges = n_total,
    n_nodes = NA_integer_,
    r = Delta_obs,
    geo_scale = "country",
    quantity = "layer_contrast_delta"
  ),
  tibble(
    label = "Layer contrast p-value",
    n_edges = n_total,
    n_nodes = NA_integer_,
    r = p_value,
    geo_scale = "country",
    quantity = "layer_contrast_pvalue"
  )
)

write_csv(results_summary, "Data/results_multilayer_quantities.csv")
message("\nResults saved to Data/results_multilayer_quantities.csv")

message("\n=== SUMMARY ===")
message("Supra-adjacency assortativity decomposes the full network's geographic")
message("structure into layer contributions. The layer-contrast test provides a")
message("p-value for the claim that nomination homophily exceeds institutional homophily.")
message("Both are genuinely multilayer quantities — they characterize relationships")
message("between layers, not properties of any single layer.")

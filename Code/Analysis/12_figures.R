# =============================================================================
# File: 12_figures.R
# Title: Generation of Manuscript Figures for Nobel Prize Homophily Study
#
# Author: Chad M. Topaz
# Last Updated: February 2025
#
# Purpose and Goals:
#   This script generates all publication-quality figures for the manuscript
#   "Geographic Homophily in the Nobel Prize Selection Network" submitted to
#   Science. It reads pre-computed statistical results from 11_formal_analysis.R
#   (CSV files with permutation test outcomes, diversity metrics, equity measures)
#   and transforms them into ggplot2 visualizations designed for the Science
#   journal format (2-column layout, accessibility-optimized color palette,
#   minimal ink). All five figures are generated: Fig 1 (network schematic),
#   Fig 2 (edge-type comparison), Fig 3 (homophily heatmap), Fig 4 (temporal
#   paradox), and Fig S1 (nomination flow asymmetry, supplementary). Each figure
#   includes informative captions, axis labels, and annotations to guide reader
#   interpretation.
#
# Methodological Decisions and Rationale:
#   - ggplot2 provides publication-quality graphics with full customization.
#   - patchwork allows combining subplots (e.g., Figure 4 panels A and B) while
#     maintaining aligned axes and consistent theme across multi-panel figures.
#   - Color palette designed for colorblind accessibility: uses distinct hues
#     (green, purple, terracotta, steel blue, tan) rather than red-green contrasts.
#   - Homophily ratio (H = O/E) visualized prominently: H=1 dashed line shows
#     "no homophily" reference, making effect sizes immediately interpretable.
#   - Geographic scales (country, subregion, continent) emphasized through faceting
#     or subplot layout, as homophily strength scales with geographic granularity.
#   - Fig 1 uses programmatically-generated complete bipartite edges (expand.grid)
#     to ensure institutional edges appear as dense meshes, visually distinct from
#     sparse nomination edges (where homophily creates clustering).
#   - Fig 3 heatmap row/column ordering (by homophily ratio, by self-nomination
#     rate) highlights the strongest patterns without arbitrary alphabetic sorting.
#   - Fig 4 dual-axis plot (left: % Europe and effective continents; right:
#     homophily ratio) visually demonstrates the temporal paradox.
#
# Inputs (all from Data/ directory, outputs of 11_formal_analysis.R):
#   - results_edge_homophily.csv: All edge-type homophily tests
#   - results_prize_homophily.csv: Homophily by discipline/prize
#   - results_temporal_homophily.csv: Homophily by decade
#   - results_self_nomination_by_prize.csv: Within-subregion rates
#   - results_nomination_equity.csv: Net nomination flows
#   - Data/intermediate/nominations.csv: Raw nominations for diversity calculation (output from 11_formal_analysis.R)
#
# VALIDATION: Ensure all CSV files exist before running. If 11_formal_analysis.R
# has not been executed, this script will fail with descriptive file-not-found errors.
#
# Outputs (all in Manuscript/Figures/ as PDF):
#   Filenames match manuscript figure numbering:
#   - fig1_schematic.pdf:        Fig 1  — Two-panel network structure (A: general, B: 1958 Physics)
#   - fig2_edge_homophily.pdf:   Fig 2  — Horizontal bar chart by edge type
#   - fig3_heatmap.pdf:          Fig 3  — Two-panel heatmap (A: prize × subregion, B: marginal bars)
#   - fig4_temporal.pdf:         Fig 4  — Two-panel temporal (A: diversity trends, B: homophily)
#   - figS1_flow_asymmetry.pdf:  Fig S1 — Horizontal bar chart by subregion (supplementary)
#
# Dependencies:
#   - tidyverse: readr (CSV), dplyr (filtering/joining), tidyr (reshape), ggplot2
#   - patchwork: Multi-panel plot composition (wrap_elements, plot_layout, +)
#   - scales: percent() for axis labels, squish() for out-of-bounds colorscale handling
#
# Design Principles:
#   - Minimal ink: Remove unnecessary gridlines, legends, plot backgrounds
#   - Direct labeling: Annotate values directly on plots where space permits
#   - Accessibility: Colorblind-friendly palette; avoid red/green alone
#   - Consistency: All figures use shared color scheme for layers/edges
#   - Clarity: Captions and annotations explain methodology (e.g., "10000 permutations")
#
# =============================================================================

library(tidyverse)
library(patchwork)
library(scales)

# --- Path helper functions ---
fig_path  <- function(f) file.path("Manuscript", "Figures", f)
data_path <- function(f) file.path("Data", f)

# Create output directory for figure PDF files
dir.create(file.path("Manuscript", "Figures"), showWarnings = FALSE, recursive = TRUE)

# --- Shared color palette ---
# Designed for colorblind accessibility using distinct hues (not red/green alone).
# Colors used consistently across all figures to aid reader pattern recognition.
layer_colors <- c(
  "Governing\nbody"  = "#5a9e6f",   # Forest green (HSL: 135°, 31%, 50%)
  "Vetting\nbody"    = "#7b68ae",   # Medium purple (HSL: 270°, 25%, 56%)
  "Nominators"       = "#c75a3a",   # Terracotta (HSL: 15°, 65%, 50%)
  "Nominees"         = "#3a7cc7",   # Steel blue (HSL: 210°, 60%, 55%)
  "Laureates"        = "#c7953a"    # Tan (HSL: 35°, 60%, 55%)
)

# Highlight colors for key edge types
nom_color  <- "#c75a3a"   # Terracotta — nomination edges (where homophily is strongest)
inst_color <- "#3a7cc7"   # Steel blue — institutional edges (comparison, no homophily)

# NOTE ON EXECUTION ORDER: Figures are generated in the order 1, 3, 4, 2, S1
# (not 1, 2, 3, 4, S1) because Fig. 3 and Fig. 4 share data objects with
# earlier pipeline steps. Output filenames match paper numbering regardless of
# execution order. Variable names (p_fig2, p_fig3, p_fig4) reflect execution
# order, not paper figure numbers.

# =============================================================================
# FIGURE 1: Multilayer network schematic  [Paper Fig. 1]
# =============================================================================
# Two-panel schematic illustrating the structure of the Nobel Prize multilayer
# network. Left panel shows the general structure with idealized node counts
# and edge patterns. Right panel shows a specific 1958 Physics example using
# real individuals (e.g., Lars Onsager, Igor Tamm, Pavel Cherenkov).
#
# KEY DESIGN ELEMENTS:
#   1. Layers ordered vertically (top to bottom): Governing → Vetting → Nominators
#      → Nominees → Laureates. Represents information/decision flow.
#   2. Institutional edges (gov→vet, vet→nom, gov→laur, vet→laur) drawn as
#      COMPLETE BIPARTITE using expand.grid(). This creates dense mesh pattern
#      visually showing bureaucratic structure.
#   3. Nomination edges (nom→nom) drawn SELECTIVELY (not complete bipartite).
#      This visually shows that nominators are strategic/selective, not random.
#   4. Nomination edges colored in terracotta (nom_color), institutional edges
#      in pale grey to visually emphasize the nomination pathway.
#   5. Each node placed in a band with vertical jitter to avoid overlap while
#      maintaining layer identity.
#   6. Right panel uses REAL individuals from 1958 Physics: demonstrating that
#      both committees and nominators were Swedish/European, yet selected diverse
#      nominees (Bethe=US, Kastler=FR, Cherenkov=SU).
#
message("=== Figure 1: Schematic ===")

# Set up vertical positions for the 5 network layers
layer_names <- names(layer_colors)
layer_y <- c(5, 4, 3, 2, 1)  # y-coordinates for layers (top to bottom)
names(layer_y) <- layer_names

# --- Reproducibility: Set seed for figure generation ---
# This ensures node positions and any random graphical elements are reproducible.
# The same seed (42) is used in both scripts for consistency.
set.seed(42)  # Reproducible randomization of node positions within bands

# --- Left panel: Generic structure ---
# Create idealized nodes for the generic schematic. Positions chosen to show
# reasonable sample sizes and to avoid visual congestion while remaining realistic.
# Each node is jittered vertically within layer band to prevent overlap.
generic_nodes <- tribble(
  ~layer,              ~x,
  "Governing\nbody",   -0.3,
  "Governing\nbody",    0.2,
  "Governing\nbody",    0.7,
  "Vetting\nbody",      0.0,
  "Vetting\nbody",      0.5,
  "Nominators",        -0.4,
  "Nominators",         0.0,
  "Nominators",         0.4,
  "Nominators",         0.8,
  "Nominees",          -0.3,
  "Nominees",           0.2,
  "Nominees",           0.6,
  "Laureates",         -0.1,
  "Laureates",          0.4
) %>%
  mutate(y = layer_y[layer] + runif(n(), -0.08, 0.08))  # Small jitter for visual clarity

# --- Build edges programmatically ---
# Use expand.grid() to create COMPLETE BIPARTITE edges for institutional layers.
# This ensures every source-layer individual connects to every target-layer
# individual, showing the bureaucratic/structural nature of these selections.
# In contrast, nomination edges (nom→nominee) are selective/sparse.

gov_x  <- c(-0.3, 0.2, 0.7)       # x-positions of governing body nodes
vet_x  <- c(0.0, 0.5)              # x-positions of vetting body nodes
nom_x  <- c(-0.4, 0.0, 0.4, 0.8)  # x-positions of nominator nodes
nee_x  <- c(-0.3, 0.2, 0.6)       # x-positions of nominee nodes
laur_x <- c(-0.1, 0.4)             # x-positions of laureate nodes

# Gov -> Vet: COMPLETE bipartite (3 gov × 2 vet = 6 edges)
# Shows that all governing members participate in vetting body selection
gov_vet <- expand.grid(from_x = gov_x, to_x = vet_x) %>%
  mutate(from_layer = "Governing\nbody", to_layer = "Vetting\nbody")

# Vet -> Nom: COMPLETE bipartite (2 vet × 4 nom = 8 edges)
# Shows that all vetting members nominate to all nominators (Chem/Phys/Med pathways)
vet_nom <- expand.grid(from_x = vet_x, to_x = nom_x) %>%
  mutate(from_layer = "Vetting\nbody", to_layer = "Nominators")

# Nom -> Nominee: SELECTIVE (5 edges shown)
# Unlike institutional edges, nominators are selective: they choose specific
# nominees. This is where homophily operates (nominators preferentially select
# people from their own geographic regions).
nom_nee <- tribble(
  ~from_x, ~to_x,
  -0.4,    -0.3,   # Nominator 1 → Nominee 1
   0.0,     0.2,   # Nominator 2 → Nominee 2
   0.0,    -0.3,   # Nominator 2 → Nominee 1 (also nominates 1)
   0.4,     0.2,   # Nominator 3 → Nominee 2
   0.8,     0.6,   # Nominator 4 → Nominee 3
) %>%
  mutate(from_layer = "Nominators", to_layer = "Nominees")

# Gov -> Laur: COMPLETE bipartite to one laureate (3 gov × 1 laur = 3 edges)
# For non-Peace prizes, governing body votes on laureate (complete connection)
gov_laur <- expand.grid(from_x = gov_x, to_x = laur_x[1]) %>%
  mutate(from_layer = "Governing\nbody", to_layer = "Laureates")

# Vet -> Laur: COMPLETE bipartite to other laureate (2 vet × 1 laur = 2 edges)
# For Peace Prize, vetting body votes on laureate
vet_laur <- expand.grid(from_x = vet_x, to_x = laur_x[2]) %>%
  mutate(from_layer = "Vetting\nbody", to_layer = "Laureates")

# Combine all edge types and compute actual y-positions from node jitter
generic_edges <- bind_rows(gov_vet, vet_nom, nom_nee, gov_laur, vet_laur) %>%
  mutate(
    from_y = layer_y[from_layer],  # Approximate y from layer position
    to_y   = layer_y[to_layer],
    is_nom_edge = (from_layer == "Nominators" & to_layer == "Nominees")  # Flag nomination vs institutional
  )

# Adjust y-coordinates to match the jittered positions of actual nodes
# This ensures edges connect to nodes visually, not just approximately
for (i in seq_len(nrow(generic_edges))) {
  # Find source node and get its actual y-position
  fn <- generic_nodes %>%
    filter(layer == generic_edges$from_layer[i],
           abs(x - generic_edges$from_x[i]) < 0.01)  # Match by x-position
  if (nrow(fn) > 0) generic_edges$from_y[i] <- fn$y[1]

  # Find destination node and get its actual y-position
  tn <- generic_nodes %>%
    filter(layer == generic_edges$to_layer[i],
           abs(x - generic_edges$to_x[i]) < 0.01)
  if (nrow(tn) > 0) generic_edges$to_y[i] <- tn$y[1]
}

# Create band/backdrop for each layer showing geographic identity
band_data <- tibble(
  layer = layer_names, y = layer_y,
  ymin = layer_y - 0.22, ymax = layer_y + 0.22,
  xmin = -0.80, xmax = 1.0
)

# LEFT PANEL: Generic network structure
p_generic <- ggplot() +
  # Background bands for each layer (color-coded by layer)
  geom_rect(data = band_data,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = layer),
            alpha = 0.12, color = NA) +
  # Institutional edges — thin and pale (dense mesh is visual noise)
  # Drawn first so they appear behind nomination edges
  geom_segment(data = generic_edges %>% filter(!is_nom_edge),
               aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
               arrow = arrow(length = unit(0.04, "cm"), type = "closed"),
               color = "grey65", linewidth = 0.15, alpha = 0.3) +
  # Nomination edges — bold and colored in terracotta
  # Visually emphasizes selective homophilic structure
  geom_segment(data = generic_edges %>% filter(is_nom_edge),
               aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
               arrow = arrow(length = unit(0.09, "cm"), type = "closed"),
               color = nom_color, linewidth = 0.55, alpha = 0.85) +
  # Nodes drawn ON TOP of edges (z-ordering)
  geom_point(data = generic_nodes,
             aes(x = x, y = y, color = layer), size = 2.5, shape = 16) +
  # Layer labels on left margin
  geom_text(data = band_data,
            aes(x = xmin - 0.05, y = y, label = layer),
            hjust = 1, size = 2.4, lineheight = 0.85) +
  scale_fill_manual(values = layer_colors, guide = "none") +
  scale_color_manual(values = layer_colors, guide = "none") +
  coord_cartesian(xlim = c(-1.4, 1.05), ylim = c(0.4, 5.6), expand = FALSE) +
  labs(subtitle = "General structure") +
  theme_void(base_size = 10) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 9, face = "italic",
                                      margin = margin(b = 3)),
        plot.margin = margin(5, 5, 5, 5))

# --- Right panel: 1958 Physics example with REAL individuals ---
# GOVERNING BODY: Royal Swedish Academy members (all Swedish)
# VETTING BODY: Nobel Committee for Physics (all Swedish)
# NOMINATORS: International scientists nominating from Germany, France, US
# NOMINEES: Candidates from various countries (US, France, Soviet Union)
# LAUREATES: Winners Tamm, Frank, Cherenkov (all Soviet Union)
#
# This example illustrates the key phenomenon: despite committees and nominators
# being predominantly Swedish/European, they select diverse nominees. The Arnulf
# (FR) → Kastler (FR) edge shows same-country nomination (homophily), while
# Pollard (US) → Cherenkov (SU) shows cross-country nomination.

phys_nodes <- tribble(
  ~name,                  ~layer,              ~x,     ~label_nudge,  ~label_y_nudge,
  # Governing body — 3 real Academy members
  "L. Onsager",           "Governing\nbody",  -0.35,   0.08,          0.0,
  "M. Siegbahn",          "Governing\nbody",   0.15,   0.08,          0.0,
  "O. Klein",             "Governing\nbody",   0.65,   0.08,          0.0,
  # Vetting body — 3 real Committee members (all Swedish)
  "E. Hulthén",           "Vetting\nbody",    -0.25,   0.08,          0.0,
  "I. Waller",            "Vetting\nbody",     0.25,   0.08,          0.0,
  "A.E. Lindh",           "Vetting\nbody",     0.70,   0.08,          0.0,
  # Nominators — 4 real nominators (mix of countries)
  "F. Hund\n(DE)",        "Nominators",       -0.50,   0.08,          0.0,
  "A. Arnulf\n(FR)",      "Nominators",       -0.05,   0.08,          0.0,
  "E. Meyer\n(DE)",       "Nominators",        0.40,   0.08,          0.0,
  "E. Pollard\n(US)",     "Nominators",        0.85,   0.08,          0.0,
  # Nominees — 3 real nominees
  "H. Bethe\n(US)",       "Nominees",         -0.35,   0.08,          0.0,
  "A. Kastler\n(FR)",     "Nominees",          0.20,   0.08,          0.0,
  "P. Cherenkov\n(SU)",   "Nominees",          0.70,   0.08,          0.0,
  # Laureates — 3 real laureates (all Soviet Union)
  "I. Tamm",              "Laureates",        -0.25,   0.08,          0.0,
  "I. Frank",             "Laureates",         0.20,   0.08,          0.0,
  "P. Cherenkov",         "Laureates",         0.65,   0.08,          0.0,
) %>%
  mutate(
    y = layer_y[layer],
    node_size = 2.0,
    node_shape = 16
  )

# Build Physics edges programmatically using complete bipartite for institutional types
p_gov_x  <- c(-0.35, 0.15, 0.65)       # 3 Academy members
p_vet_x  <- c(-0.25, 0.25, 0.70)       # 3 Committee members
p_nom_x  <- c(-0.50, -0.05, 0.40, 0.85) # 4 Nominators
p_laur_x <- c(-0.25, 0.20, 0.65)       # 3 Laureates

# Gov -> Vet: COMPLETE bipartite (3 gov × 3 vet = 9 edges)
# Academy selects all committee members
p_gov_vet <- expand.grid(from_x = p_gov_x, to_x = p_vet_x) %>%
  mutate(from_y = 5, to_y = 4, is_nom_edge = FALSE)

# Vet -> Nom: COMPLETE bipartite (3 vet × 4 nom = 12 edges)
# Committee nominates all nominators
p_vet_nom <- expand.grid(from_x = p_vet_x, to_x = p_nom_x) %>%
  mutate(from_y = 4, to_y = 3, is_nom_edge = FALSE)

# Nom -> Nominee: SELECTIVE (4 nomination edges)
# Key point: Arnulf (FR) nominates Kastler (FR) — same-country nomination
# This is the homophily phenomenon measured by the permutation tests.
p_nom_nee <- tribble(
  ~from_x, ~to_x,
  -0.50,   -0.35,    # Hund (Germany) nominates Bethe (USA)
  -0.05,    0.20,    # Arnulf (France) nominates Kastler (France) ← HOMOPHILY
   0.40,   -0.35,    # Meyer (Germany) nominates Bethe (USA)
   0.85,    0.70,    # Pollard (USA) nominates Cherenkov (USSR)
) %>%
  mutate(from_y = 3, to_y = 2, is_nom_edge = TRUE)

# Gov -> Laur: COMPLETE bipartite (3 gov × 3 laur = 9 edges)
# Academy votes on all laureates
p_gov_laur <- expand.grid(from_x = p_gov_x, to_x = p_laur_x) %>%
  mutate(from_y = 5, to_y = 1, is_nom_edge = FALSE)

# Combine all edges for physics example
phys_edges <- bind_rows(p_gov_vet, p_vet_nom, p_nom_nee, p_gov_laur)

# Extend bands for right panel to accommodate longer node labels
band_data_r <- band_data %>% mutate(xmin = -0.75, xmax = 1.25)

# RIGHT PANEL: 1958 Physics specific example
p_physics <- ggplot() +
  # Background bands for each layer
  geom_rect(data = band_data_r,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = layer),
            alpha = 0.15, color = NA) +
  # Institutional edges — thin and pale
  geom_segment(data = phys_edges %>% filter(!is_nom_edge),
               aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
               arrow = arrow(length = unit(0.04, "cm"), type = "closed"),
               color = "grey55", linewidth = 0.15, alpha = 0.25) +
  # Nomination edges — bold terracotta
  geom_segment(data = phys_edges %>% filter(is_nom_edge),
               aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
               arrow = arrow(length = unit(0.09, "cm"), type = "closed"),
               color = nom_color, linewidth = 0.5, alpha = 0.8) +
  # Nodes
  geom_point(data = phys_nodes,
             aes(x = x, y = y, color = layer),
             size = phys_nodes$node_size, shape = phys_nodes$node_shape) +
  # Node labels (names and country codes)
  geom_text(data = phys_nodes,
            aes(x = x + label_nudge, y = y + label_y_nudge, label = name),
            hjust = 0, size = 1.7, lineheight = 0.8) +
  scale_fill_manual(values = layer_colors, guide = "none") +
  scale_color_manual(values = layer_colors, guide = "none") +
  coord_cartesian(xlim = c(-0.80, 1.55), ylim = c(0.4, 5.6), expand = FALSE) +
  labs(subtitle = "Physics, 1958 (partial)") +
  theme_void(base_size = 10) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 9, face = "italic",
                                      margin = margin(b = 3)),
        plot.margin = margin(5, 5, 5, 5))

# Combine left and right panels using patchwork
# widths=c(1, 1.1) gives slightly more space to right panel for name labels
# plot_annotation adds bold (A)/(B) panel tags matching the manuscript caption
p_fig1 <- (p_generic + p_physics + plot_layout(widths = c(1, 1.1))) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 12))

# Export as PDF with publication-quality dimensions (8" wide, 4" tall)
# Note: Figure filenames use manuscript numbering, not generation order.
# This ensures filenames in R code match references in main.tex (fig1, fig2, fig3, fig4).
ggsave(fig_path("fig1_schematic.pdf"), p_fig1, width = 8, height = 4, dpi = 300)
message("  -> fig1_schematic.pdf (Multilayer network schematic)")


# =============================================================================
# FIGURE 3: Self-nomination heatmap — Prize x Subregion  [Paper Fig. 3]
# =============================================================================
# Heatmap showing the rate at which each subregion nominates within itself,
# broken down by prize. Rows = subregions (ordered by average self-nomination rate).
# Columns = prizes (ordered by country-level homophily ratio). A value of 80%
# in the FR/Phys cell means French nominators in Physics nominated French nominees
# 80% of the time. The column marginal bars show country-level homophily ratio
# (H = O/E), summarizing the overall homophily for that prize.
#
message("\n=== Figure 3: Heatmap ===")

# Load per-(prize, subregion) self-nomination rates from results file
self_nom <- read_csv(data_path("results_self_nomination_by_prize.csv"),
                     show_col_types = FALSE)

# Load country-level homophily ratios by prize for column marginal bars
prize_hom <- read_csv(data_path("results_prize_homophily.csv"),
                      show_col_types = FALSE) %>%
  filter(geo_level == "country") %>%
  select(prize, homophily_ratio, ratio_ci_lo, ratio_ci_hi, p_value) %>%
  # Create abbreviated prize labels for heatmap headers (wrap long name)
  mutate(
    prize_label = case_when(
      prize == "Physiology/Medicine" ~ "Phys./Med.",
      TRUE ~ prize
    ),
    # Significance stars (Bonferroni-adjusted across 49 formal tests)
    p_adj = pmin(1, p_value * 49),
    sig_star = case_when(
      p_adj < 0.001 ~ "***",
      p_adj < 0.01  ~ "**",
      p_adj < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

# Order prizes by homophily ratio (left to right: low to high)
# This clustering puts "least homophilic" prizes on left, "most homophilic" on right
prize_hom <- prize_hom %>% arrange(homophily_ratio)
prize_order <- prize_hom$prize
prize_label_order <- prize_hom$prize_label

# Keep only subregions with sufficient data (>= 3 prizes with data)
# Avoids sparse rows that make heatmap hard to interpret
region_coverage <- self_nom %>%
  count(nominator_subregion) %>%
  filter(n >= 3)

self_nom_filtered <- self_nom %>%
  filter(nominator_subregion %in% region_coverage$nominator_subregion)

# Order subregions by average self-nomination rate (low at bottom, high at top)
# Weighted average accounts for different N across prizes
region_avg <- self_nom_filtered %>%
  group_by(nominator_subregion) %>%
  summarise(avg_self = weighted.mean(self_rate, n_out), .groups = "drop") %>%
  arrange(avg_self)  # Ascending: lowest self-nomination at bottom
region_order <- region_avg$nominator_subregion

# Prepare heatmap data with proper ordering
heatmap_data <- self_nom_filtered %>%
  mutate(
    # Abbreviate prize names for compact display
    prize_label = case_when(
      prize_clean == "Physiology/Medicine" ~ "Phys./Med.",
      TRUE ~ prize_clean
    ),
    # Convert to factors with specified order (already sorted by homophily ratio)
    prize_label = factor(prize_label, levels = prize_label_order),
    nominator_subregion = factor(nominator_subregion, levels = region_order)
  )

# Prepare marginal bar chart data (country-level homophily ratios)
col_marginal <- prize_hom %>%
  mutate(prize_label = factor(prize_label, levels = prize_label_order))

# Main heatmap: self-nomination rate by (subregion, prize)
p_heat <- ggplot(heatmap_data,
                 aes(x = prize_label, y = nominator_subregion, fill = self_rate)) +
  # Tile geom with white gridlines separating cells
  geom_tile(color = "white", linewidth = 0.8) +
  # Text labels showing percentage (white text on dark cells, dark text on light)
  geom_text(aes(label = sprintf("%.0f%%", 100 * self_rate)),
            size = 3.0,
            color = ifelse(heatmap_data$self_rate > 0.65, "white", "grey20")) +
  # Color scale: light grey (low) → teal (mid) → dark blue (high)
  # Designed for colorblind accessibility (replaces red-yellow-orange gradient)
  # Limits set to [0.2, 1.0] with squish oob handling to show full dynamic range
  scale_fill_gradient2(
    low = "#f7f7f7", mid = "#41b6c4", high = "#225ea8",
    midpoint = 0.55, limits = c(0.2, 1.0),
    labels = percent,
    name = "Same-subregion\nnomination rate",
    oob = squish  # Squish out-of-bounds values to color limits
  ) +
  scale_x_discrete(position = "top") +  # Prize labels at top of heatmap
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x.top = element_text(face = "bold", size = 9),
    axis.text.y = element_text(size = 9),
    panel.grid = element_blank(),  # Remove all gridlines (tiles provide structure)
    legend.position = "right",
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(0.4, "cm"),
    legend.title = element_text(size = 8),
    plot.margin = margin(5, 5, 0, 5)
  )

# Marginal bar chart: country-level homophily ratio (H = O/E) per prize
# Aligned with heatmap columns, showing the aggregate homophily for each prize
# Includes 95% null reference interval whiskers and significance stars
p_col_marginal <- ggplot(col_marginal, aes(x = prize_label, y = homophily_ratio)) +
  # Bar height shows homophily ratio (effect size)
  geom_col(fill = "#4d7ea8", width = 0.6) +  # Steel blue for visual consistency with Figure 1
  # 95% null reference interval whiskers from permutation distribution
  geom_errorbar(aes(ymin = ratio_ci_lo, ymax = ratio_ci_hi),
                width = 0.25, linewidth = 0.4, color = "grey30") +
  # Direct label with ratio and significance stars
  geom_text(aes(label = sprintf("%.1f×%s", homophily_ratio, sig_star)),
            vjust = -0.3, size = 2.8, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(x = NULL, y = "Country\nhomophily\nratio") +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_blank(),  # Reuse labels from heatmap
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 7.5, angle = 0, vjust = 0.5, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0, 5, 5, 5)
  )

# Stack heatmap and marginal bar vertically using patchwork
# Heights ratio (4:1.2) allocates more space to main heatmap
# plot_annotation adds bold (A)/(B) panel tags matching the manuscript caption
p_fig2 <- (p_heat / p_col_marginal) +
  plot_layout(heights = c(4, 1.2)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 12))

ggsave(fig_path("fig3_heatmap.pdf"), p_fig2, width = 7, height = 5.5, dpi = 300)
message("  -> fig3_heatmap.pdf (Within-subregion nomination rates by prize)")


# =============================================================================
# FIGURE 4: Temporal paradox (two-panel)  [Paper Fig. 4]
# =============================================================================
# The central paradox of the paper: as the Nobel Prize became more globally
# inclusive (% European nominees dropped from 75% to 50%), did geographic
# homophily in nominations decrease? Or did it persist, suggesting structural
# institutional bias rather than simple demographic effects?
#
# Panel A (left): Diversity metrics over time
#   - Solid line: % European nominees (proxy for geographic concentration)
#   - Dashed line: Effective number of continents (Shannon entropy-based diversity)
# Panel B (right): Homophily ratio over time
#   - Shows that despite globalization, homophily ratio remained high (~2.0-2.5)
#   - Shaded band: 95% CI from permutation null distribution
#
message("\n=== Figure 4: Temporal ===")

# Load raw nominations data to compute diversity metrics by decade
noms <- read_csv(file.path("Data", "intermediate", "nominations.csv"),
                 show_col_types = FALSE)

# Compute diversity of nominee pool by decade
diversity_decade <- noms %>%
  filter(!is.na(nominee_continent)) %>%
  mutate(decade = 10 * (year %/% 10)) %>%
  group_by(decade) %>%
  summarise(
    n_noms = n(),
    # Percentage of nominees from Europe (concentration measure)
    pct_europe = 100 * sum(nominee_continent == "Europe") / n(),
    # Effective number of continents (Hill number, from Shannon entropy)
    eff_continents = {
      ct <- table(nominee_continent)
      props <- as.numeric(ct) / sum(ct)
      exp(-sum(props * log(props)))  # exp(Shannon_entropy)
    },
    .groups = "drop"
  )

# Load permutation test results for homophily by decade
temporal <- read_csv(data_path("results_temporal_homophily.csv"),
                     show_col_types = FALSE)
fig3_cont <- temporal %>% filter(geo_level == "continent")  # Use continent-level for stability

# PANEL A: Diversity trends (% Europe and effective continents over time)
p3a <- ggplot(diversity_decade, aes(x = decade)) +
  # Primary axis: % European nominees (solid line, steel blue)
  geom_line(aes(y = pct_europe), color = inst_color, linewidth = 1) +
  geom_point(aes(y = pct_europe), color = inst_color, size = 2) +
  # Secondary axis: Effective continents (dashed line, terracotta)
  # Scaled by 25 to fit on same y-axis as % Europe
  geom_line(aes(y = eff_continents * 25), color = nom_color, linewidth = 1,
            linetype = "dashed") +
  geom_point(aes(y = eff_continents * 25), color = nom_color, size = 2, shape = 17) +
  # Dual-axis design: left = % Europe, right = effective continents (after dividing by 25)
  scale_y_continuous(
    name = "% European nominees",
    limits = c(0, 100),
    sec.axis = sec_axis(~ . / 25, name = "Effective no. continents")
  ) +
  scale_x_continuous(breaks = seq(1900, 1970, 10),
                     labels = paste0(seq(1900, 1970, 10), "s")) +
  labs(x = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    # Color-code axis labels to match their line colors
    axis.title.y.left = element_text(color = inst_color),
    axis.title.y.right = element_text(color = nom_color)
  )

# PANEL B: Homophily ratio over time with permutation null CI
p3b <- ggplot(fig3_cont, aes(x = decade)) +
  # Shaded band: 95% CI from permutation null distribution (ratio scale)
  # CI computed as [null_ci_lo / expected_rate, null_ci_hi / expected_rate]
  # Shows typical range of homophily under random pairing
  geom_ribbon(aes(ymin = null_ci_lo / expected_rate,
                  ymax = null_ci_hi / expected_rate),
              fill = "grey80", alpha = 0.5) +
  # Reference line: H=1 (no homophily)
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  # Observed homophily ratio (solid line with points)
  geom_line(aes(y = homophily_ratio), color = nom_color, linewidth = 1) +
  geom_point(aes(y = homophily_ratio), color = nom_color, size = 2.5) +
  scale_x_continuous(breaks = seq(1900, 1970, 10),
                     labels = paste0(seq(1900, 1970, 10), "s")) +
  labs(x = "Decade",
       y = "Homophily ratio\n(observed / expected same-continent rate)") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank())

# Combine panels A and B vertically with patchwork
p_fig3 <- (p3a / p3b) +
  plot_layout(heights = c(1, 1)) +
  # Add panel labels (A, B) using plot_annotation
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 12))

ggsave(fig_path("fig4_temporal.pdf"), p_fig3, width = 6.5, height = 7, dpi = 300)
message("  -> fig4_temporal.pdf (Globalization paradox - temporal trends)")


# =============================================================================
# FIGURE 2: Edge-type homophily bar chart  [Paper Fig. 2]
# =============================================================================
# Horizontal bar chart showing homophily ratios for all edge types at country level.
# Two colors: terracotta for nomination edges (where homophily is strong),
# steel blue for institutional edges (where homophily is absent).
# Bars sorted by homophily ratio (ascending) to show the dramatic difference.
#
message("\n=== Figure 2: Edge-type homophily ===")

edge_hom <- read_csv(data_path("results_edge_homophily.csv"),
                     show_col_types = FALSE) %>%
  filter(geo_level == "country") %>%  # Use country-level (finest granularity)
  # Remove duplicate: "nominator → nominee" with small n (QID subset)
  # Keep only the "nominator → nominee" with large n (full data)
  filter(!(edge_type == "nominator → nominee" & n_edges < 20000)) %>%
  mutate(
    # Create clean labels for display, adding data source notes
    edge_label = case_when(
      edge_type == "nominator → nominee" ~ "Nominator → Nominee\n(full data)",
      edge_type == "nominator → nominee (QID)" ~ "Nominator → Nominee\n(QID-matched)",
      edge_type == "governing_body → vetting_body" ~ "Governing → Vetting",
      edge_type == "governing_body → laureate" ~ "Governing → Laureate",
      edge_type == "vetting_body → nominator" ~ "Vetting → Nominator",
      edge_type == "vetting_body → laureate" ~ "Vetting → Laureate",
      TRUE ~ edge_type
    ),
    # Flag nomination vs institutional edges for color coding
    is_nomination = str_detect(edge_type, "nominator → nominee"),
    # Significance stars based on Bonferroni-adjusted p-values (49 formal tests)
    p_adj = pmin(1, p_value * 49),
    sig_star = case_when(
      p_adj < 0.001 ~ "***",
      p_adj < 0.01  ~ "**",
      p_adj < 0.05  ~ "*",
      TRUE ~ ""
    )
  ) %>%
  # Sort by homophily ratio for visual clarity (low to high)
  arrange(homophily_ratio) %>%
  mutate(
    # Manual y-positioning with gap between institutional and nomination bars
    # This visual separation emphasizes the difference in homophily
    y_pos = row_number(),
    y_pos = if_else(is_nomination, y_pos + 1.0, as.double(y_pos))
  )

# Create horizontal bar chart of homophily ratios with 95% null reference intervals
p_fig4 <- ggplot(edge_hom, aes(x = y_pos, y = homophily_ratio, fill = is_nomination)) +
  # Bars colored by edge type (nomination vs institutional)
  geom_col(width = 0.7) +
  # 95% null reference interval whiskers from permutation distribution
  geom_errorbar(aes(ymin = ratio_ci_lo, ymax = ratio_ci_hi),
                width = 0.3, linewidth = 0.5, color = "grey30") +
  # Reference line: H=1 shows "no homophily" baseline
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  # Direct labels showing homophily ratio and significance stars
  geom_text(aes(label = sprintf("%.2f%s", homophily_ratio, sig_star)),
            hjust = -0.1, size = 3.2, fontface = "bold") +
  # Flip coordinates to make horizontal bars (easier to read labels)
  coord_flip(ylim = c(0, max(edge_hom$homophily_ratio) * 1.25)) +
  # Set y-axis labels to edge types
  scale_x_continuous(breaks = edge_hom$y_pos, labels = edge_hom$edge_label) +
  # Color by edge type: terracotta for nomination (homophilic), blue for institutional (null)
  scale_fill_manual(values = c("TRUE" = nom_color, "FALSE" = inst_color),
                    guide = "none") +
  labs(x = NULL,
       y = "Homophily ratio (observed / expected same-country rate)",
       caption = "Whiskers show 95% null reference interval. ***p < 0.001, **p < 0.01, *p < 0.05 (Bonferroni-adjusted).") +
  # Annotation explaining the H=1 reference
  annotate("text", x = 0.5, y = 1.05, label = "No homophily",
           hjust = 0, size = 2.6, color = "grey40", fontface = "italic") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),  # Remove horizontal gridlines
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9),
    plot.margin = margin(10, 25, 10, 10),  # Extra right margin for labels
    plot.caption = element_text(size = 7, color = "grey40", hjust = 0)
  )

ggsave(fig_path("fig2_edge_homophily.pdf"), p_fig4, width = 7, height = 4.5, dpi = 300)
message("  -> fig2_edge_homophily.pdf (Homophily ratios by edge type)")


# =============================================================================
# FIGURE S1: Flow asymmetry — nomination equity by subregion  [Supplement Fig. S1]
# =============================================================================
# Horizontal bar chart showing the asymmetry in nomination flows between subregions.
# Return ratio = nominations received / nominations sent. Values:
#   - >1: "Net importer" — receives more nominations than it sends out
#     (e.g., Europe, North America benefit from international nominations)
#   - <1: "Net exporter" — sends more nominations than it receives
#     (e.g., Latin America, Africa send nominations but receive few in return)
# This is supplementary evidence of the homophily and geographic bias in the system.
#
message("\n=== Figure S1: Flow asymmetry ===")

# Load nomination equity (flow balance) by subregion
equity <- read_csv(data_path("results_nomination_equity.csv"),
                   show_col_types = FALSE) %>%
  mutate(
    # Reorder by return ratio (ascending) for visual clarity
    subregion = fct_reorder(subregion, return_ratio),
    # Classify as importer (ratio > 1) or exporter (ratio < 1)
    fill_color = if_else(return_ratio >= 1, "Net importer", "Net exporter")
  )

p_fig5 <- ggplot(equity, aes(x = subregion, y = return_ratio, fill = fill_color)) +
  # Bar height shows return ratio
  geom_col(width = 0.7) +
  # Reference line: ratio=1 shows "balanced" nomination flow
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  # Direct labels with ratio values; position depends on side of reference line
  geom_text(aes(label = sprintf("%.2f", return_ratio)),
            hjust = ifelse(equity$return_ratio >= 1, -0.1, 1.1),
            size = 2.8) +
  # Horizontal bars with flipped coordinates
  coord_flip(ylim = c(0, max(equity$return_ratio) * 1.15)) +
  # Color by flow direction: terracotta for importers, blue for exporters
  scale_fill_manual(values = c("Net importer" = nom_color, "Net exporter" = inst_color),
                    name = NULL) +
  labs(x = NULL,
       y = "Return ratio (nominations received / nominations sent)") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),  # Remove horizontal gridlines
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(size = 9),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave(fig_path("figS1_flow_asymmetry.pdf"), p_fig5, width = 7, height = 5, dpi = 300)
message("  -> figS1_flow_asymmetry.pdf (Supplementary Fig S1: nomination flow asymmetry)")


# =============================================================================
# COMPLETION MESSAGE
# =============================================================================
message("\n=== ALL FIGURES COMPLETE ===")
message(sprintf("Saved to: %s", normalizePath(file.path("Manuscript", "Figures"))))

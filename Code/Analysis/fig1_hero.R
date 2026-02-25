# =============================================================================
# fig1_hero.R
# Generates the combined "hero" Figure 1 for the manuscript:
#   Panel A: Flat schematic of the multilayer network (two sub-panels:
#            generic structure + 1958 Physics example)
#   Panel B: Edge-type homophily bar chart
#
# Dependencies: tidyverse, patchwork, scales
# Input:  Data/results_edge_homophily.csv
# Output: Manuscript/Figures/fig1_combined.pdf
# =============================================================================

library(tidyverse)
library(patchwork)
library(scales)

fig_path <- function(f) file.path("Manuscript", "Figures", f)


# =============================================================================
# PANEL A: Schematic of the multilayer network
# =============================================================================

# --- Color palette for layers (muted, professional) ---
layer_colors <- c(
  "Governing\nbody"  = "#5a9e6f",   # sage green
  "Vetting\nbody"    = "#7b68ae",   # muted purple
  "Nominators"       = "#c75a3a",   # terracotta
  "Nominees"         = "#3a7cc7",   # steel blue
  "Laureates"        = "#c7953a"    # muted gold
)

layer_names <- names(layer_colors)
layer_y <- c(5, 4, 3, 2, 1)  # top to bottom
names(layer_y) <- layer_names

# ---- Left sub-panel: Generic abstract schematic ----

set.seed(42)

# Nodes per layer (abstract) — spread wider for clarity
generic_nodes <- tribble(
  ~layer,              ~x,
  "Governing\nbody",   -0.4,
  "Governing\nbody",    0.0,
  "Governing\nbody",    0.4,
  "Governing\nbody",    0.7,
  "Vetting\nbody",     -0.2,
  "Vetting\nbody",      0.3,
  "Nominators",        -0.55,
  "Nominators",        -0.2,
  "Nominators",         0.15,
  "Nominators",         0.5,
  "Nominators",         0.8,
  "Nominees",          -0.4,
  "Nominees",          -0.05,
  "Nominees",           0.3,
  "Nominees",           0.65,
  "Laureates",         -0.1,
  "Laureates",          0.3
) %>%
  mutate(y = layer_y[layer] + runif(n(), -0.08, 0.08))

# Edges (generic, showing each edge type)
generic_edges <- tribble(
  ~from_layer,          ~from_x,  ~to_layer,          ~to_x,
  # Governing -> Vetting
  "Governing\nbody",    -0.4,     "Vetting\nbody",    -0.2,
  "Governing\nbody",     0.0,     "Vetting\nbody",     0.3,
  "Governing\nbody",     0.4,     "Vetting\nbody",     0.3,
  # Vetting -> Nominators
  "Vetting\nbody",      -0.2,     "Nominators",        -0.55,
  "Vetting\nbody",      -0.2,     "Nominators",        -0.2,
  "Vetting\nbody",       0.3,     "Nominators",         0.15,
  "Vetting\nbody",       0.3,     "Nominators",         0.5,
  "Vetting\nbody",       0.3,     "Nominators",         0.8,
  # Nominators -> Nominees  (THE KEY EDGE)
  "Nominators",         -0.55,    "Nominees",          -0.4,
  "Nominators",         -0.2,     "Nominees",          -0.05,
  "Nominators",          0.15,    "Nominees",          -0.05,
  "Nominators",          0.5,     "Nominees",           0.3,
  "Nominators",          0.8,     "Nominees",           0.65,
  "Nominators",         -0.2,     "Nominees",          -0.4,
  # Governing -> Laureate
  "Governing\nbody",     0.7,     "Laureates",          0.3,
  # Vetting -> Laureate
  "Vetting\nbody",       0.3,     "Laureates",         -0.1,
)

# Match edge endpoints to actual node y positions
generic_edges <- generic_edges %>%
  mutate(from_y = layer_y[from_layer], to_y = layer_y[to_layer])

for (i in seq_len(nrow(generic_edges))) {
  fn <- generic_nodes %>%
    filter(layer == generic_edges$from_layer[i],
           abs(x - generic_edges$from_x[i]) < 0.01)
  if (nrow(fn) > 0) generic_edges$from_y[i] <- fn$y[1]

  tn <- generic_nodes %>%
    filter(layer == generic_edges$to_layer[i],
           abs(x - generic_edges$to_x[i]) < 0.01)
  if (nrow(tn) > 0) generic_edges$to_y[i] <- tn$y[1]
}

# Flag nomination edges for highlighting
generic_edges <- generic_edges %>%
  mutate(is_nom_edge = (from_layer == "Nominators" & to_layer == "Nominees"))

# Layer band rectangles
band_data <- tibble(
  layer = layer_names,
  y = layer_y,
  ymin = layer_y - 0.22,
  ymax = layer_y + 0.22,
  xmin = -0.80,
  xmax = 1.0
)

p_generic <- ggplot() +
  # Layer bands
  geom_rect(data = band_data,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = layer),
            alpha = 0.15, color = NA) +
  # Institutional edges (grey, thin)
  geom_segment(data = generic_edges %>% filter(!is_nom_edge),
               aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
               arrow = arrow(length = unit(0.07, "cm"), type = "closed"),
               color = "grey55", linewidth = 0.3, alpha = 0.5) +
  # Nomination edges (highlighted — thicker, colored)
  geom_segment(data = generic_edges %>% filter(is_nom_edge),
               aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
               arrow = arrow(length = unit(0.09, "cm"), type = "closed"),
               color = "#c75a3a", linewidth = 0.5, alpha = 0.8) +
  # Nodes
  geom_point(data = generic_nodes,
             aes(x = x, y = y, color = layer),
             size = 2.2, shape = 16) +
  # Layer labels on left
  geom_text(data = band_data,
            aes(x = xmin - 0.05, y = y, label = layer),
            hjust = 1, size = 2.4, lineheight = 0.85) +
  scale_fill_manual(values = layer_colors, guide = "none") +
  scale_color_manual(values = layer_colors, guide = "none") +
  coord_cartesian(xlim = c(-1.4, 1.05), ylim = c(0.4, 5.6), expand = FALSE) +
  labs(subtitle = "General structure") +
  theme_void(base_size = 10) +
  theme(
    plot.subtitle = element_text(hjust = 0.5, size = 9, face = "italic",
                                  margin = margin(b = 3)),
    plot.margin = margin(5, 5, 5, 5)
  )


# ---- Right sub-panel: 1958 Physics example ----

# Place nodes with more horizontal spread to avoid label collisions
phys_nodes <- tribble(
  ~name,                               ~layer,              ~x,      ~label_hjust, ~label_x_nudge,
  "Royal Swedish\nAcad. of Sciences",  "Governing\nbody",   0.0,     0,             0.08,
  "Nobel Committee\nfor Physics",      "Vetting\nbody",     0.0,     0,             0.08,
  "A. Gullstrand",                     "Nominators",       -0.45,    0,             0.08,
  "H.S. Nyborg",                       "Nominators",        0.15,    0,             0.08,
  "L.B. Ekeberg",                      "Nominators",        0.65,    0,             0.08,
  "A. Gullstrand",                     "Nominees",         -0.40,    0,             0.08,
  "P. Pringsheim",                     "Nominees",          0.15,    0,             0.08,
  "M. Born",                           "Nominees",          0.70,    0,             0.08,
  "M. Born",                           "Laureates",         0.15,    0,             0.08
) %>%
  mutate(y = layer_y[layer])

# Make governing & vetting slightly larger (institutional nodes)
phys_nodes <- phys_nodes %>%
  mutate(
    node_size = case_when(
      layer %in% c("Governing\nbody", "Vetting\nbody") ~ 3.0,
      TRUE ~ 2.2
    ),
    node_shape = case_when(
      layer %in% c("Governing\nbody", "Vetting\nbody") ~ 15,  # square
      TRUE ~ 16  # circle
    )
  )

# Edges with explicit from/to coordinates to avoid ambiguity
phys_edge_coords <- tribble(
  ~from_x, ~from_y, ~to_x, ~to_y,  ~is_nom_edge,
  # Governing -> Vetting
   0.0,     5,       0.0,   4,      FALSE,
  # Vetting -> Nominators
   0.0,     4,      -0.45,  3,      FALSE,
   0.0,     4,       0.15,  3,      FALSE,
   0.0,     4,       0.65,  3,      FALSE,
  # Nominators -> Nominees (THE KEY EDGES — highlighted)
  -0.45,    3,      -0.40,  2,      TRUE,   # Gullstrand -> Gullstrand
  -0.45,    3,       0.15,  2,      TRUE,   # Gullstrand -> Pringsheim
   0.15,    3,       0.70,  2,      TRUE,   # Nyborg -> Born
   0.65,    3,       0.70,  2,      TRUE,   # Ekeberg -> Born
  # Governing -> Laureate
   0.0,     5,       0.15,  1,      FALSE,
)

# Band data for right panel (same layers, but wider for labels)
band_data_r <- tibble(
  layer = layer_names,
  y = layer_y,
  ymin = layer_y - 0.22,
  ymax = layer_y + 0.22,
  xmin = -0.70,
  xmax = 1.15
)

p_physics <- ggplot() +
  # Layer bands
  geom_rect(data = band_data_r,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = layer),
            alpha = 0.15, color = NA) +
  # Institutional edges
  geom_segment(data = phys_edge_coords %>% filter(!is_nom_edge),
               aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
               arrow = arrow(length = unit(0.07, "cm"), type = "closed"),
               color = "grey55", linewidth = 0.3, alpha = 0.5) +
  # Nomination edges (highlighted)
  geom_segment(data = phys_edge_coords %>% filter(is_nom_edge),
               aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
               arrow = arrow(length = unit(0.09, "cm"), type = "closed"),
               color = "#c75a3a", linewidth = 0.5, alpha = 0.8) +
  # Nodes
  geom_point(data = phys_nodes,
             aes(x = x, y = y, color = layer),
             size = phys_nodes$node_size,
             shape = phys_nodes$node_shape) +
  # Node labels (nudged to avoid collision)
  geom_text(data = phys_nodes,
            aes(x = x + label_x_nudge, y = y, label = name),
            hjust = 0, size = 1.9, lineheight = 0.85) +
  scale_fill_manual(values = layer_colors, guide = "none") +
  scale_color_manual(values = layer_colors, guide = "none") +
  coord_cartesian(xlim = c(-0.75, 1.5), ylim = c(0.4, 5.6), expand = FALSE) +
  labs(subtitle = "Physics, 1958 (partial)") +
  theme_void(base_size = 10) +
  theme(
    plot.subtitle = element_text(hjust = 0.5, size = 9, face = "italic",
                                  margin = margin(b = 3)),
    plot.margin = margin(5, 5, 5, 5)
  )

# Combine the two sub-panels side by side for Panel A
p_schematic <- p_generic + p_physics +
  plot_layout(widths = c(1, 1.1))


# =============================================================================
# PANEL B: Edge-type homophily bar chart
# =============================================================================

fig1_data <- read_csv("Data/results_edge_homophily.csv", show_col_types = FALSE) %>%
  filter(geo_level == "country") %>%
  mutate(
    edge_label = case_when(
      edge_type == "nominator → nominee" ~ "Nominator → Nominee\n(full nomination data)",
      edge_type == "nominator → nominee (QID)" ~ "Nominator → Nominee\n(QID-matched subset)",
      edge_type == "governing_body → vetting_body" ~ "Governing → Vetting",
      edge_type == "governing_body → laureate" ~ "Governing → Laureate",
      edge_type == "vetting_body → nominator" ~ "Vetting → Nominator",
      edge_type == "vetting_body → laureate" ~ "Vetting → Laureate",
      TRUE ~ edge_type
    ),
    is_nomination = str_detect(edge_type, "nominator → nominee")
  ) %>%
  arrange(homophily_ratio) %>%
  # Assign y-positions manually: institutional bars at bottom, then a gap,
  # then nomination bars at top. This prevents label collision.
  mutate(
    y_pos = row_number(),
    # Add extra gap above the institutional bars (push nomination bars up)
    y_pos = if_else(is_nomination, y_pos + 0.8, as.double(y_pos))
  )

p_bars <- ggplot(fig1_data, aes(x = y_pos, y = homophily_ratio, fill = is_nomination)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", homophily_ratio)),
            hjust = -0.1, size = 3.0, fontface = "bold") +
  coord_flip(ylim = c(0, max(fig1_data$homophily_ratio) * 1.25),
             clip = "off") +
  scale_x_continuous(
    breaks = fig1_data$y_pos,
    labels = fig1_data$edge_label
  ) +
  scale_fill_manual(values = c("TRUE" = "#c75a3a", "FALSE" = "#3a7cc7"),
                    guide = "none") +
  labs(
    x = NULL,
    y = "Homophily ratio (observed / expected same-country rate)"
  ) +
  annotate("text", x = 0.3, y = 1.05, label = "No homophily",
           hjust = 0, size = 2.5, color = "grey40", fontface = "italic") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 8.5),
    plot.margin = margin(10, 25, 10, 10)
  )


# =============================================================================
# COMBINE: Panel A (schematic) / Panel B (bars)
# =============================================================================

# Wrap the nested patchwork (Panel A) so it can receive a tag
p_A <- wrap_elements(full = p_schematic)
p_B <- p_bars

p_final <- (p_A / p_B) +
  plot_layout(heights = c(1, 1.1)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 14))

ggsave(fig_path("fig1_combined.pdf"), p_final, width = 8, height = 8.5)
message("  -> fig1_combined.pdf saved")

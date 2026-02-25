# =============================================================================
# 12_figures.R
# Manuscript figures for "Geographic Homophily in the Nobel Prize Selection
# Network." Reads pre-computed results from 11_formal_analysis.R.
#
# Produces:
#   fig1_schematic.pdf    — multilayer network schematic (A: general, B: 1958 Physics)
#   fig2_heatmap.pdf      — self-nomination heatmap: prize × subregion
#   fig3_temporal.pdf     — globalization paradox (two-panel)
#   fig4_edge_homophily.pdf — bar chart of homophily by edge type (for SM or main)
#   fig5_flow_asymmetry.pdf — nomination equity by subregion (for SM)
#
# Dependencies: tidyverse, patchwork, scales
# =============================================================================

library(tidyverse)
library(patchwork)
library(scales)

fig_path  <- function(f) file.path("Manuscript", "Figures", f)
data_path <- function(f) file.path("Data", f)

dir.create(file.path("Manuscript", "Figures"), showWarnings = FALSE, recursive = TRUE)

# --- Shared color palette ---
layer_colors <- c(
  "Governing\nbody"  = "#5a9e6f",
  "Vetting\nbody"    = "#7b68ae",
  "Nominators"       = "#c75a3a",
  "Nominees"         = "#3a7cc7",
  "Laureates"        = "#c7953a"
)

nom_color  <- "#c75a3a"   # terracotta — nomination edges
inst_color <- "#3a7cc7"   # steel blue — institutional edges


# =============================================================================
# FIGURE 1: Multilayer network schematic
# =============================================================================
message("=== Figure 1: Schematic ===")

layer_names <- names(layer_colors)
layer_y <- c(5, 4, 3, 2, 1)
names(layer_y) <- layer_names

set.seed(42)

# --- Left: Generic structure ---
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

generic_edges <- tribble(
  ~from_layer,          ~from_x,  ~to_layer,          ~to_x,
  "Governing\nbody",    -0.4,     "Vetting\nbody",    -0.2,
  "Governing\nbody",     0.0,     "Vetting\nbody",     0.3,
  "Governing\nbody",     0.4,     "Vetting\nbody",     0.3,
  "Vetting\nbody",      -0.2,     "Nominators",        -0.55,
  "Vetting\nbody",      -0.2,     "Nominators",        -0.2,
  "Vetting\nbody",       0.3,     "Nominators",         0.15,
  "Vetting\nbody",       0.3,     "Nominators",         0.5,
  "Vetting\nbody",       0.3,     "Nominators",         0.8,
  "Nominators",         -0.55,    "Nominees",          -0.4,
  "Nominators",         -0.2,     "Nominees",          -0.05,
  "Nominators",          0.15,    "Nominees",          -0.05,
  "Nominators",          0.5,     "Nominees",           0.3,
  "Nominators",          0.8,     "Nominees",           0.65,
  "Nominators",         -0.2,     "Nominees",          -0.4,
  "Governing\nbody",     0.7,     "Laureates",          0.3,
  "Vetting\nbody",       0.3,     "Laureates",         -0.1,
) %>%
  mutate(
    from_y = layer_y[from_layer],
    to_y   = layer_y[to_layer],
    is_nom_edge = (from_layer == "Nominators" & to_layer == "Nominees")
  )

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

band_data <- tibble(
  layer = layer_names, y = layer_y,
  ymin = layer_y - 0.22, ymax = layer_y + 0.22,
  xmin = -0.80, xmax = 1.0
)

p_generic <- ggplot() +
  geom_rect(data = band_data,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = layer),
            alpha = 0.12, color = NA) +
  # Institutional edges — slightly transparent, thinner
  geom_segment(data = generic_edges %>% filter(!is_nom_edge),
               aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
               arrow = arrow(length = unit(0.06, "cm"), type = "closed"),
               color = "grey65", linewidth = 0.25, alpha = 0.4) +
  # Nomination edges — bold and colored
  geom_segment(data = generic_edges %>% filter(is_nom_edge),
               aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
               arrow = arrow(length = unit(0.09, "cm"), type = "closed"),
               color = nom_color, linewidth = 0.55, alpha = 0.85) +
  # Nodes drawn ON TOP of edges
  geom_point(data = generic_nodes,
             aes(x = x, y = y, color = layer), size = 2.5, shape = 16) +
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

# --- Right: 1958 Physics example with REAL individuals ---
# Governing body: actual Royal Swedish Academy members (Sweden)
# Vetting body: actual Nobel Committee for Physics members (all Sweden)
# Nominators/Nominees: from 1958 nomination archive
# Laureates: Tamm, Frank, Cherenkov (all Soviet Union)

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

phys_edges <- tribble(
  ~from_x, ~from_y, ~to_x, ~to_y,  ~is_nom_edge,
  # Governing -> Vetting (select from Academy to Committee)
  -0.35,    5,      -0.25,  4,      FALSE,
   0.15,    5,       0.25,  4,      FALSE,
   0.65,    5,       0.70,  4,      FALSE,
  # Vetting -> Nominators (Committee invites nominators)
  -0.25,    4,      -0.50,  3,      FALSE,
  -0.25,    4,      -0.05,  3,      FALSE,
   0.25,    4,       0.40,  3,      FALSE,
   0.70,    4,       0.85,  3,      FALSE,
  # Nominators -> Nominees (THE KEY EDGES — highlighted)
  -0.50,    3,      -0.35,  2,      TRUE,   # Hund (DE) -> Bethe (US)
  -0.05,    3,       0.20,  2,      TRUE,   # Arnulf (FR) -> Kastler (FR)  <- same-country!
   0.40,    3,      -0.35,  2,      TRUE,   # Meyer (DE) -> Bethe (US)
   0.85,    3,       0.70,  2,      TRUE,   # Pollard (US) -> Cherenkov (SU)
  # Governing -> Laureate
   0.15,    5,       0.20,  1,      FALSE,
   0.65,    5,       0.65,  1,      FALSE,
)

band_data_r <- band_data %>% mutate(xmin = -0.75, xmax = 1.25)

p_physics <- ggplot() +
  geom_rect(data = band_data_r,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = layer),
            alpha = 0.15, color = NA) +
  geom_segment(data = phys_edges %>% filter(!is_nom_edge),
               aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
               arrow = arrow(length = unit(0.07, "cm"), type = "closed"),
               color = "grey55", linewidth = 0.3, alpha = 0.5) +
  geom_segment(data = phys_edges %>% filter(is_nom_edge),
               aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
               arrow = arrow(length = unit(0.09, "cm"), type = "closed"),
               color = nom_color, linewidth = 0.5, alpha = 0.8) +
  geom_point(data = phys_nodes,
             aes(x = x, y = y, color = layer),
             size = phys_nodes$node_size, shape = phys_nodes$node_shape) +
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

p_fig1 <- wrap_elements(full = (p_generic + p_physics + plot_layout(widths = c(1, 1.1))))

ggsave(fig_path("fig1_schematic.pdf"), p_fig1, width = 8, height = 4)
message("  -> fig1_schematic.pdf")


# =============================================================================
# FIGURE 2: Self-nomination heatmap — Prize x Subregion
# =============================================================================
message("\n=== Figure 2: Heatmap ===")

self_nom <- read_csv(data_path("results_self_nomination_by_prize.csv"),
                     show_col_types = FALSE)

# Prize homophily ratios for column marginals
prize_hom <- read_csv(data_path("results_prize_homophily.csv"),
                      show_col_types = FALSE) %>%
  filter(geo_level == "country") %>%
  select(prize, homophily_ratio) %>%
  # Abbreviate for display
  mutate(prize_label = case_when(
    prize == "Physiology/Medicine" ~ "Phys./Med.",
    TRUE ~ prize
  ))

# Order prizes by homophily ratio (low to high, left to right)
prize_hom <- prize_hom %>% arrange(homophily_ratio)
prize_order <- prize_hom$prize
prize_label_order <- prize_hom$prize_label

# Filter: keep only subregions with data in >= 3 prizes
region_coverage <- self_nom %>%
  count(nominator_subregion) %>%
  filter(n >= 3)

self_nom_filtered <- self_nom %>%
  filter(nominator_subregion %in% region_coverage$nominator_subregion)

# Order subregions by overall self-nomination rate (low at bottom, high at top)
region_avg <- self_nom_filtered %>%
  group_by(nominator_subregion) %>%
  summarise(avg_self = weighted.mean(self_rate, n_out), .groups = "drop") %>%
  arrange(avg_self)
region_order <- region_avg$nominator_subregion

# Map prize names to abbreviated labels
heatmap_data <- self_nom_filtered %>%
  mutate(
    prize_label = case_when(
      prize_clean == "Physiology/Medicine" ~ "Phys./Med.",
      TRUE ~ prize_clean
    ),
    prize_label = factor(prize_label, levels = prize_label_order),
    nominator_subregion = factor(nominator_subregion, levels = region_order)
  )

# Column marginals
col_marginal <- prize_hom %>%
  mutate(prize_label = factor(prize_label, levels = prize_label_order))

p_heat <- ggplot(heatmap_data,
                 aes(x = prize_label, y = nominator_subregion, fill = self_rate)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.0f%%", 100 * self_rate)),
            size = 3.0,
            color = ifelse(heatmap_data$self_rate > 0.65, "white", "grey20")) +
  scale_fill_gradient2(
    low = "#f7f7f7", mid = "#fdae61", high = "#d73027",
    midpoint = 0.55, limits = c(0.2, 1.0),
    labels = percent,
    name = "Same-subregion\nnomination rate",
    oob = squish
  ) +
  scale_x_discrete(position = "top") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x.top = element_text(face = "bold", size = 9),
    axis.text.y = element_text(size = 9),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(0.4, "cm"),
    legend.title = element_text(size = 8),
    plot.margin = margin(5, 5, 0, 5)
  )

# Marginal bar showing country-level homophily ratio per prize
p_col_marginal <- ggplot(col_marginal, aes(x = prize_label, y = homophily_ratio)) +
  geom_col(fill = "grey45", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f×", homophily_ratio)),
            vjust = -0.3, size = 2.8, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(x = NULL, y = "Country\nhomophily\nratio") +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 7.5, angle = 0, vjust = 0.5, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0, 5, 5, 5)
  )

# Combine: align axes using patchwork
p_fig2 <- (p_heat / p_col_marginal) +
  plot_layout(heights = c(4, 1.2))

ggsave(fig_path("fig2_heatmap.pdf"), p_fig2, width = 7, height = 5.5)
message("  -> fig2_heatmap.pdf")


# =============================================================================
# FIGURE 3: Temporal paradox (two-panel)
# =============================================================================
message("\n=== Figure 3: Temporal ===")

# Load intermediate nominations for diversity metrics
noms <- read_csv(file.path("Data", "intermediate", "nominations.csv"),
                 show_col_types = FALSE)

diversity_decade <- noms %>%
  filter(!is.na(nominee_continent)) %>%
  mutate(decade = 10 * (year %/% 10)) %>%
  group_by(decade) %>%
  summarise(
    n_noms = n(),
    pct_europe = 100 * sum(nominee_continent == "Europe") / n(),
    eff_continents = {
      ct <- table(nominee_continent)
      props <- as.numeric(ct) / sum(ct)
      exp(-sum(props * log(props)))
    },
    .groups = "drop"
  )

temporal <- read_csv(data_path("results_temporal_homophily.csv"),
                     show_col_types = FALSE)
fig3_cont <- temporal %>% filter(geo_level == "continent")

p3a <- ggplot(diversity_decade, aes(x = decade)) +
  geom_line(aes(y = pct_europe), color = inst_color, linewidth = 1) +
  geom_point(aes(y = pct_europe), color = inst_color, size = 2) +
  geom_line(aes(y = eff_continents * 25), color = nom_color, linewidth = 1,
            linetype = "dashed") +
  geom_point(aes(y = eff_continents * 25), color = nom_color, size = 2, shape = 17) +
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
    axis.title.y.left = element_text(color = inst_color),
    axis.title.y.right = element_text(color = nom_color)
  )

p3b <- ggplot(fig3_cont, aes(x = decade)) +
  geom_ribbon(aes(ymin = null_ci_lo / expected_rate,
                  ymax = null_ci_hi / expected_rate),
              fill = "grey80", alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_line(aes(y = homophily_ratio), color = nom_color, linewidth = 1) +
  geom_point(aes(y = homophily_ratio), color = nom_color, size = 2.5) +
  scale_x_continuous(breaks = seq(1900, 1970, 10),
                     labels = paste0(seq(1900, 1970, 10), "s")) +
  labs(x = "Decade",
       y = "Homophily ratio\n(observed / expected same-continent rate)") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank())

p_fig3 <- (p3a / p3b) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 12))

ggsave(fig_path("fig3_temporal.pdf"), p_fig3, width = 6.5, height = 7)
message("  -> fig3_temporal.pdf")


# =============================================================================
# FIGURE 4: Edge-type homophily bar chart (can be main or SM)
# =============================================================================
message("\n=== Figure 4: Edge-type homophily ===")

edge_hom <- read_csv(data_path("results_edge_homophily.csv"),
                     show_col_types = FALSE) %>%
  filter(geo_level == "country") %>%
  # Remove the unlabeled duplicate: "nominator → nominee" with n=8119
  # (this is the QID subset that also appears as "nominator → nominee (QID)")
  filter(!(edge_type == "nominator → nominee" & n_edges < 20000)) %>%
  mutate(
    edge_label = case_when(
      edge_type == "nominator → nominee" ~ "Nominator → Nominee\n(full data)",
      edge_type == "nominator → nominee (QID)" ~ "Nominator → Nominee\n(QID-matched)",
      edge_type == "governing_body → vetting_body" ~ "Governing → Vetting",
      edge_type == "governing_body → laureate" ~ "Governing → Laureate",
      edge_type == "vetting_body → nominator" ~ "Vetting → Nominator",
      edge_type == "vetting_body → laureate" ~ "Vetting → Laureate",
      TRUE ~ edge_type
    ),
    is_nomination = str_detect(edge_type, "nominator → nominee")
  ) %>%
  arrange(homophily_ratio) %>%
  mutate(
    # Manual y-positions with gap between institutional and nomination bars
    y_pos = row_number(),
    y_pos = if_else(is_nomination, y_pos + 1.0, as.double(y_pos))
  )

p_fig4 <- ggplot(edge_hom, aes(x = y_pos, y = homophily_ratio, fill = is_nomination)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", homophily_ratio)),
            hjust = -0.1, size = 3.2, fontface = "bold") +
  coord_flip(ylim = c(0, max(edge_hom$homophily_ratio) * 1.25)) +
  scale_x_continuous(breaks = edge_hom$y_pos, labels = edge_hom$edge_label) +
  scale_fill_manual(values = c("TRUE" = nom_color, "FALSE" = inst_color),
                    guide = "none") +
  labs(x = NULL,
       y = "Homophily ratio (observed / expected same-country rate)") +
  annotate("text", x = 0.5, y = 1.05, label = "No homophily",
           hjust = 0, size = 2.6, color = "grey40", fontface = "italic") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9),
    plot.margin = margin(10, 25, 10, 10)
  )

ggsave(fig_path("fig4_edge_homophily.pdf"), p_fig4, width = 7, height = 4.5)
message("  -> fig4_edge_homophily.pdf")


# =============================================================================
# FIGURE 5: Flow asymmetry — nomination equity (SM)
# =============================================================================
message("\n=== Figure 5: Flow asymmetry ===")

equity <- read_csv(data_path("results_nomination_equity.csv"),
                   show_col_types = FALSE) %>%
  mutate(
    subregion = fct_reorder(subregion, return_ratio),
    fill_color = if_else(return_ratio >= 1, "Net importer", "Net exporter")
  )

p_fig5 <- ggplot(equity, aes(x = subregion, y = return_ratio, fill = fill_color)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", return_ratio)),
            hjust = ifelse(equity$return_ratio >= 1, -0.1, 1.1),
            size = 2.8) +
  coord_flip(ylim = c(0, max(equity$return_ratio) * 1.15)) +
  scale_fill_manual(values = c("Net importer" = nom_color, "Net exporter" = inst_color),
                    name = NULL) +
  labs(x = NULL,
       y = "Return ratio (nominations received / nominations sent)") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(size = 9),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave(fig_path("fig5_flow_asymmetry.pdf"), p_fig5, width = 7, height = 5)
message("  -> fig5_flow_asymmetry.pdf")


# =============================================================================
message("\n=== ALL FIGURES COMPLETE ===")
message(sprintf("Saved to: %s", normalizePath(file.path("Manuscript", "Figures"))))

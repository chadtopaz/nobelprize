# =============================================================================
# 11_formal_analysis.R
# Formal statistical analysis of geographic homophily in the Nobel Prize
# multilayer network. Produces publication-quality figures (PDF) and
# LaTeX-ready tables for the manuscript.
#
# Three core findings:
#   1. Homophily is localized to the nomination edge (not institutional edges)
#   2. Homophily scales with discipline-specific knowledge locality
#   3. Relative homophily intensifies as the nominee pool globalizes
#
# Statistical approach:
#   - Permutation tests (N=1000 rewirings preserving marginals) for homophily
#   - 95% confidence intervals from permutation null distributions
#   - Effect size: homophily ratio = observed / expected
#
# Parallelization:
#   All ~80 permutation tests are independent. We collect them into a single
#   job list and dispatch via furrr::future_map with progressr progress bars.
#
# Outputs:
#   Figures/fig1_edge_type_homophily.pdf
#   Figures/fig2_prize_homophily.pdf
#   Figures/fig3_temporal_homophily.pdf
#   Figures/fig4_flow_asymmetry.pdf
#   Tables/tab1_layer_composition.tex
#   Tables/tab2_homophily_by_edge_type.tex
#   Tables/tab3_temporal_homophily.tex
# =============================================================================

library(tidyverse)
library(scales)
library(patchwork)
library(furrr)
library(progressr)

# --- Paths ---
data_path <- function(f) file.path("Data", f)
int_path  <- function(f) file.path("Data", "intermediate", f)
fig_path  <- function(f) file.path("Manuscript", "Figures", f)
tab_path  <- function(f) file.path("Manuscript", "Tables", f)

dir.create(file.path("Manuscript", "Figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("Manuscript", "Tables"), showWarnings = FALSE, recursive = TRUE)

N_PERM <- 1000  # permutation replicates

# --- Parallel setup ---
n_cores <- max(1, availableCores() - 1)
plan(multisession, workers = n_cores)
message(sprintf("Parallel backend: %d workers", n_cores))

# Enable progressr globally (auto-detects best handler: RStudio pane, shiny, or txt)
handlers(global = TRUE)

# =============================================================================
# LOAD DATA
# =============================================================================
message("Loading data...")
demo   <- read_csv(int_path("demographics.csv"), show_col_types = FALSE)
gov    <- read_csv(int_path("governing_bodies.csv"), show_col_types = FALSE)
vet    <- read_csv(int_path("vetting_bodies.csv"), show_col_types = FALSE)
noms   <- read_csv(int_path("nominations.csv"), show_col_types = FALSE)
nom_pq <- read_csv(int_path("nomination_people_qids.csv"), show_col_types = FALSE)
laur   <- read_csv(int_path("laureates.csv"), show_col_types = FALSE)
nodes  <- read_csv(data_path("nodes.csv"), show_col_types = FALSE)
edges  <- read_csv(data_path("edges.csv"), show_col_types = FALSE)

# =============================================================================
# HELPER: Permutation test for geographic homophily (single job)
# =============================================================================
# Pure function: takes vectors, returns a tibble or NULL.
# No side effects, safe for parallel execution.

homophily_permtest <- function(from_geo, to_geo, n_perm = N_PERM,
                                geo_level = "country", seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Remove pairs with missing geo
  keep <- !is.na(from_geo) & !is.na(to_geo)
  from_geo <- from_geo[keep]
  to_geo   <- to_geo[keep]
  n <- length(from_geo)

  if (n < 50) return(NULL)

  observed_same <- sum(from_geo == to_geo)
  obs_rate      <- observed_same / n

  # Null model: expected rate under independence of marginals
  from_dist <- table(from_geo) / n
  to_dist   <- table(to_geo) / n
  shared    <- intersect(names(from_dist), names(to_dist))
  expected_rate <- sum(from_dist[shared] * to_dist[shared])

  # Permutation null distribution
  perm_rates <- numeric(n_perm)
  for (i in seq_len(n_perm)) {
    perm_to <- sample(to_geo)
    perm_rates[i] <- sum(from_geo == perm_to) / n
  }

  # p-value: fraction of permutations >= observed
  p_value <- (sum(perm_rates >= obs_rate) + 1) / (n_perm + 1)

  # CI for null distribution
  null_ci_lo <- quantile(perm_rates, 0.025)
  null_ci_hi <- quantile(perm_rates, 0.975)

  tibble(
    geo_level      = geo_level,
    n_edges        = n,
    observed_rate  = obs_rate,
    expected_rate  = expected_rate,
    homophily_ratio = obs_rate / expected_rate,
    null_ci_lo     = as.numeric(null_ci_lo),
    null_ci_hi     = as.numeric(null_ci_hi),
    p_value        = p_value
  )
}


# =============================================================================
# BUILD JOB LIST: Collect ALL permutation tasks before dispatching
# =============================================================================
message("Building permutation job list...")

jobs <- list()  # each element: list(from_geo, to_geo, geo_level, n_perm, metadata)

# --- Helper to add a job ---
add_job <- function(from_geo, to_geo, geo_level, n_perm = N_PERM, ...) {
  meta <- list(...)
  jobs[[length(jobs) + 1]] <<- list(
    from_geo  = from_geo,
    to_geo    = to_geo,
    geo_level = geo_level,
    n_perm    = n_perm,
    meta      = meta
  )
}

# ---- FINDING 1: Edge-type decomposition ----

# 1a. Nominator -> Nominee (from nominations.csv, full dataset)
nom_geo <- noms %>%
  filter(!is.na(nominator_continent), !is.na(nominee_continent))

add_job(nom_geo$nominator_continent, nom_geo$nominee_continent,
        "continent", finding = "edge_type", edge_type = "nominator → nominee")
add_job(nom_geo$nominator_subregion, nom_geo$nominee_subregion,
        "subregion", finding = "edge_type", edge_type = "nominator → nominee")
add_job(nom_geo$nominator_country_modern, nom_geo$nominee_country_modern,
        "country", finding = "edge_type", edge_type = "nominator → nominee")

# 1b. Institutional edge types (QID-based)
edges_geo <- edges %>%
  filter(!str_starts(from_qid, "NOM:"), !str_starts(to_qid, "NOM:")) %>%
  inner_join(nodes %>% select(qid, birth_continent, birth_subregion, birth_country_modern),
             by = c("from_qid" = "qid")) %>%
  rename(from_continent = birth_continent, from_subregion = birth_subregion,
         from_country = birth_country_modern) %>%
  inner_join(nodes %>% select(qid, birth_continent, birth_subregion, birth_country_modern),
             by = c("to_qid" = "qid")) %>%
  rename(to_continent = birth_continent, to_subregion = birth_subregion,
         to_country = birth_country_modern) %>%
  mutate(edge_type = paste0(from_layer, " → ", to_layer))

for (et in unique(edges_geo$edge_type)) {
  e_sub <- edges_geo %>% filter(edge_type == et)
  if (nrow(e_sub) < 100) next
  add_job(e_sub$from_continent, e_sub$to_continent,
          "continent", finding = "edge_type", edge_type = et)
  add_job(e_sub$from_country, e_sub$to_country,
          "country", finding = "edge_type", edge_type = et)
}

# 1c. Nominator->nominee from edges.csv (QID-matched subset)
nom_qid_edges <- edges_geo %>% filter(edge_type == "nominator → nominee")
if (nrow(nom_qid_edges) >= 100) {
  add_job(nom_qid_edges$from_continent, nom_qid_edges$to_continent,
          "continent", finding = "edge_type", edge_type = "nominator → nominee (QID)")
  add_job(nom_qid_edges$from_country, nom_qid_edges$to_country,
          "country", finding = "edge_type", edge_type = "nominator → nominee (QID)")
}


# ---- FINDING 2: Prize-specific homophily ----

for (p in c("Chemistry", "Physics", "Physiology/Medicine", "Literature", "Peace")) {
  nom_p <- nom_geo %>%
    filter(prize == p | (p == "Physiology/Medicine" & prize == "Medicine"))

  add_job(nom_p$nominator_continent, nom_p$nominee_continent,
          "continent", finding = "prize", prize = p)
  add_job(nom_p$nominator_subregion, nom_p$nominee_subregion,
          "subregion", finding = "prize", prize = p)
  add_job(nom_p$nominator_country_modern, nom_p$nominee_country_modern,
          "country", finding = "prize", prize = p)
}


# ---- FINDING 3: Temporal trends ----

# 3a. Overall temporal homophily by decade
homophily_by_decade <- noms %>%
  filter(!is.na(nominator_continent), !is.na(nominee_continent)) %>%
  mutate(decade = 10 * (year %/% 10))

for (d in sort(unique(homophily_by_decade$decade))) {
  sub <- homophily_by_decade %>% filter(decade == d)
  add_job(sub$nominator_continent, sub$nominee_continent,
          "continent", finding = "temporal", decade = d)
  add_job(sub$nominator_country_modern, sub$nominee_country_modern,
          "country", finding = "temporal", decade = d)
}

# 3b. Temporal homophily by prize (for supplementary)
for (p in c("Chemistry", "Physics", "Physiology/Medicine", "Literature", "Peace")) {
  nom_p <- noms %>%
    mutate(prize_clean = if_else(prize == "Medicine", "Physiology/Medicine", prize)) %>%
    filter(prize_clean == p,
           !is.na(nominator_continent), !is.na(nominee_continent)) %>%
    mutate(decade = 10 * (year %/% 10))

  for (d in sort(unique(nom_p$decade))) {
    sub <- nom_p %>% filter(decade == d)
    add_job(sub$nominator_continent, sub$nominee_continent,
            "continent", n_perm = 500,
            finding = "prize_temporal", prize = p, decade = d)
  }
}

message(sprintf("  %d permutation jobs queued", length(jobs)))


# =============================================================================
# DISPATCH ALL JOBS IN PARALLEL via furrr::future_map + progressr
# =============================================================================
message(sprintf("Running %d permutation tests across %d workers...", length(jobs), n_cores))

# Assign reproducible seeds per job
base_seed <- 2025
job_seeds <- base_seed + seq_along(jobs)

run_results <- with_progress({
  p <- progressor(along = jobs)

  future_map2(jobs, job_seeds, function(job, seed) {
    p(sprintf("%s | %s", job$meta$finding, job$geo_level))
    res <- homophily_permtest(
      from_geo  = job$from_geo,
      to_geo    = job$to_geo,
      geo_level = job$geo_level,
      n_perm    = job$n_perm,
      seed      = seed
    )
    if (!is.null(res)) {
      # Attach metadata
      for (nm in names(job$meta)) {
        res[[nm]] <- job$meta[[nm]]
      }
    }
    res
  }, .options = furrr_options(seed = NULL))
})

# Combine results, dropping NULLs
all_results <- bind_rows(compact(run_results))
message(sprintf("  %d / %d jobs returned results", nrow(all_results), length(jobs)))


# =============================================================================
# SPLIT RESULTS BY FINDING
# =============================================================================

all_edge_results    <- all_results %>% filter(finding == "edge_type")
prize_results       <- all_results %>% filter(finding == "prize")
decade_perm_results <- all_results %>% filter(finding == "temporal")
prize_decade_results <- all_results %>% filter(finding == "prize_temporal")


# =============================================================================
# FIGURE 1: Edge-type decomposition of homophily
# =============================================================================
message("\n=== Generating Figure 1: Edge-type homophily ===")

fig1_data <- all_edge_results %>%
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
    is_nomination = str_detect(edge_type, "nominator → nominee"),
    edge_label = fct_reorder(edge_label, homophily_ratio)
  )

p1 <- ggplot(fig1_data, aes(x = edge_label, y = homophily_ratio, fill = is_nomination)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", homophily_ratio)),
            hjust = -0.15, size = 3.2) +
  coord_flip(ylim = c(0, max(fig1_data$homophily_ratio) * 1.15)) +
  scale_fill_manual(values = c("TRUE" = "#d62728", "FALSE" = "#1f77b4"),
                    guide = "none") +
  labs(
    x = NULL,
    y = "Homophily ratio (observed / expected same-country rate)",
    title = NULL
  ) +
  annotate("text", x = 0.6, y = 1.05, label = "No homophily",
           hjust = 0, size = 2.8, color = "grey40", fontface = "italic") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave(fig_path("fig1_edge_type_homophily.pdf"), p1, width = 7, height = 4.5)
message("  -> fig1_edge_type_homophily.pdf saved")


# =============================================================================
# COMBINED FIGURE 1: Schematic (A) + Edge-type homophily (B) — "hero figure"
# =============================================================================
message("  Generating combined Figure 1 (hero figure)...")

library(png)
library(grid)

# Load the schematic PNG
schematic_img <- readPNG(file.path("Manuscript", "Fig1.png"))

# Panel A: schematic as a ggplot-wrapped raster
p1a <- ggplot() +
  annotation_raster(schematic_img, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
  coord_fixed(ratio = nrow(schematic_img) / ncol(schematic_img),
              xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  labs(tag = "A") +
  theme_void() +
  theme(
    plot.tag = element_text(face = "bold", size = 14, hjust = 0, vjust = 1),
    plot.margin = margin(5, 10, 5, 5)
  )

# Panel B: bar chart with tag
p1b <- p1 +
  labs(tag = "B") +
  theme(
    plot.tag = element_text(face = "bold", size = 14, hjust = 0, vjust = 1)
  )

# Combine with patchwork: schematic on top, bar chart below
p1_combined <- p1a / p1b +
  plot_layout(heights = c(1, 1.3))

ggsave(fig_path("fig1_combined.pdf"), p1_combined, width = 7.5, height = 9)
message("  -> fig1_combined.pdf saved")


# =============================================================================
# TABLE 2: Full homophily results by edge type
# =============================================================================
message("  Generating Table 2...")

tab2 <- all_edge_results %>%
  mutate(
    edge_type_clean = case_when(
      edge_type == "nominator → nominee" ~ "Nominator $\\rightarrow$ Nominee",
      edge_type == "nominator → nominee (QID)" ~ "Nominator $\\rightarrow$ Nominee (QID subset)",
      edge_type == "governing_body → vetting_body" ~ "Governing $\\rightarrow$ Vetting",
      edge_type == "governing_body → laureate" ~ "Governing $\\rightarrow$ Laureate",
      edge_type == "vetting_body → nominator" ~ "Vetting $\\rightarrow$ Nominator",
      edge_type == "vetting_body → laureate" ~ "Vetting $\\rightarrow$ Laureate",
      TRUE ~ edge_type
    )
  ) %>%
  arrange(geo_level, desc(homophily_ratio)) %>%
  select(edge_type_clean, geo_level, n_edges, observed_rate, expected_rate,
         homophily_ratio, null_ci_lo, null_ci_hi, p_value)

tex_lines <- c(
  "\\begin{table}[ht]",
  "\\centering",
  "\\caption{Geographic homophily by edge type and geographic scale. Homophily ratio is observed same-geography rate divided by expected rate under random pairing. $p$-values from 1{,}000 permutations.}",
  "\\label{tab:homophily_edge_type}",
  "\\small",
  "\\begin{tabular}{llrrrrc}",
  "\\toprule",
  "Edge type & Scale & $N$ & Obs. & Exp. & Ratio & $p$ \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(tab2))) {
  r <- tab2[i, ]
  p_str <- if (r$p_value < 0.001) "$<$0.001" else sprintf("%.3f", r$p_value)
  tex_lines <- c(tex_lines, sprintf(
    "%s & %s & %s & %.1f\\%% & %.1f\\%% & %.2f & %s \\\\",
    r$edge_type_clean, r$geo_level,
    format(r$n_edges, big.mark = ","),
    100 * r$observed_rate, 100 * r$expected_rate,
    r$homophily_ratio, p_str
  ))
}

tex_lines <- c(tex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(tex_lines, tab_path("tab2_homophily_by_edge_type.tex"))
message("  -> tab2_homophily_by_edge_type.tex saved")


# =============================================================================
# FIGURE 2: Prize-specific homophily at three geographic scales
# =============================================================================
message("\n=== Generating Figure 2: Prize homophily ===")

fig2_data <- prize_results %>%
  mutate(
    geo_level = factor(geo_level, levels = c("continent", "subregion", "country")),
    prize = factor(prize, levels = c("Literature", "Peace",
                                     "Physiology/Medicine", "Chemistry", "Physics"))
  )

p2 <- ggplot(fig2_data, aes(x = prize, y = homophily_ratio, fill = geo_level)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  scale_fill_manual(
    values = c("continent" = "#aec7e8", "subregion" = "#1f77b4", "country" = "#08306b"),
    labels = c("Continent", "Subregion", "Country"),
    name = "Geographic scale"
  ) +
  labs(
    x = NULL,
    y = "Homophily ratio (observed / expected)",
    title = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.x = element_text(size = 9)
  )

ggsave(fig_path("fig2_prize_homophily.pdf"), p2, width = 7, height = 4.5)
message("  -> fig2_prize_homophily.pdf saved")


# =============================================================================
# FIGURE 3: Temporal homophily paradox (two-panel)
# =============================================================================
message("\n=== Generating Figure 3: Temporal paradox ===")

# Diversity metrics by decade (no permutation needed)
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
    eff_subregions = {
      st <- table(nominee_subregion)
      props <- as.numeric(st) / sum(st)
      exp(-sum(props * log(props)))
    },
    .groups = "drop"
  )

fig3_cont <- decade_perm_results %>%
  filter(geo_level == "continent")

fig3_div <- diversity_decade

p3a <- ggplot(fig3_div, aes(x = decade)) +
  geom_line(aes(y = pct_europe), color = "#1f77b4", linewidth = 1) +
  geom_point(aes(y = pct_europe), color = "#1f77b4", size = 2) +
  geom_line(aes(y = eff_continents * 25), color = "#d62728", linewidth = 1,
            linetype = "dashed") +
  geom_point(aes(y = eff_continents * 25), color = "#d62728", size = 2, shape = 17) +
  scale_y_continuous(
    name = "% European nominees",
    limits = c(0, 100),
    sec.axis = sec_axis(~ . / 25, name = "Effective no. continents")
  ) +
  scale_x_continuous(breaks = seq(1900, 1970, 10),
                     labels = paste0(seq(1900, 1970, 10), "s")) +
  labs(x = NULL, title = "A. Nominee pool diversification") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.y.left = element_text(color = "#1f77b4"),
    axis.title.y.right = element_text(color = "#d62728"),
    plot.title = element_text(face = "bold", size = 11)
  )

p3b <- ggplot(fig3_cont, aes(x = decade)) +
  geom_ribbon(aes(ymin = null_ci_lo / expected_rate,
                  ymax = null_ci_hi / expected_rate),
              fill = "grey80", alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_line(aes(y = homophily_ratio), color = "#d62728", linewidth = 1) +
  geom_point(aes(y = homophily_ratio), color = "#d62728", size = 2.5) +
  scale_x_continuous(breaks = seq(1900, 1970, 10),
                     labels = paste0(seq(1900, 1970, 10), "s")) +
  labs(
    x = "Decade",
    y = "Homophily ratio\n(observed / expected same-continent rate)",
    title = "B. Geographic homophily over time"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 11)
  )

p3 <- p3a / p3b + plot_layout(heights = c(1, 1))
ggsave(fig_path("fig3_temporal_homophily.pdf"), p3, width = 6.5, height = 7)
message("  -> fig3_temporal_homophily.pdf saved")


# =============================================================================
# TABLE 3: Temporal homophily with permutation stats
# =============================================================================
message("  Generating Table 3...")

tab3 <- decade_perm_results %>%
  filter(geo_level == "continent") %>%
  left_join(diversity_decade, by = "decade") %>%
  select(decade, n_edges, pct_europe, eff_continents,
         observed_rate, expected_rate, homophily_ratio, p_value) %>%
  arrange(decade)

tex3 <- c(
  "\\begin{table}[ht]",
  "\\centering",
  "\\caption{Temporal trends in nominee pool diversity and nomination homophily. Homophily ratio measures continent-level geographic matching relative to random expectation. $p$-values from 1{,}000 permutations.}",
  "\\label{tab:temporal_homophily}",
  "\\small",
  "\\begin{tabular}{lrrrrrrc}",
  "\\toprule",
  "Decade & $N$ & \\% Eur. & Eff. cont. & Obs. & Exp. & Ratio & $p$ \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(tab3))) {
  r <- tab3[i, ]
  p_str <- if (r$p_value < 0.001) "$<$0.001" else sprintf("%.3f", r$p_value)
  tex3 <- c(tex3, sprintf(
    "%ds & %s & %.1f & %.2f & %.1f\\%% & %.1f\\%% & %.2f & %s \\\\",
    r$decade, format(r$n_edges, big.mark = ","),
    r$pct_europe, r$eff_continents,
    100 * r$observed_rate, 100 * r$expected_rate,
    r$homophily_ratio, p_str
  ))
}

tex3 <- c(tex3, "\\bottomrule", "\\end{tabular}", "\\end{table}")
writeLines(tex3, tab_path("tab3_temporal_homophily.tex"))
message("  -> tab3_temporal_homophily.tex saved")


# =============================================================================
# FIGURE 4: Flow asymmetry (nomination equity)
# =============================================================================
message("\n=== Generating Figure 4: Flow asymmetry ===")

outgoing <- noms %>%
  filter(!is.na(nominator_subregion)) %>%
  count(nominator_subregion, name = "sent")

incoming <- noms %>%
  filter(!is.na(nominee_subregion)) %>%
  count(nominee_subregion, name = "received")

equity <- full_join(outgoing, incoming,
                    by = c("nominator_subregion" = "nominee_subregion")) %>%
  rename(subregion = nominator_subregion) %>%
  replace_na(list(sent = 0, received = 0)) %>%
  mutate(
    net = received - sent,
    return_ratio = received / pmax(sent, 1),
    total_volume = sent + received
  ) %>%
  filter(total_volume >= 100) %>%
  arrange(desc(return_ratio))

fig4_data <- equity %>%
  mutate(
    subregion = fct_reorder(subregion, return_ratio),
    fill_color = if_else(return_ratio >= 1, "Net importer", "Net exporter")
  )

p4 <- ggplot(fig4_data, aes(x = subregion, y = return_ratio, fill = fill_color)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", return_ratio)),
            hjust = ifelse(fig4_data$return_ratio >= 1, -0.1, 1.1),
            size = 2.8) +
  coord_flip(ylim = c(0, max(fig4_data$return_ratio) * 1.15)) +
  scale_fill_manual(
    values = c("Net importer" = "#d62728", "Net exporter" = "#1f77b4"),
    name = NULL
  ) +
  labs(
    x = NULL,
    y = "Return ratio (nominations received / nominations sent)",
    title = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.y = element_text(size = 9),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave(fig_path("fig4_flow_asymmetry.pdf"), p4, width = 7, height = 5)
message("  -> fig4_flow_asymmetry.pdf saved")


# =============================================================================
# TABLE 1: LAYER-BY-LAYER COMPOSITION SUMMARY
# =============================================================================
message("\n=== Generating Table 1: Layer composition ===")

layer_stats <- list()

# Governing
gov_demo <- gov %>% select(-name) %>% inner_join(demo, by = "qid")
g_known <- gov_demo %>% filter(!is.na(gender))
geo_known <- gov_demo %>% filter(!is.na(birth_continent))
ct <- table(geo_known$birth_continent)
props <- as.numeric(ct) / sum(ct)
layer_stats$governing <- tibble(
  layer = "Governing bodies",
  n = nrow(gov_demo),
  pct_female = 100 * sum(g_known$gender == "female") / nrow(g_known),
  pct_europe = 100 * sum(ct[names(ct) == "Europe"]) / sum(ct),
  eff_continents = exp(-sum(props * log(props))),
  eff_subregions = {
    st <- table(geo_known$birth_subregion)
    ps <- as.numeric(st) / sum(st)
    exp(-sum(ps * log(ps)))
  }
)

# Vetting
vet_demo <- vet %>% filter(!is.na(qid)) %>% select(-name) %>% inner_join(demo, by = "qid")
g_known <- vet_demo %>% filter(!is.na(gender))
geo_known <- vet_demo %>% filter(!is.na(birth_continent))
ct <- table(geo_known$birth_continent)
props <- as.numeric(ct) / sum(ct)
layer_stats$vetting <- tibble(
  layer = "Vetting bodies",
  n = nrow(vet_demo),
  pct_female = 100 * sum(g_known$gender == "female") / nrow(g_known),
  pct_europe = 100 * sum(ct[names(ct) == "Europe"]) / sum(ct),
  eff_continents = exp(-sum(props * log(props))),
  eff_subregions = {
    st <- table(geo_known$birth_subregion)
    ps <- as.numeric(st) / sum(st)
    exp(-sum(ps * log(ps)))
  }
)

# Nominators
nominator_unique <- noms %>%
  filter(!is.na(nominator_person_id)) %>%
  distinct(nominator_person_id, .keep_all = TRUE) %>%
  left_join(nom_pq %>% select(person_id, gender),
            by = c("nominator_person_id" = "person_id"))
g_known_nr <- nominator_unique %>% filter(!is.na(gender))
geo_known_nr <- nominator_unique %>% filter(!is.na(nominator_continent))
ct <- table(geo_known_nr$nominator_continent)
props <- as.numeric(ct) / sum(ct)
layer_stats$nominators <- tibble(
  layer = "Nominators",
  n = nrow(nominator_unique),
  pct_female = 100 * sum(g_known_nr$gender == "F") / nrow(g_known_nr),
  pct_europe = 100 * sum(ct[names(ct) == "Europe"]) / sum(ct),
  eff_continents = exp(-sum(props * log(props))),
  eff_subregions = {
    st <- table(geo_known_nr$nominator_subregion)
    ps <- as.numeric(st) / sum(st)
    exp(-sum(ps * log(ps)))
  }
)

# Nominees
nominee_unique <- noms %>%
  filter(!is.na(nominee_person_id)) %>%
  distinct(nominee_person_id, .keep_all = TRUE) %>%
  left_join(nom_pq %>% select(person_id, gender),
            by = c("nominee_person_id" = "person_id"))
g_known_ne <- nominee_unique %>% filter(!is.na(gender))
geo_known_ne <- nominee_unique %>% filter(!is.na(nominee_continent))
ct <- table(geo_known_ne$nominee_continent)
props <- as.numeric(ct) / sum(ct)
layer_stats$nominees <- tibble(
  layer = "Nominees",
  n = nrow(nominee_unique),
  pct_female = 100 * sum(g_known_ne$gender == "F") / nrow(g_known_ne),
  pct_europe = 100 * sum(ct[names(ct) == "Europe"]) / sum(ct),
  eff_continents = exp(-sum(props * log(props))),
  eff_subregions = {
    st <- table(geo_known_ne$nominee_subregion)
    ps <- as.numeric(st) / sum(st)
    exp(-sum(ps * log(ps)))
  }
)

# Laureates (nomination era)
laur_nom <- laur %>%
  filter(year <= 1975) %>%
  select(-gender) %>%
  inner_join(demo, by = "qid")
g_known_l <- laur_nom %>% filter(!is.na(gender))
geo_known_l <- laur_nom %>% filter(!is.na(birth_continent))
ct <- table(geo_known_l$birth_continent)
props <- as.numeric(ct) / sum(ct)
layer_stats$laureates <- tibble(
  layer = "Laureates (1901--1975)",
  n = nrow(laur_nom),
  pct_female = 100 * sum(g_known_l$gender == "female") / nrow(g_known_l),
  pct_europe = 100 * sum(ct[names(ct) == "Europe"]) / sum(ct),
  eff_continents = exp(-sum(props * log(props))),
  eff_subregions = {
    st <- table(geo_known_l$birth_subregion)
    ps <- as.numeric(st) / sum(st)
    exp(-sum(ps * log(ps)))
  }
)

tab1 <- bind_rows(layer_stats)

tex1 <- c(
  "\\begin{table}[ht]",
  "\\centering",
  "\\caption{Demographic composition of each layer in the Nobel Prize multilayer network. Effective continents and subregions are computed as the exponential of Shannon entropy.}",
  "\\label{tab:layer_composition}",
  "\\small",
  "\\begin{tabular}{lrrrrr}",
  "\\toprule",
  "Layer & $N$ & \\% Female & \\% Europe & Eff. cont. & Eff. sub. \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(tab1))) {
  r <- tab1[i, ]
  tex1 <- c(tex1, sprintf(
    "%s & %s & %.1f & %.1f & %.2f & %.2f \\\\",
    r$layer, format(r$n, big.mark = ","),
    r$pct_female, r$pct_europe,
    r$eff_continents, r$eff_subregions
  ))
}

tex1 <- c(tex1, "\\bottomrule", "\\end{tabular}", "\\end{table}")
writeLines(tex1, tab_path("tab1_layer_composition.tex"))
message("  -> tab1_layer_composition.tex saved")


# =============================================================================
# Self-nomination rates by prize (no permutation needed, for supplementary)
# =============================================================================
self_nom_prize <- noms %>%
  filter(!is.na(nominator_subregion), !is.na(nominee_subregion)) %>%
  mutate(prize_clean = if_else(prize == "Medicine", "Physiology/Medicine", prize)) %>%
  group_by(prize_clean, nominator_subregion) %>%
  summarise(
    n_out = n(),
    n_self = sum(nominator_subregion == nominee_subregion),
    self_rate = n_self / n_out,
    .groups = "drop"
  ) %>%
  filter(n_out >= 100) %>%
  arrange(prize_clean, desc(self_rate))


# =============================================================================
# SAVE ALL COMPUTED RESULTS AS CSV
# =============================================================================
message("\n=== Saving computed results as CSV ===")
write_csv(all_edge_results, data_path("results_edge_homophily.csv"))
write_csv(prize_results, data_path("results_prize_homophily.csv"))
write_csv(decade_perm_results, data_path("results_temporal_homophily.csv"))
write_csv(prize_decade_results, data_path("results_prize_temporal_homophily.csv"))
write_csv(equity, data_path("results_nomination_equity.csv"))
write_csv(self_nom_prize, data_path("results_self_nomination_by_prize.csv"))
write_csv(tab1, data_path("results_layer_composition.csv"))

# --- Shut down parallel workers ---
plan(sequential)

message("\n=== FORMAL ANALYSIS COMPLETE ===")
message(sprintf("Figures saved to: %s", normalizePath(file.path("Manuscript", "Figures"))))
message(sprintf("Tables saved to: %s", normalizePath(file.path("Manuscript", "Tables"))))
message(sprintf("CSV results saved to: %s", normalizePath("Data")))

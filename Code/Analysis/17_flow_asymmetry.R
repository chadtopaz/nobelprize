# =============================================================================
# File: 17_flow_asymmetry.R
# Title: Cross-Region Nomination Flow Asymmetry
#
# Author: Chad M. Topaz et al. (Nobel Prize project), QSS Revision 1
# Date: August 2026
#
# Purpose:
#   Produces the flow-asymmetry results reported in §5.7 and §S8 (Table 5,
#   Table S9): received-to-sent return ratios by UN subregion with
#   cluster-bootstrap confidence intervals, a direction test on cross-region
#   flows, the directed O - E flow matrix with within-block permutation bands,
#   the margin-adjusted Scandinavia<->Germany comparison, country vignettes
#   (Germany, Russia/USSR), and null-free degree descriptives.
#
# Methodological notes:
#   - Return ratios are MARGINAL quantities: permuting nominee labels within
#     prize-year blocks preserves each subregion's total received exactly, so a
#     pairing-permutation null has zero variance for ratios. Uncertainty is
#     instead quantified by a cluster bootstrap over nominators (a nominator's
#     records resample together; anonymous-nominator records are singleton
#     clusters).
#   - The direction test is computed on CROSS-REGION flows only. A binomial
#     test on total sent vs. received would be invalid because within-region
#     records appear in both totals; restricting to cross-region flows gives a
#     valid Bernoulli labeling test (H0: a cross-region record involving the
#     region is equally likely to be outbound or inbound).
#   - O - E flow-matrix cells ARE pairing quantities: these get within-block
#     (prize-year) permutation bands, which are valid there.
#   - The Scandinavia<->Germany comparison reports direction-specific expected
#     counts under random matching within the realized pools
#     (expected = N * p_sender-group * p_receiver-group) and the margin-implied
#     asymmetry ratio, so the raw count asymmetry can be read against the
#     margins (§5.7).
#
# EXPECTED BENCHMARKS (from the 2026-08 final run; analytic set as in
#   11_formal_analysis.R, N = 25,844):
#   - Return ratios: N.America 1.46, S.America 1.45, W.Europe 0.98,
#     N.Europe 0.765; cross-region direction test W.Europe p ~= 0.031.
#   - O - E matrix: 77 of 225 cross-region cells outside the permutation band.
#   - Scandinavia->Germany 383 obs vs ~569 expected (67%); Germany->Scandinavia
#     171 obs vs ~292 expected (59%); observed ratio 2.24 vs margin-implied 1.95.
#
# Inputs:  Data/intermediate/nominations.csv
# Outputs: Data/results_flow_return_ratios_{overall,by_period,by_prize}.csv,
#          Data/results_flow_matrix_overall.csv (with permutation bands),
#          Data/results_flow_matrix_{<period>}.csv,
#          Data/results_vignettes_germany_ussr.csv, Data/results_r18_scan_german.csv,
#          Data/results_degree_descriptives.csv, Data/fig_vignettes_draft.pdf
# Run from the repository root (NobelPrize.Rproj).
# Runtime at final constants (B_BOOT = 10000, B_PERM_MATRIX = 2000): ~30-90 min.
# =============================================================================

library(tidyverse)

int_path  <- function(f) file.path("Data", "intermediate", f)
data_path <- function(f) file.path("Data", f)
set.seed(1901)

B_BOOT        <- 10000  # bootstrap replicates for return-ratio CIs
B_PERM_MATRIX <- 2000   # permutation replicates for O-E matrix bands
MIN_VOLUME    <- 100    # headline-table threshold (matches pipeline convention)

message("Loading nominations...")
noms <- read_csv(int_path("nominations.csv"), show_col_types = FALSE)
d <- noms %>%
  filter(!is.na(nominator_continent), !is.na(nominee_continent)) %>%
  transmute(prize, year,
            cluster_id = as.character(nominator_person_id),   # NA -> singleton below
            s_ctry = nominator_country_modern, r_ctry = nominee_country_modern,
            s_sub  = nominator_subregion,      r_sub  = nominee_subregion,
            period = cut(year, breaks = c(1900, 1918, 1945, 1976),
                         labels = c("1901-1918", "1919-1945", "1946-1975")))
message("Analytic records: ", nrow(d), " (expect 25,844)")

# ── Return ratios: points + cluster-bootstrap CIs + cross-flow direction test ─
return_ratios <- function(s_sub, r_sub) {
  full_join(as_tibble(table(subregion = s_sub)) %>% rename(sent = n),
            as_tibble(table(subregion = r_sub)) %>% rename(received = n),
            by = "subregion") %>%
    mutate(across(c(sent, received), ~replace_na(., 0L)),
           ratio = received / pmax(sent, 1))
}

cross_flows <- function(s_sub, r_sub) {
  # Cross-region flows only: outbound = region sends to a different region;
  # inbound = region receives from a different region. Valid Bernoulli test.
  ok <- !is.na(s_sub) & !is.na(r_sub) & s_sub != r_sub
  tibble(subregion = union(unique(s_sub[!is.na(s_sub)]),
                           unique(r_sub[!is.na(r_sub)]))) %>%
    rowwise() %>%
    mutate(cross_sent = sum(ok & s_sub == subregion),
           cross_received = sum(ok & r_sub == subregion),
           p_dir_cross = ifelse(cross_sent + cross_received > 0,
                                binom.test(cross_received,
                                           cross_sent + cross_received,
                                           0.5)$p.value, NA_real_)) %>%
    ungroup()
}

boot_ratios <- function(dd, B = B_BOOT) {
  obs <- return_ratios(dd$s_sub, dd$r_sub)
  cl  <- ifelse(is.na(dd$cluster_id),
                paste0("anon_row_", seq_len(nrow(dd))), dd$cluster_id)
  idx_by_cl <- split(seq_len(nrow(dd)), cl)
  n_cl <- length(idx_by_cl)
  sims <- map_dfr(seq_len(B), function(b) {
    ix <- unlist(idx_by_cl[sample.int(n_cl, n_cl, replace = TRUE)],
                 use.names = FALSE)
    return_ratios(dd$s_sub[ix], dd$r_sub[ix]) %>% mutate(.b = b)
  })
  band <- sims %>% group_by(subregion) %>%
    summarise(boot_lo = quantile(ratio, 0.025), boot_hi = quantile(ratio, 0.975),
              .groups = "drop")
  obs %>%
    left_join(band, by = "subregion") %>%
    left_join(cross_flows(dd$s_sub, dd$r_sub), by = "subregion") %>%
    mutate(total_volume  = sent + received,
           low_volume    = total_volume < MIN_VOLUME,
           ci_excludes_1 = (boot_lo > 1) | (boot_hi < 1)) %>%
    arrange(desc(ratio))
}

message("Return ratios: overall (B_boot = ", B_BOOT, ")...")
rr_overall <- boot_ratios(d)
print(rr_overall %>% filter(!low_volume) %>%
        select(subregion, sent, received, ratio, boot_lo, boot_hi,
               ci_excludes_1, cross_sent, cross_received, p_dir_cross))
write_csv(rr_overall, data_path("results_flow_return_ratios_overall.csv"))

message("Return ratios: by period...")
rr_period <- d %>% group_split(period) %>%
  map_dfr(~ boot_ratios(.x) %>%
            mutate(period = as.character(.x$period[1]), .before = 1))
write_csv(rr_period, data_path("results_flow_return_ratios_by_period.csv"))

message("Return ratios: by prize...")
rr_prize <- d %>% group_split(prize) %>%
  map_dfr(~ boot_ratios(.x) %>% mutate(prize = .x$prize[1], .before = 1))
write_csv(rr_prize, data_path("results_flow_return_ratios_by_prize.csv"))

# ── Directed O - E flow matrices, with valid permutation bands (overall) ────
flow_matrix <- function(dd) {
  n <- nrow(dd)
  obs <- dd %>% count(from = s_sub, to = r_sub, name = "obs")
  fs  <- dd %>% count(from = s_sub) %>% mutate(p_from = n / sum(n)) %>% select(from, p_from)
  ts  <- dd %>% count(to   = r_sub) %>% mutate(p_to   = n / sum(n)) %>% select(to, p_to)
  crossing(fs, ts) %>%
    mutate(expected = n * p_from * p_to) %>%
    left_join(obs, by = c("from", "to")) %>%
    mutate(obs = replace_na(obs, 0L),
           o_minus_e = obs - expected)
}

message("O - E flow matrix with within-block permutation bands (B = ",
        B_PERM_MATRIX, ")...")
fm_obs <- flow_matrix(d)
blocks <- split(seq_len(nrow(d)), paste(d$prize, d$year))
r_orig <- d$r_sub
fm_sims <- map_dfr(seq_len(B_PERM_MATRIX), function(b) {
  r_perm <- r_orig
  for (ix in blocks) r_perm[ix] <- sample(r_perm[ix])
  dd2 <- d
  dd2$r_sub <- r_perm
  flow_matrix(dd2) %>% select(from, to, o_minus_e) %>% mutate(.b = b)
})
fm_band <- fm_sims %>% group_by(from, to) %>%
  summarise(perm_lo = quantile(o_minus_e, 0.025),
            perm_hi = quantile(o_minus_e, 0.975), .groups = "drop")
fm <- fm_obs %>%
  left_join(fm_band, by = c("from", "to")) %>%
  mutate(outside_null = o_minus_e < perm_lo | o_minus_e > perm_hi) %>%
  arrange(desc(abs(o_minus_e)))
write_csv(fm, data_path("results_flow_matrix_overall.csv"))
message("Cross-region cells outside the permutation band: ",
        sum(fm$outside_null & fm$from != fm$to, na.rm = TRUE), " of ",
        sum(fm$from != fm$to))

# Per-period matrices (point estimates)
d %>% group_split(period) %>%
  walk(~ write_csv(flow_matrix(.x),
        data_path(paste0("results_flow_matrix_", .x$period[1], ".csv"))))

# ── Scandinavia <-> Germany directionality, margin-adjusted (§5.7) ─────────
# Raw reciprocal counts plus direction-specific expected counts under random
# matching within the realized pools (expected = N * p_sender * p_receiver
# computed from the analytic set's margins), and the margin-implied asymmetry.
scan <- c("Sweden", "Norway", "Denmark")
n_tot     <- nrow(d)
s_scan_n  <- sum(d$s_ctry %in% scan, na.rm = TRUE)
r_scan_n  <- sum(d$r_ctry %in% scan, na.rm = TRUE)
s_de_n    <- sum(d$s_ctry == "Germany", na.rm = TRUE)
r_de_n    <- sum(d$r_ctry == "Germany", na.rm = TRUE)
r18 <- tibble(
  scan_to_germany     = sum(d$s_ctry %in% scan & d$r_ctry == "Germany", na.rm = TRUE),
  exp_scan_to_germany = s_scan_n * r_de_n / n_tot,
  germany_to_scan     = sum(d$s_ctry == "Germany" & d$r_ctry %in% scan, na.rm = TRUE),
  exp_germany_to_scan = s_de_n * r_scan_n / n_tot) %>%
  mutate(frac_of_exp_scan_to_de = scan_to_germany / exp_scan_to_germany,
         frac_of_exp_de_to_scan = germany_to_scan / exp_germany_to_scan,
         ratio_observed         = scan_to_germany / germany_to_scan,
         ratio_margin_implied   = exp_scan_to_germany / exp_germany_to_scan)
# Benchmarks: 383 vs ~569 (0.67); 171 vs ~292 (0.59); 2.24 vs 1.95.
print(r18)
write_csv(r18, data_path("results_r18_scan_german.csv"))

# ── Country vignettes: Germany and Russia/USSR ─────────────────────────────
vignette <- function(ctry) {
  d %>% group_by(year) %>%
    summarise(sent     = sum(s_ctry == ctry, na.rm = TRUE),
              received = sum(r_ctry == ctry, na.rm = TRUE), .groups = "drop") %>%
    mutate(country = ctry)
}
vg <- bind_rows(vignette("Germany"), vignette("Russia"))
write_csv(vg, data_path("results_vignettes_germany_ussr.csv"))

ussr_1956 <- vg %>%
  filter(country == "Russia", year >= 1946, year <= 1965) %>%
  mutate(era = if_else(year < 1956, "1946-1955", "1956-1965")) %>%
  group_by(era) %>% summarise(sent = sum(sent), received = sum(received), .groups = "drop")
message("USSR pre/post-1956:"); print(ussr_1956)

p <- ggplot(vg, aes(year)) +
  geom_line(aes(y = sent,     linetype = "sent")) +
  geom_line(aes(y = received, linetype = "received")) +
  geom_vline(data = tibble(country = c("Germany", "Russia"), x = c(1919, 1956)),
             aes(xintercept = x), color = "grey60", linetype = "dotted") +
  facet_wrap(~country, scales = "free_y") +
  labs(y = "nominations", linetype = NULL,
       title = "Nomination flows over time: Germany and Russia/USSR",
       subtitle = "Dotted: 1919 (post-WW1 boycott era begins), 1956 (Khrushchev thaw)") +
  theme_minimal()
ggsave(data_path("fig_vignettes_draft.pdf"), p, width = 9, height = 4)

# ── Null-free degree descriptives by country ───────────────────────────────
deg <- full_join(
  d %>% count(country = s_ctry, name = "sent"),
  d %>% count(country = r_ctry, name = "received"),
  by = "country") %>%
  mutate(across(c(sent, received), ~replace_na(., 0L))) %>%
  filter(!is.na(country)) %>%
  arrange(desc(received))
write_csv(deg, data_path("results_degree_descriptives.csv"))

message("Done.")
# =============================================================================

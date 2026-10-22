# The Geography of Nobel Prize Nomination, 1901–1975

Replication package for:

> Topaz, C. M., Kurongonayini, A., Ptak, B., Fluehr, A., Mendible, A., Roca, R., Rodríguez, N., Xian, L., Schwartz, R., & Higdon, J. (in press). The geography of Nobel Prize nomination, 1901–1975. *Quantitative Science Studies*.

## Overview

This repository provides the data and code used in the manuscript. We construct a multilayer network of the Nobel Prize selection process across Physics, Chemistry, Physiology or Medicine, Literature, and Peace, and quantify geographic homophily at the nomination stage of the selection pipeline. The analytic window is 1901–1975 for nominations, with Physiology/Medicine truncated at 1953 because its governing body has not released later records. Institutional-layer rosters extend further where the historical record permits.

The pipeline gathers data from the Nobel Prize Nomination Archive, Wikidata, Swedish Wikipedia, and the digitized Swedish state calendar; resolves individuals to Wikidata QIDs; constructs a node–edge representation of the multilayer network; and runs the statistical analyses reported in the paper: permutation-based homophily ratio estimation (pooled and blockwise within prize-years), Newman assortativity, layer-contrast tests, decade-by-decade temporal trend tests, fixed-margin adverse stress tests, a gravity-style Poisson (PPML) model of dyadic nomination flows with sender- and receiver-country×prize×decade fixed effects, an edge-level conditional logit with a nominee-prominence control, representativeness diagnostics and an inverse-probability-weighted re-estimate for the Wikidata-matched subset, cross-region flow asymmetry with cluster-bootstrap intervals, and a descriptive laureate-association analysis on the subset of nominees with unambiguous Wikidata links.

## What this package contains

- **`Code/`** — all R scripts that gather data, build the network, and run the analyses. Numbered to indicate execution order.
- **`Data/`** — every intermediate and final data product produced by the pipeline. Because some upstream APIs (Wikidata, Wikipedia) change over time, we ship complete data files so the analyses can be reproduced without re-running the gathering stage. **Note:** `Data/intermediate/KI profs.xlsx` is a hand-curated, non-regenerable input (Karolinska Institutet professor rosters transcribed from digitized state calendars); do not delete it when clearing intermediate files.
- **`README.md`** (this file) and **`LICENSE`** (CC BY-NC-ND 4.0).

The manuscript text and supplementary materials are not included here; they live in a separate repository.

## Repository structure

```
├── Code/
│   ├── Data Gathering/          # Scripts 00–09: data acquisition and network construction
│   │   ├── 00_utils.R                 # Shared utility functions (Wikidata/Wikipedia API helpers)
│   │   ├── 01_governing_bodies.R      # Scrape governing body membership
│   │   ├── 02_vetting_bodies.R        # Scrape vetting body (Nobel Committee) membership
│   │   ├── 03_nominators_nominees.R   # Parse nomination archive records
│   │   ├── 04_laureates.R             # Gather laureate data
│   │   ├── 05_match_nomination_qids.R # Match nomination records to Wikidata QIDs
│   │   ├── 06_wikidata_demographics.R # Query Wikidata for birth country, dates, gender
│   │   ├── 07_standardize_geography.R # Map birth countries to UN subregions/continents
│   │   ├── 08_build_nodes_edges.R     # Construct node and edge tables
│   │   └── 09_data_diagnostics.R      # Data quality checks and coverage statistics
│   └── Analysis/                # Scripts 10–18: statistical analysis and visualization
│       ├── 10_exploratory_analysis.R      # Descriptive statistics
│       ├── 11_formal_analysis.R           # Permutation tests, homophily ratios, stress tests
│       ├── 12_figures.R                   # Generate manuscript figures
│       ├── 13_multilayer_quantities.R     # Supra-adjacency assortativity, layer contrasts
│       ├── 14_supplementary_analyses.R    # Laureate prediction, nominator heterogeneity, trends
│       ├── 15_dyadic_model.R              # Gravity/PPML model of dyadic nomination flows
│       ├── 16_qid_representativeness.R    # QID-subset representativeness, IPW re-estimate
│       ├── 17_flow_asymmetry.R            # Cross-region flow asymmetry, bootstrap intervals
│       └── 18_clogit_prominence.R         # Conditional logit with nominee-prominence control
├── Data/
│   ├── nodes.csv                # Final node table (all individuals with geographic attributes)
│   ├── edges.csv                # Final edge table (all network edges with types and layers)
│   ├── results_*.csv            # Output tables from analysis scripts
│   └── intermediate/            # Intermediate data products from the gathering pipeline
├── LICENSE
└── README.md
```

## Version 2.2 (October 2026): corrected control sampling in the conditional logit

Version 2.2 corrects one further error, found in an October 2026 read-only review of the code against the saved results and confined to the conditional-logit companion analysis (`18_clogit_prominence.R`). When a record's prize-year pool contained exactly one alternative candidate, `sample()` on a single numeric identifier drew from `1:id` instead of returning the identifier, and the record was dropped. Three Literature 1963 records were lost this way; the sampling now selects by index. The case-control design grows from 527,199 rows across 25,837 records to 527,205 rows across 25,840 records (four records with no alternative candidate remain excluded), and the fitted odds ratios are unchanged at the precision reported in the paper. See `CHANGELOG.md` for the full before/after comparison. All other scripts, data files, and result tables are unchanged from version 2.1.

## Version 2.1 (October 2026): corrected laureate-association analysis

Version 2.1 corrects two errors identified in a September 2026 audit of the replication package, during review; the corrections were completed in October 2026, after the paper was accepted and before publication. See `CHANGELOG.md` for the full before/after account. (1) The secondary laureate-association analysis (`14_supplementary_analyses.R`, helper `Code/Analysis/helpers/laureate_matched_subset.R`) previously coded nominees without a Wikidata link as non-laureates and counted an award in any prize category as a win. It now uses only nominees whose archive identifier has exactly one Wikidata match, defines the outcome as an award in the *nominated* prize through 2025, and excludes nomination records after a nominee's first award in that prize. Its sample flow, exclusions, and the before/after audit are written to `Data/results_laureate_*.csv`. (2) One stress-test row (`missing_geography_dedup`, H = 4.70) in `results_sensitivity_bounds.csv` divided a record-level numerator by an edge-level denominator and has been withdrawn (`11_formal_analysis.R`). All other scripts, data files, and result tables are unchanged from version 2.0. The primary analyses use the 25,844 expanded nominator–nominee records with complete geography; these records are not deduplicated into the network's 26,654 nomination edges.

## Reproduction instructions

### Requirements

- **R** (version 4.0 or later recommended)
- **R packages** (install with `install.packages()`):

  broom, cli, dplyr, fixest, furrr, future, httr, httr2, igraph, jsonlite, lubridate, parallel, patchwork, progressr, purrr, readr, readxl, rvest, scales, stringr, survival, tidyverse

### Running the pipeline

Scripts are numbered to indicate execution order. All scripts assume the working directory is the repository root (automatic when opening `NobelPrize.Rproj` in RStudio).

**Data gathering (scripts 00–09).** These scripts query external APIs and build the network data. Because API responses may change over time — and because one matching stage (script 05) exploits a nondeterministic external search API, with the exact resolutions preserved in the shipped caches — we ship all intermediate and final data products in `Data/` so that downstream analyses can be reproduced without re-running the gathering stage.

```r
# From the repository root, in R:
source("Code/Data Gathering/00_utils.R")
source("Code/Data Gathering/01_governing_bodies.R")
# ... through 09_data_diagnostics.R
```

**Analysis (scripts 10–18).** These scripts read from `Data/` and produce all statistical results, tables, and figures reported in the manuscript and supplement.

```r
# In R:
source("Code/Analysis/10_exploratory_analysis.R")
source("Code/Analysis/11_formal_analysis.R")
source("Code/Analysis/12_figures.R")
source("Code/Analysis/13_multilayer_quantities.R")
source("Code/Analysis/14_supplementary_analyses.R")
source("Code/Analysis/15_dyadic_model.R")
source("Code/Analysis/16_qid_representativeness.R")
source("Code/Analysis/17_flow_asymmetry.R")
source("Code/Analysis/18_clogit_prominence.R")
```

Permutation tests use 10,000 iterations and are parallelized via the `furrr` package; the flow-asymmetry bootstrap (script 17) uses 10,000 replicates. On a modern multicore machine, the full analysis pipeline completes in roughly 2–3 hours.

## Data sources

- **Nobel Prize Nomination Archive** — nomination records (1901–1975 across four prizes; through 1953 for Physiology/Medicine), available under the 50-year secrecy rule at https://www.nobelprize.org/nomination/archive/.
- **Wikidata** — birth country, birth/death dates, gender, and other biographical attributes for nominees, nominators, committee members, and laureates, queried via the Wikidata Query Service (SPARQL).
- **Swedish Wikipedia** — membership lists for the Royal Swedish Academy of Sciences and the Nobel Committees for Chemistry, Physics, and Physiology or Medicine.
- **Swedish Academy website** — service records for members of the Nobel Committee for Literature.
- **Project Runeberg** — digitized editions of the Swedish state calendar, used to recover Karolinska Institutet professor rosters (1881–1970; digitization ends circa 1972).
- **UN Statistics Division** — country-to-subregion-to-continent geographic classification (M49 standard).

## Key output files

| File | Description |
|------|-------------|
| `nodes.csv` | All individuals in the network, with Wikidata QID, birth country, UN subregion, and continent |
| `edges.csv` | All network edges, with source, target, edge type, prize, and year |
| `results_edge_homophily.csv` | Homophily ratios and p-values by edge type and geographic scale |
| `results_blockwise_homophily.csv` | Blockwise (prize-year feasible-set) homophily ratios |
| `results_sensitivity_bounds.csv` | Fixed-margin adverse stress tests on the primary H |
| `results_temporal_homophily.csv` | Decade-by-decade homophily ratios |
| `results_prize_homophily.csv` | Prize-specific homophily ratios |
| `results_prize_temporal_homophily.csv` | Prize-by-decade homophily ratios |
| `results_gravity_main.csv`, `results_gravity_by_prize.csv` | Gravity/PPML same-country premia (pooled and by prize) |
| `results_clogit_prominence.csv` | Conditional-logit odds ratios with prominence control |
| `results_qid_*.csv` | QID-subset coverage, propensity, movers, and IPW re-estimate |
| `results_flow_return_ratios_*.csv` | Cross-region return ratios with bootstrap CIs and direction tests |
| `results_flow_matrix_*.csv` | Directed O−E flow matrices with permutation bands |
| `results_laureate_prediction.csv` | Laureate association on the unambiguously linked nominee subset: comparison of mean same-country shares, permutation p-value, and the single pooled logistic regression |
| `results_laureate_prediction_by_prize.csv` | Descriptive counts, mean same-country shares, and differences by prize (no separate regressions) |
| `results_laureate_analysis_sample.csv`, `results_laureate_sample_flow.csv`, `results_laureate_record_exclusions.csv`, `results_laureate_linkage_used.csv` | Analysis sample, sample flow, excluded records, and linkage used for the laureate association |
| `results_laureate_outcome_audit.csv`, `results_laureate_outcome_changes.csv` | Audit of the v2.1 outcome correction relative to the v2.0 pooled coding |
| `results_multilayer_quantities.csv` | Supra-adjacency assortativity coefficients |
| `results_nomination_equity.csv` | Early flow tabulation (superseded by `results_flow_return_ratios_*.csv`) |

## Citation

If you use this data or code, please cite the paper:

> Topaz, C. M., Kurongonayini, A., Ptak, B., Fluehr, A., Mendible, A., Roca, R., Rodríguez, N., Xian, L., Schwartz, R., & Higdon, J. (in press). The geography of Nobel Prize nomination, 1901–1975. *Quantitative Science Studies*.

## Contact

Chad M. Topaz — cmt6@williams.edu

# The Geography of Nobel Prize Selection

Replication package for:

> Topaz, C. M., Kurongonayini, A., Ptak, B., Fluehr, A., Mendible, A., Roca, R., Rodríguez, N., Schwartz, R., Xian, L., & Higdon, J. "The Geography of Nobel Prize Selection." *Science* (submitted).

## Overview

This repository contains data and code to reproduce all analyses in the paper. We construct a multilayer network of the Nobel Prize selection process (1901–1966 nominations across Physics, Chemistry, Physiology or Medicine, Literature, and Peace) and quantify geographic homophily at the nomination stage. The pipeline gathers data from the Nobel Prize Nomination Archive, Wikidata, and Swedish Wikipedia; builds a node–edge network representation; and performs permutation-based statistical analyses including homophily ratio estimation, assortativity measurement, logistic regression, and temporal trend testing.

## Repository Structure

```
├── Code/
│   ├── Data Gathering/          # Scripts 00–09: data acquisition and network construction
│   │   ├── 00_utils.R           # Shared utility functions (Wikidata/Wikipedia API helpers)
│   │   ├── 01_governing_bodies.R    # Scrape governing body membership
│   │   ├── 02_vetting_bodies.R      # Scrape vetting body (Nobel Committee) membership
│   │   ├── 03_nominators_nominees.R # Parse nomination archive records
│   │   ├── 04_laureates.R           # Gather laureate data
│   │   ├── 05_match_nomination_qids.R # Match nomination records to Wikidata QIDs
│   │   ├── 06_wikidata_demographics.R # Query Wikidata for birth country, dates, gender
│   │   ├── 07_standardize_geography.R # Map birth countries to UN subregions/continents
│   │   ├── 08_build_nodes_edges.R     # Construct node and edge tables
│   │   └── 09_data_diagnostics.R      # Data quality checks and coverage statistics
│   └── Analysis/                # Scripts 10–14: statistical analysis and visualization
│       ├── 10_exploratory_analysis.R      # Descriptive statistics
│       ├── 11_formal_analysis.R           # Permutation tests, homophily ratios, p-values
│       ├── 12_figures.R                   # Generate all manuscript figures
│       ├── 13_multilayer_quantities.R     # Supra-adjacency assortativity, layer contrasts
│       └── 14_supplementary_analyses.R    # Sensitivity, clustered tests, flow asymmetry
├── Data/
│   ├── nodes.csv                # Final node table (all individuals with geographic attributes)
│   ├── edges.csv                # Final edge table (all network edges with types and layers)
│   ├── results_*.csv            # Output tables from analysis scripts
│   └── intermediate/            # Intermediate data products from the gathering pipeline
└── README.md
```

## Reproduction Instructions

### Requirements

- **R** (version 4.0 or later recommended)
- **R packages** (install with `install.packages()`):

  cli, dplyr, furrr, future, httr, httr2, igraph, jsonlite, lubridate, parallel, patchwork, progressr, purrr, readr, readxl, rvest, scales, stringr, tidyverse

### Running the Pipeline

Scripts are numbered to indicate execution order.

**Data gathering** (scripts 00–09): These scripts query external APIs (Wikidata, Wikipedia, the Nobel Nomination Archive) and build the network data. Because API responses may change over time, we include all intermediate and final data products in `Data/` so that analyses can be reproduced without re-running the gathering stage.

```bash
# From the repository root, in R:
source("Code/Data Gathering/00_utils.R")
source("Code/Data Gathering/01_governing_bodies.R")
# ... through 09_data_diagnostics.R
```

**Analysis** (scripts 10–14): These scripts read from `Data/` and produce all statistical results, tables, and figures reported in the manuscript and supplement.

```bash
# In R:
source("Code/Analysis/10_exploratory_analysis.R")
source("Code/Analysis/11_formal_analysis.R")
source("Code/Analysis/12_figures.R")
source("Code/Analysis/13_multilayer_quantities.R")
source("Code/Analysis/14_supplementary_analyses.R")
```

Permutation tests use 10,000 iterations and are parallelized via the `furrr` package. On a modern multicore machine, the full analysis pipeline completes in approximately 1–2 hours.

## Data Sources

- **Nobel Prize Nomination Archive**: Nomination records (1901–1966) from the [Nobel Prize Nomination Archive](https://www.nobelprize.org/nomination/archive/), which provides public access to nominations after a 50-year secrecy period.
- **Wikidata**: Birth country, birth/death dates, and gender for nominees, nominators, committee members, and laureates, accessed via the Wikidata Query Service (SPARQL).
- **Swedish Wikipedia**: Membership lists for the Royal Swedish Academy of Sciences, the Nobel Assembly at Karolinska Institutet, the Swedish Academy, and the Norwegian Nobel Committee.
- **UN Statistics Division**: Country-to-subregion-to-continent geographic classification (M49 standard).

## Key Output Files

| File | Description |
|------|-------------|
| `nodes.csv` | All individuals in the network with Wikidata QID, birth country, UN subregion, and continent |
| `edges.csv` | All network edges with source, target, edge type, prize, and year |
| `results_edge_homophily.csv` | Homophily ratios and p-values by edge type and geographic scale |
| `results_temporal_homophily.csv` | Decade-by-decade homophily ratios |
| `results_prize_homophily.csv` | Prize-specific homophily ratios |
| `results_laureate_prediction.csv` | Logistic regression results for laureate prediction |
| `results_multilayer_quantities.csv` | Supra-adjacency assortativity coefficients |
| `results_nomination_equity.csv` | Nomination flow asymmetry by subregion |

## Citation

If you use this data or code, please cite the paper:

> Topaz, C. M., Kurongonayini, A., Ptak, B., Fluehr, A., Mendible, A., Roca, R., Rodríguez, N., Schwartz, R., Xian, L., & Higdon, J. "The Geography of Nobel Prize Selection." *Science* (submitted).

## Contact

Chad M. Topaz — cmt6@williams.edu

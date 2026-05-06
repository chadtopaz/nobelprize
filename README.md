# The Geography of Nobel Prize Selection

Replication package for:

> Topaz, C. M., Kurongonayini, A., Ptak, B., Fluehr, A., Mendible, A., Roca, R., Rodríguez, N., Schwartz, R., Xian, L., & Higdon, J. "The Geography of Nobel Prize Selection." Manuscript.

## Overview

This repository provides the data and code used in the manuscript. We construct a multilayer network of the Nobel Prize selection process across Physics, Chemistry, Physiology or Medicine, Literature, and Peace, and quantify geographic homophily at each stage of the selection pipeline. The analytic window is 1901–1975 for nominations, with Physiology/Medicine truncated at 1953 because its governing body has not released later records. Institutional-layer rosters extend further where the historical record permits.

The pipeline gathers data from the Nobel Prize Nomination Archive, Wikidata, Swedish Wikipedia, and the digitized Swedish state calendar; resolves individuals to Wikidata QIDs; constructs a node–edge representation of the multilayer network; and runs permutation-based statistical analyses including homophily ratio estimation, Newman assortativity, logistic regression for laureate prediction, layer-contrast tests, and decade-by-decade temporal trend tests.

## What this package contains

- **`Code/`** — all R scripts that gather data, build the network, and run the analyses. Numbered to indicate execution order.
- **`Data/`** — every intermediate and final data product produced by the pipeline. Because some upstream APIs (Wikidata, Wikipedia) change over time, we ship complete data files so the analyses can be reproduced without re-running the gathering stage.
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
│   └── Analysis/                # Scripts 10–14: statistical analysis and visualization
│       ├── 10_exploratory_analysis.R      # Descriptive statistics
│       ├── 11_formal_analysis.R           # Permutation tests, homophily ratios, p-values
│       ├── 12_figures.R                   # Generate manuscript figures
│       ├── 13_multilayer_quantities.R     # Supra-adjacency assortativity, layer contrasts
│       └── 14_supplementary_analyses.R    # Sensitivity analyses, clustered tests, flow asymmetry
├── Data/
│   ├── nodes.csv                # Final node table (all individuals with geographic attributes)
│   ├── edges.csv                # Final edge table (all network edges with types and layers)
│   ├── results_*.csv            # Output tables from analysis scripts
│   └── intermediate/            # Intermediate data products from the gathering pipeline
├── LICENSE
└── README.md
```

## Reproduction instructions

### Requirements

- **R** (version 4.0 or later recommended)
- **R packages** (install with `install.packages()`):

  cli, dplyr, furrr, future, httr, httr2, igraph, jsonlite, lubridate, parallel, patchwork, progressr, purrr, readr, readxl, rvest, scales, stringr, tidyverse

### Running the pipeline

Scripts are numbered to indicate execution order.

**Data gathering (scripts 00–09).** These scripts query external APIs and build the network data. Because API responses may change over time, we ship all intermediate and final data products in `Data/` so that downstream analyses can be reproduced without re-running the gathering stage.

```r
# From the repository root, in R:
source("Code/Data Gathering/00_utils.R")
source("Code/Data Gathering/01_governing_bodies.R")
# ... through 09_data_diagnostics.R
```

**Analysis (scripts 10–14).** These scripts read from `Data/` and produce all statistical results, tables, and figures reported in the manuscript and supplement.

```r
# In R:
source("Code/Analysis/10_exploratory_analysis.R")
source("Code/Analysis/11_formal_analysis.R")
source("Code/Analysis/12_figures.R")
source("Code/Analysis/13_multilayer_quantities.R")
source("Code/Analysis/14_supplementary_analyses.R")
```

Permutation tests use 10,000 iterations and are parallelized via the `furrr` package. On a modern multicore machine, the full analysis pipeline completes in roughly 1–2 hours.

## Data sources

- **Nobel Prize Nomination Archive** — nomination records (1901–1975 across four prizes; through 1953 for Physiology/Medicine), available under the 50-year secrecy rule at https://www.nobelprize.org/nomination/archive/.
- **Wikidata** — birth country, birth/death dates, gender, and other biographical attributes for nominees, nominators, committee members, and laureates, queried via the Wikidata Query Service (SPARQL).
- **Swedish Wikipedia** — membership lists for the Royal Swedish Academy of Sciences and the Nobel Committees for Chemistry, Physics, and Physiology or Medicine.
- **Swedish Academy website** — service records for members of the Nobel Committee for Literature.
- **Project Runeberg** — digitized editions of the Swedish state calendar, used to recover Karolinska Institutet professor rosters (1881–1970).
- **UN Statistics Division** — country-to-subregion-to-continent geographic classification (M49 standard).

## Key output files

| File | Description |
|------|-------------|
| `nodes.csv` | All individuals in the network, with Wikidata QID, birth country, UN subregion, and continent |
| `edges.csv` | All network edges, with source, target, edge type, prize, and year |
| `results_edge_homophily.csv` | Homophily ratios and p-values by edge type and geographic scale |
| `results_temporal_homophily.csv` | Decade-by-decade homophily ratios |
| `results_prize_homophily.csv` | Prize-specific homophily ratios |
| `results_prize_temporal_homophily.csv` | Prize-by-decade homophily ratios |
| `results_laureate_prediction.csv` | Logistic regression results for laureate prediction |
| `results_multilayer_quantities.csv` | Supra-adjacency assortativity coefficients |
| `results_nomination_equity.csv` | Nomination flow asymmetry by subregion |

## Citation

If you use this data or code, please cite the paper:

> Topaz, C. M., Kurongonayini, A., Ptak, B., Fluehr, A., Mendible, A., Roca, R., Rodríguez, N., Schwartz, R., Xian, L., & Higdon, J. "The Geography of Nobel Prize Selection." Manuscript.

## Contact

Chad M. Topaz — cmt6@williams.edu

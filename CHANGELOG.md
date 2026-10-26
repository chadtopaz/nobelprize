# Changelog

## Version 2.3 — October 2026

Two further issues were identified in an October 2026 read-only review of the package, after version 2.2 and before publication. The roster correction changes network bookkeeping but leaves retained homophily, regression, and flow estimates, intervals, and p-values unchanged. The other issue withdraws an auxiliary check from the paper; its old output is retained as legacy material.

### 1. Governing-body service date (`Data/intermediate/roster_corrections.csv`, `Code/Data Gathering/00b_roster_corrections.R`; scripts 08, 09, 10, 11)

**What was wrong.** RSAS end years come from Wikidata death dates (script 06). Anders Lidtgren (Q5950526), elected to the Academy in 1788, has no date of death in Wikidata, so he carried no end year and script 08 treated him as active in every year of the study window. He generated 1,318 governing-body edges (888 governing → vetting, 430 governing → laureate), none with geography. The Academy's own register (Kungl. Svenska Vetenskapsakademien, personförteckningar 1739–1915, entry 247; https://runeberg.org/ksvperson/0070.html) records "F. 1729, d. 1815". He is the only one of the 441 RSAS members without an end year who was inducted before 1925; the other 440 were inducted in 1957 or later.

**What changed.** A one-row, sourced corrections file sets his end year to 1815. The new helper `apply_roster_corrections()` applies it, with guards that stop the run if the file is missing or malformed, if a correction does not match exactly one roster row, if the row's start year differs from the expected value, or if the row already carries a different end year. Scripts 08, 09, 10, and 11 apply the correction when they read the roster; `governing_bodies.csv` itself is unchanged. Scripts 08 and 09 were rerun. Scripts 10, 11, and 13 were not rerun. The complete `edges.csv` and `nodes.csv` files changed, but the member had no birth geography: the 25,844 geography-complete nomination records are unchanged, and the geography-complete institutional edges with their endpoint geographies are identical before and after. The saved analysis result tables are unchanged.

**Effect (before → after), verified row by row against the version 2.2 files.** `edges.csv`: 514,111 → 512,793 rows (exactly the 1,318 rows involving Q5950526 removed; no other row added or altered). `nodes.csv`: 8,134 → 8,133 (his row only). Governing → vetting 252,476 → 251,588; governing → laureate 146,761 → 146,331; institutional edges 487,457 → 486,139; QID-resolved endpoints 7,003 → 7,002; unique endpoint identifiers 17,514 → 17,513; QID-to-QID participants 6,157 → 6,156 (mean degree 142.6 → 142.2, median 28); RSAS members without an end year 441 → 440. Unchanged: the 26,654 nomination edges, the 10,511 unresolved identifiers, the 846 QIDs paired only with unresolved identifiers, the 1,131 roster nodes without edges, the 1,374 self-loops, and every result table. `data_diagnostics_report.txt` is regenerated and now reports both the raw (441) and corrected (440) counts.

### 2. Clustered nominator-level permutation check (`Code/Analysis/11_formal_analysis.R`, `Data/results_clustered_homophily.csv`): withdrawn from the paper

**What was wrong.** The check's observed same-country rate uses each record's own nominator country, whereas its permutations assign every nominator a single country (that of the nominator's first record). Because 210 nominators carry more than one country across their records, the observed configuration is not one that the permutation family generates, so the printed reference interval does not apply to the printed statistic.

**What changed.** The paper no longer cites this check (main text §4.5 and §5.2; Supplement §S6.2, now a statement of the repeated-nominator limitation). The code and its output are retained for provenance, marked LEGACY / WITHDRAWN in the script header, and can still be regenerated; they are not used for inference. The check was never part of the 49-test Bonferroni family, which is unchanged. No repaired version was run.

### Paper text

The accompanying manuscript revision also calibrates the interpretation of the temporal results (Newman assortativity, already saved in `results_temporal_homophily.csv`, declines from 0.60 in the 1900s to 0.37 in the 1970s while the homophily ratio rises and then plateaus) and narrows several causal and scope statements. These are text changes only; no result file changes.

## Version 2.2 — October 2026

One further error, confined to the conditional-logit companion analysis (`Code/Analysis/18_clogit_prominence.R`), was identified in an October 2026 read-only review of the code against the saved results, after version 2.1 was released and before publication. All other scripts, data files, and result tables are unchanged from version 2.1.

### Conditional-logit control sampling (`Code/Analysis/18_clogit_prominence.R`)

**What was wrong.** For each nomination record, the script samples up to 20 control candidates from the other candidates nominated in the same prize-year with `sample(alts, min(K_CONTROLS, length(alts)))`. Candidate identifiers are numeric. When a record's pool contains exactly one alternative, R's `sample()` treats the single number as a range and draws an integer from `1:alts`, producing a nonexistent candidate identifier. That control then failed the covariate join and the record was dropped from the risk sets. Three Literature 1963 records were lost this way. Four Literature 1961 records have no alternative candidate at all and are excluded legitimately.

**What changed.** The sampling line is now `alts[sample.int(length(alts), min(K_CONTROLS, length(alts)))]`, which selects elements by index. The zero-alternative guard, the seed (1901), K = 20, the covariates, and the estimation are unchanged. No other script was rerun; nothing else in the pipeline reads this script's output.

**Sample.** 527,199 case-control rows across 25,837 records → 527,205 rows across 25,840 records (of 25,844 geography-complete records; the four records without an alternative candidate remain excluded). Because R's sampler consumes a variable number of random draws, the control sets of two subsequently processed Literature 1964 records also differ from version 2.1; all other control sets are unchanged.

**Results, before → after** (`Data/results_clogit_prominence.csv`; the version 2.1 file is preserved outside the package).

| Model | Term | OR v2.1 | OR v2.2 | Robust SE v2.1 | Robust SE v2.2 | p v2.1 | p v2.2 |
|---|---|---|---|---|---|---|---|
| bare | same_country | 11.14850 | 11.14582 | 0.030133 | 0.030137 | 0 (underflow) | 0 (underflow) |
| with_lang_prominence | same_country | 5.77698 | 5.77622 | 0.046418 | 0.046418 | 2.02e-312 | 2.27e-312 |
| with_lang_prominence | shared_lang | 2.17337 | 2.17308 | 0.046475 | 0.046474 | 1.24e-62 | 1.30e-62 |
| with_lang_prominence | log_prior | 1.318969 | 1.318948 | 0.0077971 | 0.0077970 | 3.83e-276 | 4.06e-276 |

Every value reported in the paper (odds ratios 11.1/11.15, 5.78, 2.17, and 1.32; p < 10^-61 and p < 10^-275) is unchanged at its reported precision. The two sample counts in the supplement (§S8) are updated.

## Version 2.1 — October 2026

Two errors were identified in a September 2026 audit of the replication package, during review of the paper; the corrections were completed in October 2026, after the paper was accepted and before publication. Both were confined to a secondary analysis and a single robustness row; the primary analyses (homophily ratios, blockwise and clustered permutation tests, gravity/PPML model, conditional logit, representativeness and IPW re-estimate, flow asymmetry, temporal trends) and their result tables are unchanged from version 2.0.

### 1. Laureate-association analysis (`Code/Analysis/14_supplementary_analyses.R`, new helper `Code/Analysis/helpers/laureate_matched_subset.R`)

**What was wrong.** Version 2.0 coded a nominee as a laureate if the nominee's Wikidata identifier appeared in the laureate roster for *any* prize, and coded every nominee *without* a Wikidata identifier as a non-laureate. Nominees without a link (1,061 of 3,927 nominee–prize units) include real laureates, and awards in other categories were being counted as wins for the nominated prize.

**What changed.** The analysis is now restricted to nominees whose archive identifier has a Wikidata identifier with exactly one birth-year-matching candidate under the documented matching procedure (nominees without an identifier, and nominees whose identifier has more than one candidate match, are excluded rather than coded as non-laureates; 44 identities are ambiguous in the full lookup table, of which 38 nominees, accounting for 41 nominee–prize units, occur in this sample). The outcome is an award in the *nominated* prize at any time through 2025. Nomination records dated after the nominee's first award in that prize are excluded; award-year records are retained. The unit of analysis (archive identifier × prize), the logistic regression `is_laureate ~ same_country_frac + log(total_nominations)`, and the 10,000-permutation comparison of means are unchanged.

**Sample.** 3,927 nominee–prize units with complete geography → 2,866 with a Wikidata identifier → 2,825 with a unique match → 2,817 remaining after the post-award exclusion (483 nomination records dated after a nominee's first same-prize award removed; 8 units lost all of their records). 417 laureates and 2,400 non-laureates. Relative to the pooled v2.0 coding, 101 retained units change from laureate to non-laureate.

**Results, before → after.**

| Quantity | v2.0 (3,927 units) | v2.1 (2,817 units) |
|---|---|---|
| Mean same-country share, laureates | 0.3696 | 0.3650 |
| Mean same-country share, non-laureates | 0.5266 | 0.5125 |
| Difference (non-laureate − laureate) | 0.1570 | 0.1475 |
| One-sided permutation p (10,000 permutations) | 1.0e-4 | 1.0e-4 |
| Logit coefficient on same-country share (SE) | −1.0676 (0.1380) | −1.1768 (0.1655) |
| z | −7.74 | −7.11 |
| Odds ratio (95% Wald CI) | 0.344 | 0.308 (0.223–0.426) |
| Logit coefficient on log nominations (SE) | 0.8207 (0.0415) | 0.8482 (0.0492) |

In the descriptive by-prize breakdown (counts, means, and differences; there is a single pooled regression), laureates have the lower mean same-country share in all five prize categories before and after the correction (differences, non-laureate minus laureate: Chemistry 0.129 → 0.135; Literature 0.203 → 0.192; Peace 0.225 → 0.205; Physics 0.075 → 0.073; Physiology/Medicine 0.161 → 0.158). The corrected association describes the unambiguously linked subset, not all nominees; identity resolution is more complete for prominent nominees.

New output files: `results_laureate_analysis_sample.csv`, `results_laureate_sample_flow.csv`, `results_laureate_record_exclusions.csv`, `results_laureate_linkage_used.csv`, `results_laureate_outcome_audit.csv`, `results_laureate_outcome_changes.csv`. `results_laureate_prediction.csv` gains two rows (`logit_or_ci_lo`, `logit_or_ci_hi`).

### 2. Withdrawn stress-test row (`Code/Analysis/11_formal_analysis.R`, `Data/results_sensitivity_bounds.csv`)

The row `missing_geography_dedup` (H = 4.70) divided the record-level same-country count by the number of deduplicated network edges (26,654), mixing two units. It has been removed from the script and from the saved table; every other row of the table is byte-identical to version 2.0. The retained missing-geography stress test (all 2,114 missing-geography records assumed cross-country, H = 4.48) is unchanged.

### 3. Documentation

README: the primary analyses use the 27,958 expanded nominator–nominee records (25,844 with complete geography on both endpoints); these records are not a subset of the 26,654 deduplicated nomination edges of the network (780 records lack an endpoint identifier and 524 repeat a nominator–nominee–year–prize tuple). Citation updated to the accepted paper.

## Version 2.0 — August 2026

Replication package accompanying the revised manuscript: gravity/PPML, conditional-logit, representativeness/IPW, and flow-asymmetry analyses added; CC BY-NC-ND 4.0 license.

## Version 1.0 — March 2026

Initial release.

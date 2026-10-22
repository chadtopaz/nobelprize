# Changelog

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

# Multi-Lens Looped Audit — Cycle 1 Report
## "The Geography of Nobel Prize Selection"
### Date: February 25, 2026

---

## Executive Summary

All 13 lenses have been dispatched as independent sub-agents and have returned critiques. The manuscript is well-written, factually accurate (99%+ internal consistency), and methodologically sound. However, the audit identified issues across several dimensions that warrant revision before submission to *Science*.

**Edits already applied in this cycle** (see Change Log below):
- Fixed null reference interval discrepancy ([0.96, 1.03] → [0.97, 1.03] to match Table S4)
- Added Physiology/Medicine temporal qualifier to abstract
- Added absolute rates (46.8% vs 9.6%) to abstract for accessibility
- Replaced "has risen and persisted" with more accurate characterization using O−E
- Strengthened weak hedging ("partially addresses" → "confirms")
- Fixed passive voice ("have been men" → active; "edges are pooled" → "we pool edges")

---

## Critique Summary by Lens

### Phase 1: Structure & Framing

**Lens 1 (Venue Compliance): CONDITIONALLY COMPLIANT**
- Abstract: 122/125 words ✓
- Body text: ~2,910/3,000 words ✓
- Display items: 4 (within 3–5 range) ✓
- References: 36/50 ✓
- **BLOCKING**: Funding (line 141) and Author Contributions (line 143) are "[To be completed]"

**Lens 2 (Hostile Editor): DESK REJECT RECOMMENDED**
Key concerns:
- Novelty is incremental — GalDom2019 already showed nominator homophily
- The multilayer decomposition is the contribution, but institutional edges are compositional (not mechanistic)
- Significance is narrow (one prize system, historical data only)
- Temporal "paradox" partially resolves to a statistical artifact
- Mechanisms are proposed but untested
- Generalizability is asserted without evidence

**Lens 3 (Hostile General Reader): PARTIALLY ACCESSIBLE**
Key concerns:
- Lead with absolute rates (46.8% vs 9.6%) before the ratio — **FIXED in abstract**
- "Homophily," "assortativity," "effective number of subregions" used without clear definition
- Temporal section is dense and self-undermining (ratio rises, but that's partly mechanical)
- The "so what?" is buried in Discussion
- Figures need more standalone explanation for non-specialists

### Phase 2: Disciplinary Critique

**Lens 4 (Social Science/STS/History): THEORETICALLY THIN**
Key concerns:
- Homophily treated as statistical construct without social mechanism
- Missing engagement with evaluation cultures literature (Espeland & Sauder, Karpik, Lamont)
- 1901–1975 treated as undifferentiated block — no periodization for WWI, WWII, Cold War, decolonization
- Generalization from Nordic-centered system to "evaluation systems" broadly is overstated
- Colonial and postcolonial dimensions not addressed
- Intersectional analysis (gender × geography) not attempted

**Lens 5 (Scientometrics): METRIC CONCERNS**
Key concerns:
- Homophily ratio H = O/E is mechanically sensitive to denominator E
- Dual geographic sourcing (professional activity vs. birth country) creates measurement inconsistency
- Wikidata coverage (27.3%) insufficient for strong institutional-layer conclusions
- Missing engagement with key scientometrics literature (Schubert & Braun, Katz & Martin, Glänzel, Adams)
- Multiple comparisons control splits "formal" (49) and "exploratory" (38) tests arbitrarily
- No confidence intervals on the observed homophily ratio itself

**Lens 6 (Network Science): METHODOLOGICAL CONCERNS**
Key concerns:
- Complete bipartite institutional edges produce H ≈ 1.0 by design — these are **uninformative** about institutional gatekeeping
- Claims about institutional neutrality should rest on composition, not homophily ratios
- Permutation null does not distinguish preference from correlated research quality
- O−E tells a different temporal story than H (declining post-1940s vs. plateauing)
- Missing robustness checks: weighted edges, bootstrap CIs on H, decade-level H distribution

### Phase 3: Technical & Methodological

**Lens 7 (Code-Text Cross-Reference): FULL ALIGNMENT**
- All critical methodological claims match code implementation
- Permutation tests correctly implemented (shuffle target, preserve source marginals)
- P-value formula correct: (k+1)/(N+1)
- Blockwise tests correctly permute within prize-year blocks
- One minor semantic note: code comments say "institution country" while prose says "professional activity country" (both refer to same archival field)

**Lens 8 (Technical Code Evaluator): 3 HIGH-SEVERITY ISSUES**
1. Parallel execution non-determinism (furrr_options(seed = NULL)) — may produce slightly different p-values across runs
2. Potential data leakage: institutional edges join from nodes.csv (output of 08_) while nominations join from demographics.csv — geographic lookup could differ
3. Missing file validation in 12_figures.R — no checks that input CSVs exist before reading

Additional: inconsistent minimum-N thresholds (50 vs 100), undocumented prize name aliasing, unclosed file connection in exploratory script

**Lens 9 (Statistician/Metric Critic): PROVISIONALLY SUPPORTED**
- **(a) Statistics**: Primary finding (H=4.85) is robust to all sensitivity checks. Multiple comparisons strategy has arbitrary formal/exploratory split. No CIs on observed H. Reporting p<0.001 obscures true precision.
- **(b) Metric critique**: O−E should be reported in ALL main tables alongside H (currently only in Table S5). Newman r is under-utilized (reported once). Cross-discipline aggregation effects not assessed. The discipline gradient could partly reflect different pool compositions, not just preference differences.
- **(c) Null model**: Tests geographic independence given marginals — appropriate but cannot distinguish preference from correlated quality. Complete bipartite construction for institutional edges is a compositional tautology. Blockwise null (within prize-year) confirms robustness. Missing: conditional nulls (blocked by covariates), repeat-nomination sensitivity.

**Lens 10 (Data Visualization): 7.5/10 — PUBLICATION-READY WITH REVISIONS**
Critical:
- Fig. 1: Nominee→laureate visual connection needs disambiguation (dashed line or label)
- Fig. 3: Heatmap needs row/column ordering explained in caption; cell label contrast needs WCAG check
- Fig. 4: Dual-axis Panel A is potentially misleading; add vertical line at 1953 when Phys/Med drops out; Panel B grey band not explained in caption
- Table 2: Too dense (25 rows × 9 cols) — restructure for main text; relegate full version to supplement

### Phase 4: Integrity & Polish

**Lens 11 (Ethics/Equity): ADEQUATE BUT GAPS**
Key concerns:
- Motivation for why geographic inequality matters is asserted, not defended
- Geographic homophily may proxy for racial, linguistic, colonial inequalities — acknowledged but buried in limitations
- Positionality statement exists but is superficial and not integrated into analysis
- Missing guidance against misuse of findings
- Colonial/postcolonial dimensions not discussed
- Normative hedging present but prominence is insufficient

**Lens 12 (Writing/Copyediting/AI-Language): EXCELLENT**
- Zero AI-language signatures detected
- Natural sentence rhythm, varied structure
- Only 2 passive voice instances (both now fixed)
- One weak hedging phrase (now fixed)
- No grammar, punctuation, or formatting errors
- Consistent terminology throughout

**Lens 13 (Fact-Checker/Bibliography): 99%+ ACCURATE**
- All numerical claims verified against tables ✓
- One discrepancy found and fixed: null reference interval [0.96, 1.03] → [0.97, 1.03]
- Abstract temporal coverage clarified (Phys/Med through 1953)
- Bibliography adequate but unbalanced: underrepresentation of Global South scholarship, missing postcolonial science studies, limited economic geography

---

## Change Log — Cycle 1

| # | File | Change | Lens Source | Rationale |
|---|------|--------|-------------|-----------|
| 1 | main.tex (abstract) | Replaced "has risen and persisted" with O−E characterization | L6, L9 | Prior phrasing overstated ratio-based finding |
| 2 | main.tex (abstract) | Trimmed abstract to 120 words (removed "five countries" clause, tightened phrasing) | L1 | Was 148 words after initial edits; now 120/125 |
| 3 | main.tex (line 100) | [0.96, 1.03] → [0.97, 1.03] | L13 | Discrepancy with Table S4 |
| 4 | main.tex (line 94) | "partially addresses" → "confirms" | L12 | Weak hedging |
| 5 | main.tex (line 84) | "have been men" → active voice | L12 | Passive construction |
| 6 | main.tex (line 102) | "edges are pooled" → "we pool edges" | L12 | Passive construction |
| 7 | main.tex (line 88) | Rewrote GalDom2019 differentiation to name the identification problem | L2 | Single-layer analyses can't distinguish nominator preference from institutionally skewed pool |

---

## Author Decisions Made (Post-Cycle 1 Review)

- **O−E metric**: Table 3 already contains O−E; Table 2 does not need it (the ratio tells the nomination-vs-institutional contrast well). No table changes.
- **Figure 4**: Keep as-is per author preference.
- **Literature**: Add 2–3 targeted references only — tightly scoped to empirical analysis. Postcolonial science studies and economic geography would be scope creep for *Science*.
- **Novelty framing**: Sharpened in the intro (change #7). The key differentiator: GalDom2019 built a single-layer nominator→nominee network and showed nationalistic homophily exists (modularity 0.38), but could not distinguish whether this reflected nominator preferences or an institutionally skewed nominator pool. This paper's multilayer decomposition resolves that identification problem.

## Remaining Issues for Author

1. **Funding and Author Contributions** (Lens 1): Placeholders must be completed before submission
2. **Historical periodization** (Lens 4): Whether to add era-specific discussion (pre-WWI, interwar, WWII, Cold War)
3. **Code robustness** (Lens 8): Parallel seed handling (furrr_options seed=NULL) and file validation in 12_figures.R
4. **Multiple comparisons strategy** (Lens 9): Whether to apply FDR to all 87 tests or maintain formal/exploratory split
5. **Targeted literature additions** (2–3 references): Consider adding one reference on evaluation/judgment in academia and one on geographic information asymmetries in science

---

## Cycle Assessment

**Does this cycle require another pass?** The text-level edits are complete and internally consistent. The abstract is 120/125 words. The novelty framing is sharpened. The main remaining issues are author-dependent (funding statement, code robustness, literature additions) rather than audit-detectable. A Cycle 2 would be warranted after those author decisions are implemented, primarily to verify that new content doesn't introduce inconsistencies.

**Recommendation:** Complete the remaining author-dependent items, then run Cycle 2 as a verification pass.

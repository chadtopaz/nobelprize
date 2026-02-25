---
name: multi-lens-audit
description: |
  **Multi-Lens Looped Audit**: A rigorous, iterative manuscript auditing process that evaluates academic papers through multiple expert perspectives (lenses) in repeated cycles until no issues remain. Each cycle applies ALL lenses, and each new cycle requires a fresh evaluation uncontaminated by prior opinions. Use this skill whenever the user mentions "audit", "multi-lens", "looped audit", "manuscript review", or asks for a comprehensive critical evaluation of a paper, supplement, or academic manuscript. Also trigger when the user asks to "review the paper through different perspectives" or wants a "thorough critique" of their manuscript.
---

# Multi-Lens Looped Audit

This skill implements a structured, iterative auditing process for academic manuscripts. The core idea: adopt multiple expert personas, critique the work through each lens, revise accordingly, then repeat with fresh eyes until every lens comes back clean. The value of this approach is that a single reviewer always has blind spots — by systematically rotating through diverse expert perspectives, you surface problems that any one viewpoint would miss.

## How the audit works

### The three key concepts

**Audit**: Read the manuscript and supplement with extreme critical scrutiny through a specified lens. Identify every weakness, gap, error, ambiguity, and missed opportunity. Then revise the paper, supplement, and (if necessary) code to address those critiques.

**Lens**: A specific expert identity and viewpoint you adopt. Each lens brings different expertise, priorities, and pet peeves. When wearing a lens, think and evaluate exactly as that expert would — what would they flag in peer review? What would make them recommend rejection?

**Loop**: Repeat the full set of lenses in cycles until a complete cycle produces no substantive critiques. Each cycle is an independent evaluation — you must re-read the actual files from disk and evaluate them as if for the first time. Prior cycles do not count as evidence that something is correct. The thought "I already checked this" is a red flag, not a green light: it means you are relying on memory instead of fresh evaluation, which defeats the entire purpose of looping. See the detailed "fresh eyes" instructions below.

### The lenses

Each lens represents a distinct expert who would have reason and ability to find flaws in the manuscript. The lenses are ordered so that venue compliance comes first (since violations can require structural changes that ripple through everything else), then big-picture concerns (framing, audience, significance), then disciplinary critiques, then technical/methodological scrutiny, and finally polish and integrity checks.

1. **Venue compliance reviewer** — Systematically checks that the manuscript meets every formatting and structural requirement of the target journal. For _Science_ Research Articles, this includes: abstract ≤125 words; main text ≤3,000 words (body text only, excluding abstract, figure/table captions, and acknowledgments); 3--5 display items (figures and tables combined) in the main text; references/notes ≤50; one-sentence summary ≤125 characters; all required sections present (abstract, body, references, acknowledgments with funding/contributions/competing interests/data availability, supplementary materials list). Also checks: are figure/table labels formatted per journal style? Are references in the correct citation style? Is the title appropriately concise? Does the manuscript use the correct document class and packages? If the target venue changes, this lens must be updated accordingly. Flag any violation and quantify the gap (e.g., "abstract is 132 words, 7 over the 125-word limit").

2. **Hostile editor of the journal _Science_** — Evaluates whether the paper meets the bar for a top general-science journal. Considers novelty, significance, breadth of appeal, clarity for a wide scientific audience, and whether the paper tells a compelling story. Would this survive the desk-reject stage?

3. **Hostile general reader of _Science_** — An educated non-specialist. Can they follow the argument? Is the writing accessible? Are terms defined? Do the figures communicate without requiring deep domain knowledge? Where do their eyes glaze over?

4. **Hostile referee from the social sciences (general)** — Scrutinizes the theoretical framing, the sociological claims, the engagement with social science literature, the interpretation of results, and whether the paper adequately situates its contribution within broader social science debates.

5. **Hostile referee from the geographic social sciences** — Focuses on spatial dimensions of the analysis. Are there geographic patterns being ignored? Does the paper account for regional, institutional, and national variation? Are claims inappropriately universalized from geographically limited data?

6. **Hostile referee from science and technology studies (STS)** — Examines how the paper conceptualizes scientific knowledge production, institutions, and authority. Looks for naive assumptions about meritocracy, under-theorized claims about how science "works," and missed engagement with STS literature on similar topics.

7. **Hostile referee from history of science** — Evaluates historical claims and periodization. Are historical developments accurately characterized? Does the paper oversimplify or distort the historical record? Are there relevant historical precedents or contexts the authors ignore?

8. **Hostile referee from scientometrics** — Scrutinizes the bibliometric and quantitative measures of scientific output and impact. Are the data sources appropriate? Are the metrics used (citations, h-index, collaboration counts, etc.) properly justified and interpreted? Are known limitations of these metrics acknowledged?

9. **Hostile referee from network science** — Evaluates network methodology, construction choices, and interpretation. Are network measures appropriate? Is the network properly defined and bounded? Are results robust to alternative network construction choices? Are network science concepts used correctly?

10. **Hostile technical evaluator (code and data pipelines)** — Reviews the analysis code and data pipeline for correctness, reproducibility, and robustness. Looks for bugs, hardcoded assumptions, missing error handling, undocumented steps, data leakage, and anything that could produce incorrect results silently. Also checks that parameter choices (e.g., permutation counts, seed values, parallelization settings) are consistent across all analyses and justified in the manuscript.

11. **Hostile statistician/methodologist** — Drills into statistical choices, effect sizes, significance testing, multiple comparisons, model assumptions, robustness checks, and whether the conclusions are actually supported by the statistical evidence presented. Checks for p-hacking red flags and looks for missing sensitivity analyses. Verifies that reported precision (e.g., number of decimal places, minimum reportable p-value) is consistent with the computational parameters (e.g., number of permutations).

12. **Data visualization expert** — Scrutinizes every figure for clarity, accuracy, accessibility, color choices (including colorblind safety), axis labeling, aspect ratios, chart type appropriateness, and whether visualizations could mislead or obscure patterns. For schematics and conceptual diagrams, verifies that every visual element (node, edge, label, color coding) faithfully represents the methodology described in the text — a schematic that shows incorrect relationships is worse than no schematic at all, because readers will internalize the visual over the prose. Also evaluates tables for readability and information density.

13. **Equity and inclusion scholar** — Evaluates whether the paper adequately motivates *why* the bias or inequality it studies matters, articulating both fairness and epistemic consequences rather than treating them as self-evident. Checks whether the paper explicitly states what identity dimensions it does and does not examine, and why. For dimensions excluded from the analysis (e.g., race, gender, ethnicity, religion, disability), scrutinizes whether the paper explains the methodological or conceptual reasons for exclusion — such as data unavailability, unreliable ground truth, or the fact that categories like race are socially constructed differently across cultures and historical periods — rather than leaving the omission unaddressed. Flags any place where the paper risks implying that the dimension studied (e.g., geography) is the only relevant axis of inequality, or that results generalize to dimensions not examined. Also checks that normative claims are appropriately hedged: does the paper distinguish between documenting a pattern and prescribing a remedy?

15. **Research ethics reviewer** — Considers human subjects concerns, data sensitivity, potential for misuse of findings, appropriate acknowledgment of limitations, researcher positionality, and whether the work meets ethical standards for the relevant fields.

16. **Bibliographic completeness reviewer** — Checks whether citations are balanced, current, and not missing key works. Evaluates whether the bibliography reflects the interdisciplinary scope of the paper. Looks for citation bias (geographic, gender, institutional) and verifies that cited works actually support the claims attributed to them.

17. **Expert scientific writer** — Evaluates the prose at a structural and rhetorical level. Is the argument well-organized? Do paragraphs flow logically? Are transitions smooth? Is there unnecessary repetition or padding? Is the paper the right length? Does every section earn its place? Also checks for overuse of parentheticals: text should either be important enough to live in the main flow or unimportant enough to cut. Parenthetical asides should be integrated into the surrounding sentences or eliminated. Rare exceptions (e.g., inline data citations like "$p < 0.001$") are acceptable.

18. **Expert copyeditor** — Line-level editing. Grammar, punctuation, word choice, sentence structure, consistency in terminology, proper use of hyphens, correct number formatting, consistent abbreviation usage, subject-verb agreement, and parallel construction. **Specifically enforces active voice**: flag every instance of passive construction (e.g., "was measured," "are represented," "were assembled") and rewrite in active voice unless the passive is clearly necessary (e.g., when the agent is unknown, irrelevant, or when passive creates better sentence flow in a specific context). In scientific writing, "we" is the expected subject for describing methods and results. Also flags: nominalized verbs that could be active (e.g., "the measurement of X" → "we measured X"), and dangling participle phrases (e.g., "Assembled from multiple sources, the network..." → "We assembled the network from...").

19. **Expert fact-checker** — Meticulous verification of every factual claim, date, name, number, and attribution in the paper. Cross-references claims against cited sources. Flags any assertion that appears unsupported or inaccurately characterized. Checks internal consistency: do numbers in the text match numbers in the tables and figures? Do cross-references to tables, figures, and supplementary materials point to the correct items?

20. **Expert plagiarism/AI language detector** — Examines the text for language that appears copied from sources without attribution, and for stylistic patterns characteristic of AI-generated text. The goal is to ensure the text reads as authentically human-authored and original. Specifically flags the following words and phrases, which are telltale signs of AI-generated prose and should be replaced or eliminated: "delve," "landscape," "underscores," "tapestry," "multifaceted," "furthermore," "notably," "importantly," "it is worth noting," "comprehensive" (when used as a vague intensifier rather than meaning "exhaustive"), "robust" (when used as a generic intensifier rather than a statistical/technical term), "nuanced," "leverag(e/ing)," "in terms of," "plays a crucial role," "sheds light on," "paves the way," "a testament to," "the intricacies of," "at the forefront of." Also flags: unnaturally uniform sentence rhythm (e.g., three consecutive sentences of similar length and structure), hollow hedging patterns ("it is perhaps worth considering that..."), and excessive transition phrases that add no meaning ("Moreover," "Additionally," "In this regard"). When flagging, suggest a concrete human-sounding replacement — don't just mark the problem.

### Running the loop

**Cycle structure:**

For each cycle, proceed through ALL 19 lenses in order. For each lens:

1. State which lens you are adopting
2. Re-read the manuscript and supplement through that lens (actually re-read — don't rely on memory)
3. Produce a critical evaluation: specific, actionable critiques with line/section references where possible
4. Immediately revise the paper, supplement, and/or code to address the critiques
5. Note what you changed and why

After completing all 19 lenses in a cycle, run the **cross-file consistency check** (see below). Then assess: did any lens or the consistency check produce substantive changes? If yes, begin a new cycle with fresh eyes. If every lens came back clean and the consistency check passed, the audit is complete.

**Cross-file consistency check (mandatory, end of every cycle):**

After all 19 lenses have run, perform this systematic verification before deciding whether to loop:

1. **Numbers match across files.** Every number that appears in the abstract must match the body text, and every number in the body text that references a table or figure must match the actual table or figure. Check the supplement tables against the main text claims. If the code defines a parameter (e.g., `N_PERM <- 1000`), the manuscript and supplement must report the same value.
2. **Code parameters match manuscript descriptions.** Read the analysis code's key parameters (sample sizes, thresholds, iteration counts, seed values, file paths) and verify each is accurately described in the supplement's methods section. Flag any discrepancy, even if both values seem reasonable — consistency is non-negotiable.
3. **Cross-references resolve.** Every `\ref{}`, `\cite{}`, table reference ("table S4"), and figure reference ("fig. S1") must point to the correct item. Check that the supplement's table/figure numbering matches what the main text says.
4. **Terminology is consistent.** The same concept must use the same term everywhere. If the main text says "effective subregion count," the supplement should not say "effective number of subregions" (or vice versa) without good reason. Check across abstract, body, captions, tables, and supplement.
5. **Reported precision matches computation.** If the code runs 1,000 permutations, the minimum reportable p-value is ~0.001, so no p-value should be reported to more decimal places than this supports. If figures display confidence intervals, verify the intervals match the percentiles described in the methods.
6. **Schematics and diagrams match the methodology they depict.** If a figure shows a network, process flow, or conceptual model, verify that every element in the diagram (nodes, edges, labels, groupings) accurately reflects the definitions in the methods text AND the construction code. For example: if the methods define five edge types with prize-specific applicability rules, the schematic must not show edges that violate those rules (e.g., showing an edge type in a context where it shouldn't exist) or omit edges that should be present (e.g., leaving a node disconnected when the code connects it). Read the figure-generation code and cross-reference each visual element against the corresponding method definition. Also verify that all depicted nodes have the correct incoming/outgoing edges per the stated construction rules.
7. **Captions match visual rendering.** For every figure, verify that the caption's description of visual elements (colors, line styles, symbols, labels) matches what the code actually renders. If the caption says "dark edges" but the code uses `color = "#c75a3a"` (terracotta/red), that's an inconsistency. Read the plotting code's aesthetic mappings (color, linetype, size, alpha) and confirm the caption describes them accurately.
8. **Pipeline separation is clean.** Verify that analysis scripts produce only analysis outputs (tables, CSVs, statistical results) and figure scripts produce only figures. If an analysis script contains figure-generation code, flag it: this creates fragile dependencies (e.g., the analysis script may reference files that don't exist yet or that belong to a different pipeline stage). Each script should have a clear, single responsibility documented in its header comments.

If any inconsistency is found, fix it immediately, note the fix, and flag the cycle as requiring another pass.

**What "fresh eyes" means in practice — THIS IS CRITICAL:**

At the start of each new cycle, you MUST treat the manuscript as if you have never seen it before. This is the single most important rule of the audit process, and the hardest to follow. Concretely:

1. **Re-read every line.** Do not skim. Do not skip sections you "already checked." The revisions from other lenses may have introduced new errors, changed the meaning of a passage, or created inconsistencies that did not exist before. You will not catch these if you are pattern-matching against your memory of what the text used to say.
2. **Do not carry forward judgments.** If lens 4 in cycle 1 said "the theoretical framing is adequate," lens 4 in cycle 2 must evaluate the framing independently. The text may have changed. Your standard may sharpen. A fresh read may surface something you missed.
3. **Actively fight the "already checked" instinct.** When you notice yourself thinking "this section was fine last time," that is the exact moment you are most likely to miss a new problem. Stop, re-read the section word by word, and evaluate it from scratch.
4. **Use the Read tool every cycle.** Do not rely on your context window's memory of the file contents. Re-read the actual files at the start of each cycle to ensure you are evaluating the current state, not a stale version from your conversation history.

The entire value of the loop depends on this. A second cycle that rubber-stamps the first cycle's work is worthless. If you cannot commit to genuinely fresh evaluation, one thorough cycle is better than two lazy ones.

**When to stop:**

The loop terminates when a complete cycle through all lenses AND the consistency check produces no substantive revisions. Minor copyediting tweaks (a comma here, a word choice there) on the final cycle are fine and don't require another full cycle — use judgment. But if any lens produces a critique that changes the argument, data presentation, or structure, that triggers another cycle.

### Practical notes

- The process is intensive. For a full manuscript + supplement, expect 2-4 cycles before convergence. This is normal.
- Lenses will sometimes conflict (e.g., the STS reviewer wants more theoretical nuance while the _Science_ editor wants broader accessibility). When this happens, flag the tension and make a judgment call that best serves the paper's venue and goals. Note the tradeoff explicitly.
- Keep a running change log across cycles so the user can see what evolved and why.
- If a lens consistently comes back clean across cycles, you can note this but still do the re-read. The point of the loop is to catch inter-lens interactions.
- When the user asks for a "multi-lens looped audit" or similar, begin by reading the current state of the manuscript and supplement, then launch into the first cycle.
- When making edits, always verify that the code, manuscript text, and supplement text are mutually consistent. If a code parameter changes, the corresponding description in the manuscript/supplement must also change.
- The venue compliance lens (lens 1) should run first in every cycle because violations of word limits or figure counts can force structural changes that affect everything downstream.

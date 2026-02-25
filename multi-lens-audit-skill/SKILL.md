---
name: multi-lens-audit
description: |
  **Multi-Lens Looped Audit**: A rigorous, iterative manuscript auditing process that evaluates academic papers through multiple expert perspectives (lenses) in repeated cycles until no issues remain. Each cycle applies ALL lenses, and each new cycle requires a fresh evaluation uncontaminated by prior opinions. Use this skill whenever the user mentions "audit", "multi-lens", "looped audit", "manuscript review", or asks for a comprehensive critical evaluation of a paper, supplement, or academic manuscript. Also trigger when the user asks to "review the paper through different perspectives" or wants a "thorough critique" of their manuscript.
---

# Multi-Lens Looped Audit

This skill implements a structured, iterative auditing process for academic manuscripts. The core idea: evaluate the work through multiple expert personas using **isolated sub-agents**, critique through each lens, revise accordingly, then repeat until every lens comes back clean.

**Why sub-agents?** A single reviewer in a single conversation context cannot have genuinely fresh eyes on a second pass — it has already seen its own prior evaluations. By dispatching each lens as an independent sub-agent via the `Task` tool, each lens starts with a clean context window containing only the current file contents and its specific evaluation instructions. This is the only way to achieve the "fresh eyes" that make the loop valuable.

## Architecture overview

The **orchestrator** (the main conversation) manages the audit loop. It does NOT evaluate the manuscript itself. Instead, it:

1. Identifies all relevant file paths (manuscript, supplement, code, figures, tables)
2. Dispatches each lens as a sub-agent via the `Task` tool
3. Collects critiques from each sub-agent
4. Makes edits to address the critiques (or dispatches an editing sub-agent)
5. After all lenses in a cycle, dispatches the **cross-file consistency check** as its own sub-agent
6. Decides whether another cycle is needed
7. Reports a change log to the user after each cycle

## How the audit works

### The three key concepts

**Audit**: Evaluate the manuscript and supplement with extreme critical scrutiny through a specified lens. Identify every weakness, gap, error, ambiguity, and missed opportunity. Then revise the paper, supplement, and (if necessary) code to address those critiques.

**Lens**: A specific expert identity and viewpoint. Each lens brings different expertise, priorities, and pet peeves. When wearing a lens, think and evaluate exactly as that expert would — what would they flag in peer review? What would make them recommend rejection?

**Loop**: Repeat the full set of lenses in cycles until a complete cycle produces no substantive critiques. Because each lens runs as an independent sub-agent, each cycle genuinely starts fresh — no memory of prior cycles' judgments contaminating the evaluation.

### The lenses

The lenses are consolidated from a broader set to maximize depth per lens while maintaining comprehensive coverage. They are ordered: venue compliance first (structural changes ripple), then big-picture concerns, then disciplinary critiques, then technical/methodological scrutiny (including the critical code-text and metric-critique lenses), and finally polish and integrity checks.

**Phase 1: Structure and framing**

1. **Venue compliance reviewer** — Systematically checks that the manuscript meets every formatting and structural requirement of the target journal. For _Science_ Research Articles, this includes: abstract ≤125 words; main text ≤3,000 words (body text only, excluding abstract, figure/table captions, and acknowledgments); 3--5 display items (figures and tables combined) in the main text; references/notes ≤50; one-sentence summary ≤125 characters; all required sections present (abstract, body, references, acknowledgments with funding/contributions/competing interests/data availability, supplementary materials list). Also checks: are figure/table labels formatted per journal style? Are references in the correct citation style? Is the title appropriately concise? Does the manuscript use the correct document class and packages? If the target venue changes, this lens must be updated accordingly. Flag any violation and quantify the gap (e.g., "abstract is 132 words, 7 over the 125-word limit").

2. **Hostile editor of the target journal** — Evaluates whether the paper meets the bar for the target venue (e.g., a top general-science journal). Considers novelty, significance, breadth of appeal, clarity for a wide scientific audience, and whether the paper tells a compelling story. Would this survive the desk-reject stage? Does the framing match what this journal publishes?

3. **Hostile general reader** — An educated non-specialist. Can they follow the argument? Is the writing accessible? Are terms defined? Do the figures communicate without requiring deep domain knowledge? Where do their eyes glaze over? Are there jargon barriers?

**Phase 2: Disciplinary critique**

4. **Social science, STS, and history of science referee** — Combines the perspectives of a social scientist, an STS scholar, and a historian of science into a single deep evaluation. Scrutinizes: theoretical framing and engagement with social science literature; how the paper conceptualizes scientific knowledge production, institutions, and authority; whether it makes naive assumptions about meritocracy or under-theorized claims about how science "works"; historical accuracy and periodization; whether historical developments are accurately characterized or oversimplified; spatial dimensions and whether claims are inappropriately universalized from geographically limited data; and whether the paper adequately situates its contribution within broader debates across these fields. This lens should engage deeply with the paper's intellectual claims, not just check boxes.

5. **Scientometrics referee** — Scrutinizes the bibliometric and quantitative measures of scientific output and impact. Are the data sources appropriate? Are the metrics used (citations, h-index, collaboration counts, etc.) properly justified and interpreted? Are known limitations of these metrics acknowledged? Does the paper engage with relevant scientometrics literature?

6. **Network science referee** — Evaluates network methodology, construction choices, and interpretation. Are network measures appropriate? Is the network properly defined and bounded? Are results robust to alternative network construction choices? Are network science concepts used correctly? Does the paper engage with the relevant network science literature?

**Phase 3: Technical and methodological scrutiny**

7. **Code-text cross-referencing auditor** — THIS IS THE HIGHEST-VALUE SINGLE LENS. Its sole job is to read the analysis code and the methods descriptions (in both the main text and supplement) and verify they match, element by element. Specifically:
   - For every methodological claim in the prose ("we pair X with Y", "we filter by Z", "we permute within blocks of W"), find the corresponding code and verify the prose accurately describes what the code does. Flag ANY discrepancy, no matter how small.
   - For every key parameter in the code (sample sizes, thresholds, iteration counts, seed values, filtering criteria, variable definitions), verify it is accurately described in the manuscript/supplement.
   - Check that the code's data pipeline (which files are read, how they are filtered, how edges are constructed, what is excluded) matches the narrative in the methods section.
   - Check that variable names and concepts in the code map correctly to the terms used in the prose.
   - This lens should read code files IN FULL, not skim. The most dangerous discrepancies are subtle — e.g., the prose says "professional activity country" but the code uses birth country, or the prose says "within each prize-year block" but the code permutes globally.
   - Report findings as a structured list: [Code location] vs. [Prose location] — what differs.

8. **Technical evaluator (code and data pipelines)** — Reviews the analysis code and data pipeline for correctness, reproducibility, and robustness BEYOND the code-text match (which lens 7 handles). Looks for: bugs, hardcoded assumptions, missing error handling, undocumented steps, data leakage, edge cases that could produce incorrect results silently, and whether the pipeline is reproducible from raw data. Also checks that parameter choices (e.g., permutation counts, seed values, parallelization settings) are justified and consistent across all scripts.

9. **Statistician, methodologist, and metric critic** — This lens has THREE distinct sub-tasks that must all be performed:
   - **(a) Statistical evaluation:** Drills into statistical choices, effect sizes, significance testing, multiple comparisons, model assumptions, robustness checks, and whether the conclusions are actually supported by the statistical evidence. Checks for p-hacking red flags and missing sensitivity analyses. Verifies that reported precision (e.g., decimal places, minimum reportable p-value) is consistent with computational parameters (e.g., number of permutations).
   - **(b) Metric critique:** For EVERY ratio, normalized measure, or index reported in the paper, asks: (i) Is there a complementary absolute or margin-robust measure that would strengthen the argument or guard against ratio artifacts? (ii) Does the paper acknowledge the mechanical sensitivity of its chosen metrics — e.g., how a ratio $O/E$ behaves when $E$ changes, how a normalized index behaves at boundary values? (iii) Are there scenarios where the chosen metric could be misleading (e.g., ratio inflation when the denominator shrinks, Simpson's paradox, ecological fallacy)? If the paper uses a ratio as its primary measure, the metric critic MUST assess whether an absolute difference ($O - E$), a log-ratio, or another margin-robust alternative should accompany it. If such a complement already exists, verify it is discussed adequately.
   - **(c) Null model evaluation:** Scrutinizes the null model / baseline / permutation scheme. Does it test the right hypothesis? What does it condition on, and what does it leave free? Could a more targeted null (e.g., blocked by covariates) be more informative? Are the null model's assumptions stated clearly in the text?

10. **Data visualization expert** — Scrutinizes every figure for clarity, accuracy, accessibility, color choices (including colorblind safety), axis labeling, aspect ratios, chart type appropriateness, and whether visualizations could mislead or obscure patterns. For schematics and conceptual diagrams, verifies that every visual element (node, edge, label, color coding) faithfully represents the methodology described in the text — a schematic that shows incorrect relationships is worse than no schematic at all, because readers will internalize the visual over the prose. Also evaluates tables for readability and information density.

**Phase 4: Integrity and polish**

11. **Ethics, equity, and research integrity reviewer** — Combines equity/inclusion and research ethics perspectives. Evaluates whether the paper adequately motivates *why* the bias or inequality it studies matters, articulating both fairness and epistemic consequences rather than treating them as self-evident. Checks whether the paper explicitly states what identity dimensions it does and does not examine, and why. For dimensions excluded from the analysis (e.g., race, gender, ethnicity, religion, disability), scrutinizes whether the paper explains the methodological or conceptual reasons for exclusion — such as data unavailability, unreliable ground truth, or the fact that categories like race are socially constructed differently across cultures and historical periods — rather than leaving the omission unaddressed. Flags any place where the paper risks implying that the dimension studied (e.g., geography) is the only relevant axis of inequality, or that results generalize to dimensions not examined. Also checks that normative claims are appropriately hedged: does the paper distinguish between documenting a pattern and prescribing a remedy? Also considers: human subjects concerns, data sensitivity, potential for misuse of findings, appropriate acknowledgment of limitations, and researcher positionality.

12. **Writing quality, copyediting, and AI-language reviewer** — Combines three perspectives into one deep pass:
   - **(a) Structural and rhetorical evaluation:** Is the argument well-organized? Do paragraphs flow logically? Are transitions smooth? Is there unnecessary repetition or padding? Does every section earn its place? Checks for overuse of parentheticals: text should either be important enough to live in the main flow or unimportant enough to cut. Parenthetical asides should be integrated into the surrounding sentences or eliminated. Rare exceptions (e.g., inline data citations like "$p < 0.001$") are acceptable.
   - **(b) Line-level copyediting:** Grammar, punctuation, word choice, sentence structure, consistency in terminology, proper use of hyphens, correct number formatting, consistent abbreviation usage, subject-verb agreement, and parallel construction. **Specifically enforces active voice**: flag every instance of passive construction (e.g., "was measured," "are represented," "were assembled") and rewrite in active voice unless the passive is clearly necessary (e.g., when the agent is unknown, irrelevant, or when passive creates better sentence flow in a specific context). In scientific writing, "we" is the expected subject for describing methods and results. Also flags: nominalized verbs that could be active (e.g., "the measurement of X" → "we measured X"), and dangling participle phrases (e.g., "Assembled from multiple sources, the network..." → "We assembled the network from...").
   - **(c) AI-language detection:** Examines the text for language that appears copied from sources without attribution, and for stylistic patterns characteristic of AI-generated text. The goal is to ensure the text reads as authentically human-authored and original. Specifically flags the following words and phrases, which are telltale signs of AI-generated prose and should be replaced or eliminated: "delve," "landscape," "underscores," "tapestry," "multifaceted," "furthermore," "notably," "importantly," "it is worth noting," "comprehensive" (when used as a vague intensifier rather than meaning "exhaustive"), "robust" (when used as a generic intensifier rather than a statistical/technical term), "nuanced," "leverag(e/ing)," "in terms of," "plays a crucial role," "sheds light on," "paves the way," "a testament to," "the intricacies of," "at the forefront of." Also flags: unnaturally uniform sentence rhythm (e.g., three consecutive sentences of similar length and structure), hollow hedging patterns ("it is perhaps worth considering that..."), and excessive transition phrases that add no meaning ("Moreover," "Additionally," "In this regard"). When flagging, suggest a concrete human-sounding replacement — don't just mark the problem.

13. **Fact-checker and bibliographic reviewer** — Combines two perspectives:
   - **(a) Fact-checking:** Meticulous verification of every factual claim, date, name, number, and attribution in the paper. Cross-references claims against cited sources. Flags any assertion that appears unsupported or inaccurately characterized. Checks internal consistency: do numbers in the text match numbers in the tables and figures? Do cross-references to tables, figures, and supplementary materials point to the correct items?
   - **(b) Bibliographic completeness:** Checks whether citations are balanced, current, and not missing key works. Evaluates whether the bibliography reflects the interdisciplinary scope of the paper. Looks for citation bias (geographic, gender, institutional) and verifies that cited works actually support the claims attributed to them.

### Running the loop

**Cycle structure:**

For each cycle, the orchestrator dispatches ALL 13 lenses as sub-agents. The orchestrator should run lenses in order but may run non-conflicting lenses in parallel where possible (e.g., lenses 4, 5, 6 can run simultaneously since they evaluate different disciplinary dimensions).

**How to dispatch a lens sub-agent:**

Use the `Task` tool with `subagent_type: "general-purpose"`. The prompt for each sub-agent must include:

1. The lens number, name, and full evaluation instructions (copied from the lens description above)
2. The file paths to read (manuscript, supplement, and — for lenses 7, 8, 9 — code files)
3. Explicit instruction: "Read all specified files using the Read tool. Evaluate them through the specified lens. Produce a structured critique with specific line/section references. Do NOT make edits — return your critique only. Be maximally critical: your job is to find problems, not to reassure."
4. For lens 7 (code-text auditor): also include "Read every code file in full. For each methodological claim in the manuscript/supplement, find the corresponding code and verify they match. Report discrepancies as: [Prose: file, line/section, claim] vs [Code: file, line, what code actually does]."
5. For lens 9 (metric critic): also include "For every ratio or normalized measure, explicitly state whether a complementary absolute or margin-robust measure exists or should exist, and whether the paper adequately discusses the metric's mechanical sensitivities."

**After each lens sub-agent returns:**

1. Review its critique
2. For each critique item, decide: fix it, flag it for the user, or explain why it's not actionable
3. Make edits to address actionable items
4. Log what changed and why

**After all 13 lenses complete in a cycle, dispatch the cross-file consistency check as its own sub-agent** (see below). This runs with clean context and focuses solely on mechanical consistency.

**Cross-file consistency check (mandatory, run as independent sub-agent at end of every cycle):**

Dispatch via the `Task` tool with `subagent_type: "general-purpose"`. The sub-agent's prompt must include all file paths and the following checklist:

1. **Numbers match across files.** Every number that appears in the abstract must match the body text, and every number in the body text that references a table or figure must match the actual table or figure. Check the supplement tables against the main text claims. If the code defines a parameter (e.g., `N_PERM <- 1000`), the manuscript and supplement must report the same value.
2. **Code parameters match manuscript descriptions.** Read the analysis code's key parameters (sample sizes, thresholds, iteration counts, seed values, file paths) and verify each is accurately described in the supplement's methods section. Flag any discrepancy, even if both values seem reasonable — consistency is non-negotiable.
3. **Cross-references resolve.** Every `\ref{}`, `\cite{}`, table reference ("table S4"), and figure reference ("fig. S1") must point to the correct item. Check that the supplement's table/figure numbering matches what the main text says.
4. **Terminology is consistent.** The same concept must use the same term everywhere. If the main text says "effective subregion count," the supplement should not say "effective number of subregions" (or vice versa) without good reason. Check across abstract, body, captions, tables, and supplement.
5. **Reported precision matches computation.** If the code runs 1,000 permutations, the minimum reportable p-value is ~0.001, so no p-value should be reported to more decimal places than this supports. If figures display confidence intervals, verify the intervals match the percentiles described in the methods.
6. **Schematics and diagrams match the methodology they depict.** If a figure shows a network, process flow, or conceptual model, verify that every element in the diagram (nodes, edges, labels, groupings) accurately reflects the definitions in the methods text AND the construction code. For example: if the methods define five edge types with prize-specific applicability rules, the schematic must not show edges that violate those rules (e.g., showing an edge type in a context where it shouldn't exist) or omit edges that should be present (e.g., leaving a node disconnected when the code connects it). Read the figure-generation code and cross-reference each visual element against the corresponding method definition. Also verify that all depicted nodes have the correct incoming/outgoing edges per the stated construction rules.
7. **Captions match visual rendering.** For every figure, verify that the caption's description of visual elements (colors, line styles, symbols, labels) matches what the code actually renders. If the caption says "dark edges" but the code uses `color = "#c75a3a"` (terracotta/red), that's an inconsistency. Read the plotting code's aesthetic mappings (color, linetype, size, alpha) and confirm the caption describes them accurately.
8. **Pipeline separation is clean.** Verify that analysis scripts produce only analysis outputs (tables, CSVs, statistical results) and figure scripts produce only figures. If an analysis script contains figure-generation code, flag it: this creates fragile dependencies (e.g., the analysis script may reference files that don't exist yet or that belong to a different pipeline stage). Each script should have a clear, single responsibility documented in its header comments.
9. **Inline table copies match generated tables.** If the supplement or main text contains hand-typed copies of tables that are also generated by code, verify every value matches the generated `.tex` file exactly. Rounding discrepancies count — even a difference of 0.1 must be flagged.

Report ALL inconsistencies found. Do NOT fix them — return the list to the orchestrator.

**Loop decision:**

After collecting results from all 13 lenses and the consistency check, the orchestrator assesses: did any lens or the consistency check produce substantive changes? If yes, begin a new cycle. If every lens came back clean and the consistency check passed, the audit is complete.

**When to stop:**

The loop terminates when a complete cycle through all lenses AND the consistency check produces no substantive revisions. Minor copyediting tweaks (a comma, a word choice) on the final cycle are fine and don't require another full cycle — use judgment. But if any lens produces a critique that changes the argument, data presentation, or structure, that triggers another cycle.

### Practical notes

- The process is intensive. For a full manuscript + supplement, expect 2-4 cycles before convergence. This is normal.
- Lenses will sometimes conflict (e.g., the STS perspective wants more theoretical nuance while the editor wants broader accessibility). When this happens, flag the tension and make a judgment call that best serves the paper's venue and goals. Note the tradeoff explicitly.
- Keep a running change log across cycles so the user can see what evolved and why.
- If a lens consistently comes back clean across cycles, you can note this but still dispatch it. The point of the loop is to catch inter-lens interactions — a revision from lens 9 might create a problem visible only to lens 4.
- When the user asks for a "multi-lens looped audit" or similar, begin by identifying all relevant file paths (ask the user if unclear), then launch the first cycle.
- When making edits, always verify that the code, manuscript text, and supplement text are mutually consistent. If a code parameter changes, the corresponding description in the manuscript/supplement must also change.
- The venue compliance lens (lens 1) should run first in every cycle because violations of word limits or figure counts can force structural changes that affect everything downstream.
- **Content preservation discipline.** Before making any structural edit (removing a paragraph, condensing a section, reorganizing), explicitly assess whether the content being cut contains substantive scholarly value — citations, mechanisms, specific numbers, methodological arguments, or contextual framing that strengthens the paper. If it does, either preserve the material elsewhere in the manuscript, move it to the supplement, or flag it for the user rather than silently discarding it. Track word count before and after each cycle and report any net losses exceeding 100 words, explaining what was removed and why. The goal is to improve the paper, not to thin it. If the paper is under the word limit, cuts should only be made to improve quality, not to save space.
- **Target venue word limit.** For _Science_ Research Articles, the body text limit is 3,000 words (excluding abstract, captions, acknowledgments, and references). The audit should aim to use the available word count effectively — a 2,200-word paper submitted to a venue allowing 3,000 words is underusing its budget unless every sentence is essential.
- **Parallelism strategy.** Within each phase, lenses can often run in parallel since they evaluate independent dimensions. The orchestrator should batch sub-agent dispatches where possible:
  - Phase 1: Lenses 1, 2, 3 can run in parallel (all read-only evaluation of different aspects)
  - Phase 2: Lenses 4, 5, 6 can run in parallel (independent disciplinary perspectives)
  - Phase 3: Lenses 7, 8 can run in parallel (both examine code but from different angles); lens 9 can also run in parallel with 7-8; lens 10 is independent
  - Phase 4: Lenses 11, 12, 13 can run in parallel (independent evaluation dimensions)
  - The consistency check runs AFTER all lenses complete and all edits from the cycle are applied
- **Adapting lenses to the paper's disciplinary footprint.** The 13 lenses above are designed for an interdisciplinary social science paper targeting a general-science venue. For papers in other domains, the orchestrator should adapt: replace disciplinary lenses (4, 5, 6) with lenses appropriate to the paper's fields, but always keep the structural lenses (1-3), technical lenses (7-9), and integrity/polish lenses (10-13). The code-text auditor (lens 7) and metric critic (lens 9b) should NEVER be dropped — these catch the most consequential errors.

# Corrected, restricted secondary laureate analysis. No file I/O or new matching.
# A usable match means a nonmissing QID and multi_match_count == 1 under the
# existing automated linkage procedure; it does not mean human verification.
# Organizations are eligible under exactly the same rule. Archive IDs remain
# distinct, including multiple archive IDs linked to the same Wikidata entity.
# Outcome: the NOMINATED prize won by 2025. Nominations after the FIRST award in
# that prize are excluded; nominations in the award year are retained because
# only calendar-year timing is available. No directional censoring claim is made.

prepare_matched_laureate_data <- function(nominations, qid_map, laureates,
                                          followup_year = 2025L) {
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package dplyr is required.")
  if (length(followup_year) != 1L || is.na(followup_year) || followup_year != 2025L) {
    stop("This repair preserves fixed award follow-up through 2025.")
  }
  require_columns <- function(x, columns, label) {
    missing <- setdiff(columns, names(x))
    if (length(missing)) stop(label, " lacks: ", paste(missing, collapse = ", "))
  }
  require_columns(nominations, c("nominee_person_id", "year", "prize",
    "nominator_country_modern", "nominee_country_modern"), "nominations")
  require_columns(qid_map, c("person_id", "qid", "multi_match_count"), "qid_map")
  require_columns(laureates, c("qid", "prize", "year"), "laureates")
  missing_text <- function(x) is.na(x) | trimws(as.character(x)) %in% c("", "NA")
  valid_years <- function(x) {
    num <- suppressWarnings(as.numeric(as.character(x)))
    length(num) == length(x) && all(is.finite(num) & num == floor(num) & num >= 1901)
  }
  prizes <- c("Chemistry", "Literature", "Peace", "Physics", "Physiology/Medicine")
  for (input in list(nominations = nominations, laureates = laureates)) {
    if (any(missing_text(input$prize)) || any(!input$prize %in% prizes)) {
      stop("Missing or unsupported prize label; do not silently convert it to a non-winner.")
    }
    if (!valid_years(input$year)) stop("Missing or invalid nomination/award year.")
  }
  if (any(as.numeric(as.character(nominations$year)) > 1975)) {
    stop("Nomination input exceeds the frozen study window ending in 1975.")
  }
  if (any(missing_text(qid_map$person_id)) || anyDuplicated(as.character(qid_map$person_id))) {
    stop("qid_map must contain exactly one row per nonmissing archive person ID.")
  }
  if (any(missing_text(laureates$qid)) ||
      any(!grepl("^Q[1-9][0-9]*$", as.character(laureates$qid)))) {
    stop("Every award must have a valid QID; an incomplete roster cannot define non-winners.")
  }
  qid_present <- !missing_text(qid_map$qid)
  if (any(qid_present & !grepl("^Q[1-9][0-9]*$", as.character(qid_map$qid)))) {
    stop("Malformed nonmissing QID in the existing identity map.")
  }
  count <- suppressWarnings(as.numeric(as.character(qid_map$multi_match_count)))
  count_present <- !missing_text(qid_map$multi_match_count)
  if (any(count_present & (!is.finite(count) | count < 0 | count != floor(count)))) {
    stop("multi_match_count must be a nonnegative integer or missing.")
  }

  # Work on local copies; neither inputs nor any script14 nom_data are modified.
  map <- data.frame(
    nominee_person_id = as.character(qid_map$person_id),
    nominee_qid = ifelse(qid_present, as.character(qid_map$qid), NA_character_),
    multi_match_count = count,
    match_method = if ("match_method" %in% names(qid_map)) as.character(qid_map$match_method) else NA_character_,
    stringsAsFactors = FALSE
  )
  if ("name" %in% names(qid_map)) map$linkage_name <- as.character(qid_map$name)
  raw <- as.data.frame(nominations, stringsAsFactors = FALSE)
  collision <- intersect(names(raw), c("source_record_row", "nominee_qid", "multi_match_count",
    "match_method", "linkage_name", "first_same_prize_award_year", "is_laureate",
    "same_country", "exclusion_reason"))
  if (length(collision)) stop("Nomination input contains derived columns: ", paste(collision, collapse = ", "))
  raw$source_record_row <- seq_len(nrow(raw))
  raw$nominee_person_id <- as.character(raw$nominee_person_id)
  raw$nominee_person_id[missing_text(raw$nominee_person_id)] <- NA_character_
  raw$year <- as.integer(as.character(raw$year))
  raw$prize <- as.character(raw$prize)
  raw <- dplyr::left_join(raw, map, by = "nominee_person_id")
  awards <- as.data.frame(laureates, stringsAsFactors = FALSE)
  awards$year <- as.integer(as.character(awards$year))
  awards$qid <- as.character(awards$qid)
  eligible_awards <- awards[awards$year <= followup_year, , drop = FALSE]
  if (!nrow(eligible_awards)) stop("No awards through 2025: check the frozen laureate roster.")
  first_awards <- dplyr::summarise(
    dplyr::group_by(eligible_awards, qid, prize),
    first_same_prize_award_year = min(year), .groups = "drop"
  )
  raw <- dplyr::left_join(raw, first_awards,
    by = c("nominee_qid" = "qid", "prize" = "prize"))
  raw$same_country <- as.integer(raw$nominator_country_modern == raw$nominee_country_modern)
  geo <- !missing_text(raw$nominator_country_modern) & !missing_text(raw$nominee_country_modern)
  identified <- geo & !is.na(raw$nominee_person_id)
  linked <- identified & !is.na(raw$nominee_qid)
  unique_link <- linked & !is.na(raw$multi_match_count) & raw$multi_match_count == 1L
  post_award <- !is.na(raw$first_same_prize_award_year) & raw$year > raw$first_same_prize_award_year
  retained <- unique_link & !post_award
  raw$is_laureate <- NA_integer_
  raw$is_laureate[unique_link] <- as.integer(!is.na(raw$first_same_prize_award_year[unique_link]))
  raw$exclusion_reason <- NA_character_
  raw$exclusion_reason[!geo] <- "missing_geography"
  raw$exclusion_reason[geo & is.na(raw$nominee_person_id)] <- "missing_nominee_archive_id"
  raw$exclusion_reason[identified & is.na(raw$nominee_qid)] <- "missing_qid"
  raw$exclusion_reason[linked & !unique_link] <- "not_unique_existing_match"
  raw$exclusion_reason[unique_link & post_award] <- "after_first_same_prize_award"
  records <- raw[retained, setdiff(names(raw), "exclusion_reason"), drop = FALSE]
  excluded <- raw[!retained, , drop = FALSE]
  aggregate_records <- function(x) {
    dplyr::summarise(dplyr::group_by(x, nominee_person_id, prize),
      nominee_qid = dplyr::first(nominee_qid),
      total_nominations = dplyr::n(),
      same_country_noms = sum(same_country),
      cross_country_noms = total_nominations - same_country_noms,
      same_country_frac = same_country_noms / total_nominations,
      is_laureate = dplyr::first(is_laureate),
      first_same_prize_award_year = dplyr::first(first_same_prize_award_year),
      first_nomination_year = min(year), last_nomination_year = max(year),
      .groups = "drop")
  }
  nominee_level <- aggregate_records(records)
  nominee_level$log_total_noms <- log(nominee_level$total_nominations)

  # The old label is calculated solely for a transparent before/after audit.
  # All old geography-complete, archive-identified groups remain in this audit;
  # a group excluded by the repair has a NEW outcome of NA, never zero.
  old_records <- raw[identified, , drop = FALSE]
  old_records$is_laureate <- as.integer(old_records$nominee_qid %in% unique(awards$qid))
  old_groups <- aggregate_records(old_records)
  old_groups <- dplyr::transmute(old_groups, nominee_person_id, prize, nominee_qid,
    old_is_laureate = is_laureate, old_total_nominations = total_nominations,
    old_same_country_frac = same_country_frac)
  new_groups <- dplyr::transmute(nominee_level, nominee_person_id, prize,
    new_is_laureate = is_laureate, new_total_nominations = total_nominations,
    new_same_country_frac = same_country_frac)
  outcome_changes <- dplyr::left_join(old_groups, new_groups, by = c("nominee_person_id", "prize"))
  outcome_changes <- dplyr::left_join(outcome_changes,
    map[, c("nominee_person_id", "multi_match_count"), drop = FALSE], by = "nominee_person_id")
  outcome_changes$retained <- !is.na(outcome_changes$new_is_laureate)
  outcome_changes$exclusion_reason <- ifelse(outcome_changes$retained, NA_character_,
    ifelse(is.na(outcome_changes$nominee_qid), "missing_qid",
      ifelse(is.na(outcome_changes$multi_match_count) | outcome_changes$multi_match_count != 1L,
        "not_unique_existing_match", "no_records_before_or_in_first_same_prize_award_year")))
  outcome_changes$label_changed <- ifelse(outcome_changes$retained,
    outcome_changes$old_is_laureate != outcome_changes$new_is_laureate, NA)

  count_groups <- function(keep) {
    x <- raw[keep & !is.na(raw$nominee_person_id), c("nominee_person_id", "prize"), drop = FALSE]
    nrow(unique(x))
  }
  stages <- list(all_input_records = rep(TRUE, nrow(raw)),
    complete_geography = geo, nominee_archive_id_available = identified,
    existing_qid_available = linked, existing_unique_qid_match = unique_link,
    before_or_in_first_same_prize_award_year = retained)
  sample_flow <- data.frame(stage = names(stages),
    n_records = vapply(stages, sum, integer(1)),
    n_nominee_prize_groups = vapply(stages, count_groups, integer(1)),
    row.names = NULL)
  used <- unique(records$nominee_person_id)
  linkage_used <- map[map$nominee_person_id %in% used, , drop = FALSE]
  linkage_used <- dplyr::left_join(linkage_used,
    dplyr::count(records, nominee_person_id, name = "retained_nomination_records"),
    by = "nominee_person_id")
  map_ambiguous <- !is.na(map$nominee_qid) & !is.na(map$multi_match_count) & map$multi_match_count > 1L
  ambiguous_nominees <- map$nominee_person_id[map_ambiguous & map$nominee_person_id %in% raw$nominee_person_id[identified]]
  outcome_audit <- data.frame(statistic = c(
    "followup_through_year", "qid_map_people_total", "qid_map_api_rank_multimatch_people",
    "api_rank_multimatch_nominees_in_old_geographic_sample", "old_nominee_prize_groups",
    "retained_nominee_prize_groups", "excluded_missing_qid_groups", "excluded_nonunique_match_groups",
    "excluded_all_records_post_award_groups", "retained_groups_with_changed_outcome",
    "post_same_prize_award_records_excluded", "award_year_records_retained",
    "eligible_award_records_through_followup", "award_records_after_followup_ignored"),
    value = c(followup_year, nrow(map), sum(map_ambiguous), length(ambiguous_nominees),
      nrow(old_groups), nrow(nominee_level),
      sum(outcome_changes$exclusion_reason == "missing_qid", na.rm = TRUE),
      sum(outcome_changes$exclusion_reason == "not_unique_existing_match", na.rm = TRUE),
      sum(outcome_changes$exclusion_reason == "no_records_before_or_in_first_same_prize_award_year", na.rm = TRUE),
      sum(outcome_changes$label_changed, na.rm = TRUE), sum(unique_link & post_award),
      sum(retained & !is.na(raw$first_same_prize_award_year) & raw$year == raw$first_same_prize_award_year),
      nrow(eligible_awards), sum(awards$year > followup_year)), stringsAsFactors = FALSE)
  list(records = records, nominee_level = nominee_level,
    outcome_changes = outcome_changes, outcome_audit = outcome_audit,
    record_exclusions = excluded, sample_flow = sample_flow, linkage_used = linkage_used)
}

run_matched_laureate_analysis <- function(nominations, qid_map, laureates,
                                         n_perm = 10000L, seed = 42L) {
  if (length(n_perm) != 1L || is.na(n_perm) || n_perm < 1 || n_perm != floor(n_perm)) {
    stop("n_perm must be a positive integer.")
  }
  prepared <- prepare_matched_laureate_data(nominations, qid_map, laureates)
  dat <- prepared$nominee_level
  yes <- dat$same_country_frac[dat$is_laureate == 1L]
  no <- dat$same_country_frac[dat$is_laureate == 0L]
  if (!length(yes) || !length(no)) stop("Both outcome groups are required for the existing comparison and regression.")

  # Restore the caller's RNG exactly. This local secondary calculation must not
  # silently change the random stream of any other analysis in script14.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) previous_seed <- get(".Random.seed", envir = .GlobalEnv)
  previous_kind <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(previous_kind))
    if (had_seed) assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  difference <- mean(no) - mean(yes)
  all_fracs <- c(yes, no)
  n_lau <- length(yes)
  n_total <- length(all_fracs)
  # Same unstratified, one-sided permutation test as the accepted analysis:
  # non-laureate minus laureate mean share, with the original plus-one correction.
  perm_diffs <- replicate(n_perm, {
    perm <- sample(all_fracs)
    mean(perm[seq.int(n_lau + 1L, n_total)]) - mean(perm[seq_len(n_lau)])
  })
  permutation_p <- (sum(perm_diffs >= difference) + 1) / (n_perm + 1)
  # Original archive-person-ID x prize unit, covariate, and model; no prize fixed
  # effects, time model, deduplication, or additional regressions are introduced.
  estimation_warnings <- character()
  model <- withCallingHandlers(
    stats::glm(is_laureate ~ same_country_frac + log_total_noms,
      data = dat, family = stats::binomial(link = "logit")),
    warning = function(w) {
      message <- conditionMessage(w)
      if (grepl("algorithm did not converge|fitted probabilities numerically 0 or 1", message)) {
        estimation_warnings <<- c(estimation_warnings, message)
        invokeRestart("muffleWarning")
      }
    }
  )
  if (length(estimation_warnings)) {
    stop("Corrected regression has an estimation warning; no result should be reported: ",
      paste(unique(estimation_warnings), collapse = "; "))
  }
  coefs <- summary(model)$coefficients
  terms_needed <- c("same_country_frac", "log_total_noms")
  if (!isTRUE(model$converged) || isTRUE(model$boundary) || model$rank < 3L ||
      !all(terms_needed %in% rownames(coefs)) || any(!is.finite(coefs[terms_needed, , drop = FALSE]))) {
    stop("Corrected regression is not estimable reliably; do not report unchanged results.")
  }
  b <- coefs["same_country_frac", "Estimate"]
  se <- coefs["same_country_frac", "Std. Error"]
  reported_or <- c(exp(b), exp(b - stats::qnorm(0.975) * se),
    exp(b + stats::qnorm(0.975) * se))
  if (any(!is.finite(reported_or) | reported_or <= 0)) {
    stop("Corrected regression odds ratio or confidence limits overflow/underflow; ",
      "possible separation or unstable estimation. No result should be reported.")
  }
  predictions <- data.frame(statistic = c(
    "n_laureates", "n_non_laureates", "mean_same_frac_laureates", "mean_same_frac_non_laureates",
    "diff_non_minus_lau", "permutation_p_value", "logit_coef_same_frac", "logit_se_same_frac",
    "logit_z_same_frac", "logit_odds_ratio_same_frac", "logit_coef_log_noms",
    "logit_se_log_noms", "logit_z_log_noms", "logit_or_ci_lo",
    "logit_or_ci_hi"),
    value = c(length(yes), length(no), mean(yes), mean(no), difference, permutation_p,
      b, se, coefs["same_country_frac", "z value"], reported_or[1L],
      coefs["log_total_noms", "Estimate"], coefs["log_total_noms", "Std. Error"],
      coefs["log_total_noms", "z value"], reported_or[2L], reported_or[3L]),
    stringsAsFactors = FALSE)
  prizes <- c("Chemistry", "Literature", "Peace", "Physics", "Physiology/Medicine")
  by_prize <- do.call(rbind, lapply(prizes, function(prize_name) {
    no_prize <- dat$same_country_frac[dat$prize == prize_name & dat$is_laureate == 0L]
    yes_prize <- dat$same_country_frac[dat$prize == prize_name & dat$is_laureate == 1L]
    no_mean <- if (length(no_prize)) mean(no_prize) else NA_real_
    yes_mean <- if (length(yes_prize)) mean(yes_prize) else NA_real_
    data.frame(prize = prize_name, n_0 = length(no_prize), n_1 = length(yes_prize),
      mean_frac_0 = no_mean, mean_frac_1 = yes_mean, diff = no_mean - yes_mean)
  }))
  direction_testable <- !is.na(by_prize$diff)
  prepared$outcome_audit <- rbind(prepared$outcome_audit,
    data.frame(statistic = c("permutations", "permutation_seed", "prizes_with_both_outcomes",
      "prizes_with_lower_mean_same_country_share_for_laureates", "all_five_prizes_same_direction"),
      value = c(n_perm, seed, sum(direction_testable), sum(by_prize$diff > 0, na.rm = TRUE),
        if (all(direction_testable)) as.integer(all(by_prize$diff > 0)) else NA_real_)))
  list(
    "results_laureate_prediction.csv" = predictions,
    "results_laureate_prediction_by_prize.csv" = by_prize,
    "results_laureate_analysis_sample.csv" = dat,
    "results_laureate_outcome_changes.csv" = prepared$outcome_changes,
    "results_laureate_outcome_audit.csv" = prepared$outcome_audit,
    "results_laureate_record_exclusions.csv" = prepared$record_exclusions,
    "results_laureate_sample_flow.csv" = prepared$sample_flow,
    "results_laureate_linkage_used.csv" = prepared$linkage_used
  )
}

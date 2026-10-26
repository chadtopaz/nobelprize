# =============================================================================
# FILE: 00b_roster_corrections.R
# TITLE: Sourced corrections to governing-body service dates
# =============================================================================
# Added in version 2.3 (October 2026).
#
# Data/intermediate/governing_bodies.csv is produced by scripts 01 and 06. Its
# RSAS end years come from Wikidata death dates (script 06). One member, Anders
# Lidtgren (Q5950526, elected 1788), has no date of death in Wikidata although
# the Academy's own register records his death in 1815. Without an end year he
# was treated as active in every year of the study window and generated 1,318
# governing-body edges (888 governing -> vetting, 430 governing -> laureate),
# none of which carried geography.
#
# This file defines apply_roster_corrections(), which reads
# Data/intermediate/roster_corrections.csv (columns: qid, body,
# startyear_expected, endyear, source, note) and sets the end year of each
# matching roster row. The source CSV is left unchanged, so the correction must
# be applied by every script that reads the roster (08, 09, 10, 11).
#
# The function fails closed. It stops if the corrections file is missing,
# empty, or lacks the required columns; if a correction row has a missing or
# non-integer year, an end year before its start year, or a blank source; if a
# correction does not match exactly one roster row by (qid, body); if that row's
# start year differs from startyear_expected; or if the row already carries a
# different end year. Re-applying a correction that is already in place is
# allowed (idempotent). No other rows are touched and no global rule (such as
# an age cutoff) is applied.
# =============================================================================

apply_roster_corrections <- function(gb,
                                     path = file.path("Data", "intermediate", "roster_corrections.csv")) {
  if (!file.exists(path)) {
    stop(sprintf("roster_corrections: required file not found: %s", path))
  }
  corr <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character"),
                   error = function(e) stop(sprintf("roster_corrections: cannot read %s (%s)", path, conditionMessage(e))))
  required <- c("qid", "body", "startyear_expected", "endyear", "source", "note")
  missing_cols <- setdiff(required, names(corr))
  if (length(missing_cols) > 0) {
    stop(sprintf("roster_corrections: missing column(s): %s", paste(missing_cols, collapse = ", ")))
  }
  if (nrow(corr) == 0) {
    stop("roster_corrections: file contains no correction rows")
  }
  for (col in c("qid", "body", "startyear", "endyear")) {
    if (!col %in% names(gb)) stop(sprintf("roster_corrections: roster lacks column '%s'", col))
  }
  as_year <- function(x, what, i) {
    x <- trimws(x)
    if (is.na(x) || x == "" || !grepl("^[0-9]{4}$", x)) {
      stop(sprintf("roster_corrections: row %d has invalid %s '%s' (expected a four-digit year)", i, what, x))
    }
    as.integer(x)
  }
  for (i in seq_len(nrow(corr))) {
    qid  <- trimws(corr$qid[i]); body <- trimws(corr$body[i])
    if (is.na(qid) || qid == "" || is.na(body) || body == "") {
      stop(sprintf("roster_corrections: row %d has an empty qid or body", i))
    }
    if (is.na(corr$source[i]) || trimws(corr$source[i]) == "") {
      stop(sprintf("roster_corrections: row %d (%s) has no source", i, qid))
    }
    start_expected <- as_year(corr$startyear_expected[i], "startyear_expected", i)
    end_new        <- as_year(corr$endyear[i], "endyear", i)
    if (end_new < start_expected) {
      stop(sprintf("roster_corrections: row %d (%s) has endyear %d before startyear_expected %d", i, qid, end_new, start_expected))
    }
    idx <- which(gb$qid == qid & gb$body == body)
    if (length(idx) != 1) {
      stop(sprintf("roster_corrections: %s / %s matched %d roster rows (expected exactly 1)", qid, body, length(idx)))
    }
    if (is.na(gb$startyear[idx]) || gb$startyear[idx] != start_expected) {
      stop(sprintf("roster_corrections: %s start year is %s, expected %d", qid, gb$startyear[idx], start_expected))
    }
    if (!is.na(gb$endyear[idx]) && gb$endyear[idx] != end_new) {
      stop(sprintf("roster_corrections: %s already has end year %s (correction %d)", qid, gb$endyear[idx], end_new))
    }
    gb$endyear[idx] <- end_new
    message(sprintf("  roster correction applied: %s (%s) end year set to %d; source: %s",
                    qid, body, end_new, trimws(corr$source[i])))
  }
  gb
}

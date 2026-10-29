# =============================================================================
# 07b_m49_subregion_fix.R  (version 2.4)
# =============================================================================
# PURPOSE
#   Apply the UN M49 subregion corrections of version 2.4 to the shipped
#   intermediate data files in place, without re-running the gathering stage
#   (scripts 03-07 depend on external APIs and on the nondeterministic matching
#   caches; see README). The mapping table in 07_standardize_geography.R carries
#   the same corrections for any future full run.
#
#   Corrections (https://unstats.un.org/unsd/methodology/m49/):
#     Mongolia: "Central Asia" -> "Eastern Asia"
#     Iran:     "Western Asia" -> "Southern Asia"
#   Continents are unchanged (Asia). Country names are unchanged.
#
# FILES CHANGED IN PLACE
#   Data/intermediate/nominations.csv   (nominator_subregion, nominee_subregion)
#   Data/intermediate/demographics.csv  (birth_subregion)
#
# GUARDS
#   The script stops unless it finds exactly the expected rows of the frozen
#   version-2.3 files: 1 nomination record with Mongolia as nominee country and
#   none as nominator country; no record with Iran on either side; 1 demographics
#   row born in Mongolia and 12 born in Iran. Re-running it on already-corrected
#   files is a no-op that also passes the guards.
#
# RUN AFTER THIS SCRIPT
#   08_build_nodes_edges.R, 09_data_diagnostics.R, then analysis scripts 10-18.
# =============================================================================

library(tidyverse)

int_path <- function(f) file.path("Data", "intermediate", f)

fixes <- tribble(
  ~country,   ~old_subregion,  ~new_subregion,
  "Mongolia", "Central Asia",  "Eastern Asia",
  "Iran",     "Western Asia",  "Southern Asia"
)

fix_col <- function(df, country_col, subregion_col) {
  for (i in seq_len(nrow(fixes))) {
    hit <- !is.na(df[[country_col]]) & df[[country_col]] == fixes$country[i]
    bad <- hit & !is.na(df[[subregion_col]]) &
      !(df[[subregion_col]] %in% c(fixes$old_subregion[i], fixes$new_subregion[i]))
    if (any(bad)) stop("Unexpected subregion for ", fixes$country[i], " in ", subregion_col)
    df[[subregion_col]][hit] <- fixes$new_subregion[i]
  }
  df
}

# --- nominations.csv ---
nom_file <- int_path("nominations.csv")
nom <- read_csv(nom_file, show_col_types = FALSE)
stopifnot(nrow(nom) == 27958)
stopifnot(sum(nom$nominee_country_modern   %in% "Mongolia") == 1)
stopifnot(sum(nom$nominator_country_modern %in% "Mongolia") == 0)
stopifnot(sum(nom$nominee_country_modern   %in% "Iran") == 0)
stopifnot(sum(nom$nominator_country_modern %in% "Iran") == 0)
nom2 <- nom %>%
  fix_col("nominee_country_modern",   "nominee_subregion") %>%
  fix_col("nominator_country_modern", "nominator_subregion")
changed_nom <- sum(nom2$nominee_subregion != nom$nominee_subregion, na.rm = TRUE) +
  sum(nom2$nominator_subregion != nom$nominator_subregion, na.rm = TRUE)
write_csv(nom2, nom_file, na = "NA")
message(sprintf("nominations.csv: %d subregion value(s) corrected (expect 1 on first run, 0 thereafter)", changed_nom))

# --- demographics.csv ---
demo_file <- int_path("demographics.csv")
demo <- read_csv(demo_file, show_col_types = FALSE)
stopifnot(nrow(demo) == 10206)
stopifnot(sum(demo$birth_country_modern %in% "Mongolia") == 1)
stopifnot(sum(demo$birth_country_modern %in% "Iran") == 12)
demo2 <- demo %>% fix_col("birth_country_modern", "birth_subregion")
changed_demo <- sum(demo2$birth_subregion != demo$birth_subregion, na.rm = TRUE)
write_csv(demo2, demo_file, na = "NA")
message(sprintf("demographics.csv: %d subregion value(s) corrected (expect 13 on first run, 0 thereafter)", changed_demo))

stopifnot(!any(nom2$nominee_subregion %in% "Central Asia"),
          !any(nom2$nominator_subregion %in% "Central Asia"),
          !any(demo2$birth_subregion %in% "Central Asia"))
message("UN M49 subregion corrections applied. Now run 08_build_nodes_edges.R, 09_data_diagnostics.R, and scripts 10-18.")

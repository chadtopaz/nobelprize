# =============================================================================
# 07_standardize_geography.R
# Standardize country names to modern countries, UN subregions, and continents
# =============================================================================
#
# Input:  Data/intermediate/demographics.csv
#         Data/intermediate/nominations.csv
# Output: Enriched versions of both files (new columns added in place)
#
# This script runs AFTER 06_wikidata_demographics.R and BEFORE
# 08_build_nodes_edges.R. It does not require any upstream scripts to rerun.
# =============================================================================

source("Code/Data Gathering/00_utils.R")

message("\n====================================================================")
message("  SCRIPT 07: STANDARDIZE GEOGRAPHY")
message("====================================================================\n")


# =============================================================================
# 1. Define comprehensive country mapping
# =============================================================================
# Maps every observed country name (from Wikidata labels and nobelprize.org)
# to a modern country, UN M49 subregion, and continent.
# Matching is case-insensitive; one table serves both data sources.
# =============================================================================

country_mapping <- tribble(
  ~raw_name,                          ~modern_country,            ~un_subregion,              ~continent,

  # --- AFRICA ---
  # Northern Africa
  "Algeria",                          "Algeria",                  "Northern Africa",          "Africa",
  "ALGERIA",                          "Algeria",                  "Northern Africa",          "Africa",
  "Egypt",                            "Egypt",                    "Northern Africa",          "Africa",
  "EGYPT",                            "Egypt",                    "Northern Africa",          "Africa",
  "Libya",                            "Libya",                    "Northern Africa",          "Africa",
  "Morocco",                          "Morocco",                  "Northern Africa",          "Africa",
  "Sudan",                            "Sudan",                    "Northern Africa",          "Africa",
  "SUDAN",                            "Sudan",                    "Northern Africa",          "Africa",
  "Tunisia",                          "Tunisia",                  "Northern Africa",          "Africa",
  "TUNISIA",                          "Tunisia",                  "Northern Africa",          "Africa",

  # Sub-Saharan Africa
  "Central African Republic",         "Central African Republic", "Sub-Saharan Africa",       "Africa",
  "Democratic Republic of the Congo", "DR Congo",                 "Sub-Saharan Africa",       "Africa",
  "Ethiopia",                         "Ethiopia",                 "Sub-Saharan Africa",       "Africa",
  "ETHIOPIA",                         "Ethiopia",                 "Sub-Saharan Africa",       "Africa",
  "Ghana",                            "Ghana",                    "Sub-Saharan Africa",       "Africa",
  "Ivory Coast",                      "Ivory Coast",              "Sub-Saharan Africa",       "Africa",
  "IVORY COAST",                      "Ivory Coast",              "Sub-Saharan Africa",       "Africa",
  "Kenya",                            "Kenya",                    "Sub-Saharan Africa",       "Africa",
  "KENYA",                            "Kenya",                    "Sub-Saharan Africa",       "Africa",
  "Liberia",                          "Liberia",                  "Sub-Saharan Africa",       "Africa",
  "LIBERIA",                          "Liberia",                  "Sub-Saharan Africa",       "Africa",
  "Madagascar",                       "Madagascar",               "Sub-Saharan Africa",       "Africa",
  "Mauritania",                       "Mauritania",               "Sub-Saharan Africa",       "Africa",
  "Nigeria",                          "Nigeria",                  "Sub-Saharan Africa",       "Africa",
  "Rwanda",                           "Rwanda",                   "Sub-Saharan Africa",       "Africa",
  "Senegal",                          "Senegal",                  "Sub-Saharan Africa",       "Africa",
  "SENEGAL",                          "Senegal",                  "Sub-Saharan Africa",       "Africa",
  "Somalia",                          "Somalia",                  "Sub-Saharan Africa",       "Africa",
  "South Africa",                     "South Africa",             "Sub-Saharan Africa",       "Africa",
  "SOUTH AFRICA",                     "South Africa",             "Sub-Saharan Africa",       "Africa",
  "Tanzania",                         "Tanzania",                 "Sub-Saharan Africa",       "Africa",
  "TANZANIA",                         "Tanzania",                 "Sub-Saharan Africa",       "Africa",
  "Zambia",                           "Zambia",                   "Sub-Saharan Africa",       "Africa",
  "ZAMBIA",                           "Zambia",                   "Sub-Saharan Africa",       "Africa",
  "Zimbabwe",                         "Zimbabwe",                 "Sub-Saharan Africa",       "Africa",

  # --- AMERICAS ---
  # Caribbean
  "Cuba",                             "Cuba",                     "Caribbean",                "Americas",
  "CUBA",                             "Cuba",                     "Caribbean",                "Americas",
  "Dominican Republic",               "Dominican Republic",       "Caribbean",                "Americas",
  "DOMINICAN REPUBLIC",               "Dominican Republic",       "Caribbean",                "Americas",
  "Grenada",                          "Grenada",                  "Caribbean",                "Americas",
  "Haiti",                            "Haiti",                    "Caribbean",                "Americas",
  "HAITI",                            "Haiti",                    "Caribbean",                "Americas",
  "Puerto Rico",                      "Puerto Rico",              "Caribbean",                "Americas",
  "PUERTO RICO",                      "Puerto Rico",              "Caribbean",                "Americas",
  "Saint Lucia",                      "Saint Lucia",              "Caribbean",                "Americas",
  "Trinidad and Tobago",              "Trinidad and Tobago",      "Caribbean",                "Americas",

  # Central America
  "Costa Rica",                       "Costa Rica",               "Central America",          "Americas",
  "COSTA RICA",                       "Costa Rica",               "Central America",          "Americas",
  "El Salvador",                      "El Salvador",              "Central America",          "Americas",
  "EL SALVADOR",                      "El Salvador",              "Central America",          "Americas",
  "Guatemala",                        "Guatemala",                "Central America",          "Americas",
  "GUATEMALA",                        "Guatemala",                "Central America",          "Americas",
  "Mexico",                           "Mexico",                   "Central America",          "Americas",
  "MEXICO",                           "Mexico",                   "Central America",          "Americas",
  "Nicaragua",                        "Nicaragua",                "Central America",          "Americas",
  "NICARAGUA",                        "Nicaragua",                "Central America",          "Americas",
  "Panama",                           "Panama",                   "Central America",          "Americas",
  "PANAMA",                           "Panama",                   "Central America",          "Americas",

  # South America
  "Argentina",                        "Argentina",                "South America",            "Americas",
  "ARGENTINA",                        "Argentina",                "South America",            "Americas",
  "Bolivia",                          "Bolivia",                  "South America",            "Americas",
  "BOLIVIA",                          "Bolivia",                  "South America",            "Americas",
  "Brazil",                           "Brazil",                   "South America",            "Americas",
  "BRAZIL",                           "Brazil",                   "South America",            "Americas",
  "Chile",                            "Chile",                    "South America",            "Americas",
  "CHILE",                            "Chile",                    "South America",            "Americas",
  "Colombia",                         "Colombia",                 "South America",            "Americas",
  "COLOMBIA",                         "Colombia",                 "South America",            "Americas",
  "Ecuador",                          "Ecuador",                  "South America",            "Americas",
  "ECUADOR",                          "Ecuador",                  "South America",            "Americas",
  "Peru",                             "Peru",                     "South America",            "Americas",
  "PERU",                             "Peru",                     "South America",            "Americas",
  "Uruguay",                          "Uruguay",                  "South America",            "Americas",
  "URUGUAY",                          "Uruguay",                  "South America",            "Americas",
  "Venezuela",                        "Venezuela",                "South America",            "Americas",
  "VENEZUELA",                        "Venezuela",                "South America",            "Americas",

  # Northern America
  "Bermuda",                          "Bermuda",                  "Northern America",         "Americas",
  "Canada",                           "Canada",                   "Northern America",         "Americas",
  "CANADA",                           "Canada",                   "Northern America",         "Americas",
  "Greenland",                        "Greenland",                "Northern America",         "Americas",
  "United States",                    "United States",            "Northern America",         "Americas",
  "UNITED STATES",                    "United States",            "Northern America",         "Americas",

  # --- ASIA ---
  # Central Asia
  "Mongolia",                         "Mongolia",                 "Central Asia",             "Asia",
  "MONGOLIA",                         "Mongolia",                 "Central Asia",             "Asia",

  # Eastern Asia
  "China",                            "China",                    "Eastern Asia",             "Asia",
  "CHINA",                            "China",                    "Eastern Asia",             "Asia",
  "People's Republic of China",       "China",                    "Eastern Asia",             "Asia",
  "Republic of China",                "China",                    "Eastern Asia",             "Asia",
  "Manchukuo",                        "China",                    "Eastern Asia",             "Asia",
  "HONG KONG",                        "China",                    "Eastern Asia",             "Asia",
  "Japan",                            "Japan",                    "Eastern Asia",             "Asia",
  "JAPAN",                            "Japan",                    "Eastern Asia",             "Asia",
  "Empire of Japan",                  "Japan",                    "Eastern Asia",             "Asia",
  "Taiwan under Japanese rule",       "Taiwan",                   "Eastern Asia",             "Asia",
  "TAIWAN",                           "Taiwan",                   "Eastern Asia",             "Asia",
  "North Korea",                      "North Korea",              "Eastern Asia",             "Asia",
  "South Korea",                      "South Korea",              "Eastern Asia",             "Asia",
  "KOREA",                            "South Korea",              "Eastern Asia",             "Asia",
  "SOUTH KOREA",                      "South Korea",              "Eastern Asia",             "Asia",

  # South-Eastern Asia
  "Indonesia",                        "Indonesia",                "South-Eastern Asia",       "Asia",
  "Dutch East Indies",                "Indonesia",                "South-Eastern Asia",       "Asia",
  "Malaysia",                         "Malaysia",                 "South-Eastern Asia",       "Asia",
  "MALAYSIA",                         "Malaysia",                 "South-Eastern Asia",       "Asia",
  "Myanmar",                          "Myanmar",                  "South-Eastern Asia",       "Asia",
  "BURMA",                            "Myanmar",                  "South-Eastern Asia",       "Asia",
  "Philippines",                      "Philippines",              "South-Eastern Asia",       "Asia",
  "PHILIPPINES",                      "Philippines",              "South-Eastern Asia",       "Asia",
  "Rattanakosin Kingdom",             "Thailand",                 "South-Eastern Asia",       "Asia",
  "THAILAND",                         "Thailand",                 "South-Eastern Asia",       "Asia",
  "Timor-Leste",                      "Timor-Leste",              "South-Eastern Asia",       "Asia",
  "Vietnam",                          "Vietnam",                  "South-Eastern Asia",       "Asia",
  "VIETNAM",                          "Vietnam",                  "South-Eastern Asia",       "Asia",
  "SOUTH VIETNAM",                    "Vietnam",                  "South-Eastern Asia",       "Asia",
  "CAMBODIA",                         "Cambodia",                 "South-Eastern Asia",       "Asia",
  "LAOS",                             "Laos",                     "South-Eastern Asia",       "Asia",
  "SINGAPORE",                        "Singapore",                "South-Eastern Asia",       "Asia",

  # Southern Asia
  "Afghanistan",                      "Afghanistan",              "Southern Asia",            "Asia",
  "AFGHANISTAN",                      "Afghanistan",              "Southern Asia",            "Asia",
  "Bangladesh",                       "Bangladesh",               "Southern Asia",            "Asia",
  "BANGLADESH",                       "Bangladesh",               "Southern Asia",            "Asia",
  "India",                            "India",                    "Southern Asia",            "Asia",
  "INDIA",                            "India",                    "Southern Asia",            "Asia",
  "Pakistan",                         "Pakistan",                 "Southern Asia",            "Asia",
  "PAKISTAN",                          "Pakistan",                 "Southern Asia",            "Asia",
  "Sri Lanka",                        "Sri Lanka",                "Southern Asia",            "Asia",
  "SRI LANKA",                        "Sri Lanka",                "Southern Asia",            "Asia",
  "Ceylon",                           "Sri Lanka",                "Southern Asia",            "Asia",
  "CEYLON",                           "Sri Lanka",                "Southern Asia",            "Asia",

  # Western Asia
  "Armenia",                          "Armenia",                  "Western Asia",             "Asia",
  "ARMENIA",                          "Armenia",                  "Western Asia",             "Asia",
  "Azerbaijan",                       "Azerbaijan",               "Western Asia",             "Asia",
  "Georgia",                          "Georgia",                  "Western Asia",             "Asia",
  "Iran",                             "Iran",                     "Western Asia",             "Asia",
  "IRAN",                             "Iran",                     "Western Asia",             "Asia",
  "Iraq",                             "Iraq",                     "Western Asia",             "Asia",
  "Israel",                           "Israel",                   "Western Asia",             "Asia",
  "ISRAEL",                           "Israel",                   "Western Asia",             "Asia",
  "Jordan",                           "Jordan",                   "Western Asia",             "Asia",
  "JORDAN",                           "Jordan",                   "Western Asia",             "Asia",
  "Lebanon",                          "Lebanon",                  "Western Asia",             "Asia",
  "LEBANON",                          "Lebanon",                  "Western Asia",             "Asia",
  "Palestine",                        "Palestine",                "Western Asia",             "Asia",
  "PALESTINIAN TERRITORY",            "Palestine",                "Western Asia",             "Asia",
  "Syria",                            "Syria",                    "Western Asia",             "Asia",
  "SYRIAN ARAB REPUBLIC",             "Syria",                    "Western Asia",             "Asia",
  "Turkey",                           "Turkey",                   "Western Asia",             "Asia",
  "TURKEY",                           "Turkey",                   "Western Asia",             "Asia",
  "Yemen",                            "Yemen",                    "Western Asia",             "Asia",
  "KUWAIT",                           "Kuwait",                   "Western Asia",             "Asia",
  "SAUDI ARABIA",                     "Saudi Arabia",             "Western Asia",             "Asia",
  "UNITED ARAB EMIRATES",             "UAE",                      "Western Asia",             "Asia",

  # --- EUROPE ---
  # Eastern Europe
  "Belarus",                          "Belarus",                  "Eastern Europe",           "Europe",
  "Bulgaria",                         "Bulgaria",                 "Eastern Europe",           "Europe",
  "BULGARIA",                         "Bulgaria",                 "Eastern Europe",           "Europe",
  "Czech Republic",                   "Czech Republic",           "Eastern Europe",           "Europe",
  "CZECHOSLOVAKIA",                   "Czech Republic",           "Eastern Europe",           "Europe",
  "Hungary",                          "Hungary",                  "Eastern Europe",           "Europe",
  "HUNGARY",                          "Hungary",                  "Eastern Europe",           "Europe",
  "Moldova",                          "Moldova",                  "Eastern Europe",           "Europe",
  "Poland",                           "Poland",                   "Eastern Europe",           "Europe",
  "POLAND",                           "Poland",                   "Eastern Europe",           "Europe",
  "Romania",                          "Romania",                  "Eastern Europe",           "Europe",
  "ROMANIA",                          "Romania",                  "Eastern Europe",           "Europe",
  "Russia",                           "Russia",                   "Eastern Europe",           "Europe",
  "RUSSIAN FEDERATION",               "Russia",                   "Eastern Europe",           "Europe",
  "Russian Empire",                   "Russia",                   "Eastern Europe",           "Europe",
  "Soviet Union",                     "Russia",                   "Eastern Europe",           "Europe",
  "U.S.S.R.",                         "Russia",                   "Eastern Europe",           "Europe",
  "Slovakia",                         "Slovakia",                 "Eastern Europe",           "Europe",
  "Ukraine",                          "Ukraine",                  "Eastern Europe",           "Europe",
  "UKRAINE",                          "Ukraine",                  "Eastern Europe",           "Europe",
  "Free City of Danzig",              "Poland",                   "Eastern Europe",           "Europe",

  # Northern Europe
  "Denmark",                          "Denmark",                  "Northern Europe",          "Europe",
  "DENMARK",                          "Denmark",                  "Northern Europe",          "Europe",
  "Denmark\u2013Norway",              "Denmark",                  "Northern Europe",          "Europe",
  "Kingdom of Denmark",               "Denmark",                  "Northern Europe",          "Europe",
  "Estonia",                          "Estonia",                  "Northern Europe",          "Europe",
  "ESTONIA",                          "Estonia",                  "Northern Europe",          "Europe",
  "Faroe Islands",                    "Faroe Islands",            "Northern Europe",          "Europe",
  "FAROE ISLANDS",                    "Faroe Islands",            "Northern Europe",          "Europe",
  "Finland",                          "Finland",                  "Northern Europe",          "Europe",
  "FINLAND",                          "Finland",                  "Northern Europe",          "Europe",
  "Iceland",                          "Iceland",                  "Northern Europe",          "Europe",
  "ICELAND",                          "Iceland",                  "Northern Europe",          "Europe",
  "Ireland",                          "Ireland",                  "Northern Europe",          "Europe",
  "IRELAND",                          "Ireland",                  "Northern Europe",          "Europe",
  "Latvia",                           "Latvia",                   "Northern Europe",          "Europe",
  "LATVIA",                           "Latvia",                   "Northern Europe",          "Europe",
  "Lithuania",                        "Lithuania",                "Northern Europe",          "Europe",
  "Norway",                           "Norway",                   "Northern Europe",          "Europe",
  "NORWAY",                           "Norway",                   "Northern Europe",          "Europe",
  "Kingdom of Norway",                "Norway",                   "Northern Europe",          "Europe",
  "Sweden",                           "Sweden",                   "Northern Europe",          "Europe",
  "SWEDEN",                           "Sweden",                   "Northern Europe",          "Europe",
  "United Kingdom",                   "United Kingdom",           "Northern Europe",          "Europe",
  "UNITED KINGDOM",                   "United Kingdom",           "Northern Europe",          "Europe",
  "Kingdom of England",               "United Kingdom",           "Northern Europe",          "Europe",
  "British Empire",                   "United Kingdom",           "Northern Europe",          "Europe",

  # Southern Europe
  "Albania",                          "Albania",                  "Southern Europe",          "Europe",
  "Bosnia and Herzegovina",           "Bosnia and Herzegovina",   "Southern Europe",          "Europe",
  "Croatia",                          "Croatia",                  "Southern Europe",          "Europe",
  "CROATIA",                          "Croatia",                  "Southern Europe",          "Europe",
  "Greece",                           "Greece",                   "Southern Europe",          "Europe",
  "GREECE",                           "Greece",                   "Southern Europe",          "Europe",
  "Italy",                            "Italy",                    "Southern Europe",          "Europe",
  "ITALY",                            "Italy",                    "Southern Europe",          "Europe",
  "Malta",                            "Malta",                    "Southern Europe",          "Europe",
  "MALTA",                            "Malta",                    "Southern Europe",          "Europe",
  "Montenegro",                       "Montenegro",               "Southern Europe",          "Europe",
  "North Macedonia",                  "North Macedonia",          "Southern Europe",          "Europe",
  "Portugal",                         "Portugal",                 "Southern Europe",          "Europe",
  "PORTUGAL",                         "Portugal",                 "Southern Europe",          "Europe",
  "Serbia",                           "Serbia",                   "Southern Europe",          "Europe",
  "SERBIA",                           "Serbia",                   "Southern Europe",          "Europe",
  "Slovenia",                         "Slovenia",                 "Southern Europe",          "Europe",
  "Spain",                            "Spain",                    "Southern Europe",          "Europe",
  "SPAIN",                            "Spain",                    "Southern Europe",          "Europe",
  "Yugoslavia",                       "Serbia",                   "Southern Europe",          "Europe",
  "YUGOSLAVIA",                       "Serbia",                   "Southern Europe",          "Europe",

  # Western Europe
  "Austria",                          "Austria",                  "Western Europe",           "Europe",
  "AUSTRIA",                          "Austria",                  "Western Europe",           "Europe",
  "Austrian Empire",                  "Austria",                  "Western Europe",           "Europe",
  "Austria\u2013Hungary",             "Austria",                  "Western Europe",           "Europe",
  "AUSTRIA-HUNGARY",                  "Austria",                  "Western Europe",           "Europe",
  "Habsburg monarchy",                "Austria",                  "Western Europe",           "Europe",
  "Belgium",                          "Belgium",                  "Western Europe",           "Europe",
  "BELGIUM",                          "Belgium",                  "Western Europe",           "Europe",
  "France",                           "France",                   "Western Europe",           "Europe",
  "FRANCE",                           "France",                   "Western Europe",           "Europe",
  "Germany",                          "Germany",                  "Western Europe",           "Europe",
  "GERMANY",                          "Germany",                  "Western Europe",           "Europe",
  "German Empire",                    "Germany",                  "Western Europe",           "Europe",
  "Nazi Germany",                     "Germany",                  "Western Europe",           "Europe",
  "Prussia",                          "Germany",                  "Western Europe",           "Europe",
  "Luxembourg",                       "Luxembourg",               "Western Europe",           "Europe",
  "Monaco",                           "Monaco",                   "Western Europe",           "Europe",
  "Netherlands",                      "Netherlands",              "Western Europe",           "Europe",
  "NETHERLANDS",                      "Netherlands",              "Western Europe",           "Europe",
  "Switzerland",                      "Switzerland",              "Western Europe",           "Europe",
  "SWITZERLAND",                      "Switzerland",              "Western Europe",           "Europe",

  # --- OCEANIA ---
  "Australia",                        "Australia",                "Australia and New Zealand", "Oceania",
  "AUSTRALIA",                        "Australia",                "Australia and New Zealand", "Oceania",
  "New Zealand",                      "New Zealand",              "Australia and New Zealand", "Oceania",
  "NEW ZEALAND",                      "New Zealand",              "Australia and New Zealand", "Oceania"
)


# =============================================================================
# 2. Helper function: clean and map a country name
# =============================================================================

#' Pre-clean a nomination-archive country value before mapping
#' Strips legacy "now COUNTRY" annotations, ISO codes, and quotes
clean_nomination_country <- function(x) {
  x %>%
    # Remove leading/trailing quotes
    str_remove_all('^"|"$') %>%
    # Strip " (XX) now COUNTRY" legacy annotations → keep the original name
    str_remove("\\s*\\([A-Z]{2}\\)\\s*now\\s+.*$") %>%
    # Strip any remaining ISO code suffixes: " (XX)"
    str_remove("\\s*\\([A-Z]{2}\\)\\s*$") %>%
    str_squish()
}

#' Detect clearly invalid country values (city names, partial text, etc.)
#' Returns TRUE if value appears invalid
is_invalid_country <- function(x) {
  # Known invalid patterns: starts with lowercase, is a known city/artifact,
  # or contains telltale fragments
  invalid_patterns <- c(
    "^[a-z]",            # Starts with lowercase (city names from CSV misalignment)
    "^\\s",              # Starts with whitespace (artifact)
    "Institute",         # Institutional name fragments
    "Academy",           # Institutional name fragments
    "Association",       # Institutional name fragments
    "Committee",         # Institutional name fragments
    "University",        # Institutional name fragments
    "explorer",          # Profession fragments
    "statesman",         # Profession fragments
    "poet",              # Profession fragments
    "activist",          # Profession fragments
    "Tübingen",          # City artifacts
    "Tucker"             # Person name artifacts
  )
  str_detect(x, paste(invalid_patterns, collapse = "|"))
}

#' Map a vector of country names to the reference table
#' Returns a tibble with modern_country, un_subregion, continent
map_countries <- function(names_vec, mapping = country_mapping) {
  tibble(raw_name = names_vec) %>%
    left_join(mapping, by = "raw_name")
}


# =============================================================================
# 3. Enrich demographics.csv
# =============================================================================
message("=== Enriching demographics.csv ===\n")

demo_file <- data_path("demographics.csv")
demo <- read_csv(demo_file, show_col_types = FALSE)
message(sprintf("  Loaded demographics: %d rows", nrow(demo)))

# Map birth_country
demo_geo <- demo %>%
  left_join(
    country_mapping %>% select(raw_name, modern_country, un_subregion, continent),
    by = c("birth_country" = "raw_name")
  ) %>%
  rename(
    birth_country_modern = modern_country,
    birth_subregion = un_subregion,
    birth_continent = continent
  )

# Map nationality (semicolon-separated: split, map each, re-collapse)
map_nationality <- function(nat_string) {
  if (is.na(nat_string)) return(NA_character_)
  parts <- str_split(nat_string, ";\\s*")[[1]]
  mapped <- country_mapping$modern_country[match(parts, country_mapping$raw_name)]
  # For unmapped parts, keep the original
  mapped <- ifelse(is.na(mapped), parts, mapped)
  # Deduplicate and collapse
  paste(unique(mapped), collapse = "; ")
}
demo_geo$nationality_modern <- vapply(demo$nationality, map_nationality, character(1))

# Report coverage
n_mapped_bc <- sum(!is.na(demo_geo$birth_country_modern) & !is.na(demo_geo$birth_country))
n_with_bc <- sum(!is.na(demo_geo$birth_country))
message(sprintf("  Birth country mapped: %d / %d (%.1f%%)",
                n_mapped_bc, n_with_bc, 100 * n_mapped_bc / n_with_bc))

unmapped_bc <- demo_geo %>%
  filter(!is.na(birth_country), is.na(birth_country_modern)) %>%
  count(birth_country, sort = TRUE)
if (nrow(unmapped_bc) > 0) {
  message("  Unmapped birth_country values:")
  for (i in seq_len(nrow(unmapped_bc))) {
    message(sprintf("    %s (%d)", unmapped_bc$birth_country[i], unmapped_bc$n[i]))
  }
}

# Write enriched demographics
write_csv(demo_geo, demo_file)
message(sprintf("  Wrote enriched demographics: %s", demo_file))


# =============================================================================
# 4. Enrich nominations.csv
# =============================================================================
message("\n=== Enriching nominations.csv ===\n")

nom_file <- data_path("nominations.csv")
nom <- read_csv(nom_file, show_col_types = FALSE)
message(sprintf("  Loaded nominations: %d rows", nrow(nom)))

# Pre-clean nomination country fields
nom <- nom %>%
  mutate(
    nominee_country_clean = clean_nomination_country(nominee_country),
    nominator_country_clean = clean_nomination_country(nominator_country)
  )

# Flag invalid values (city names, partial text, etc.)
nom <- nom %>%
  mutate(
    nominee_country_clean = ifelse(
      !is.na(nominee_country_clean) & is_invalid_country(nominee_country_clean),
      NA_character_, nominee_country_clean),
    nominator_country_clean = ifelse(
      !is.na(nominator_country_clean) & is_invalid_country(nominator_country_clean),
      NA_character_, nominator_country_clean)
  )

n_invalidated_nominee <- sum(
  !is.na(nom$nominee_country) & is.na(nom$nominee_country_clean), na.rm = TRUE)
n_invalidated_nominator <- sum(
  !is.na(nom$nominator_country) & is.na(nom$nominator_country_clean), na.rm = TRUE)
message(sprintf("  Invalidated: %d nominee_country, %d nominator_country values (artifacts)",
                n_invalidated_nominee, n_invalidated_nominator))

# Map nominee country
nom <- nom %>%
  left_join(
    country_mapping %>% select(raw_name, modern_country, un_subregion, continent),
    by = c("nominee_country_clean" = "raw_name")
  ) %>%
  rename(
    nominee_country_modern = modern_country,
    nominee_subregion = un_subregion,
    nominee_continent = continent
  )

# Map nominator country
nom <- nom %>%
  left_join(
    country_mapping %>% select(raw_name, modern_country, un_subregion, continent),
    by = c("nominator_country_clean" = "raw_name")
  ) %>%
  rename(
    nominator_country_modern = modern_country,
    nominator_subregion = un_subregion,
    nominator_continent = continent
  )

# Drop the intermediate _clean columns
nom <- nom %>% select(-nominee_country_clean, -nominator_country_clean)

# Report coverage
for (role in c("nominee", "nominator")) {
  orig_col <- paste0(role, "_country")
  mapped_col <- paste0(role, "_country_modern")
  n_with <- sum(!is.na(nom[[orig_col]]))
  n_mapped <- sum(!is.na(nom[[mapped_col]]) & !is.na(nom[[orig_col]]))
  message(sprintf("  %s country mapped: %d / %d (%.1f%%)",
                  str_to_title(role), n_mapped, n_with, 100 * n_mapped / n_with))

  unmapped <- nom %>%
    filter(!is.na(!!sym(orig_col)), is.na(!!sym(mapped_col))) %>%
    mutate(cleaned = clean_nomination_country(!!sym(orig_col))) %>%
    count(cleaned, sort = TRUE)
  if (nrow(unmapped) > 0 && nrow(unmapped) <= 20) {
    message(sprintf("  Unmapped %s country values:", role))
    for (i in seq_len(nrow(unmapped))) {
      message(sprintf("    %s (%d)", unmapped$cleaned[i], unmapped$n[i]))
    }
  } else if (nrow(unmapped) > 20) {
    message(sprintf("  %d unmapped %s country values (showing top 20):", nrow(unmapped), role))
    for (i in seq_len(min(20, nrow(unmapped)))) {
      message(sprintf("    %s (%d)", unmapped$cleaned[i], unmapped$n[i]))
    }
  }
}

# Write enriched nominations
write_csv(nom, nom_file)
message(sprintf("  Wrote enriched nominations: %s", nom_file))


# =============================================================================
# 5. Summary
# =============================================================================
message("\n=== Geography Standardization Summary ===\n")

# Demographics summary
demo_summary <- demo_geo %>%
  filter(!is.na(birth_country_modern)) %>%
  count(birth_continent, birth_subregion, sort = TRUE)
message("  Demographics — birth country by continent/subregion:")
for (i in seq_len(nrow(demo_summary))) {
  message(sprintf("    %-12s / %-25s : %d",
                  demo_summary$birth_continent[i],
                  demo_summary$birth_subregion[i],
                  demo_summary$n[i]))
}

# Nominations summary
nom_summary <- nom %>%
  filter(!is.na(nominee_subregion)) %>%
  count(nominee_continent, nominee_subregion, sort = TRUE)
message("\n  Nominations — nominee country by continent/subregion:")
for (i in seq_len(nrow(nom_summary))) {
  message(sprintf("    %-12s / %-25s : %d",
                  nom_summary$nominee_continent[i],
                  nom_summary$nominee_subregion[i],
                  nom_summary$n[i]))
}

message("\n=== DONE ===")

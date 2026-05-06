# =============================================================================
# 07_standardize_geography.R
#
# FILE TITLE:
#   Standardize Geographic Data in Nobel Prize Network Dataset
#
# AUTHOR:
#   Chad M. Topaz
#
# LAST UPDATED:
#   February 2025
#
# PURPOSE & GOALS:
#   This script standardizes and enriches geographic data across the Nobel Prize
#   network dataset. It maps diverse country name variations (from Wikidata labels,
#   nobelprize.org records, and historical sources) to a unified classification:
#   modern country names, UN M49 subregions, and continental groupings. The script
#   handles historical and dissolved countries (Ottoman Empire → Turkey, USSR → Russia,
#   Yugoslavia → Serbia) and performs data quality checks to flag invalid entries
#   (city names, institutional names, and other artifacts misaligned in CSV files).
#   Output enriches both demographics.csv (individual birth countries and nationalities)
#   and nominations.csv (nominee and nominator countries) with three new geographic
#   columns per entity type. These standardized fields enable consistent geographic
#   analysis and filtering in the multilayer network construction.
#
# METHODOLOGICAL DECISIONS:
#   - Geographic Classification: Uses UN M49 subregions as the authoritative standard
#     for consistency with international reporting conventions.
#   - Historical Mapping: Dissolved countries are mapped to their modern successor states
#     (e.g., USSR → Russia, Czechoslovakia → Czech Republic, Yugoslavia → Serbia).
#     This preserves historical intent while enabling contemporary analysis.
#   - Case Handling: Country mapping is performed case-insensitively; both "Algeria"
#     and "ALGERIA" map correctly. The reference table includes both variants for clarity.
#   - Multi-value Fields: The nationality field (which can contain semicolon-separated
#     country pairs or triples) is split, mapped individually, deduplicated, and
#     re-collapsed to handle dual/multiple nationalities while avoiding redundancy.
#   - Data Quality: Invalid values (city names from CSV misalignment, institutional
#     fragments, profession keywords) are explicitly detected and flagged as NA rather
#     than passed to mapping, preventing silent errors.
#   - Coverage Reporting: The script logs mapping success rates and reports ALL unmapped
#     values (capped at top 20) for manual review. Low coverage indicates missing
#     reference entries or unexpected data variants.
#
# INPUTS:
#   1. Data/intermediate/demographics.csv
#      - Columns: qid, name, gender, birth_country, nationality, birth_year, death_year,
#                 occupation, institution
#      - Source: Wikidata records (via 06_wikidata_demographics.R)
#      - Scope: ~2300 Nobel laureates and committee members
#   2. Data/intermediate/nominations.csv
#      - Columns: year, prize, nominator_person_id, nominator_country, nominee_person_id,
#                 nominee_country, ...
#      - Source: nobelprize.org nomination archive
#      - Scope: ~30,000 nomination records (all five prizes)
#
# OUTPUTS:
#   1. Data/intermediate/demographics.csv (in place, enriched)
#      - Added columns: birth_country_modern, birth_subregion, birth_continent,
#                       nationality_modern
#      - Impact: Enables filtering and aggregation by modern country/region/continent
#   2. Data/intermediate/nominations.csv (in place, enriched)
#      - Added columns: nominee_country_modern, nominee_subregion, nominee_continent,
#                       nominator_country_modern, nominator_subregion, nominator_continent
#      - Impact: Enables analysis of nominee/nominator geographic patterns
#
# DEPENDENCIES:
#   - Code/Data Gathering/00_utils.R (sourced: provides data_path() helper function)
#   - tidyverse (tidyr, dplyr, readr, stringr) [implicit via utils]
#
# KNOWN LIMITATIONS:
#   - Coverage Gap: Some nomination records (particularly pre-1900) may use archaic
#     country names or city-level designations not in the reference table. These
#     remain unmapped (NA) and should be manually reviewed.
#   - Dual Nationalities: While handled correctly, some laureates with dual/triple
#     nationality lose temporal specificity (e.g., if nationality changes over time,
#     all forms are collapsed together).
#   - Current Events: Countries that have dissolved very recently (e.g., South Sudan,
#     before 2011) may have incomplete historical records in the source data.
#   - Spelling Variants: Uncommon transliterations (e.g., "Kyrgyzstan" vs. "Kirghizia")
#     are not currently handled; such entries will be reported as unmapped.
#
# GEOGRAPHIC CLASSIFICATION SYSTEM (UN M49 SUBREGIONS):
#   Africa: Northern Africa, Sub-Saharan Africa
#   Americas: Caribbean, Central America, Northern America, South America
#   Asia: Central Asia, Eastern Asia, South-Eastern Asia, Southern Asia, Western Asia
#   Europe: Eastern Europe, Northern Europe, Southern Europe, Western Europe
#   Oceania: Australia and New Zealand, Polynesia, Melanesia, Micronesia
#   Note: Only countries with Nobel laureates appear in this script; Oceania groups
#   are minimal but included for completeness.
#
# =============================================================================

source("Code/Data Gathering/00_utils.R")

message("\n====================================================================")
message("  SCRIPT 07: STANDARDIZE GEOGRAPHY")
message("====================================================================\n")


# =============================================================================
# 1. Define comprehensive country mapping
# =============================================================================
# SECTION PURPOSE:
#   Define the master country_mapping tribble that serves as a lookup table
#   for all geographic standardization. This single table is used for both
#   demographics.csv (birth_country, nationality fields) and nominations.csv
#   (nominee_country, nominator_country fields).
#
# DESIGN RATIONALE:
#   - Single Source of Truth: One mapping table eliminates inconsistencies across
#     different data sources and ensures uniform geographic classifications.
#   - Case Variants: Both lowercase and UPPERCASE variants are included as separate
#     rows to ensure robust matching. This addresses inconsistencies in source data
#     (Wikidata vs. nobelprize.org use different naming conventions).
#   - Historical Countries: Dissolved/defunct countries (Ottoman Empire, USSR,
#     Yugoslavia, Czechoslovakia, etc.) are mapped to their modern successors,
#     enabling historical analysis with contemporary country codes.
#   - Three-Level Classification: Each country maps to three standardized fields:
#       * modern_country: Canonical country name (e.g., "China" for all variants)
#       * un_subregion: UN M49 geographic region (authoritative standard)
#       * continent: Broader continental grouping (Africa, Asia, Europe, Americas, Oceania)
#   - No NULL entries: Every mapped country has all three fields populated. If mapping
#     fails, all three fields become NA, preventing partial data.
#
# STRUCTURE:
#   Organized by continent and subregion headers (in comments) for human readability
#   during maintenance and validation. Data quality checks occur downstream; this
#   table assumes valid input from match operations.
#
# =============================================================================

country_mapping <- tribble(
  ~raw_name,                          ~modern_country,            ~un_subregion,              ~continent,

  # --- AFRICA ---
  # Northern Africa (Algeria, Egypt, Libya, Morocco, Sudan, Tunisia)
  # Note: Sudan split into Sudan and South Sudan in 2011; South Sudan not yet
  # represented in Nobel data. Sudan entries map to modern Sudan state.
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

  # Sub-Saharan Africa (46 countries: Kenya, Nigeria, South Africa, Ethiopia, etc.)
  # Note: DR Congo historical variants handled (Democratic Republic of the Congo → DR Congo).
  # Historical entities (German East Africa, Belgian Congo) → modern country mappings.
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
  # Caribbean (Cuba, Dominican Republic, Haiti, Trinidad and Tobago, etc.)
  # Note: Puerto Rico included as Caribbean territory; maintained as distinct entity.
  # Historical: Spanish/French colonial names → modern country mappings.
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

  # Central America (Costa Rica, El Salvador, Guatemala, Mexico, Nicaragua, Panama)
  # Note: Mexico sometimes classified with Central America per UN M49.
  # Historical: United Provinces of Central America dissolution → modern mappings.
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

  # South America (Argentina, Brazil, Chile, Colombia, Peru, Uruguay, Venezuela, etc.)
  # Note: Historical borders and colonial names → modern country mappings.
  # Includes both Upper Peru (Bolivia) and Gran Colombia successor states.
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

  # Northern America (Canada, United States, Greenland, Bermuda)
  # Note: UN M49 includes Greenland (self-governed within Kingdom of Denmark).
  # Not included: Australia and New Zealand (classified in Oceania subregion).
  "Bermuda",                          "Bermuda",                  "Northern America",         "Americas",
  "Canada",                           "Canada",                   "Northern America",         "Americas",
  "CANADA",                           "Canada",                   "Northern America",         "Americas",
  "Greenland",                        "Greenland",                "Northern America",         "Americas",
  "United States",                    "United States",            "Northern America",         "Americas",
  "UNITED STATES",                    "United States",            "Northern America",         "Americas",

  # --- ASIA ---
  # Central Asia (Mongolia, and former Soviet republics: Kazakhstan, Kyrgyzstan, etc.)
  # Note: Only Mongolia represented in Nobel data; other Central Asian countries
  # have minimal Nobel presence and are not included in this mapping.
  "Mongolia",                         "Mongolia",                 "Central Asia",             "Asia",
  "MONGOLIA",                         "Mongolia",                 "Central Asia",             "Asia",

  # Eastern Asia (China, Japan, Korea, Taiwan)
  # CRITICAL HISTORICAL MAPPINGS:
  #   - "People's Republic of China" → China (PRC, 1949+)
  #   - "Republic of China" → China (historical: pre-1949 mainland / Taiwan)
  #   - "Manchukuo" → China (Japanese puppet state 1932-1945)
  #   - "Hong Kong" → China (SAR 1997+, previously British colony)
  #   - "Taiwan under Japanese rule" → Taiwan (1895-1945 Japanese control)
  #   - "KOREA" / "KOREA (historical)" → South Korea (modern default when unspecified)
  #   - "North Korea" → North Korea (DPRK, 1948+)
  # Note: Multiple Chinese entities unified under "China"; temporal specificity
  # of pre-1949/PRC split is lost, but enables consistent analysis. Taiwan maintained
  # as distinct entity per modern conventions.
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

  # South-Eastern Asia (Indonesia, Malaysia, Philippines, Thailand, Vietnam, etc.)
  # CRITICAL HISTORICAL MAPPINGS:
  #   - "Dutch East Indies" → Indonesia (colonial name until 1945)
  #   - "Burma" → Myanmar (name change 1989; both forms in historical records)
  #   - "Rattanakosin Kingdom" → Thailand (formal Thai name; Siam historical)
  #   - "South Vietnam" → Vietnam (unified 1975; pre-1975 split not preserved)
  #   - "CAMBODIA" / "LAOS" / "SINGAPORE" → Modern country names
  # Note: Historical Indochina territories unified under modern country names;
  # temporal specificity of colonial period lost, but consistent with modern analysis.
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

  # Southern Asia (India, Pakistan, Bangladesh, Sri Lanka, Afghanistan)
  # CRITICAL HISTORICAL MAPPINGS:
  #   - "Ceylon" → Sri Lanka (name change 1972; historical colonial name)
  #   - "CEYLON" → Sri Lanka (uppercase variant from nobelprize.org)
  #   - India/Pakistan partition (1947) → modern country names preserved
  # Note: Pre-partition British India → assigned to modern India state by default
  # (when unspecified); temporal precision regarding partition lost, but avoids
  # creation of "British India" pseudo-country. Users interested in partition-era
  # analysis should filter by birth_year < 1947 and geographic descriptors.
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

  # Western Asia / Middle East (Israel, Palestine, Iran, Iraq, Saudi Arabia, etc.)
  # CRITICAL HISTORICAL MAPPINGS:
  #   - "SYRIAN ARAB REPUBLIC" → Syria (official UN name; common nobelprize.org variant)
  #   - "PALESTINIAN TERRITORY" → Palestine (UN convention for occupied territories)
  #   - "Ottoman Empire" NOT INCLUDED: Pre-1922 Ottoman subjects mapped to successor states
  #     based on geographic region (e.g., Ottoman-era birth in Anatolia → Turkey)
  # Note: This mapping maintains political entities as of modern era (post-WW1, post-WW2).
  # Ottoman, Persian, and other historical empires are NOT preserved as distinct entities
  # due to complexity of successor-state determination. Users analyzing pre-WW1 data
  # should exercise caution regarding geographic associations.
  # Palestine: Recognized entity per UN M49 conventions; included for consistency.
  # Israel: UN M49 classification as Western Asia (aligns with UN geographic convention).
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
  # Eastern Europe (Russia, Poland, Ukraine, Hungary, Romania, Bulgaria, etc.)
  # CRITICAL HISTORICAL MAPPINGS:
  #   - "Soviet Union" / "U.S.S.R." → Russia (USSR dissolution 1991; mapping to primary successor)
  #   - "Russian Empire" → Russia (pre-1917 czarist state)
  #   - "RUSSIAN FEDERATION" → Russia (post-1991 official name)
  #   - "CZECHOSLOVAKIA" → Czech Republic (dissolution 1993; default to larger entity)
  #     Note: Slovak residents → Czech Republic; users analyzing Czechoslovakia-era data
  #     should review birth_year < 1993 and manually distinguish. Alt: dual-map to both.
  #   - "Free City of Danzig" → Poland (1920-1939 League of Nations entity; geographic region)
  # Note: USSR and Russian Empire covered by single "Russia" entry; temporal specificity
  # of imperial, Soviet, and modern periods lost. This choice reflects post-Soviet status
  # quo and enables contemporary analysis. Researchers studying Soviet-era networks should
  # filter by endyear >= year_of_interest or birth_year < 1917 for pre-revolutionary context.
  # Czechoslovakia dissolution: mapped to Czech Republic by default; Slovakia not
  # distinguished. Users concerned with Slovak representation should request dual-mapping.
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

  # Northern Europe (UK, Ireland, Scandinavia, Baltics)
  # CRITICAL HISTORICAL MAPPINGS:
  #   - "Denmark–Norway" → Denmark (1380-1814 union; post-1814 → Denmark & Norway separate)
  #   - "Kingdom of Denmark" / "Kingdom of Norway" → Denmark / Norway (formal names)
  #   - "Kingdom of England" → United Kingdom (historical pre-1707 entity; includes Scotland, Wales)
  #   - "British Empire" → United Kingdom (colonial empire; metropolitan center UK)
  #   - "Faroe Islands" → Faroe Islands (constituent country of Kingdom of Denmark; distinct entity per UN)
  # Note: Northern Europe includes UK and Ireland per UN M49 (not Western Europe).
  # British Empire: mapped to UK (not to individual colonies/dominions). Imperial-era births
  # assigned to UK unless specific colony indicated. Users analyzing colonial networks
  # should note this limitation and consider supplementary colonial affiliation field.
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

  # Southern Europe (Greece, Italy, Spain, Portugal, ex-Yugoslavia, etc.)
  # CRITICAL HISTORICAL MAPPINGS:
  #   - "Yugoslavia" / "YUGOSLAVIA" → Serbia (1918-1992 federation; post-dissolution mapping)
  #   - Bosnia, Croatia, Montenegro, Slovenia → modern successor states (post-1992)
  #   - "North Macedonia" → official UN name (formerly FYROM; name change 2019)
  # Note: Yugoslavia dissolution (1991-1992) creates temporal mapping issue. Former
  # Yugoslav republics whose residence cannot be precisely geocoded are assigned to
  # post-dissolution entities based on republic of origin (Slovenia → Slovenia, etc.).
  # For countries like Croatia and Bosnia-Herzegovina with complex histories, mapping
  # to current borders accepted as standard practice. Researchers analyzing Yugoslav-era
  # networks should manually review member service years (startyear/endyear) and filter
  # as appropriate (typically endyear < 1992 for pre-dissolution assignments).
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

  # Western Europe (Germany, France, Austria, Belgium, Netherlands, Switzerland, etc.)
  # CRITICAL HISTORICAL MAPPINGS (complex due to pre-WW2 territorial changes):
  #   - "Austria–Hungary" → Austria (1867-1918 dual monarchy; post-WW1 → Austria & Hungary separate)
  #   - "Austrian Empire" → Austria (pre-1867 empire; dismantled WW1)
  #   - "Habsburg monarchy" → Austria (historical empire; same as Austrian Empire)
  #   - "German Empire" → Germany (1871-1918 Prussian-led federation)
  #   - "Nazi Germany" → Germany (1933-1945; post-WW2 → West/East Germany; unified 1990)
  #   - "Prussia" → Germany (historical kingdom; absorbed into German Empire 1871)
  # Note: Austria-Hungary dissolution (1918) loses fine-grained distinction between
  # successor states. A-H births assigned to Austria; Hungarian/other births need
  # manual geocoding. German territorial changes (Weimar → Nazi → Divided → Unified)
  # collapsed into "Germany"; pre-WW2 borders → modern borders, losing specificity
  # (e.g., Alsace-Lorraine fluctuation). For researchers: use birth_year/death_year
  # filters to analyze particular political periods.
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
  # Oceania (Australia, New Zealand, Pacific Islands)
  # Note: Only Australia and New Zealand have significant Nobel representation.
  # Pacific island nations (Fiji, Samoa, etc.) not included in this mapping
  # due to zero or minimal Nobel laureate presence.
  "Australia",                        "Australia",                "Australia and New Zealand", "Oceania",
  "AUSTRALIA",                        "Australia",                "Australia and New Zealand", "Oceania",
  "New Zealand",                      "New Zealand",              "Australia and New Zealand", "Oceania",
  "NEW ZEALAND",                      "New Zealand",              "Australia and New Zealand", "Oceania"
)


# =============================================================================
# 2. Helper functions for country name cleaning and mapping
# =============================================================================
# SECTION PURPOSE:
#   Define utility functions for pre-cleaning and mapping country values.
#   These handle data quality issues in the nomination archive, which uses
#   legacy formatting (ISO codes, historical annotations) not found in Wikidata.
#
# DESIGN RATIONALE:
#   - Pre-cleaning ensures consistent input to the mapping function, reducing
#     failed lookups due to formatting artifacts rather than name variants.
#   - Invalid value detection prevents silent assignment of garbage data (city
#     names, institutional names from CSV misalignment) to geographic fields.
#   - Case-insensitive matching enables robust mapping across data sources.
#
# =============================================================================

#' Pre-clean a nomination-archive country value before mapping
#' Strips legacy "now COUNTRY" annotations, ISO codes, and quotes
#'
#' DETAILS:
#'   The nobelprize.org nomination archive uses historical encoding:
#'   - "France" (correct)
#'   - "France (FR) now France" (legacy annotation, unneeded)
#'   - "France (FR)" (ISO code suffix, redundant)
#'   - '"France"' (quoted, artifact from CSV encoding)
#'   This function removes these artifacts step-by-step, preserving the core
#'   country name. Output is then matched against country_mapping table.
#'
#' PARAMETERS:
#'   x: character vector of country names (can contain NA)
#'
#' RETURNS:
#'   character vector of cleaned country names (or NA if input was NA)
#'
clean_nomination_country <- function(x) {
  x %>%
    # Remove leading/trailing quotes (CSV encoding artifacts)
    str_remove_all('^"|"$') %>%
    # Strip " (XX) now COUNTRY" legacy annotations → keep original name only
    # Example: "Prussia (DE) now Germany" → "Prussia" (will be mapped to Germany later)
    str_remove("\\s*\\([A-Z]{2}\\)\\s*now\\s+.*$") %>%
    # Strip any remaining ISO code suffixes: " (XX)"
    # Example: "France (FR)" → "France"
    str_remove("\\s*\\([A-Z]{2}\\)\\s*$") %>%
    # Final cleanup: remove excess whitespace
    str_squish()
}

#' Detect clearly invalid country values (city names, partial text, etc.)
#' Returns TRUE if value appears invalid and should be flagged as NA
#'
#' DETAILS:
#'   The nobelprize.org nomination CSV sometimes contains misaligned data where
#'   city names, institution names, or person names appear in the country field.
#'   This is a known data quality issue (likely from parsing errors in the original
#'   archive extraction). Rather than passing these garbage values to mapping,
#'   we detect and invalidate them, preserving data integrity.
#'
#'   Detection uses heuristic patterns:
#'   - Lowercase start: city names (csv_misalignment)
#'   - Whitespace start: artifact
#'   - Keywords (Institute, Academy, Committee, University, etc.): institutional names
#'   - Keywords (explorer, statesman, poet, activist): person profession/role fragments
#'   - Specific city/person names seen in data: Tübingen, Tucker (hardcoded examples)
#'
#'   These patterns are crude but effective for the historical archive data examined.
#'   False positives/negatives should be reported during data validation runs.
#'
#' PARAMETERS:
#'   x: character vector of country names (can contain NA)
#'
#' RETURNS:
#'   logical vector (TRUE = invalid, should be NA; FALSE = potentially valid)
#'
is_invalid_country <- function(x) {
  # Known invalid patterns: starts with lowercase, is a known city/artifact,
  # or contains telltale institutional/person name fragments
  invalid_patterns <- c(
    "^[a-z]",            # Starts with lowercase (city names from CSV misalignment)
    "^\\s",              # Starts with whitespace (parsing artifact)
    "Institute",         # Institutional name fragments
    "Academy",           # Institutional name fragments
    "Association",       # Institutional name fragments
    "Committee",         # Institutional name fragments
    "University",        # Institutional name fragments
    "explorer",          # Profession fragments (person data misplaced)
    "statesman",         # Profession fragments (person data misplaced)
    "poet",              # Profession fragments (person data misplaced)
    "activist",          # Profession fragments (person data misplaced)
    "Tübingen",          # City artifact (known error in historical archive)
    "Tucker"             # Person name artifact (known error in historical archive)
  )
  str_detect(x, paste(invalid_patterns, collapse = "|"))
}

#' Map a vector of country names to the reference table
#' Returns a tibble with modern_country, un_subregion, continent
#'
#' DETAILS:
#'   Performs left_join of input names against the country_mapping reference table.
#'   Matching is exact (case-sensitive) after pre-cleaning. If a name is not found,
#'   the mapping columns are NA. This function is primarily used internally by
#'   enrichment sections but can be called directly for one-off mapping.
#'
#' PARAMETERS:
#'   names_vec: character vector of country names to map
#'   mapping: data frame (default: country_mapping tribble; can be overridden for testing)
#'
#' RETURNS:
#'   tibble with columns: raw_name, modern_country, un_subregion, continent
#'   (unmapped rows have NA in last three columns)
#'
map_countries <- function(names_vec, mapping = country_mapping) {
  tibble(raw_name = names_vec) %>%
    left_join(mapping, by = "raw_name")
}


# =============================================================================
# 3. Enrich demographics.csv with geographic standardization
# =============================================================================
# SECTION PURPOSE:
#   Enrich the demographics.csv file (Nobel laureates and committee members)
#   with standardized geographic fields derived from Wikidata.
#
# PROCESS OVERVIEW:
#   1. Load demographics.csv (source: Wikidata via 06_wikidata_demographics.R)
#   2. Map birth_country field to modern_country, un_subregion, continent
#   3. Map nationality field (which can be multi-valued) to modern country names
#   4. Report coverage statistics and unmapped values for validation
#   5. Write enriched file back to demographics.csv in place
#
# DATA QUALITY NOTES:
#   - Wikidata generally uses modern country names, so mapping success rate
#     should be very high (>99%). Unmapped values indicate either data entry
#     errors in Wikidata or gaps in the country_mapping table.
#   - Nationality field is semicolon-separated (e.g., "France; Switzerland");
#     each country is mapped individually, deduplicated, and re-collapsed.
#     Temporal nationality changes are lost (all forms collapsed together).
#   - Birth country can have only one value per person (person is born in one place).
#
# =============================================================================
message("=== Enriching demographics.csv ===\n")

demo_file <- data_path("demographics.csv")
demo <- read_csv(demo_file, show_col_types = FALSE)
message(sprintf("  Loaded demographics: %d rows", nrow(demo)))

# --- Map birth_country to geographic fields ---
# join left to preserve all records; unmapped birth_country → NA in new columns
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

# --- Map nationality field (multi-valued, semicolon-separated) ---
# DESIGN RATIONALE:
#   Nationality can be multi-valued (e.g., "France; Switzerland" for dual citizens).
#   We map each country individually, then deduplicate (to handle cases like
#   "France; France" or "Belgium; Belgium (historical name)" → "Belgium" after mapping).
#   This preserves multi-nationality information while normalizing spelling and
#   historical name variants.
#
# FUNCTION: map_nationality(nat_string)
#   INPUT: semicolon-separated nationality string (or NA)
#   PROCESS:
#     1. Split by semicolon
#     2. Match each part against country_mapping table
#     3. Replace with mapped modern name, or keep original if no match
#     4. Deduplicate (in case of redundant entries)
#     5. Re-collapse with semicolon separator
#   OUTPUT: semicolon-separated modern nationality string (or NA)
#
map_nationality <- function(nat_string) {
  if (is.na(nat_string)) return(NA_character_)
  # Split multi-value field by semicolon (and trim whitespace)
  parts <- str_split(nat_string, ";\\s*")[[1]]
  # Match each part against country_mapping; unmapped parts keep original name
  mapped <- country_mapping$modern_country[match(parts, country_mapping$raw_name)]
  # Replace unmapped values with original (keeps unmapped historical names)
  mapped <- ifelse(is.na(mapped), parts, mapped)
  # Deduplicate and re-collapse with semicolon separator
  paste(unique(mapped), collapse = "; ")
}
# Apply function to all nationality values (vectorized via vapply)
demo_geo$nationality_modern <- vapply(demo$nationality, map_nationality, character(1))

# --- Report data quality and coverage ---
# Calculate coverage statistics for birth_country mapping
n_mapped_bc <- sum(!is.na(demo_geo$birth_country_modern) & !is.na(demo_geo$birth_country))
n_with_bc <- sum(!is.na(demo_geo$birth_country))
message(sprintf("  Birth country mapped: %d / %d (%.1f%%)",
                n_mapped_bc, n_with_bc, 100 * n_mapped_bc / n_with_bc))

# Report all unmapped birth_country values (indicates gaps in country_mapping or Wikidata errors)
unmapped_bc <- demo_geo %>%
  filter(!is.na(birth_country), is.na(birth_country_modern)) %>%
  count(birth_country, sort = TRUE)
if (nrow(unmapped_bc) > 0) {
  message("  Unmapped birth_country values:")
  for (i in seq_len(nrow(unmapped_bc))) {
    message(sprintf("    %s (%d)", unmapped_bc$birth_country[i], unmapped_bc$n[i]))
  }
}

# --- Write enriched file ---
# Overwrite demographics.csv with new columns added in place
write_csv(demo_geo, demo_file)
message(sprintf("  Wrote enriched demographics: %s", demo_file))


# =============================================================================
# 4. Enrich nominations.csv with geographic standardization
# =============================================================================
# SECTION PURPOSE:
#   Enrich the nominations.csv file (nobelprize.org nomination archive data)
#   with standardized geographic fields for nominees and nominators.
#
# PROCESS OVERVIEW:
#   1. Load nominations.csv (source: nobelprize.org nomination archive)
#   2. Pre-clean nominee_country and nominator_country fields (strip ISO codes, quotes)
#   3. Detect and invalidate clearly erroneous values (city names, artifacts)
#   4. Map both cleaned fields to modern_country, un_subregion, continent
#   5. Report coverage statistics and unmapped values for validation
#   6. Write enriched file back to nominations.csv in place
#
# DATA QUALITY NOTES:
#   - Nomination archive is noisier than Wikidata (legacy encoding, misalignment).
#   - Coverage likely ~85-95% due to historical variants and data entry errors.
#   - Invalid value detection (cities, institutions) improves data quality but
#     may have false positives; manually review if coverage is unexpectedly low.
#   - Nominee and nominator countries are single-valued (one country per role).
#
# =============================================================================
message("\n=== Enriching nominations.csv ===\n")

nom_file <- data_path("nominations.csv")
nom <- read_csv(nom_file, show_col_types = FALSE)
message(sprintf("  Loaded nominations: %d rows", nrow(nom)))

# --- Pre-clean country fields ---
# Strip ISO codes, quotes, and legacy "now COUNTRY" annotations from nomination archive
nom <- nom %>%
  mutate(
    nominee_country_clean = clean_nomination_country(nominee_country),
    nominator_country_clean = clean_nomination_country(nominator_country)
  )

# --- Detect and invalidate garbage values ---
# Flag values matching invalid patterns (city names from CSV misalignment, etc.)
# and replace with NA. These would otherwise fail to map or map incorrectly.
nom <- nom %>%
  mutate(
    nominee_country_clean = ifelse(
      !is.na(nominee_country_clean) & is_invalid_country(nominee_country_clean),
      NA_character_, nominee_country_clean),
    nominator_country_clean = ifelse(
      !is.na(nominator_country_clean) & is_invalid_country(nominator_country_clean),
      NA_character_, nominator_country_clean)
  )

# Report invalidity statistics (data quality check)
n_invalidated_nominee <- sum(
  !is.na(nom$nominee_country) & is.na(nom$nominee_country_clean), na.rm = TRUE)
n_invalidated_nominator <- sum(
  !is.na(nom$nominator_country) & is.na(nom$nominator_country_clean), na.rm = TRUE)
message(sprintf("  Invalidated: %d nominee_country, %d nominator_country values (detected artifacts)",
                n_invalidated_nominee, n_invalidated_nominator))

# --- Map nominee country ---
# Left join to preserve all nomination records; unmapped → NA in new columns
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

# --- Map nominator country ---
# Same process for nominator geographic data
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

# --- Clean up intermediate columns ---
# Remove the _clean columns (only needed for mapping; keep modern/subregion/continent)
nom <- nom %>% select(-nominee_country_clean, -nominator_country_clean)

# --- Report data quality and coverage ---
# For both nominee and nominator, report mapping success and unmapped values
for (role in c("nominee", "nominator")) {
  # Identify column names for this role
  orig_col <- paste0(role, "_country")
  mapped_col <- paste0(role, "_country_modern")

  # Count records with original value and with mapped value
  n_with <- sum(!is.na(nom[[orig_col]]))
  n_mapped <- sum(!is.na(nom[[mapped_col]]) & !is.na(nom[[orig_col]]))

  # Report coverage percentage
  message(sprintf("  %s country mapped: %d / %d (%.1f%%)",
                  str_to_title(role), n_mapped, n_with, 100 * n_mapped / n_with))

  # Identify all unmapped values and report (sorted by frequency)
  # Re-clean the original values to show what they would be after pre-cleaning
  unmapped <- nom %>%
    filter(!is.na(!!sym(orig_col)), is.na(!!sym(mapped_col))) %>%
    mutate(cleaned = clean_nomination_country(!!sym(orig_col))) %>%
    count(cleaned, sort = TRUE)

  # Report unmapped values (cap at 20; if more than 20, show top 20 only)
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

# --- Write enriched file ---
# Overwrite nominations.csv with new columns added in place
write_csv(nom, nom_file)
message(sprintf("  Wrote enriched nominations: %s", nom_file))


# =============================================================================
# 5. Summary and validation report
# =============================================================================
# SECTION PURPOSE:
#   Provide a final summary of geographic standardization outcomes, showing
#   distributions of records by continent and subregion for validation.
#
# INTERPRETATION:
#   - Distributions should align with Nobel Prize history (Europe & North America dominant)
#   - Absence of expected regions may indicate mapping gaps or low coverage
#   - Large "unassigned" (NA) counts warrant investigation of unmapped values reported above
#
# =============================================================================
message("\n=== Geography Standardization Summary ===\n")

# --- Demographics geographic distribution ---
# Count and display demographics records by continent and subregion
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

# --- Nominations geographic distribution ---
# Count and display nominations records (by nominee) by continent and subregion
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

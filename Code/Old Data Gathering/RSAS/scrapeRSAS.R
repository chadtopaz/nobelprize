# -----------------------------------------------------------------------------
# Author: Ariana Mendible, 2022
# Author: Adapted by Chad Topaz, Sep 2023
# Description: 
# This script scrapes the Wikipedia page for the Royal Swedish Academy of Sciences.
# It extracts details about former members, then queries Wikidata for additional info.
# -----------------------------------------------------------------------------

# 1. PREPARATION --------------------------------------------------------------
# Load required packages
library(tidyverse)
library(rvest)
library(WikidataR)
library(pbmcapply)

# Define constants
WIKI_URL <- "https://sv.wikipedia.org/wiki/Lista_%C3%B6ver_ledam%C3%B6ter_av_Kungliga_Vetenskapsakademien"

# 2. DATA EXTRACTION ----------------------------------------------------------
# Scrape Wikipedia page for members' details
info <- WIKI_URL %>%
  read_html() %>%
  html_nodes("p, li a, #2021:not(.extiw)")

# Process and clean the scraped data
df <- data.frame(
  txt = info %>% html_text(),
  year = info %>% html_text() %>% str_extract('^\\d{4}'),
  links = info %>% html_attr('href')
) %>%
  mutate(
    year = ifelse(txt == "Jonas Alströmer", 1739, year),
    links = ifelse(str_detect(links, '^/wiki/'), paste0('https://sv.wikipedia.org', links), links)
  ) %>%
  fill(year, .direction = "down") %>%
  filter(
    !str_detect(links, 'redlink'),
    !str_detect(txt, '^\\d{4}'),
    !str_detect(txt, '^\\w{2}:'),
    year <= 1973
  )

# 3. CONVERT LINKS TO WIKIDATA IDs ---------------------------------------------
PediaURLToQID <- function(PediaURL, max_retries = 5) {
  retries <- 0
  success <- FALSE
  result <- NULL
  
  while (!success && retries < max_retries) {
    tryCatch(
      {
        result <- PediaURL %>%
          read_html() %>%
          html_elements(xpath='//*[(@id = "t-wikibase")]') %>%
          html_elements('a') %>%
          html_attr('href') %>%
          str_extract('(?<=https://www.wikidata.org/wiki/Special:EntityPage/).+')
        success <- TRUE
      },
      error = function(e) {
        cat("Error:", conditionMessage(e), "\n")
        Sys.sleep(5)
        retries <- retries + 1
      }
    )
  }
  
  if (success) return(result)
  stop("Maximum retries reached. PediaURLToQID failed.")
}

qid <- pbmclapply(df$links, PediaURLToQID, mc.cores=23) %>% unlist

# 4. QUERY WIKIDATA FOR ADDITIONAL DETAILS ------------------------------------
query_function <- function(qs, max_retries = 10) {
  items <- paste0('wd:', qs) %>% paste(collapse=' ')
  query <- sprintf(
    'SELECT DISTINCT ?qid ?name ?deathDate ?genderLabel ?birthCountryLabel ?nationalityLabel
    WHERE {
      VALUES ?qid {%s}
      OPTIONAL { ?qid rdfs:label ?name. FILTER(LANG(?name) = "en") }
      OPTIONAL { ?qid wdt:P570 ?deathDate. }
      OPTIONAL { ?qid wdt:P21 ?gender. }
      OPTIONAL { ?qid wdt:P19/wdt:P17 ?birthCountry. }
      OPTIONAL { ?qid wdt:P27 ?nationality. }
      SERVICE wikibase:label { 
        bd:serviceParam wikibase:language "en".
        ?gender rdfs:label ?genderLabel.
        ?birthCountry rdfs:label ?birthCountryLabel.
        ?nationality rdfs:label ?nationalityLabel.
      }
    }', items)
  
  # Attempt the query with retries
  retries <- 0
  success <- FALSE
  while (!success && retries < max_retries) {
    tryCatch(
      {
        returned <- query %>%
          query_wikidata(format="smart") %>%
          as.data.frame()
        success <- TRUE
      },
      error = function(e) {
        cat("Error:", conditionMessage(e), "\n")
        Sys.sleep(5)
        retries <- retries + 1
      }
    )
  }
  
  if (success) return(returned)
  stop("Maximum retries reached. Query failed.")
}

results <- pbmclapply(qid, query_function, mc.cores=23) %>% bind_rows()

# Process the query results
results <- results %>%
  mutate(endyear = year(as.Date(deathDate))) %>%
  select(-deathDate) %>%
  group_by(qid, name, genderLabel) %>%
  summarise(
    endyear = min(endyear, na.rm = TRUE),
    birthCountryLabel = paste(unique(birthCountryLabel), collapse = "; "),
    nationalityLabel = paste(unique(nationalityLabel), collapse = "; ")
  ) %>%
  ungroup()

# 5. MERGE AND FINALIZE DATAFRAME ---------------------------------------------
RSAS <- left_join(
  data.frame(names = df$txt, startyear = df$year, qid),
  results,
  by = "qid"
) %>%
  select(-names) %>%
  rename(
    gender = genderLabel,
    birthcountry = birthCountryLabel,
    nationality = nationalityLabel
  ) %>%
  relocate(qid, name, startyear, endyear, gender, birthcountry, nationality) %>%
  filter(startyear >= 1820 & startyear <= 1973) %>%
  filter(endyear >= 1901) %>%
  drop_na

# 6. ANALYSIS AND SAVE -------------------------------------------------------
# Plot the data
1901:1973 %>%
  as_tibble() %>%
  rename(year = value) %>%
  rowwise() %>%
  mutate(count = sum(RSAS$startyear <= year & RSAS$endyear >= year)) %>%
  ggplot(aes(x = year, y = count)) +
  geom_line() +
  labs(title = "Count of rows for each year",
       x = "Year",
       y = "Count")

# Save the processed data
save(RSAS, file="RSAS.Rdata")
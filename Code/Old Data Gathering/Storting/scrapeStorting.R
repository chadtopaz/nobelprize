# -----------------------------------------------------------------------------
# Date: Sep 6, 2023
# Author: Chad Topaz
# Adapted from: Code by Ariana Mendible, 2022
# Description: 
# This script extracts info about the Storting from Wikidata.
# -----------------------------------------------------------------------------

# Load required libraries
library(tidyverse)
library(rvest)
library(WikidataR)
library(lubridate)

# --------------------------
# Extract Storting Members from Wikidata  
# --------------------------

query <- '
SELECT DISTINCT ?qid ?name ?starttime ?endtime WHERE {
  {
    SELECT DISTINCT ?qid ?endtime ?starttime ?statement1 WHERE {
        ?qid p:P31 ?statement0.
        ?statement0 ps:P31 wd:Q5.
        ?qid p:P39 ?statement1.
        ?statement1 ps:P39 wd:Q9045502.
    }
  }
  OPTIONAL { ?statement1 pq:P580 ?starttime. }
  OPTIONAL { ?statement1 pq:P582 ?endtime. }
  
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". 
                            ?qid rdfs:label ?name.
                           }
}'

# Filter for specific time range
Storting <- query %>% 
  query_wikidata(format='smart') %>% 
  as.data.frame() %>%
  filter(starttime > '1901-01-01' & starttime < '1973-12-31')

# Extract years for readability
Storting$startyear <- year(Storting$starttime)
Storting$endyear <- year(Storting$endtime)
Storting <- Storting %>% select(-starttime, -endtime)

# --------------------------
# Query Wikidata for Additional Member Details
# --------------------------

query_function <- function(qs, max_retries = 10) {
  items <- paste0('wd:', qs) %>% paste(collapse=' ')
  query <- sprintf(
    'SELECT DISTINCT ?qid ?name ?genderLabel ?birthCountryLabel ?nationalityLabel
    WHERE {
      VALUES ?qid {%s}
      OPTIONAL { ?qid rdfs:label ?name. FILTER(LANG(?name) = "en") }
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
  
  # Attempt to query with a retry mechanism
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

# Query in parallel
results <- pbmclapply(Storting$qid, query_function, mc.cores=23) %>% bind_rows()

# Process and simplify the results
results <- results %>%
  group_by(qid, name, genderLabel) %>%
  summarise(
    birthCountryLabel = paste(unique(birthCountryLabel), collapse = "; "),
    nationalityLabel = paste(unique(nationalityLabel), collapse = "; ")
  ) %>%
  ungroup()

# Merge with original data and reorder columns
Storting <- merge(Storting, results) %>%
  rename(
    gender = genderLabel,
    birthcountry = birthCountryLabel,
    nationality = nationalityLabel
  ) %>%
  relocate(qid, name, startyear, endyear, gender, birthcountry, nationality) 

# Simplify, combining year ranges when possible
Storting <- Storting %>%
  group_by(name) %>%
  arrange(name, startyear) %>%
  mutate(new_seq = if_else(is.na(lag(endyear)) | (startyear - lag(endyear) > 1), 1, 0),
         seq_group = cumsum(new_seq)) %>%
  group_by(name, seq_group) %>%
  summarize(
    qid = first(qid),
    startyear = min(startyear),
    endyear = max(endyear),
    gender = first(gender),
    birthcountry = first(birthcountry),
    nationality = first(nationality)
  ) %>%
  ungroup() %>%
  select(-seq_group) %>%
  arrange(name, startyear)

# Save results
save(Storting, file="Storting.Rdata")
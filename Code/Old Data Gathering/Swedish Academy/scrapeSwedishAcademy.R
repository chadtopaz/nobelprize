# -----------------------------------------------------------------------------
# Date: Sep 6, 2023
# Author: Chad Topaz
# Adapted from: Code by Ariana Mendible, 2022
# Description: 
# This script queries Wikidata for members of the Royal Swedish Academy.
# It retrieves membership periods and additional demographic information 
# for members, aggregates the data, and saves it to a file.
# -----------------------------------------------------------------------------

# Libraries ----------------------------------------------------------------
library(tidyverse)
library(WikidataR)
library(lubridate)
library(pbmcapply)

# Functions ----------------------------------------------------------------

# Function to handle Wikidata queries with retries
query_function <- function(qs, max_retries = 10) {
  
  # Construct the SPARQL query
  items <- paste0('wd:', qs) %>% paste(collapse=' ')
  query <- paste0(
    'SELECT DISTINCT ?qid ?name ?deathDate ?genderLabel ?birthCountryLabel ?nationalityLabel
    WHERE {
      VALUES ?qid {', items,'}
      OPTIONAL { ?qid wdt:P21 ?gender. }
      OPTIONAL { ?qid wdt:P19/wdt:P17 ?birthCountry. }
      OPTIONAL { ?qid wdt:P27 ?nationality. }
      SERVICE wikibase:label { 
        bd:serviceParam wikibase:language "en".
        ?gender rdfs:label ?genderLabel.
        ?birthCountry rdfs:label ?birthCountryLabel.
        ?nationality rdfs:label ?nationalityLabel.
      }
    }'
  )
  
  # Retry mechanism for querying Wikidata
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
        Sys.sleep(5)  # Delay before retry
        retries <- retries + 1
      }
    )
  }
  
  # Return results or stop if maximum retries reached
  if (success) {
    return(returned)
  } else {
    stop("Maximum retries reached. Query failed.")
  }
}

# Data Retrieval and Processing --------------------------------------------

# Construct and execute initial Wikidata query for SAmembers
query <- '
SELECT DISTINCT ?qid ?name ?startDate ?endDate ?positionLabel WHERE {
  {
    SELECT DISTINCT ?qid ?statement0 WHERE {
      {
        ?qid p:P39 ?statement0.
        ?statement0 (ps:P39/(wdt:P361*)) wd:Q207360.
        MINUS {?statement0 (ps:P39/(wdt:P31*)) wd:Q97563901. }
      }
    }
  }
  OPTIONAL { ?statement0 ps:P39 ?position. }
  OPTIONAL { ?statement0 pq:P580 ?startDate. }
  OPTIONAL { ?statement0 pq:P582 ?endDate. }
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". 
                              ?qid rdfs:label ?name.
                              ?position rdfs:label ?positionLabel.
                             }
}'
SAmembers <- tryCatch(query %>% query_wikidata(format = "smart"), error = function(e) data.frame())


# Refine SAmembers dataframe
SAmembers <- SAmembers %>%
  mutate(
    startyear = year(as.Date(startDate)),
    endyear = year(as.Date(endDate))
  ) %>%
  select(-startDate, -endDate, -positionLabel) %>%
  filter(startyear <= 1973 & (endyear >= 1901 | is.na(endyear)))

# Query additional details for each member of Swedish Academy
SAdemo <- pbmclapply(SAmembers$qid, query_function, mc.cores=23) %>% bind_rows()

# Aggregate SAdemo to combine multiple nationalities
SAdemo_aggregated <- SAdemo %>%
  group_by(qid, genderLabel, birthCountryLabel) %>%
  summarise(nationalityLabel = paste(nationalityLabel, collapse = "; ")) %>%
  ungroup()

# Merge SAmembers and aggregated SAdemo
SA <- merge(SAmembers, SAdemo_aggregated, by="qid", all.x=TRUE) %>%
  rename(
    gender = genderLabel, 
    birthcountry = birthCountryLabel, 
    nationality = nationalityLabel
  )

# Plot the data
1901:1973 %>%
  as_tibble() %>%
  rename(year = value) %>%
  rowwise() %>%
  mutate(count = sum(SA$startyear <= year & SA$endyear >= year)) %>%
  ggplot(aes(x = year, y = count)) +
  geom_line() +
  labs(title = "Count of rows for each year",
       x = "Year",
       y = "Count")

# Save Final Data ----------------------------------------------------------
save(SA, file = "SwedishAcademy.Rdata")
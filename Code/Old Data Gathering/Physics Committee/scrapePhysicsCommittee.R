# -----------------------------------------------------------------------------
# Date: July 1, 2022
# Author: Ariana Mendible
# Modified By: Chad M. Topaz on September 7, 2023
# Description:
# This script scrapes the Wikipedia page for the Nobel Committee for Physics.
# It uses the Swedish Wikipedia page due to better Wikidata linking. The script
# extracts the names of former members, their years of membership, and Wikidata QIDs.
# It also creates a plot to verify the number of seats in each year.
# -----------------------------------------------------------------------------

# Load required packages
library(tidyverse)
library(rvest)
library(pbmcapply)

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

# Load the list of names from the Wikipedia page
url <- "https://sv.wikipedia.org/wiki/Vetenskapsakademiens_Nobelkommitt%C3%A9_f%C3%B6r_fysik"
txt <- url %>% 
  read_html() %>%
  html_elements(css='ul:nth-child(11) li') %>% 
  html_text()

links <- url %>% 
  read_html() %>%
  html_elements(css='ul:nth-child(11) li a') %>%
  html_attr('href') %>%
  paste0('https://sv.wikipedia.org', .)

# Extract names, start, and end years
names <- txt %>% str_extract('^.+(?=,)(?<!\\d)')
startyear <- txt %>% str_extract('\\d{4}(?=–\\d{4})') %>% as.numeric()
endyear <- txt %>% str_extract('(?<=\\d{4}–)\\d{4}') %>% as.numeric()

# Convert Wikipedia page links to QIDS
PediaURLToQID <- function(PediaURL) {
PediaURL %>% 
  read_html() %>%
  html_elements(xpath='//*[(@id = "t-wikibase")]') %>%
  html_elements('a') %>%
  html_attr('href') %>%
  str_extract('(?<=https://www.wikidata.org/wiki/Special:EntityPage/).+')
}
qid <- pbmclapply(links, PediaURLToQID, mc.cores = 23) %>% unlist()
phys <- data.frame(names, startyear, endyear, qid)

# Remove irrelevant years
phys <- phys[phys$startyear <= 1973,] %>% drop_na(., startyear)

# Aggregate and merge data
demo <- pbmclapply(phys$qid, query_function, mc.cores=23) %>% bind_rows()
demo_aggregated <- demo %>%
  group_by(qid, genderLabel, birthCountryLabel) %>%
  summarise(nationalityLabel = paste(nationalityLabel, collapse = "; ")) %>%
  ungroup()

phys <- merge(phys, demo_aggregated, by="qid", all.x=TRUE) %>%
  rename(
    gender = genderLabel, 
    birthcountry = birthCountryLabel, 
    nationality = nationalityLabel
  ) %>%
  mutate_all(~ifelse(. == "NA", NA, .))

# Save the processed data
save(phys, file='PhysicsCommittee.Rdata')

# Verify the years by plotting the number of seats
phys$terms <- (phys$endyear - phys$startyear) / 3
year <- c()
seats <- c()
for (i in 1901:1973) {
  year <- c(year, i)
  seats <- c(seats, sum((phys$startyear <= i) & (i <= phys$endyear), na.rm = TRUE))
}
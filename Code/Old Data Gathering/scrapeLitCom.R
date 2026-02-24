# Load required packages
library(tidyverse)
library(rvest)
library(WikidataR)
library(lubridate)
library(pbmcapply)
library(RSelenium)

# Constants
BASE_URL <- "https://www.svenskaakademien.se/svenska-akademien/ledamotsregister?page="

# Load Swedish academy data
load("Swedish Academy/SwedishAcademy.Rdata")

# Construct member-specific links
members$link <- paste0(members$lastName, "-", members$firstName) %>%
  str_replace_all(" ", "-") %>%
  tolower() %>%
  paste0("https://www.svenskaakademien.se/svenska-akademien/ledamotsregister/", .)

# Fetch individual page texts and extract committee years
pagetext <- pbmclapply(members$link, function(x) {
  tryCatch(getPageText(x), error = function(e) "")
}, mc.cores = 23) %>% unlist
years <- do.call(rbind, lapply(pagetext, detectCommittee))
members$startComm <- years[, 1]
members$endComm <- years[, 2]

# Wikipedia Querying -----------------------------------------------------

# Construct and execute Wikidata query
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

# Process Wikidata results
SAmembers$seatNo <- SAmembers$positionLabel %>% 
  str_extract_all('(?<=seat )\\d{1,2}') %>%
  unlist() %>%
  as.numeric()
SAmembers <- SAmembers %>% select(-positionLabel)
SAmembers$startDate <- as.Date(SAmembers$startDate)
SAmembers$endDate <- as.Date(SAmembers$endDate)

# Data Merging -----------------------------------------------------------
df <- members[!is.na(members$startComm),] %>%
  select('name', 'seatNo', 'startDate', 'endDate', 'startComm', 'endComm')
lit <- merge(df, SAmembers, by = c('seatNo', 'startDate', 'endDate'))

# Data Processing --------------------------------------------------------
lit <- lit %>% select(qid, name = name.y, startyear = startComm, endyear = endComm)
SwedishAcademy <- SAmembers %>% select(qid, name, startyear = startDate, endyear = endDate)
SwedishAcademy <- SwedishAcademy[year(SwedishAcademy$endyear) >= 1901 & year(SwedishAcademy$startyear) <= 1973,]

# Define and run query function to get additional details from Wikidata ---
query_function <- function(qs, max_retries = 10) {
  # Construct query
  items <- paste0('wd:', qs) %>% paste(collapse=' ')
  query <- paste0(
    'SELECT DISTINCT
    ?qid ?name ?deathDate ?genderLabel ?birthCountryLabel ?nationalityLabel
    WHERE {
      VALUES ?qid {', items,'}
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
    }'
  )
  
  # Attempt query until success or max retries reached
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
        Sys.sleep(5)  # Wait for 5 seconds before retrying
        retries <- retries + 1
      }
    )
  }
  
  if (success) {
    return(returned)
  } else {
    stop("Maximum retries reached. Query failed.")
  }
}

# For SwedishAcademy
SwedishAcademy_results <- pbmclapply(SwedishAcademy$qid, query_function, mc.cores=23) %>% bind_rows()

# For lit
lit_results <- pbmclapply(lit$qid, query_function, mc.cores=23) %>% bind_rows()

SwedishAcademy_merged <- merge(SwedishAcademy, SwedishAcademy_results, by="qid", all.x=TRUE)
lit_merged <- merge(lit, lit_results, by="qid", all.x=TRUE)

# Save the datasets -------------------------------------------------------
save(lit, file = 'LiteratureCommittee.Rdata')
save(SwedishAcademy, file = 'SwedishAcademy.Rdata')

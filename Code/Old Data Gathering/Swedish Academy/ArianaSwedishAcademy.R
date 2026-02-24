# Date: July 1, 2022
# Author: Ariana Mendible
# Description -------------------------------------------------------------

# Load packages -----------------------------------------------------------
library(tidyverse)
library(rvest)
library(WikidataR)

# Functions ---------------------------------------------------------------
getPageText <- function(link){
  txt <- link %>% 
    read_html %>%
    html_elements(css='.even p') %>% 
    html_text %>% 
    toString
}

detectCommittee <- function(txt){
  if(str_detect(txt, 'Nobelkommittén')){
    sent <- str_extract(txt,'[^.?!]*(?<=[.?\\s!])Nobelkommittén(?=[\\s.?!])[^.?!]*[.?!]') 
    years <- str_extract_all(sent, '\\d{2,4}') %>% unlist %>% as.numeric
    years <- ifelse(years<1900, years+floor(max(years)/100)*100, years)
    startyear <- min(years)
    endyear <- max(years)
  } else {
    startyear <- NA
    endyear <- NA
  }
  c(startyear,endyear)
}

getMembers <- function(url){
  members <- url %>% 
    read_html %>%
    html_table %>% 
    .[[1]] %>%
    as.data.frame 
  
  links <- url %>% 
    read_html %>% 
    html_elements(css='td.views-field-field-fornamn a') %>%
    html_attr('href') %>%
    paste0('https://www.svenskaakademien.se/',.)
  members$links <- links
  
  members
}

# Load in table and links from search page --------------------------------
baseurl <- "https://www.svenskaakademien.se/svenska-akademien/ledamotsregister?page="
urls <- paste0(baseurl,0:4)
members <- do.call(rbind, lapply(urls, getMembers))
colnames(members) <- c("firstName", 
                       "lastName", 
                       "seatNo", 
                       "electionDate",
                       "startDate",	
                       "endDate",	
                       "entryAge",
                       "tenure",
                       "links")

members$startDate <- members$startDate %>% as.Date
members$endDate <- members$endDate %>% as.Date
members$name <- paste(members$firstName, members$lastName)

# Get bio text from links -------------------------------------------------
pagetext <- lapply(members$links, getPageText) %>% unlist

# Get the years on Nobel Committee from bio text -------------------------
years <- do.call(rbind, lapply(pagetext, detectCommittee))
members$startComm <- years[,1]
members$endComm <- years[,2]

# Query Wikipedia for Swedish Academy members -----------------------------
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

SAmembers <- query %>% 
  query_wikidata(format="smart") 

# reformat some columns for later merging
SAmembers$seatNo <- SAmembers$positionLabel %>%
  str_extract_all('(?<=seat )\\d{1,2}') %>% 
  unlist %>%
  as.numeric
SAmembers <- SAmembers %>% select(-positionLabel)
SAmembers$startDate <- SAmembers$startDate %>% as.Date
SAmembers$endDate <- SAmembers$endDate %>% as.Date

# Merge Swedish Academy Data ---------------------------------------
df <- members[!is.na(members$startComm),] %>%
  select('name', 'seatNo', 'startDate', 'endDate', 'startComm', 'endComm')

lit <- merge(df, SAmembers, by = c('seatNo', 'startDate', 'endDate'))


# Downselect data and save ------------------------------------------------
lit <- lit %>% 
  select(qid, name=name.y, startyear = startComm, endyear = endComm)
SwedishAcademy <- SAmembers %>% 
  select(qid, name, startyear = startDate, endyear = endDate)

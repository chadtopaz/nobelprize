# Date: July 1, 2022
# Author: Ariana Mendible
# Description -------------------------------------------------------------
# This script scrapes the Wikipedia page for the Nobel Committee for Physiology/Medicine.
# It gets the names of former members, years of membership, and Wikidata IDs.
# A plot is created to verify the number of seats in each year. 

# Load packages -----------------------------------------------------------
library(tidyverse)
library(rvest)
library(WikidataR)
library(pbmcapply)
library(DEoptim)

# Load in list of names --------------------------------------------------
url <- "https://sv.wikipedia.org/wiki/Karolinska_Institutets_Nobelkommitt%C3%A9"

txt <- url %>% 
  read_html %>%
  html_elements(css='ul:nth-child(36) li') %>% 
  html_text

links <- url %>% 
  read_html %>% 
  html_elements(css='ul:nth-child(36) a') %>% 
  html_attr('href') %>%
  paste0('https://sv.wikipedia.org',.)

good.links <- links %>% str_detect('/wiki/')
bad.links <- links %>% str_detect('&redlink=1')

# Pull names and years from text
name <- txt %>% str_extract('^.+(?=,)(?<!\\d)')
startyear <- txt %>% str_extract('\\d{4}(?=–)') %>% as.numeric
endyear <- txt %>% str_extract('(?<=–)\\d{4}') %>% as.numeric

df <- data.frame(name, startyear, endyear) 

# Convert good page links to QIDS -----------------------------------------
PediaURLToQID <- function(PediaURL){
  qidout <- PediaURL %>% 
    read_html %>% 
    html_elements(xpath='//*[(@id = "t-wikibase")]') %>%
    html_elements('a') %>%
    html_attr('href') %>%
    str_extract('(?<=https://www.wikidata.org/wiki/Special:EntityPage/).+')
}

qid <- pbmclapply(links[good.links], PediaURLToQID, mc.cores = 23) %>% unlist
df$qid[good.links] <- qid

# Get rid of weird ones
df <- df[!is.na(df$startyear),]

# Filter
df <- df %>%
  filter(startyear <= 1973)

items <- paste0('wd:',df$qid) %>% paste(collapse=' ')

query <- paste0(
  'SELECT DISTINCT
  ?qid ?birthDate ?deathDate
  WHERE {
    VALUES ?qid {', items,'}
    ?qid p:P31 [ ps:P31 ?instanceof ] .
    OPTIONAL{?qid p:P569 [ps:P569 ?birthDate ].}
    OPTIONAL{?qid p:P570 [ps:P570 ?deathDate ].}
    }'
)

returned <- query %>%
  query_wikidata(format="smart") %>%
  as.data.frame

med <- merge(df, returned, by = "qid", all.x = TRUE)

med_orig <- med


# Convert deathDate to year
med$deathYear <- as.numeric(format(as.Date(med$deathDate), "%Y"))
med$birthYear <- as.numeric(format(as.Date(med$birthDate), "%Y"))

unknown_endyears <- med[is.na(med$endyear), ]

# Define the cost function
calculate_cost <- function(endyears) {
  # Replace NA endYears with the proposed values
  med[is.na(med$endyear), "endyear"] <- round(endyears)
  
  # Calculate committee counts for each year
  years <- 1901:1973
  committee_counts <- sapply(years, function(y) {
    sum(med$startyear <= y & med$endyear >= y)
  })
  
  # Cost is the number of years with committee count neq 5
  cost <- sum((committee_counts - 5)^2)
  return(cost)
}

# Lower and Upper bounds for DEoptim
lower_bounds <- as.numeric(unknown_endyears$startyear)
upper_bounds <- ifelse(is.na(unknown_endyears$deathYear), 1973, as.numeric(unknown_endyears$deathYear))

# Run the DEoptim function
result <- DEoptim(fn = calculate_cost, lower = lower_bounds, upper = upper_bounds, control = list(trace = 50, itermax = 500, strategy = 3))

# The optimal end years are in result$bestmem
med[is.na(med$endyear), "endyear"] <- round(result$optim$bestmem)


# Save data ---------------------------------------------------------------
save(med, file='PhysMedCommittee.Rdata')



# Verify years ------------------------------------------------------------
year = c()
seats = c()
for (i in 1901:1973){
  year <- c(year,i)
  seats <- c(seats, sum((med$startyear <= i) * (i <= med$endyear), na.rm = TRUE))
}
plot(year,seats)
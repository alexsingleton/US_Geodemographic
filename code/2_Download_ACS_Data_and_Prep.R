# Load Packages
library(readr)
library(tidycensus)
library(tigris)
library(purrr)
library(foreach)
library(doParallel)
library(dplyr)
library(tidyverse)
library(tidygraph)
library(magrittr)
library(sf)
library(readxl)
library(httr)
library(skimr)
library(corrr)
library(reshape2)
library(ggraph)
library(viridis)
library(summarytools)
library(cluster)
library(janitor)
library(caret)
library(e1071)

setwd("~/GitHub/")

################################################################################################
# Data Import / Prep 
#
# This code downloads the raw ACS data and creates measures that were used to explore correlations
# A variable specification file is used for this purpose and was versioned while considering inputs
################################################################################################


## A set of new potential candidate variables were manually identified from the list of potential variables
vars_new <-  read_csv("data/acs_variables_initial_1.5.csv")[-1]

## Append a new TRUE / FALSE variable to indicate if a percentage
non_pct_ <- c("C18131","B19083","B25064","B25018","B25076","B25077","B25078","B25088","B25105") #Identify variables that aren't PCT

## Append an appropriate denominator and percentage ID Column
vars_new %<>%
  mutate(pct=ifelse((`Table ID` %in% non_pct_),ifelse(is.na(CONCEPT),NA,FALSE),ifelse(is.na(CONCEPT),NA,TRUE)),
         denom=ifelse(((pct==TRUE) & (!is.na(CONCEPT))),paste0(substr(UniqueID,start=1, stop=6),"_001"),NA))

## Create variable list with ACS codes and relative denominator
denom<-unique(vars_new[((vars_new$pct==TRUE) & (!is.na(vars_new$CONCEPT))),]$denom) # Denominators
numer <-vars_new[((vars_new$pct==TRUE) & (!is.na(vars_new$CONCEPT))),]$UniqueID # Numerators

nopct<-unique(vars_new[((vars_new$pct==FALSE) & (!is.na(vars_new$CONCEPT))),]$UniqueID) # Non percentages

codes <- unique(c(denom,nopct,numer))#Unique is needed to remove the denominator for Group Quarters so not captured twice




################
# Download Data 
################

# Set the Census API key and retrieve country codes
census_api_key("") # The API key provided to you from the Census formatted in quotes. A key can be acquired at http://api.census.gov/data/key_signup.html

us <- unique(fips_codes$state)[1:51]

# Setup parallel processing
c<- detectCores() - 1  
cl <- makeCluster(c)
registerDoParallel(cl)



ptm <- proc.time() 
foreach(i = 1:length(us),.packages=c('purrr','dplyr','tidycensus')) %dopar%{
  
  d<-get_acs(geography = "block group", variables = codes, 
             state = us[i], year = 2020,geometry = FALSE)
  
  saveRDS(d,paste0("data/storage_tmp/",us[i],".rds"))
  rm(d)
  
}
proc.time() - ptm


# Combine Data
ptm <- proc.time()
DF <- list.files(path = "./data/storage_tmp/", pattern = ".rds", full.names = TRUE) %>%
  map_dfr(readRDS)
proc.time() - ptm


DF %<>%
  select(-NAME,-moe) %>%
  pivot_wider(names_from = variable,values_from = estimate)

saveRDS(DF,"./data/data_BG_1.5.rds")







################
# Calculate Rates
################


# Variables that need aggregation within a table - this requires summing some variables and replacing them within the variable numerator / denominator lookup

ag_vars <- vars_new %>%
  filter(Merge == 1) %>%
  select(UniqueID,CONCEPT,DOMAIN,MEASURE,pct,denom) 

ag_vars <- split(ag_vars,ag_vars$MEASURE)

ag_vars_out <- DF %>%
  select(GEOID)


for (i in 1:length(ag_vars)) {
tmp <-  DF %>%
  select(paste0("",ag_vars[[i]]$UniqueID)) %>%
  mutate(sum = rowSums(.)) %>%
  select(sum) %>%
  rename(!!paste0("",unique(ag_vars[[i]]$MEASURE)) := sum)

ag_vars_out <- cbind(ag_vars_out,tmp)
  
}

# Amend variable list to remove variables that have been aggregated

#Remove merge variables
vars_new %<>%
  filter(is.na(Merge))

# Append aggregated variables with appropriate denominator
ag_vars %<>%
  reduce(bind_rows) %>%
  select(-UniqueID) %>%
  distinct() %>%
  mutate(UniqueID = MEASURE)

vars_new %<>%
  bind_rows(ag_vars)


#Append the aggregated variables onto Data

DF %<>%
  left_join(ag_vars_out,by = "GEOID")


# Calculate rates

getPrt<-function (.x, .y) {
  numer<-DF[,.x]
  denom<-DF[,.y]
  prt<-(numer/denom) * 100
  return(prt)
}


# Setup numerator and denominator lists & calculate rates
numer <-paste0("",vars_new[((vars_new$pct==TRUE) & (!is.na(vars_new$CONCEPT))),]$UniqueID) # Numerators
denom<-paste0("",vars_new[((vars_new$pct==TRUE) & (!is.na(vars_new$CONCEPT))),]$denom) # Denominators


# Calculate rates
prt<-purrr::map2(.x=numer,.y=denom,.f=getPrt) %>% as.data.frame()

# Append the non % / rate variables
non_pct_data <- data.frame(DF[,paste0("",nopct)])

All_data <- prt %>%
  bind_cols(non_pct_data)

#Rename Variables & add ID
names(All_data) <- vars_new$MEASURE[match(names(All_data), paste0("",vars_new$UniqueID))]

All_data  %<>%
  bind_cols(DF[,"GEOID"])

# Save the measures and description file
saveRDS(All_data,"./data/All_data_1.5.rds") # Not stored in the repo as too large (https://pcwww.liv.ac.uk/~ucfnale/us_geodemographic_lfs/All_data_1.5.rds)
saveRDS(vars_new,"./data/vars_new_1.5.rds") 




# Download Polygons


# Download All Block Groups
ctys <- counties(cb = TRUE)

state_codes <- unique(fips_codes$state_code)[1:51]

options(tigris_use_cache = TRUE)

BG_SF <- map_df(state_codes, function(state_code) {
  state <- filter(ctys, STATEFP == state_code)
  county_codes <- state$COUNTYFP
  get_acs(geography = "block group", variables = "B01001_001",
          state = state_code, county = county_codes,geometry = TRUE)
})

BG_SF %<>%
  select(GEOID)

saveRDS(BG_SF,"./data/Block_Group_SF.rds")  # Not stored in the repo as too large (https://pcwww.liv.ac.uk/~ucfnale/us_geodemographic_lfs/BG_SF.rds)




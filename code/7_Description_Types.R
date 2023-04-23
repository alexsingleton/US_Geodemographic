# Load Packages & Data
library(readr)
library(tidycensus)
library(purrr)
library(foreach)
library(doParallel)
library(dplyr)
library(tidyverse)
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
library(caret)
library(e1071)
library(arrow)
library(janitor)


#Get Source Data
data <- readRDS("./data/data_BG_1.5.rds")

usa.bg.cl <- read_parquet("./data/usa.bg.cl.type.parquet")

vars_new <-  read_csv("data/acs_variables_initial_1.5.csv")[-1]


# Append Clusters

usa.bg.cl %<>%
  rename(Type = cluster) %>%
  left_join(data) %>%
  rename(TotPop = B01001_001)

#########################
# Create Index Scores
#########################

TYPE <- usa.bg.cl %>%
  select(-c(GEOID)) %>%
group_by(Type) %>% 
  summarise_all(sum,na.rm = TRUE) 


T_n <- TYPE$Type

# Convert to %
  
  TYPE %<>%
  select(-Type) %>%
  mutate_all(~(./sum(.)*100))

  #Remove the denominator columns
  TYPE %<>%
    select(!ends_with('_001'))
  
  
# Calculate index scores

TYPE %<>%
  mutate_at(vars(colnames(TYPE[,2]):colnames(TYPE[,ncol(TYPE)])), ~(./ TotPop * 100)) %>%
  select(-TotPop)



#Create Output

TYPE %<>%
  t()%>%
  as.data.frame() %>%
  rownames_to_column(var = "UniqueID") %>%
  as_tibble(name_repair = "minimal") %>%
  left_join(vars_new, by="UniqueID") %>%
  select(UniqueID,V1:V39,Stub,CONCEPT,DOMAIN) %>%
  rename_at(vars(V1:V39), ~ T_n) %>%
  clean_names()


write_csv(TYPE,"Grand_Index_Clusters_TYPES_BG_Logit.csv")




# Variables by Types with greatest propensity

c_n <- colnames(TYPE)[2:40]

for (i in 1:length(c_n)) {

TYPE %>%
  select(!!as.name(c_n[i]),unique_id,stub) %>%
  slice_max(!!as.name(c_n[i]), n = 10) %>%
  mutate(cluster = paste0(c_n[i])) %>%
  write_csv("top_index.csv",append = TRUE)

}







############################ Map ##################

Block_Group_SF <- readRDS("./data/Block_Group_SF.rds")


Block_Group_SF %<>%
  left_join(usa.bg.cl) %>%
  select(GEOID,Type)

st_write(Block_Group_SF, "data/BG_TYPE_SF.shp")

st_write(Block_Group_SF, "data/BG_TYPE_SF.gpkg")



##############################################Index Scores 2 - more complete variables #######################################################


setwd("~/GitHub/")

################################################################################################
# Data Import / Prep 
#
# This code downloads the raw ACS data and creates measures that were used to explore correlations
# A variable specification file is used for this purpose and was versioned while considering inputs
################################################################################################


## A set of new potential candidate variables were manually identified from the list of potential variables
vars_new <-  read_csv("data/acs_variables_initial_1.5_expanded_index_scores.csv")[-1]

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
#census_api_key("28623dc12367621593ec9f56deeb0c495644e8f0")

census_api_key("20eb1998096c4eb405a63ebc23033e2cbc0df8b5")


#readRenviron("~/.Renviron")
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
  print(us[i])
  
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

saveRDS(DF,"./data/data_BG_1.5_fuller.rds")



################################################



#Get Source Data
data <- readRDS("./data/data_BG_1.5_fuller.rds")

usa.bg.cl <- read_parquet("./data/usa.bg.cl.type.parquet")

vars_new <-  read_csv("data/acs_variables_initial_1.5_expanded_index_scores.csv")[-1]


# Append Clusters

usa.bg.cl %<>%
  rename(Type = cluster) %>%
  left_join(data) %>%
  rename(TotPop = B01001_001)

#########################
# Create Index Scores
#########################

TYPE <- usa.bg.cl %>%
  select(-c(GEOID)) %>%
  group_by(Type) %>% 
  summarise_all(sum,na.rm = TRUE) 


T_n <- TYPE$Type

# Convert to %

TYPE %<>%
  select(-Type) %>%
  mutate_all(~(./sum(.)*100))

#Remove the denominator columns
TYPE %<>%
  select(!ends_with('_001'))


# Calculate index scores

TYPE %<>%
  mutate_at(vars(colnames(TYPE[,2]):colnames(TYPE[,ncol(TYPE)])), ~(./ TotPop * 100)) %>%
  select(-TotPop)



#Create Output

TYPE %<>%
  t()%>%
  as.data.frame() %>%
  rownames_to_column(var = "UniqueID") %>%
  as_tibble(name_repair = "minimal") %>%
  left_join(vars_new, by="UniqueID") %>%
  select(UniqueID,V1:V39,Stub,CONCEPT,DOMAIN) %>%
  rename_at(vars(V1:V39), ~ T_n) %>%
  clean_names()


write_csv(TYPE,"Grand_Index_Clusters_TYPES_BG_Logit_expanded.csv")







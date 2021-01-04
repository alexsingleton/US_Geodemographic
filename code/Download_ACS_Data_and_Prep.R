# Load Packages
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

## ####### ## ####### ## ####### ## ####### ## ####### ## #######
## ####### Data Import / Prep ## ####### ## ####### ## ####### ## 
## ####### ## ####### ## ####### ## ####### ## ####### ## ####### 

## A set of new potential candidate variables were manually identified from the list of potential variables

vars_new <-  read_excel("data/acs_variables_initial_1.1.xlsx")[-1]

## Append a new TRUE / FALSE variable to indicate if a percentage
non_pct_ <- c("C18131","B19083","B25064","B25018","B25076","B25077","B25078","B25088","B25105") #Identify variables that aren't PCT

## Append an appropriate denominator and percentage ID Column
vars_new %<>%
  mutate(pct=ifelse((`Table ID` %in% non_pct_),ifelse(is.na(CONCEPT),NA,FALSE),ifelse(is.na(CONCEPT),NA,TRUE)),
         denom=ifelse(((pct==TRUE) & (!is.na(CONCEPT))),paste0(substr(UniqueID,start=1, stop=6),"_001"),NA))



## Create variable list with ACS codes and relative denominator
denom<-unique(vars_new[((vars_new$pct==TRUE) & (!is.na(vars_new$CONCEPT))),]$denom) # Denominators
nopct<-unique(vars_new[((vars_new$pct==FALSE) & (!is.na(vars_new$CONCEPT))),]$UniqueID) # Non percentages
numer <-vars_new[((vars_new$pct==TRUE) & (!is.na(vars_new$CONCEPT))),]$UniqueID # Numerators

codes <- unique(c(denom,nopct,numer))#Unique is needed to remove the denominator for Group Quarters so not captured twice



## ####### ## ####### ## ####### ## ####### ## ####### ## #######
## ####### Download Data ## ####### ## ####### ## ####### ## ####
## ####### ## ####### ## ####### ## ####### ## ####### ## ####### 

# Set the Census API key and retrieve country codes
census_api_key("28623dc12367621593ec9f56deeb0c495644e8f0",overwrite = TRUE ,install = TRUE)
readRenviron("~/.Renviron")
us <- unique(fips_codes$state)[1:51]

# Setup parallel processing
c<- detectCores() - 1  
cl <- makeCluster(c)
registerDoParallel(cl)


# Pull down ACS data (estimates & margins of error)
x<-foreach(i = 1:length(codes),.packages=c('purrr','dplyr','tidycensus')) %dopar%{
  df <- map_df(us, function(x) {
    get_acs(geography = "tract", variables = codes[i], 
            state = x, year = 2019,geometry = FALSE)
  })
  error <- paste0("moe_",codes[i])
  est <- paste0("est_",codes[i])
  df <-df %>%
    select(-variable) %>%
    rename(!! est := estimate,
           !! error := moe)
}


# Transform the list into estimate and margin of error data frames

x<-x %>%
  reduce(left_join,by=c("GEOID","NAME")) 
x<-map(purrr::set_names(c("est","moe")),~select(x,starts_with(.x),c("GEOID","NAME")))

data<-x[["est"]]
moe<-x[["moe"]]

# Backup ACS data
saveRDS(moe,"./data/moe.rds")
saveRDS(data,"./data/data.rds")


## ####### ## ####### ## ####### ## ####### ## ####### ## #######
## ####### Calculate Rates ## ####### ## ####### ## ####### ## ##
## ####### ## ####### ## ####### ## ####### ## ####### ## ####### 



# Group Quarters (B26001_001) - A manual calculation is needed 
GQ <- data %>%
  mutate(Group_Quarters = (est_B26001_001 / est_B01001_001) * 100) %>%
  select(GEOID, Group_Quarters)

# Variables that needed aggregation



# Calculate rates

getPrt<-function (.x, .y) {
  numer<-data[,.x]
  denom<-data[,.y]
  prt<-(numer/denom) * 100
  return(prt)
}






# Setup numerator and denominator lists & calculate rates
numer <-paste0("est_",vars_new[((vars_new$pct==TRUE) & (!is.na(vars_new$CONCEPT))),]$UniqueID) # Numerators
denom<-paste0("est_",vars_new[((vars_new$pct==TRUE) & (!is.na(vars_new$CONCEPT))),]$denom) # Denominators


 #remove qroup quarters from the lists
numer <- numer[numer != "est_B26001_001"]
denom <- denom[denom != "est_B26001_001"]

prt<-purrr::map2(.x=numer,.y=denom,.f=getPrt) %>% as.data.frame()

# Change column names
names(prt)<-str_replace_all(names(prt),"_","")
names(prt)<-str_replace(names(prt),"est","PCT_ACS19_5yr_")

# Append the non % variables
non_pct_data <- data.frame(data[,c("GEOID",paste0("est_",nopct))])


names(non_pct_data)<-str_replace_all(names(non_pct_data),"_","")
names(non_pct_data)<-str_replace(names(non_pct_data),"est","ACS19_5yr_")


All_data <- prt %>%
  bind_cols(non_pct_data)

saveRDS(All_data,"./data/All_data.rds")


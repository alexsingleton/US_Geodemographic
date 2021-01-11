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




# Calculate rates

getPrt<-function (.x, .y) {
  num<-data[,.x]
  denom<-data[,.y]
  prt<-num/denom
  return(prt)
}

# Setup numerator and denominator lists & calculate rates
num<-paste0("est_",vars[vars$pct=="TRUE",]$code)
denom<-paste0("est_",vars[vars$pct=="TRUE",]$denom)
prt<-purrr::map2(.x=num,.y=denom,.f=getPrt) %>% as.data.frame()

# Change column names
names(prt)<-str_replace_all(names(prt),"_","")
names(prt)<-str_replace(names(prt),"est","PCT_ACS19_5yr_")

# Append the non % variables
non_pct_data <- data.frame(data[,c("GEOID",paste0("est_",nopct))])
names(non_pct_data)<-str_replace_all(names(non_pct_data),"_","")
names(non_pct_data)<-str_replace(names(non_pct_data),"est","ACS19_5yr_")

Input_Data <-cbind(non_pct_data,prt) #Input Data 

# Import Boundaries (https://catalog.data.gov/dataset/2019-cartographic-boundary-shapefile-current-census-tract-for-united-states-1-500000)

tract_SF = read_sf("data/cb_2019_us_tract_500k",layer="cb_2019_us_tract_500k")

tract_SF %<>%
  select(GEOID,ALAND, AWATER) %>%
  left_join(Input_Data,by="GEOID")

st_write(tract_SF,     "./data/Tract_2019.gpkg", "Tract")


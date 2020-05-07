library(readr)
library(tidycensus)
library(purrr)
library(foreach)
library(doParallel)
library(dplyr)
library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")
pw<-"tower2020"
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "usgeodem",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)

##IMPORT MANUALLY REFINED LIST OF VARIABLES TO BE INCLUDED IN CLASSIFICATION
##THIS FILE WAS MANUALLY EDITED IN EXCEL AND INCLUDES CODES INDICATING
##IF A VARIABLE SHOULD BE INCLUDED IN THE ANALYSIS.  THE COLUMN "NEW SET"
##IS THE FINAL VARIABLE LIST
vars <- read.csv("usa_trt_varnames_051313.csv",stringsAsFactors = F)

##SELECTED VARIABLES
in.vars <- as.character(vars[vars$new_set==0 & is.na(vars$new_set) == FALSE, "var"])
vars <- vars[vars$var %in% in.vars,]

##get variables code
vars$code<- sub('.*\\_','',vars$var)
vars$code<-paste0(substring(vars$code,1,nchar(vars$code)-3),"_",substring(vars$code,nchar(vars$code)-2,nchar(vars$code)))
v18 <- load_variables(2018, "acs5", cache = TRUE) #this is a reference to the variable names
v18 <- v18[v18$name %in% vars$code,]

##set api key and retrieve country codes
census_api_key("376325c2baafb27e6ba5629590ba601eec2ab42c")
us <- unique(fips_codes$state)[1:51]

##to parallelize the calls
c<- detectCores() - 1  
cl <- makeCluster(c)
registerDoParallel(cl)

x<-foreach(i = 4:nrow(vars),.packages=c('purrr','dplyr','tidycensus')) %dopar%{
  df <- map_df(us, function(x) {
    get_acs(geography = "tract", variables = vars[i,]$code, 
            state = x, year = 2018)
  })
  error <- paste0("moe_",vars[i,]$code)
  df <-df %>%
    select(-variable) %>%
    rename(!! vars[i,]$code := estimate,
           !! error := moe)
}

##transform the list into a unique df
x<-x %>%
  reduce(left_join,by=c("GEOID","NAME"))

##write data to db and as a csv
dbWriteTable(con, "data", x, row.names=FALSE, append=TRUE)
write.csv(x,"data.csv",row.names = F)

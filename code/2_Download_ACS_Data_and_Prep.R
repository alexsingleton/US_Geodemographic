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



################################################################################################
# Data Import / Prep 
#
# This code downloads the raw ACS data and creates measures that were used to explore correlations
# A variable specification file is used for this purpose and was versioned while considering inputs
################################################################################################


## A set of new potential candidate variables were manually identified from the list of potential variables
vars_new <-  read_excel("data/acs_variables_initial_1.4.xlsx")[-1] # WE SHOULD PROBABLY MAKE THIS A CSV BEFORE RELEASE

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


################
# Download Data 
################

# Set the Census API key and retrieve country codes
census_api_key("28623dc12367621593ec9f56deeb0c495644e8f0",overwrite = TRUE ,install = TRUE)
readRenviron("~/.Renviron")
us <- unique(fips_codes$state)[1:51]

# Setup parallel processing
c<- detectCores() - 1  
cl <- makeCluster(c)
registerDoParallel(cl)

ptm <- proc.time()
# Pull down ACS data (estimates & margins of error)

####Function to call the api and save a data backup
map_function<-function(.x) {
  d<-get_acs(geography = "block group", variables = codes[i], 
             state = .x, year = 2019,geometry = FALSE)
  d <-d %>%
    select(-variable) %>%
    rename(!! est := estimate,
           !! error := moe)
  saveRDS(d,paste0("data/storage_tmp/",codes[i],"-",.x,".rds"))
  return(d)
}

#####Loop through states and variables

foreach(i = 1:length(codes),.packages=c('purrr','dplyr','tidycensus')) %dopar%{
  error <- paste0("moe_",codes[i])
  est <- paste0("est_",codes[i])
  df<-map_df(.x=us,.f=map_function) 
}
######if everything went smoothly run this, if not go to the recovery code.
files<-map(.x=codes,function(x){list.files("data/storage_tmp",full.names = T, pattern = x)})
x<-map(files,function(x){do.call("rbind",lapply(x,readRDS))})

#############################
###### RECOVERY CODE ########
#############################
######Recover the work if the api have been disconnected and restart from where it stopped

files<-map(.x=codes,function(x){list.files("data/storage_tmp",full.names = T, pattern = x)})
states_count<-as.data.frame(do.call("rbind",map(files,length)))
states_count$id<-seq(1:nrow(states_count))
states_count$check<-ifelse(states_count$V1==51,"C","NC")
pulled_var<-states_count[states_count$check=="C" | (states_count$check=="NC" & states_count$V1>0),]$id #this is the number of pulled variables
missing_states<-states_count[states_count$check=="NC" & states_count$V1>0,1:2] #this is the number of states pulled for each variable

foreach(i = states_count[states_count$check=="NC",]$id,.packages=c('purrr','dplyr','tidycensus')) %dopar%{
  error <- paste0("moe_",codes[i])
  est <- paste0("est_",codes[i])
  if(i %in% missing_states$id){
   us<-na.omit(us[missing_states[missing_states$id==i,]$V1+1:length(us)])
  }
  df<-map_df(.x=us,.f=map_function) 
}

files<-map(.x=codes,function(x){list.files("data/storage_tmp",full.names = T, pattern = x)})
x <- map(files[1:length(pulled_var)],function(x){do.call("rbind",lapply(x,readRDS))})


#############################
#### END RECOVERY CODE ######
#############################

proc.time() - ptm

# Transform the list into estimate and margin of error data frames

x<-x %>%
  reduce(left_join,by=c("GEOID","NAME")) 
x<-map(purrr::set_names(c("est","moe")),~select(x,starts_with(.x),c("GEOID","NAME")))

data<-x[["est"]]
moe<-x[["moe"]]

# Backup ACS data
saveRDS(moe,"./data/moe.rds")
saveRDS(data,"./data/data.rds")

view(dfSummary(data), file = "Summary_Inputs.html")

miss <- data %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.))/ 23212 *100) )


miss <- as.data.frame(t(miss))
miss$ID <- row.names(miss)


nnn <- vars_new %>%
  filter(`New Variables` == 1) %>%
 select(UniqueID) %>%
  pull()

nnn_miss <- miss %>%
  filter(ID %in% paste0("est_",nnn))


################
# Calculate Rates
################

# Group Quarters (B26001_001) - A manual calculation is needed as denominator within a separate table
GQ <- data %>%
  mutate(Group_Quarters = (est_B26001_001 / est_B01001_001) * 100) %>%
  select(GEOID, Group_Quarters)

# Variables that need aggregation within a table - this requires summing some variables and replacing them within the variable numerator / denominator lookup

ag_vars <- vars_new %>%
  filter(Merge == 1) %>%
  select(UniqueID,CONCEPT,DOMAIN,MEASURE,pct,denom) 

ag_vars <- split(ag_vars,ag_vars$MEASURE)

ag_vars_out <- data %>%
  select(GEOID)

for (i in 1:length(ag_vars)) {
  
tmp <- data %>%
  select(paste0("est_",ag_vars[[i]]$UniqueID)) %>%
  mutate(sum = rowSums(.)) %>%
  select(sum) %>%
  rename(!!paste0("est_",unique(ag_vars[[i]]$MEASURE)) := sum)

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

data %<>%
  left_join(ag_vars_out,by = "GEOID")


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


# Remove qroup quarters from the lists
numer <- numer[numer != "est_B26001_001"]
denom <- denom[denom != "est_B26001_001"]

# Calculate rates
prt<-purrr::map2(.x=numer,.y=denom,.f=getPrt) %>% as.data.frame()

# Append the non % / rate variables
non_pct_data <- data.frame(data[,paste0("est_",nopct)])

All_data <- prt %>%
  bind_cols(non_pct_data)

#Rename Variables & add ID
names(All_data) <- vars_new$MEASURE[match(names(All_data), paste0("est_",vars_new$UniqueID))]

All_data  %<>%
  bind_cols(data[,"GEOID"])

# Save the measures and description file
saveRDS(All_data,"./data/All_data_1.2.rds")
saveRDS(vars_new,"./data/vars_new.rds")


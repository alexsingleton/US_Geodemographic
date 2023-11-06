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
# Universe of potential variables exploration
#
# This code pulls a list of variables that might be considered in a new geodemographic within the
# context of our previous work on the US (https://doi.org/10.1080/00045608.2015.1052335)
#################################################################################################

# Create full ACS Table List and Initial Variables
#### (https://www.census.gov/programs-surveys/acs/technical-documentation/table-shells.html)
GET("https://www2.census.gov/programs-surveys/acs/tech_docs/table_shells/table_lists/2019_DataProductList.xlsx", write_disk(tf <- tempfile(fileext = ".xlsx")))
acs_tables <- read_excel(tf)

acs_tables %<>%
  mutate(Y5=ifelse(str_detect(Year,"5"),1,0),
         Letter_End=ifelse(str_detect(`Table ID`,"[A-Z]$"),1,0)) # Check if table is available for Y5 and is a main table

# Remove 1 year and very detail cross tabulation tables
acs_tables %<>%
  filter(Y5 == 1 & Letter_End == 0) %>%
  select(`Table ID`,`Table Title`)

# Import original classification variable list (https://github.com/geoss/acs_demographic_clusters/)
vars <- read.csv("data/usa_trt_varnames_051313.csv",stringsAsFactors = F)
in.vars <- as.character(vars[vars$new_set==0, "var"])
vars <- vars[vars$var %in% in.vars,]


# Create variable list with ACS codes, relative denominator and table reference

vars %<>%
  mutate(code= sub('.*\\_','',var),
         table=substr(code,start=1, stop=6),
         code= paste0(substring(code,1,nchar(code)-3),"_",substring(code,nchar(code)-2,nchar(code))),
         pct=ifelse(substring(desc,1,3)=="PCT",TRUE,FALSE),
         denom=ifelse(pct==TRUE,paste0(substr(code,start=1, stop=6),"_001"),"NULL"))



# Add a variable to identify if a table supplied a variable for the original classification
original_tables <- unique(vars$table)[- c(1:3)] # tables used in the original classification

acs_tables %<>%
  mutate(Orig_Var = ifelse(`Table ID` %in% original_tables, 1, 0))

# write.csv(acs_tables_list,"Table_List.csv") - this was used to manually inspect


# Expand the universe of tables to examine - these were additional tables not used in the original classification; and also some replacements for tables no longer available

all_tables <- c(original_tables,c("B04007","B08007","B08008","B08009","B08013","B08015","B11017","B16009","C17002","B03002","B03003","B08301","B08302","B23024","B16004","C24030",
                                  "C18131","B19080","B19081","B19082","B25001","B25004","B25017","B25018",
                                  "B25041","B25056","B25068","B25070","B25076","B25077","B25078","B25085","B25087","B25088","B25104","B25105",
                                  "B26001","B27011","C27013","C27014","B28001","B28002","B28005","B28010","B28011",
                                  "B15003","B12001","B05012","B08302",
                                  "C24040","C02003","B22010","B09019"))
#1 year only - B07001 B07003

#Remove tables that have no block group level 

#### (https://www.census.gov/programs-surveys/acs/technical-documentation/summary-file-documentation.html)
GET("https://www2.census.gov/programs-surveys/acs/summary_file/2019/documentation/tech_docs/ACS_2019_SF_5YR_Appendices.xlsx", write_disk(tf <- tempfile(fileext = ".xlsx")))
no_block_group <- read_excel(tf)

no_block_group %<>%
  filter(`Geography Restrictions` == "No Blockgroups") %>%
  select(`Table Number`) %>%
  pull()


all_tables <- setdiff(all_tables,no_block_group)



# Download selected table variables and descriptions (https://www.census.gov/programs-surveys/acs/technical-documentation/table-shells.html)

GET("https://www2.census.gov/programs-surveys/acs/summary_file/2019/documentation/user_tools/ACS2019_Table_Shells.xlsx", write_disk(tf <- tempfile(fileext = ".xlsx")))
acs_variables <- read_excel(tf)

acs_variables_initial <- acs_variables %>%
  filter(`Table ID` %in% all_tables) %>%
  mutate(Orig_Table = ifelse(`Table ID` %in% original_tables, 1, 0), Orig_Var = ifelse(UniqueID %in% vars$code, 1, 0))

write.csv(acs_variables_initial,"./data/acs_variables_initial.csv") # These were used to explore the variables, manually examine duplicates and select initial variables for evaluation




#######################################################################################
#######################################################################################
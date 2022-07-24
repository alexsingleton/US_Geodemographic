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
  rename_at(vars(V1:V39), ~ T_n)







write_csv(TYPE,"Grand_Index_Clusters_TYPES_BG_Logit.csv")


############################ Map ##################

Block_Group_SF <- readRDS("./data/Block_Group_SF.rds")


Block_Group_SF %<>%
  left_join(usa.bg.cl) %>%
  select(GEOID,Type)

st_write(Block_Group_SF, "data/BG_TYPE_SF.shp")

st_write(Block_Group_SF, "data/BG_TYPE_SF.gpkg")




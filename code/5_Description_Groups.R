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
usa.bg.cl <- read_parquet("./data/usa.bg.cl.group.parquet")
vars_new <-  read_csv("data/acs_variables_initial_1.5.csv")[-1]


# Append Clusters

usa.bg.cl %<>%
  rename(Group = cluster) %>%
  left_join(data) %>%
  rename(TotPop = B01001_001)

#########################
# Create Index Scores
#########################

GROUP <- usa.bg.cl %>%
  select(-c(GEOID)) %>%
group_by(Group) %>% 
  summarise_all(sum,na.rm = TRUE) 

# Convert to %
  
  GROUP %<>%
  select(-Group) %>%
  mutate_all(~(./sum(.)*100))

  #Remove the denominator columns
  GROUP %<>%
    select(!ends_with('_001'))
  
  
# Calculate index scores

GROUP %<>%
  mutate_at(vars(colnames(GROUP[,2]):colnames(GROUP[,ncol(GROUP)])), ~(./ TotPop * 100)) %>%
  select(-TotPop)


#Create Output

GROUP %<>%
  t()%>%
  as.data.frame() %>%
  rownames_to_column(var = "UniqueID") %>%
  as_tibble(name_repair = "minimal") %>%
  left_join(vars_new, by="UniqueID") %>%
  select(UniqueID,V1:V7,Stub,CONCEPT,DOMAIN)


write_csv(GROUP,"data/Grand_Index_Clusters_K_7_BG_Logit.csv")






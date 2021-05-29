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
library(janitor)

#Get Source Data
data <- readRDS("./data/data.rds")
usa.trt.cl <- read_csv("Clusters_BX.csv")
vars_new <- readRDS("./data/vars_new.rds")




# Append Clusters

usa.trt.cl %<>%
  left_join(data)

#########################
# Create Index Scores
#########################

X7 <- usa.trt.cl %>%
  select(-c(GEOID,cluster,X8,X69,NAME)) %>%
group_by(X15) %>% 
  summarise_all(sum,na.rm = TRUE)


X15 %<>%
pivot_longer(
  cols = starts_with("est_")) %>%
pivot_wider(id_cols = "name",names_from = "X15")







#Get base distribution
base_pct <- X15 %>%
  filter(name == "est_B01001_001") %>% # Total Population
  adorn_percentages



# Convert to percentages (exclude totals, but include Group Quarters)
target_pct <- X15 %>%
  filter(!str_detect(name, '_001') | name == "est_B26001_001") %>%
  adorn_percentages



#Calculate index scores
index_scores <- as_tibble((target_pct[,-1] / base_pct[rep(1:nrow(base_pct), nrow(target_pct)),-1]) * 100) %>% #calculate index score (target / base * 100)
  mutate(UniqueID = sub("est_", "", target_pct$name)) %>%# append variable code
  left_join(vars_new[,c("UniqueID")])

# Get a list of ACS variables and append to index
variables <- load_variables(2019,"acs5")

index_scores %<>%
  left_join(variables,by = c("UniqueID" = "name"))


write_csv(index_scores,"grand_indexBX.csv")





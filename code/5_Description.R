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
library(janitor)


#Get Source Data
data <- readRDS("./data/data.rds")
usa.trt.cl <- read_csv("Clusters.csv")
vars_new <- readRDS("./data/vars_new.rds")




# Append Clusters

usa.trt.cl %<>%
  left_join(data)

#########################
# Create Index Scores
#########################

X10 <- usa.trt.cl %>%
  select(-c(GEOID,cluster,X45,X92,NAME)) %>%
group_by(X10) %>% 
  summarise_all(sum,na.rm = TRUE)


X10 %<>%
pivot_longer(
  cols = starts_with("est_")) %>%
pivot_wider(id_cols = "name",names_from = "X10")







#Get base distribution
base_pct <- X10 %>%
  filter(name == "est_B01001_001") %>% # Total Population
  adorn_percentages



# Convert to percentages
target_pct <- X10 %>%
  filter(!str_detect(name, '_001')) %>%
  adorn_percentages

#Calculate index scores
index_scores <- as_tibble((target_pct[,-1] / base_pct[rep(1:nrow(base_pct), nrow(target_pct)),-1]) * 100) %>% #calculate index score (target / base * 100)
  mutate(UniqueID = sub("est_", "", target_pct$name)) %>%# append variable code
  left_join(vars_new[,c("UniqueID")])

# Get a list of ACS variables and append to index
variables <- load_variables(2019,"acs5")

index_scores %<>%
  left_join(variables,by = c("UniqueID" = "name"))


write_csv(index_scores,"grand_index.csv")







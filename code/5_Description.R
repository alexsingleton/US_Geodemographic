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
usa.trt.cl <- read_csv("Clusters_K_10_BG_Logit.csv")
vars_new <- readRDS("./data/vars_new_1.5.rds")

v_used <- vars_new %>% filter(`New Variables` == 1) %>% select(MEASURE) %>% pull() # select proposed used variables


# Append Clusters

usa.trt.cl %<>%
  rename(Group = cluster) %>%
  left_join(data)

#########################
# Create Index Scores
#########################

GROUP <- usa.trt.cl %>%
  select(-c(GEOID)) %>%
group_by(Group) %>% 
  summarise_all(sum,na.rm = TRUE) 

nms <- paste(vars_new$MEASURE[match(names(GROUP), vars_new$UniqueID)])

names(GROUP) <-  c("Group","Tot_Pop",nms[3:length(nms)])

GROUP <- subset(GROUP, select=which(!duplicated(names(GROUP)))) 

GROUP %<>%
  select(all_of(c("Group","Tot_Pop",v_used)))


# Convert to %
  
  GROUP %<>%
  select(-Group) %>%
  mutate_all(~(./sum(.)*100))

# Calculate index scores

GROUP %<>%
  mutate_at(vars(colnames(GROUP[,2]):colnames(GROUP[,ncol(GROUP)])), ~(./ Tot_Pop * 100)) %>%
  select(-Tot_Pop)



write_csv(GROUP,"grand_indexBX.csv")





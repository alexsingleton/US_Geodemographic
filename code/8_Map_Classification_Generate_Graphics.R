# Load Packages
library(tidyverse)
library(magrittr)


setwd("~/GitHub/")

Index_Scores_Groups <- read_csv("Grand_Index_Clusters_K_7_BG_Logit.csv")

Index_Scores_Groups %<>% 
   rename_with(~LETTERS[1:7], 2:8)



Index_Scores_Groups_l <- reshape2::melt(Index_Scores_Groups)


long <- Index_Scores_Groups_l %>%
filter(grepl("B19001", UniqueID))
  
long$Stub <- factor(long$Stub, levels = unique(long$Stub))


 ggplot(long, aes(Stub , variable, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 100) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "", y = "", title = "", fill = "Index Score")

 
 
 
 # Map of the geodemographic
 
 
 
 
 
 
 
 
 
 #Table of cluster assignments
 
 
 library(arrow)
 
 # read the parquet file into a data frame
 clusters <- arrow::read_parquet("data/usa.bg.cl.type.parquet")
 
 
 
 
 
 
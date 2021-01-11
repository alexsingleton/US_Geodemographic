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


All_data <- readRDS("./data/All_data_1.2.rds")
data <- readRDS("./data/data.rds")
vars_new <- readRDS("./data/vars_new.rds")

# Add a state ID
All_data %<>%
  mutate(state = str_sub(GEOID,1,2))

################################################################################################
# Correlation / Input Refinement
#
# This code produces correlation descriptions and attribute summaries and was used to refine
# the input measure list which are versioned between 1.0 and 1.2
#################################################################################################



############################
# Check Correlations 1
############################

# Calculate correlations
data_corr <- All_data %>%
  select(-c("GEOID","state")) %>%
  correlate()

# Filter correlations for 0.6 threshold for inspection
g <- data_corr %>%
  shave() %>%
  stretch() %>%
  drop_na() %>%
  filter((r > 0.6)|(r < -0.6)) %>%
  mutate(band = cut(r, breaks = c(-Inf, -0.7, -0.6,0, 0.6,0.7,Inf)))

# Build a correlation graph
graph_corr <- g %>%
  as_tbl_graph()

# Append node attribute measures
graph_corr %<>%
  left_join(vars_new, by=c("name" = "MEASURE")) 

# Fix level Order
graph_corr %<>% 
  activate(edges) %>% 
  mutate(band = factor(band, levels = c("(-Inf,-0.7]","(-0.7,-0.6]","(0.6,0.7]","(0.7, Inf]")))

# Plot graph
graph_corr %>%
  ggraph(layout = "kk") +
  geom_edge_diagonal(aes(colour = band)) +
  scale_edge_colour_viridis(option = "plasma",alpha = 0.5, discrete = TRUE) +
  geom_node_point(aes(colour = CONCEPT), size = 0.7) +
  geom_node_text(aes(label = name), size = 1, check_overlap =TRUE)



############################
# Check Correlations 2
############################

# Regress all of the variables against each of the input variables.
# The resulting r-squared values saved.
# Variables which have a perfect dependency among them are omitted from the regression. 
# An example of dependency would be something like percent owner occupied and percent renter occupied 
# which together will always 100%


# d.fit <- numeric()
# 
# DF <- All_data[,-which( colnames(All_data) %in%  c("GEOID","state"))]
# 
# for(i in names(DF)){
#   print(i)
#   d <-lm(DF[,as.character(i)] ~., 
#          data=DF[,-c(which(x=names(DF)==i, arr.ind=TRUE))])
#   d.fit <- append(d.fit, summary(d)$adj.r.squared)
#   #break
# }
# 
# d.fit <- as.numeric(d.fit)
# 
# 
# 
# #save the r-squared of the regressions in a data frame
# cor_out <- data.frame(var=names(DF), rsq=d.fit)
# head(cor_out[order(-cor_out$rsq), ]) #5 highest r-square
# tail(cor_out[order(-cor_out$rsq), ]) #5 lowest r-square






######################################
# Summary of variable characteristics
######################################

view(dfSummary(All_data), file = "Summary_Inputs.html")








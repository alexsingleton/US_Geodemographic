# Load Packages & Data
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

All_data <- readRDS("./data/All_data_1.5.rds")
data <- readRDS("./data/data_BG_1.5.rds")
vars_new <- readRDS("./data/vars_new_1.5.rds")

# Add a state ID
All_data %<>%
  mutate(state = str_sub(GEOID,1,2))


################################################################################################
# Correlation / Input Refinement
#
# This code produces correlation descriptions and attribute summaries and was used to refine
# the input measure list which are versioned between 1.0 and 1.5
#################################################################################################

v_used <- vars_new %>% filter(`New Variables` == 1) %>%
          select(MEASURE) %>% 
          pull() # select proposed used variables

############################
# Check Correlations 1
############################

# Calculate correlations
data_corr <- All_data %>%
  select(all_of(v_used)) %>%
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


######################################
# Summary of variable characteristics
######################################

#view(dfSummary(All_data, graph.col = FALSE), file = "Summary_Inputs.html")




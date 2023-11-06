
# Load Packages
library(tidyverse)
library(magrittr)
library(arrow)
library(tigris)
library(ggplot2)
library(sf)

setwd("~/GitHub/")

usa.bg <- readRDS("./data/All_data_1.5.rds")
vars_new <- readRDS("./data/vars_new_1.5.rds")

usa.bg.cc <- arrow::read_parquet("data/usa.bg.cc.parquet")
usa.bg.ic <- arrow::read_parquet("data/usa.bg.ic.parquet")

clusters <- arrow::read_parquet("data/usa.bg.cl.type.parquet")

Index_Scores_Groups <- read_csv("Grand_Index_Clusters_K_7_BG_Logit.csv")

#Create Cluster Codes
clusters %<>%
  separate(cluster, c("group", "type"), sep = "_") %>%
  mutate(group = recode(group, `1` = "A", `2` = "B", `3` = "C", `4` = "D", `5` = "E",`6` = "F",`7` = "G")) %>%
  unite(type,group,type,sep = "",remove = FALSE)




# Create Colour Codes for Groups and Types


group_palette <- c("A" = "#2E8B57", "B" = "#FFA07A", "C" = "#4682B4", "D" = "#9370DB", "E" = "#FFD700", "F" = "#eb5252", "G" = "#8bb92d", "Unclassified" = "#DCDCDC")



Colours_Type <- tibble(
  type = c('A1','A2','A3','A4','A5','B1','B2','B3','B4','B5','C1','C2','C3','C4','C5','C6','C7','D1','D2','D3','D4','D5','E1','E2','E3','E4','E5','F1','F2','F3','F4','F5','F6','G1','G2','G3','G4','G5','G6',NA),
  t_col = 
c("#4CAF80","#5CCD8B","#7EDD9C","#A2EDB4","#C4F4CB",
"#FFAF8C","#FFBC99","#FFC8A7","#FFD5B5","#FFE0C2",
"#5d91b9","#74a0bf","#8bb0c5","#a2c0cb","#b9d0d1","#d0e0d7","#e7f0dd",
"#B38EDC","#D1ADD9","#E9C8DD","#F6DEE6","#FCECF6",
"#FFE242","#FFE871","#FFEB9E","#FFF1CC","#FFFCE0",
"#c13b3b","#d04b4b","#e05c5c","#ed6d6d","#f47e7e","#fb8f8f",
"#9cc237","#aecd42","#bed54c","#cedc56","#dee463","#eee86d",
"#DCDCDC")
)





 # Map of the geodemographic
 
# New York
 NYC <- block_groups(state = "NY",county = c("New York","Bronx","Kings","Queens","Richmond"),  cb = TRUE)
 NYC %<>%
   select(GEOID) %>%
   erase_water()
 
 # Boulder
 Boulder <- block_groups(state = "CO",county = "Boulder",  cb = TRUE)
 Boulder %<>%
   select(GEOID) %>%
   erase_water()
 
 # Seattle 
 Seattle   <- block_groups(state = "WA",county = c("King","Pierce","Snohomish"),  cb = TRUE)
 Seattle  %<>%
   select(GEOID) %>%
   erase_water()
 
 
 
 
 # New York Map
 
 NYC %<>%
   left_join(clusters,by = "GEOID")
 
 
 NYC <- NYC %>%
   mutate(group = factor(group, levels = c("A","B","C","D","E","F","G",NA), ordered = TRUE)) %>%
   mutate(group = replace_na(as.character(group), "Unclassified"))
 
 NYC_Counties <-  counties(state = "NY",  cb = TRUE) %>%
   filter(NAME %in% c("New York","Bronx","Kings","Queens","Richmond")) %>%
   erase_water()
 
 ggplot() +
   geom_sf(data = NYC, aes(fill = group),color = NA) +
   geom_sf(data = NYC_Counties, fill = NA, color = "black")+
   scale_fill_manual(values = group_palette,drop = FALSE,name = "Group") +
   theme_void() 
 
 
 ggsave("maps/NYC.pdf")
 
 # Boulder Map
 
 
 Boulder %<>%
   left_join(clusters,by = "GEOID")
 
 Boulder <- Boulder %>%
   mutate(group = factor(group, levels = c("A","B","C","D","E","F","G",NA), ordered = TRUE)) %>%
   mutate(group = replace_na(as.character(group), "Unclassified"))
 
Boulder_Counties <-  counties(state = "CO",  cb = TRUE) %>%
   filter(NAME %in% c("Boulder")) %>%
   erase_water()
 

 ggplot() +
   geom_sf(data = Boulder, aes(fill = group),color = NA) +
   scale_fill_manual(values = group_palette,drop = FALSE,name = "Group") +
   geom_sf(data = Boulder_Counties, fill = NA, color = "black")+
   theme_void() 
 
 
 ggsave("maps/Boulder.pdf",width = 9)
   
 # Seattle Map
 
 
 Seattle %<>%
   left_join(clusters,by = "GEOID")
 
 Seattle <- Seattle %>%
   mutate(group = factor(group, levels = c("A","B","C","D","E","F","G",NA), ordered = TRUE)) %>%
   mutate(group = replace_na(as.character(group), "Unclassified"))
 
 Seattle_Counties <-  counties(state = "WA",  cb = TRUE) %>%
   filter(NAME %in% c("King","Pierce","Snohomish")) %>%
   erase_water()
 
 
 ggplot() +
   geom_sf(data = Seattle, aes(fill = group),color = NA) +
   scale_fill_manual(values = group_palette,drop = FALSE,name = "Group") +
   geom_sf(data = Seattle_Counties, fill = NA, color = "black")+
   theme_void() 
 
 
 ggsave("maps/Seattle.pdf")
 
 
 
 
 
 
 
 
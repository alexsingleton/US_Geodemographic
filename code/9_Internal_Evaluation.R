# Load Packages
library(tidyverse)
library(magrittr)
library(arrow)
library(tigris)
library(ggplot2)
library(sf)
library(viridis)

setwd("~/GitHub/")

usa.bg <- readRDS("./data/All_data_1.5.rds")
vars_new <- readRDS("./data/vars_new_1.5.rds")

usa.bg.cc <- arrow::read_parquet("data/usa.bg.cc.parquet")
usa.bg.ic <- arrow::read_parquet("data/usa.bg.ic.parquet")

clusters <- arrow::read_parquet("data/usa.bg.cl.type.parquet")
#Create Cluster Codes
clusters %<>%
  separate(cluster, c("group", "type"), sep = "_") %>%
  mutate(group = recode(group, `1` = "A", `2` = "B", `3` = "C", `4` = "D", `5` = "E",`6` = "F",`7` = "G")) %>%
  unite(type,group,type,sep = "",remove = FALSE)





#Split the data by Type
Fit_Stat <- usa.bg.cc %>%
  left_join(clusters)


Type_Means <- Fit_Stat %>% 
  select(Male_Under_5_years:type) %>% 
  group_by(type) %>% 
  summarise(across(everything(), mean))


Fit_Stat %<>% 
  group_split(type,.keep = TRUE)



#Calculate error scores for Types


tmp_ALL <- NA

for (x in 1:length(Fit_Stat)){
  
  tmp_out <- NA
  
  for (i in 1:nrow(Fit_Stat[[x]])){
    tmp <- Fit_Stat[[x]][i,2:203]
    tmp <- abs(tmp - Type_Means[x,2:203])
    tmp_out <- rbind(tmp_out,tmp)
  }
  
  tmp_out <- data.frame(Fit_Stat[[x]]$GEOID,tmp_out[-1,])
  
  tmp_ALL <- rbind(tmp_ALL,tmp_out)
  rm(tmp_out)
  
}


tmp_ALL <- tmp_ALL[-1,]
tmp_ALL$SUM <- rowSums(tmp_ALL[,2:203])

colnames(tmp_ALL) <- c("GEOID",colnames(tmp_ALL)[2:203])


#Split the data by Type
tmp_ALL <- tmp_ALL %>%
  left_join(clusters)




write_parquet(tmp_ALL, "./data/Error_Scores_Types.parquet")



my_palette <- c("#2E8B57", "#FFA07A","#4682B4","#9370DB","#FFD700","#eb5252","#8bb92d")

ggplot(tmp_ALL, aes(x = type, y = SUM, fill = group)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = my_palette) +
  labs(x = "Type", y = "Error",fill = "Group") +
  theme_minimal() +
  coord_cartesian(ylim = c(90, 350))


ggsave("graphs/Error_by_Type.pdf",width = 9)





#############################

#Map the error

Block_Group_SF <- readRDS("~/GitHub/data/Block_Group_SF.rds")

Block_Group_SF %<>%
  left_join(tmp_ALL) %>%
  select(GEOID,SUM)

Block_Group_SF_Shifted <- shift_geometry(Block_Group_SF)


states <- states() %>%
  shift_geometry()
  

# Plot state boundaries using ggplot2
ggplot() +
  theme_void()



ggplot() + 
  geom_sf(data = Block_Group_SF_Shifted, aes(fill = SUM), color = NA) +
  scale_fill_viridis(option = "inferno") +
  geom_sf(data = states, fill = NA, color = "#FFFFFF") +
  labs(fill = "Error") +
  theme_void()

ggsave("maps/Error_Map_USA.png",width = 20,dpi=300)


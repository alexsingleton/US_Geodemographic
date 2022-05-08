


#????
results <- readRDS("./data/results_10k_7C_Logit_BG.rds")
usa.bg.cl <- readRDS("./data/usa.bg.cl.rds")

############################################
# Append back Tracts with incomplete cases
############################################

# Get Cluster Centres

centre_output <- h2o.centers(results) %>%
  as_tibble()


#NA / Row

usa.bg.ic %<>% 
  mutate(na = rowSums(is.na(.))) 

#Check whether population within BG

Population_Present <- data %>%
  select(GEOID,B01001_001) 

#Append population within BG
usa.bg.ic %<>%
  left_join(Population_Present, by='GEOID')




# Explore missing variables and population
# usa.bg.ic %>%
#   select(B01001_001,na) %>%
#   filter(B01001_001 > 0)%>%
#   View()


# Filter for areas with only one missing value
usa.bg.ic.cut <- usa.bg.ic %>%
  filter(na == 1) %>%
  select(-B01001_001,-na)







colnames(centre_output) <- colnames(usa.bg.ic.cut[,2:191])






dist.k <- data.frame()

for (row in 1:nrow(usa.bg.ic.cut)){
  
  for (k in 1:10){
    
    dist.k[row, k] <- as.vector(dist(rbind(centre_output[k,], usa.bg.ic.cut[row, 2:191]))) #1 is GEOID
    
  }
}



#Cluster membership for incomplete cases
#select column index for each observation
#column index also represents the cluster id.

mins <- data.frame(cl=NA)
for (row in 1:nrow(dist.k)){
  mins[row,] <- ifelse(test=is.na(dist.k[row,]), yes=NA, no=which.min(dist.k[row,]))
}



#Append GEOID to imputed clusters
mins %<>%
  mutate(GEOID = usa.bg.ic.cut$GEOID, cluster = cl) %>%
  select(-cl) %>%
  as_tibble()

#Create lookup for unknown clusters

# Filter for areas with only one missing value
UnK_Clusters <- usa.bg.ic %>%
  filter(na > 1) %>%
  select(GEOID) %>%
  mutate(cluster = 99)




# Append missing and unknown clusters
usa.bg.cl %<>%
  bind_rows(mins) %>%
  bind_rows(UnK_Clusters)  

#Export clusters

write_csv(usa.bg.cl,"Clusters_K_7_BG_Logit.csv")
write_parquet(usa.bg.cl, "Clusters_K_7_BG_Logit.parquet")
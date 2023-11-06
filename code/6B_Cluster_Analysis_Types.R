# Load Packages & Data
library(readr)
library(tidycensus)
library(tigris)
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
library(caret)
library(e1071)
library(h2o)
library(gtools)
library(arrow)
library(h2o)

vars_new <- readRDS("./data/vars_new_1.5.rds")
v_used <- vars_new %>% filter(`New Variables` == 1) %>% select(MEASURE) %>% pull() # select proposed used variables

################################################################################################
# Cluster Analysis
#################################################################################################

# Read Input Data
usa.bg.cc <- read_parquet("usa.bg.cc.parquet") # Complete Cases
usa.bg.ic <- read_parquet( "usa.bg.ic.parquet") # Incomplete Cases
usa.bg.cl <- read_parquet("./data/usa.bg.cl.group.parquet") # Clusters (Groups)


# Append Clusters 
usa.bg.cc %<>%
  left_join(usa.bg.cl)


## Start H2O Cluster

h2o.init(max_mem_size="50G")

#########################################################################
#### Clustering (Group 1)
#########################################################################

#Get Cluster 1 input data
aa_h20 <-usa.bg.cc %>%
  filter(cluster == 1) %>%
  select(-cluster) %>%
  as.h2o()


# H2O test

results <- h2o.kmeans(training_frame = aa_h20, k = 5, x = v_used, init = "Random",max_iterations=1,standardize = FALSE)
wss <- h2o.tot_withinss(results)

ptm <- proc.time()

for(i in 1:1000) {
 
results_run <- h2o.kmeans(training_frame = aa_h20, k = 5, x = v_used, init = "Random",max_iterations=1000,standardize = FALSE)
wss_run <- h2o.tot_withinss(results_run)

if(wss_run < wss) {
  
  wss <- wss_run
  results <- results_run
  
}
print(i)
}

proc.time() - ptm


# Save all results from 10k run ( hours)
saveRDS(results,"./data/results_10k_C1_T5_Logit_tract.rds")


ID <- usa.bg.cc %>%
  filter(cluster == 1) %>%
  select(-cluster) %>%
select("GEOID")

clusters <- as_tibble(h2o.predict(results,aa_h20))

usa.bg.cl.C1_T5 <- tibble(GEOID= ID$GEOID,cluster=paste0("1_",(clusters$predict +1)))


#########################################################################
#### Clustering (Group 2)
#########################################################################

#Get Cluster 2 input data
aa_h20 <-usa.bg.cc %>%
  filter(cluster == 2) %>%
  select(-cluster) %>%
  as.h2o()


# H2O test

results <- h2o.kmeans(training_frame = aa_h20, k = 5, x = v_used, init = "Random",max_iterations=1,standardize = FALSE)
wss <- h2o.tot_withinss(results)

ptm <- proc.time()

for(i in 1:1000) {
  
  results_run <- h2o.kmeans(training_frame = aa_h20, k = 5, x = v_used, init = "Random",max_iterations=1000,standardize = FALSE)
  wss_run <- h2o.tot_withinss(results_run)
  
  if(wss_run < wss) {
    
    wss <- wss_run
    results <- results_run
    
  }
  print(i)
}

proc.time() - ptm


# Save all results from 10k run ( hours)
saveRDS(results,"./data/results_10k_C2_T5_Logit_tract.rds")


ID <- usa.bg.cc %>%
  filter(cluster == 2) %>%
  select(-cluster) %>%
  select("GEOID")

clusters <- as_tibble(h2o.predict(results,aa_h20))

usa.bg.cl.C2_T5 <- tibble(GEOID= ID$GEOID,cluster=paste0("2_",(clusters$predict +1)))

#########################################################################
#### Clustering (Group 3)
#########################################################################

#Get Cluster 3 input data
aa_h20 <-usa.bg.cc %>%
  filter(cluster == 3) %>%
  select(-cluster) %>%
  as.h2o()


# H2O test

results <- h2o.kmeans(training_frame = aa_h20, k = 7, x = v_used, init = "Random",max_iterations=1,standardize = FALSE)
wss <- h2o.tot_withinss(results)

ptm <- proc.time()

for(i in 1:1000) {
  
  results_run <- h2o.kmeans(training_frame = aa_h20, k = 7, x = v_used, init = "Random",max_iterations=1000,standardize = FALSE)
  wss_run <- h2o.tot_withinss(results_run)
  
  if(wss_run < wss) {
    
    wss <- wss_run
    results <- results_run
    
  }
  print(i)
}

proc.time() - ptm


# Save all results from 10k run ( hours)
saveRDS(results,"./data/results_10k_C3_T7_Logit_tract.rds")


ID <- usa.bg.cc %>%
  filter(cluster == 3) %>%
  select(-cluster) %>%
  select("GEOID")

clusters <- as_tibble(h2o.predict(results,aa_h20))

usa.bg.cl.C3_T7 <- tibble(GEOID= ID$GEOID,cluster=paste0("3_",(clusters$predict +1)))


#########################################################################
#### Clustering (Group 4)
#########################################################################

#Get Cluster 4 input data
aa_h20 <-usa.bg.cc %>%
  filter(cluster == 4) %>%
  select(-cluster) %>%
  as.h2o()


# H2O test

results <- h2o.kmeans(training_frame = aa_h20, k = 5, x = v_used, init = "Random",max_iterations=1,standardize = FALSE)
wss <- h2o.tot_withinss(results)

ptm <- proc.time()

for(i in 1:1000) {
  
  results_run <- h2o.kmeans(training_frame = aa_h20, k = 5, x = v_used, init = "Random",max_iterations=1000,standardize = FALSE)
  wss_run <- h2o.tot_withinss(results_run)
  
  if(wss_run < wss) {
    
    wss <- wss_run
    results <- results_run
    
  }
  print(i)
}

proc.time() - ptm


# Save all results from 10k run ( hours)
saveRDS(results,"./data/results_10k_C4_T5_Logit_tract.rds")


ID <- usa.bg.cc %>%
  filter(cluster == 4) %>%
  select(-cluster) %>%
  select("GEOID")

clusters <- as_tibble(h2o.predict(results,aa_h20))

usa.bg.cl.C4_T5 <- tibble(GEOID= ID$GEOID,cluster=paste0("4_",(clusters$predict +1)))


#########################################################################
#### Clustering (Group 5)
#########################################################################

#Get Cluster 5 input data
aa_h20 <-usa.bg.cc %>%
  filter(cluster == 5) %>%
  select(-cluster) %>%
  as.h2o()


# H2O test

results <- h2o.kmeans(training_frame = aa_h20, k = 5, x = v_used, init = "Random",max_iterations=1,standardize = FALSE)
wss <- h2o.tot_withinss(results)

ptm <- proc.time()

for(i in 1:1000) {
  
  results_run <- h2o.kmeans(training_frame = aa_h20, k = 5, x = v_used, init = "Random",max_iterations=1000,standardize = FALSE)
  wss_run <- h2o.tot_withinss(results_run)
  
  if(wss_run < wss) {
    
    wss <- wss_run
    results <- results_run
    
  }
  print(i)
}

proc.time() - ptm


# Save all results from 10k run ( hours)
saveRDS(results,"./data/results_10k_C5_T5_Logit_tract.rds")


ID <- usa.bg.cc %>%
  filter(cluster == 5) %>%
  select(-cluster) %>%
  select("GEOID")

clusters <- as_tibble(h2o.predict(results,aa_h20))

usa.bg.cl.C5_T5 <- tibble(GEOID= ID$GEOID,cluster=paste0("5_",(clusters$predict +1)))


#########################################################################
#### Clustering (Group 6)
#########################################################################

#Get Cluster 6 input data
aa_h20 <-usa.bg.cc %>%
  filter(cluster == 6) %>%
  select(-cluster) %>%
  as.h2o()


# H2O test

results <- h2o.kmeans(training_frame = aa_h20, k = 6, x = v_used, init = "Random",max_iterations=1,standardize = FALSE)
wss <- h2o.tot_withinss(results)

ptm <- proc.time()

for(i in 1:1000) {
  
  results_run <- h2o.kmeans(training_frame = aa_h20, k = 6, x = v_used, init = "Random",max_iterations=1000,standardize = FALSE)
  wss_run <- h2o.tot_withinss(results_run)
  
  if(wss_run < wss) {
    
    wss <- wss_run
    results <- results_run
    
  }
  print(i)
}

proc.time() - ptm


# Save all results from 10k run ( hours)
saveRDS(results,"./data/results_10k_C6_T6_Logit_tract.rds")


ID <- usa.bg.cc %>%
  filter(cluster == 6) %>%
  select(-cluster) %>%
  select("GEOID")

clusters <- as_tibble(h2o.predict(results,aa_h20))

usa.bg.cl.C6_T6 <- tibble(GEOID= ID$GEOID,cluster=paste0("6_",(clusters$predict +1)))


#########################################################################
#### Clustering (Group 7)
#########################################################################

#Get Cluster 7 input data
aa_h20 <-usa.bg.cc %>%
  filter(cluster == 7) %>%
  select(-cluster) %>%
  as.h2o()


# H2O test

results <- h2o.kmeans(training_frame = aa_h20, k = 6, x = v_used, init = "Random",max_iterations=1,standardize = FALSE)
wss <- h2o.tot_withinss(results)

ptm <- proc.time()

for(i in 1:1000) {
  
  results_run <- h2o.kmeans(training_frame = aa_h20, k = 6, x = v_used, init = "Random",max_iterations=1000,standardize = FALSE)
  wss_run <- h2o.tot_withinss(results_run)
  
  if(wss_run < wss) {
    
    wss <- wss_run
    results <- results_run
    
  }
  print(i)
}

proc.time() - ptm


# Save all results from 10k run ( hours)
saveRDS(results,"./data/results_10k_C7_T6_Logit_tract.rds")


ID <- usa.bg.cc %>%
  filter(cluster == 7) %>%
  select(-cluster) %>%
  select("GEOID")

clusters <- as_tibble(h2o.predict(results,aa_h20))

usa.bg.cl.C7_T6 <- tibble(GEOID= ID$GEOID,cluster=paste0("7_",(clusters$predict +1)))


#########################################################################
#### Append All Results
#########################################################################


usa.bg.cl.T <-  bind_rows(list(usa.bg.cl.C1_T5, usa.bg.cl.C2_T5, usa.bg.cl.C3_T7, usa.bg.cl.C4_T5, usa.bg.cl.C5_T5, usa.bg.cl.C6_T6, usa.bg.cl.C7_T6))


write_parquet(usa.bg.cl.T, "./data/usa.bg.cl.type.parquet")

















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














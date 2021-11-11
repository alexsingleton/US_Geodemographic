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

usa.bg <- readRDS("./data/All_data_1.5.rds")
data <- readRDS("./data/data_BG_1.5.rds")
vars_new <- readRDS("./data/vars_new_1.5.rds")

v_used <- vars_new %>% filter(`New Variables` == 1) %>% select(MEASURE) %>% pull() # select proposed used variables

divide100 <- function(x, na.rm=FALSE) (x/100)

usa.bg %<>%
  select(all_of(c("GEOID",v_used))) %>%
  mutate_if(is.numeric, divide100)







################################################################################################
# Cluster Analysis
#
# This code creates initial 250 clusters
#################################################################################################

#Box Cox of input

# usa.bg.transform <- usa.bg %>%
#   select(all_of(v_used)) %>%
#   mutate_all(function(x) x+1) %>%
#   mutate_all(funs( BoxCoxTrans(.,na.rm = TRUE) %>% predict(.)))

# Constrained Logit

constrained_logit<-function(p,m=6){
  if (is.na(p)) {return(NA)}
  if (p <=0) {return(-m)}
  if (p >=1) {return(m)}
  else {return(min(max(log(p / (1 -p)),-6),6))}
}

usa.bg.transform <- usa.bg %>%
  select(all_of(c("GEOID",v_used))) %>%
  #filter(complete.cases(.)) %>%
  mutate(across(where(is.numeric),~map(.x,constrained_logit)),
         across(where(is.list),unlist))



#EXTRACT COMPLETE CASES (all GEOID checked and present in boundary data)
usa.bg.cc <- usa.bg.transform[complete.cases(usa.bg.transform), ]


#INCOMPELTE CASES (and count missing / problematic GEOID)
usa.bg.ic <- usa.bg.transform[!complete.cases(usa.bg.transform), ]

# Write Parquet File
write_parquet(usa.bg.cc, "usa.bg.cc.parquet")
write_parquet(usa.bg.ic, "usa.bg.ic.parquet")



# 
# 
# # Create Tract & logit standardise
# usa.tract <- usa.bg %>%
#   mutate(TRACT = str_sub(GEOID, 1,11)) %>%
#   select(-GEOID) %>%
#   group_by(TRACT) %>%
#   summarise(across( .cols = where(is.numeric), sum))
# 
# usa.tract %<>%
#   mutate(across(is.numeric,~map(.x,constrained_logit)),
#          across(where(is.list),unlist))
# 
# 
# 
# # Infill missing BG data with tract scores
# 
# usa.bg.ic_Infill <- usa.bg.ic %>%
#   select("GEOID")
# 
# for (i in 1:length(v_used)) {
# 
# v <- v_used[i]
# 
# BG_T <- usa.bg.ic %>%
#   select(c("GEOID",all_of(v))) %>%
#   rename(A = v) %>%
#   mutate(TRACT = str_sub(GEOID, 1,11))
# 
# T_T <- usa.tract %>%
#   select(c("TRACT",all_of(v))) %>%
#   rename(B = v) 
# 
# BG_T %<>%
#   left_join(T_T)
# 
# BG_T %<>%
#   mutate(C = if_else(
#     is.na(A),B,A
#   ))
# 
# BG_T %<>%
#   select("GEOID","C") %>%
#   setNames(c("GEOID",v))
# 
# usa.bg.ic_Infill %<>%
#   left_join(BG_T)
# 
# }
# 
# # Count Columns with missing Values
# 
# m_cnt <- usa.bg.ic_Infill %>%
#   is.na %>% 
#   rowSums
# 
# missing_count <- tibble(GEOID= usa.bg[!complete.cases(usa.bg.transform), ]$GEOID,missing=m_cnt)
# 
# 
# 
# #select only those GEOID that have boundaries and less than 5 missing data
# missing_count %<>%
#   filter(missing <= 50)
# 





##STANDARDIZE DATA TO A 0-1 RANGE
#range01 <- function(x){(x-min(x,na.rm = TRUE))/(max(x,na.rm = TRUE)-min(x,na.rm = TRUE))}

# input to cluster analysis is the complete cases standardized in 0-1 (range)
# aa <- usa.bg.cc %>%
#   mutate_all(range01)
# 
# # creates range standardized versions of records with at least one missing value for later cluster imputation
# bb <- usa.bg %>%
#   select(-"GEOID") %>%
#   mutate_all(range01) %>%
#   as_tibble()
# 
# bb %<>%
#   select(all_of(v_used)) %>%
#   mutate(GEOID = usa.bg$GEOID) %>%
#   filter(GEOID %in% missing_count$GEOID)





######################
# Runs k-means 10,000 (20hrs to complete on XEON W-2225 @ 4.10 GHZ )
######################

# c<- detectCores() - 1  
# cl <- makeCluster(c)
# registerDoParallel(cl)
# 
# 
# ptm <- proc.time()
# 
# #results <- foreach( i = rep(1,10000) ) %dopar% {
# results <- foreach( i = 1 ) %dopar% {
#     
#   kmeans( x=aa, centers=250, nstart=i )
# }
# proc.time() - ptm
# 
# # Stop Cluster
# stopCluster(cl)

#write.csv(aa,"US_BG_Data.csv")


h2o.init(max_mem_size="50G")

aa_h20 <- as.h2o(usa.bg.cc)

# H2O test

results <- h2o.kmeans(training_frame = aa_h20, k = 10, x = v_used, init = "Random",max_iterations=1,standardize = FALSE)
wss <- h2o.tot_withinss(results)

ptm <- proc.time()

for(i in 1:1000) {
 
results_run <- h2o.kmeans(training_frame = aa_h20, k = 10, x = v_used, init = "Random",max_iterations=1000,standardize = FALSE)
wss_run <- h2o.tot_withinss(results_run)

if(wss_run < wss) {
  
  wss <- wss_run
  results <- results_run
  
}
print(i)
}

proc.time() - ptm


# Save all results from 1k run (92.2 hours)
saveRDS(results,"./data/results_10k_10C_Logit_tract.rds")


# Pick the best result
#i <- sapply(results, function(result) result$tot.withinss)

#Graph Results
# TWSS_250_10k <- i %>%
#   as_tibble() %>%
#   rename(tot.withinss = value)
# 
# p <- ggplot(TWSS_250_10k,aes(y=tot.withinss))
# p + geom_boxplot()


# Select the result with the lowest TWSS
#result <- results[[which.min(i)]]

# Save optimal 250 cluster result
#saveRDS(result,"./data/final_k250_BX.rds")



#Append clusters to GEOID

#usa.bg.cl <- tibble(GEOID= usa.bg[complete.cases(usa.bg), ]$GEOID,cluster=result$cluster)




ID <- usa.bg %>%
select(all_of(c(v_used,"GEOID"))) %>%
filter(complete.cases(.)) %>%
select("GEOID")

clusters <- as_tibble(h2o.predict(results,aa_h20))

usa.bg.cl <- tibble(GEOID= ID$GEOID,cluster=(clusters$predict +1))




# ######################
# # Explore break points
# ######################
# 
# # Create a distance matrix for cluster centroids.
# #diss.ctr <- dist(test$centers)
# 
# diss.ctr <- dist(data.frame(h2o.centers(results)))
# 
# # Calculate Wards
# wards.ctr <-hclust(diss.ctr, method="ward.D")
# 
# # Silhouette to identify break points
# 
# sil <- NA  #hold fit statistics in a data.frame
# for (i in 2:249){
#   sil[i] <-  summary(silhouette(cutree(wards.ctr,k=i), diss.ctr))$avg.width
# }
# sil <- data.frame(sil=sil, k=1:249)[-1,]
# 
# #Plot Silhouette
# ggplot(data=sil, aes(x=log(k), y=sil, label=k)) +geom_line() + geom_vline(xintercept=log(c(9,21,44)), lty=3, lwd=.5)
# 
# #Create cut points in the tree hierarchy 
# ward.cuts <- data.frame(cluster=1:250, cutree(wards.ctr,k=c(9,21,44)))


############################################
# Append back Tracts with incomplete cases
############################################

# Get Cluster Centres

#centre_output <- result$centers %>%
#  as_tibble()

centre_output <- h2o.centers(results) %>%
as_tibble()
colnames(centre_output) <- colnames(usa.bg.ic_Infill[,2:191])


# Refine by frequency of missing variables
usa.bg.ic_Infill %<>%
  as_tibble() %>%
  filter(GEOID %in% missing_count$GEOID)






dist.k <- data.frame()

for (row in 1:nrow(usa.bg.ic_Infill)){
  
  for (k in 1:10){
    
    dist.k[row, k] <- dist(rbind(centre_output[k,], usa.bg.ic_Infill[row, 2:191])) #1 is GEOID

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
  mutate(GEOID = usa.bg.ic_Infill$GEOID, cluster = cl) %>%
  select(-cl) %>%
  as_tibble()



# Append cluster aggregates and create final lookup
# usa.bg.cl %<>%
#   #bind_rows(mins) %>%
# left_join(ward.cuts)

# Append missing clusters
usa.bg.cl %<>%
bind_rows(mins)

#Export clusters

write_csv(usa.bg.cl,"Clusters_K_10_BG_Logit.csv")



test <- data %>% 
  filter ( GEOID %in% (usa.bg.cl %>% filter(cluster == 9) %>% select(GEOID) %>% pull()))



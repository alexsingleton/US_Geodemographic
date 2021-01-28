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



usa.trt <- readRDS("./data/All_data_1.2.rds")
data <- readRDS("./data/data.rds")
vars_new <- readRDS("./data/vars_new.rds")



################################################################################################
# Cluster Analysis
#
# This code creates initial 250 clusters
#################################################################################################


#EXTRACT COMPLETE CASES (all GEOID checked and present in boundary data)
usa.trt.cc <- usa.trt[complete.cases(usa.trt), ]

#INCOMPELTE CASES (and count missing / problematic GEOID)
usa.trt.ic <- usa.trt[!complete.cases(usa.trt), ]

m_cnt <- usa.trt.ic %>%
  is.na %>% 
  rowSums

missing_count <- tibble(GEOID= usa.trt.ic$GEOID,missing=m_cnt)

TRACTS_SF <- st_read("data/cb_2019_us_tract_500k/cb_2019_us_tract_500k.shp")

# identify which GEOID have associated polygons / boundaries
missing_count %<>%
  mutate(in_boundary = if_else(GEOID %in% TRACTS_SF$GEOID,1,0))


#st_write(obj = TRACTS_SF, dsn = "missing.gpkg")


#select only those GEOID that have boundaries and less than 5 missing data
missing_count %<>%
  filter(in_boundary == 1) %>%
  filter(missing <= 5)






##STANDARDIZE DATA TO A 0-1 RANGE
range01 <- function(x){(x-min(x,na.rm = TRUE))/(max(x,na.rm = TRUE)-min(x,na.rm = TRUE))}

# input to cluster analysis is the complete cases standardized in 0-1 (range)
aa <- usa.trt.cc %>%
  select(-"GEOID") %>%
  mutate_all(range01)

# creates range standardized versions of records with at least one missing value for later cluster imputation
bb <- usa.trt %>%
  select(-"GEOID") %>%
  mutate_all(range01) %>%
  as_tibble()

bb %<>%
  mutate(GEOID = usa.trt$GEOID) %>%
  filter(GEOID %in% missing_count$GEOID)



######################
# Runs k-means 10,000 (17hrs to complete on XEON W-2225 @ 4.10 GHZ )
######################

c<- detectCores() - 1  
cl <- makeCluster(c)
registerDoParallel(cl)


#ptm <- proc.time()

results <- foreach( i = rep(1,10000) ) %dopar% {
  kmeans( x=aa, centers=250, nstart=i )
}
#proc.time() - ptm

# Stop Cluster
stopCluster(cl)


# Save all results from 10k run
saveRDS(results,"./data/results_10k_250.rds")


# Pick the best result
i <- sapply(results, function(result) result$tot.withinss)

#Graph Results
# TWSS_250_10k <- i %>%
#   as_tibble() %>%
#   rename(tot.withinss = value)
# 
# p <- ggplot(TWSS_250_10k,aes(y=tot.withinss))
# p + geom_boxplot()


# Select the result with the lowest TWSS
result <- results[[which.min(i)]]

# Save optimal 250 cluster result
saveRDS(result,"./data/final_k250.rds")



#Append clusters to GEOID

usa.trt.cl <- tibble(GEOID= usa.trt.cc$GEOID,cluster=result$cluster)










######################
# Explore break points
######################

# Create a distance matrix for cluster centroids.
diss.ctr <- dist(result$centers)

# Calculate Wards
wards.ctr <-hclust(diss.ctr, method="ward.D")

# Silhouette to identify break points

sil <- NA  #hold fit statistics in a data.frame
for (i in 2:249){
  sil[i] <-  summary(silhouette(cutree(wards.ctr,k=i), diss.ctr))$avg.width
}
sil <- data.frame(sil=sil, k=1:249)[-1,]

#Plot Silhouette
ggplot(data=sil, aes(x=log(k), y=sil, label=k)) +geom_line() + geom_vline(xintercept=log(c(10,45,92)), lty=3, lwd=.5)

#Create cut points in the tree hierarchy 
ward.cuts <- data.frame(cluster=1:250, cutree(wards.ctr,k=c(10,45,92)))


############################################
# Append back Tracts with incomplete cases
############################################

# Get 250 Cluster Centres

centre_250 <- result$centers %>%
  as_tibble()



dist.k <- data.frame()

for (row in 1:nrow(bb)){
  for (k in 1:250){
    dist.k[row, k] <- dist(rbind(centre_250[k,], bb[row, 1:205])) #206 is GEOID
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
  mutate(GEOID = bb$GEOID, cluster = cl) %>%
  select(-cl) %>%
  as_tibble()



# Append cluster aggregates and create final lookup
usa.trt.cl %<>%
  bind_rows(mins) %>%
left_join(ward.cuts)


#Export clusters

write_csv(usa.trt.cl,"Clusters.csv")



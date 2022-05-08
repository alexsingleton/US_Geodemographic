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

# Write Parquet File (input data)
write_parquet(usa.bg.cc, "usa.bg.cc.parquet")
write_parquet(usa.bg.ic, "usa.bg.ic.parquet")


#### Clustering


h2o.init(max_mem_size="50G")

aa_h20 <- as.h2o(usa.bg.cc)

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


# Save clustering results from 10k run ( hours)
saveRDS(results,"./data/results_10k_7C_Logit_BG.rds")


# Output Complete Case Lookup
ID <- usa.bg %>%
  select(all_of(c(v_used,"GEOID"))) %>%
  filter(complete.cases(.)) %>%
  select("GEOID")

clusters <- as_tibble(h2o.predict(results,aa_h20))

usa.bg.cl <- tibble(GEOID= ID$GEOID,cluster=(clusters$predict +1))

saveRDS(usa.bg.cl,"./data/usa.bg.cl.rds")














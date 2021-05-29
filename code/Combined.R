library(tidyverse)
devtools::install_github("GL-Li/totalcensus")

library(totalcensus) #install.packages("totalcensus")
library(tidycensus)
library(purrr)
library( readxl )
library(httr)
library(corrr)
library(tidygraph)
library(ggraph) #install.packages("ggraph")
library(car) #install.packages("car")
library(h2o) #install.packages("h2o")
library(cluster)
library("janitor") # install.packages("janitor")
library(sf)
library(magrittr)
library(caret)

# Geographic Data Supression (https://www.census.gov/programs-surveys/acs/technical-documentation/data-suppression.html)
#GET("http://www2.census.gov/programs-surveys/acs/tech_docs/data_suppression/geographic_restrictions.xlsx", write_disk(tf <- tempfile(fileext = ".xlsx")))
#acs_sup_tables <- read_excel(tf,sheet = "List_of_Tables")

# Test to see if block group data available

ACS_19_5Yr_Variables <- load_variables(2019, "acs5", cache = TRUE) # Get a list of all variables

vars_check <- ACS_19_5Yr_Variables %>%
  select(name) %>%
  filter(str_detect(name, '_001')) %>% # select first variable of each table
  filter(!str_detect(name, 'PR')) %>% # exclude puerto rico tables
  filter(!str_detect(name, '[:upper:]_')) %>% # exclude table derivatives 
  filter(!str_detect(name, 'B98003_001')) %>% # exclude national table
  pull()

# Set the Census API key and retrieve country codes
census_api_key("28623dc12367621593ec9f56deeb0c495644e8f0",overwrite = TRUE ,install = TRUE)
readRenviron("~/.Renviron")

#Get a list of all variables for an example state
table_check <- get_acs(geography = "block group", variables = vars_check, state = "CA",year = 2019,geometry = FALSE)
# Format table wide
table_check_W <- table_check %>%
  select(-NAME,-moe) %>%
  pivot_wider(id_cols=GEOID,names_from=variable,values_from = estimate)

# Count NA values for each variable and filter where 20% are valid
missing_V <- table_check_W %>%
  summarise_all(funs(sum(is.na(.))/23212*100)) %>%
  pivot_longer( cols = everything()) %>%
  filter(value < 20) %>%
  select(-value) %>%
  pull()

# Extract a list of tables and associated variables where block group are not NA
select_tables <- gsub("_001","",missing_V[-1]) # select tables


# Create full ACS Table List and Initial Variables
#### (https://www.census.gov/programs-surveys/acs/technical-documentation/table-shells.html)
GET("https://www2.census.gov/programs-surveys/acs/tech_docs/table_shells/table_lists/2019_DataProductList.xlsx", write_disk(tf <- tempfile(fileext = ".xlsx")))
acs_tables <- read_excel(tf)

acs_tables %<>%
  mutate(PCT = str_detect(`Table Title`,pattern = "DOLLAR|MEDIAN")) %>%
  filter(`Table ID` %in% select_tables)

# Get variables within selected tables

select_variables <- ACS_19_5Yr_Variables %>%
        filter(name != "PUMA5") %>%
        separate(name, c("table", NA) ,remove = FALSE) %>%
        filter(table %in% select_tables) %>%
        mutate(denominator = paste0(table,"_001")) %>%
        mutate(Non_PCT = str_detect(concept,pattern = "DOLLAR|MEDIAN")) %>% # identify non count variables
        mutate(label = str_replace_all(label, ":", "")) %>% # remove colons
        mutate(label = str_replace_all(label, "[[:blank:]]|-", "_")) %>% # remove spaces and replace with underscore 
        separate(label, sep = "!!",into = c("A","B","C","D","E","F","G","H"),remove = FALSE) %>%
        filter(name != denominator) %>% # split column 
        unite("V_name",C,D,E,F,G,H, sep = "_") %>% #combine to create a new variable ID
        mutate(V_name = str_replace_all(V_name, "_NA", "")) %>%
        mutate(V_name = str_replace_all(V_name, "[^a-zA-Z0-9_]", "")) %>%
        mutate(V_name = paste0(name,"_",V_name)) %>%
        select(-c(A,B)) %>%
        mutate(Non_PCT = if_else(table %in% c("B19001","B19037","B19101","B19201","B20001","B28004"),FALSE,Non_PCT)) #Couple of table corrections
  

# Create list of all variables
all_vars <- sort(unique(c(select_variables$name,select_variables$denominator)))

# Download All ACS  Data
set_path_to_census("./my_census_data")
options(timeout=5000)
#download_census("acs5", 2019, states = "AL")



#Create Block Group Table
BG_ACS <- read_acs5year(
  year = 2019,
  states = states_DC,   # all 50 states plus DC
  table_contents = all_vars,
  summary_level = "block group"
)


saveRDS(BG_ACS,"./data/BG_ACS.rds")

############################################################################
# The next stage creates % scores for all variables and examines distribution
############################################################################

# Create numerator / denominator lists

numer <- select_variables %>% filter(Non_PCT == FALSE) %>% select(name) %>% pull()
denom <- select_variables %>% filter(Non_PCT == FALSE) %>% select(denominator) %>% pull()

# Calculate rates

DF <- as_tibble(BG_ACS)

getPrt<-function (.x, .y) {
  numer<-DF[,.x]
  denom<-DF[,.y]
  prt<-(numer/denom) * 100
  return(prt)
}

# Calculate rates
prt<-purrr::map2(.x=numer,.y=denom,.f=getPrt) %>% as.data.frame()
colnames(prt) <- select_variables[select_variables$name %in% colnames(prt),"V_name"] %>% pull()


# Create Summary of variable characteristics

sum_part <- dfSummary(prt)

sum_part <- prt %>%
  dplyr::select_if(is.numeric)  %>%
  lapply(., function(x) tidy(summary(x)))  # compute tidy summary of each var

sd_part <- prt %>%
  dplyr::select_if(is.numeric)  %>%
  lapply(., function(x) tidy(sd(x,na.rm =TRUE)))  # compute tidy summary of each var

sum_part <- bind_rows(sum_part,.id = 'id')
sd_part <- bind_rows(sd_part,.id = 'id')

select_variables %<>%
  left_join(sum_part, by=c("V_name"="id")) %>%
  left_join(sd_part, by=c("V_name"="id"))

write_csv(select_variables,"./data/BG_Variables.csv") # This is used to manually create merge variables


######################################
# Calculate Merges and Final % File
######################################

BG_ACS <- readRDS("./data/BG_ACS.rds")

vars_new <- read_excel("./data/Variables_to_Include_1.5.xlsx") # Read variable list (manually edited - BG_Variables.csv)


# Variables identified for aggregation - this requires summing some variables and replacing them within the variable numerator / denominator lookup

ag_vars <- vars_new %>%
  filter(Merge_Variable == 1) # Select variables for aggregation

ag_vars <- split(ag_vars,ag_vars$Alt_Name) # Split into a list

ag_vars_out <- BG_ACS %>% #Create a table of GEOID
  select(GEOID)

# Calculates the merged variables and appends to GEOID table
for (i in 1:length(ag_vars)) {
  tmp <-  BG_ACS %>%
    select(paste0("",ag_vars[[i]]$name)) %>%
    mutate(sum = rowSums(.)) %>%
    select(sum) %>%
    rename(!!paste0("",unique(ag_vars[[i]]$Alt_Name)) := sum)
  
  ag_vars_out <- cbind(ag_vars_out,tmp)
  
}

# Clean up 

#Remove merged variables from the numerator / denominator list
vars_new %<>%
  filter(is.na(Merge_Variable))

# Append aggregated variables with appropriate denominator
ag_vars %<>%
  reduce(bind_rows) %>%
  select(c(Alt_Name,denominator))%>%
  distinct() %>%
  rename(name = Alt_Name) %>%
  mutate(Non_PCT = FALSE) %>%
  mutate(V_name = name)

vars_new %<>%
  bind_rows(ag_vars)


# Append the aggregated variables onto ACS raw data table
DF <- as_tibble(BG_ACS)
DF %<>%
  left_join(ag_vars_out,by = "GEOID")


# Calculate Final % Scores
numer <- vars_new %>% filter(Non_PCT == FALSE) %>% select(name) %>% pull()
denom <- vars_new %>% filter(Non_PCT == FALSE) %>% select(denominator) %>% pull()



getPrt<-function (.x, .y) {
  numer<-DF[,.x]
  denom<-DF[,.y]
  prt<-(numer/denom) * 100
  
  
  return(prt)
}





#DF %>% map_dbl(~sum(is.na(.)))

# Calculate rates
prt<-purrr::map2(.x=numer,.y=denom,.f=getPrt) %>% as.data.frame()




# Clean up - replace all NaN with 0 (as the base is zero)
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))# needed as is.nan has no data frame method
prt[is.nan(prt)]<-0


colnames(prt) <- vars_new[vars_new$name %in% colnames(prt),"V_name"] %>% pull()

prt %<>%
bind_cols(select(BG_ACS,"GEOID"))



# Save the measures and description file
saveRDS(prt,"./data/BG_1.5_Pct.rds")

saveRDS(vars_new,"./data/Variables_to_Include_1.5.rds")





################################################################################################
# Correlation / Input Refinement
#
# This code produces correlation descriptions and attribute summaries and was used to refine
# the input measure list which are versioned between 1.0 and 1.5
#################################################################################################

v_used <- vars_new %>% filter(Keep_Variables == 1) %>% select(V_name) %>% pull() # select proposed used variables

############################
# Check Correlations
############################

# Calculate correlations
data_corr <- prt %>%
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
vars_new %<>%
  mutate(V_name = if_else(is.na(V_name),name,V_name))

graph_corr %<>%
  left_join(vars_new, by=c("name" = "V_name"))

# Fix level Order
graph_corr %<>% 
  activate(edges) %>% 
  mutate(band = factor(band, levels = c("(-Inf,-0.7]","(-0.7,-0.6]","(0.6,0.7]","(0.7, Inf]")))

# Plot graph
graph_corr %>%
  ggraph(layout = "kk") +
  geom_edge_diagonal(aes(colour = band)) +
  scale_edge_colour_viridis(option = "plasma",alpha = 0.5, discrete = TRUE) +
  geom_node_point(aes(colour = "black"), size = 0.7) +
  geom_node_text(aes(label = name), size = 1, check_overlap =TRUE)




################################################################################################
# Clustering
#

#################################################################################################

prt <- readRDS("./data/BG_1.4_Pct.rds")
vars_new <- readRDS("./data/Variables_to_Include_1.4.rds")
v_used <- vars_new %>% filter(Keep_Variables == 1) %>% select(V_name) %>% pull() # select proposed used variables


#Logit Transform
#cluster_variables <- prt %>%
#  select(v_used) %>%
#  mutate_if(is.numeric, logit)


#Box Cox Transform
cluster_variables <- prt %>%
  select(all_of(v_used)) %>%
  mutate_all(function(x) x+1) %>%
  mutate_all(funs( BoxCoxTrans(.,na.rm = TRUE) %>% predict(.)))



  



##STANDARDIZE DATA TO A 0-1 RANGE
range01 <- function(x){(x-min(x,na.rm = TRUE))/(max(x,na.rm = TRUE)-min(x,na.rm = TRUE))}


cluster_variables %<>%
  mutate_all(range01)

#view(dfSummary(cluster_variables), file = "cluster_variables_Summary_Inputs.html")



h2o.init(max_mem_size="50G")
h2o.init()
cluster_variables_h20 <- as.h2o(cluster_variables)

# H2O test

ptm <- proc.time()
results <- h2o.kmeans(training_frame = cluster_variables_h20, k = 250, x = v_used, init = "Random",max_iterations=1,standardize = FALSE)
wss <- h2o.tot_withinss(results)
proc.time() - ptm


ptm <- proc.time()

for(i in 1:1000) {
  
  results_run <- h2o.kmeans(training_frame = cluster_variables_h20, k = 250, x = v_used, init = "Random",max_iterations=1000,standardize = FALSE)
  wss_run <- h2o.tot_withinss(results_run)
  
  if(wss_run < wss) {
    
    wss <- wss_run
    results <- results_run
    
  }
  print(i)
}

proc.time() - ptm

saveRDS(results,"./data/final_k250_Block_Group.rds")



clusters <- as_tibble(h2o.predict(results,cluster_variables_h20))

usa.bg.cl <- tibble(GEOID= prt$GEOID,cluster=clusters$predict)





######################
# Explore break points
######################

# Create a distance matrix for cluster centroids.
#diss.ctr <- dist(test$centers)

diss.ctr <- dist(data.frame(h2o.centers(results)))

# Calculate Wards
wards.ctr <-hclust(diss.ctr, method="ward.D")

# Silhouette to identify break points

sil <- NA  #hold fit statistics in a data.frame
for (i in 2:249){
  sil[i] <-  summary(silhouette(cutree(wards.ctr,k=i), diss.ctr))$avg.width
}
sil <- data.frame(sil=sil, k=1:249)[-1,]

#Plot Silhouette
ggplot(data=sil, aes(x=log(k), y=sil, label=k)) +geom_line() + geom_vline(xintercept=log(c(7,14,44)), lty=3, lwd=.5)

#Create cut points in the tree hierarchy and lookup 
ward.cuts <- data.frame(cluster=1:250, cutree(wards.ctr,k=c(7,14,44)))

usa.bg.cl %<>%
  left_join(ward.cuts)

#Export clusters

write_csv(usa.bg.cl,"Clusters_BG.csv")

############################################
# 
############################################


#Get Source Data
saveRDS(BG_ACS,"./data/BG_ACS.rds")
BG_ACS <- readRDS("./data/./data/BG_ACS.rds")

usa.bg.cl <- read_csv("Clusters_BX.csv")
vars_new <- readRDS("./data/Variables_to_Include_1.4.rds")


# Select all variables within tables used for the cluster analysis
t_used <- vars_new %>% filter(Keep_Variables == 1) %>% select(table) %>% pull() %>% unique()# select proposed used variables
index_used <- vars_new %>% filter(table %in% t_used) %>% select(name) %>% pull() # select proposed used variables


index_variables <- BG_ACS %>%
select("GEOID","B01001_001",all_of(index_used))%>%
  left_join(usa.bg.cl)
 

X11 <- index_variables %>%
  select(-c(GEOID,cluster,X20,X30)) %>%
  group_by(X11) %>% 
  summarise_all(sum,na.rm = TRUE)



# Convert to percentages & index

all_pct <- X11 %>%
  adorn_percentages(denominator = "col")


  
  all_index <- all_pct %>% 
    mutate(
      across(all_of(index_used),
             .fns = ~./B01001_001 *100)) %>%
    select(-B01001_001)
  
  names(all_index) <- c("X11",vars_new$V_name[match(names(all_index), vars_new$name)][-1])

  
  all_index %<>%
    rownames_to_column() %>%
    pivot_longer(-rowname, 'variable', 'value') %>%
    pivot_wider(variable, rowname)
  
  
  
  write_csv(all_index,"grand_index_BLOCK_GROUP.csv")
  
  
  
  
  Block_Group_SF <- readRDS("./data/Block_Group_SF.rds")
  
  
  usa.bg.cl %<>%
    mutate(GEOID = str_replace(GEOID,"15000US", ""))
  
  
  
  Block_Group_SF %<>%
    left_join(usa.bg.cl)
  
  
  st_write(obj = Block_Group_SF, dsn = "Block_Group_SF.gpkg")
  

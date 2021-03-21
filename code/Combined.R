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

vars_new <- read_excel("Variables_to_Include.xlsx") # Read variable list (manually edited - BG_Variables.csv)


# Variables identified for aggregation - this requires summing some variables and replacing them within the variable numerator / denominator lookup

ag_vars <- vars_new %>%
  filter(Merge_Variable == 1) # Select variables for aggregation

ag_vars <- split(ag_vars,ag_vars$Alt_Name) # Split into a list

ag_vars_out <- DF %>% #Create a table of GEOID
  select(GEOID)

# Calculates the merged variables and appends to GEOID table
for (i in 1:length(ag_vars)) {
  tmp <-  DF %>%
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
  mutate(Non_PCT = FALSE)

vars_new %<>%
  bind_rows(ag_vars)


# Append the aggregated variables onto ACS raw data table
DF %<>%
  left_join(ag_vars_out,by = "GEOID")


# Calculate Final % Scores
numer <- vars_new %>% filter(Non_PCT == FALSE) %>% select(name) %>% pull()
denom <- vars_new %>% filter(Non_PCT == FALSE) %>% select(denominator) %>% pull()



# Calculate rates
prt<-purrr::map2(.x=numer,.y=denom,.f=getPrt) %>% as.data.frame()
colnames(prt) <- select_variables[select_variables$name %in% colnames(prt),"V_name"] %>% pull()








################################################################################################
# Correlation / Input Refinement
#
# This code produces correlation descriptions and attribute summaries and was used to refine
# the input measure list which are versioned between 1.0 and 1.5
#################################################################################################

v_used <- vars_new %>% filter(Keep_Variables == 1) %>% select(V_name) %>% pull() # select proposed used variables

############################
# Check Correlations 1
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







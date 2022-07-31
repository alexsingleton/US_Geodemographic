
#Summary Statistics
variables_to_get <- c(
med_home_value = "B25077_001",
med_rooms = "B25018_001",
total_population = "B01003_001",
med_house_income = "B19013_001",
med_year_built = "B25037_001",
rent_burden = "B25071_001",
white = "B03002_003",
black = "B03002_004",
asian = "B03002_006",
hispanic = "B03002_012",
Age_5_to_9_years_M = "B01001_004",
Age_10_to_14_years_M = "B01001_005",
Age_15_to_17_years_M = "B01001_006",
Age_18_and_19_years_M = "B01001_007",
Age_20_years_M = "B01001_008",
Age_21_years_M = "B01001_009",
Age_22_to_24_years_M = "B01001_010",
Age_25_to_29_years_M = "B01001_011",
Age_30_to_34_years_M = "B01001_012",
Age_35_to_39_years_M = "B01001_013",
Age_40_to_44_years_M = "B01001_014",
Age_45_to_49_years_M = "B01001_015",
Age_50_to_54_years_M = "B01001_016",
Age_55_to_59_years_M = "B01001_017",
Age_60_and_61_years_M = "B01001_018",
Age_62_to_64_years_M = "B01001_019",
Age_65_and_66_years_M = "B01001_020",
Age_67_to_69_years_M = "B01001_021",
Age_70_to_74_years_M = "B01001_022",
Age_75_to_79_years_M = "B01001_023",
Age_80_to_84_years_M = "B01001_024",
Age_85_years_and_over_M = "B01001_025",
Age_0_5_years_F = "B01001_027",
Age_5_to_9_years_F = "B01001_028",
Age_10_to_14_years_F = "B01001_029",
Age_15_to_17_years_F = "B01001_030",
Age_18_and_19_years_F = "B01001_031",
Age_20_years_F = "B01001_032",
Age_21_years_F = "B01001_033",
Age_22_to_24_years_F = "B01001_034",
Age_25_to_29_years_F = "B01001_035",
Age_30_to_34_years_F = "B01001_036",
Age_35_to_39_years_F = "B01001_037",
Age_40_to_44_years_F = "B01001_038",
Age_45_to_49_years_F = "B01001_039",
Age_50_to_54_years_F = "B01001_040",
Age_55_to_59_years_F = "B01001_041",
Age_60_and_61_years_F = "B01001_042",
Age_62_to_64_years_F = "B01001_043",
Age_65_and_66_years_F = "B01001_044",
Age_67_to_69_years_F = "B01001_045",
Age_70_to_74_years_F = "B01001_046",
Age_75_to_79_years_F = "B01001_047",
Age_80_to_84_years_F = "B01001_048",
Age_85_years_and_over_F = "B01001_049")


#B03002_005: Native American alone (Not Hispanic or Latino)
#B03002_006: Asian alone (Not Hispanic or Latino)
#B03002_007: Native Hawaiian or Pacific Islander alone (Not Hispanic or Latino)
#B03002_009: Multiple Races (Not Hispanic or Latino)



################
# Download Data 
################
# Set the Census API key and retrieve country codes


census_api_key("20eb1998096c4eb405a63ebc23033e2cbc0df8b5") #Set API Key

us <- unique(fips_codes$state)[1:51] # Get a FIPS list

# Setup parallel processing
c<- detectCores() - 1  
cl <- makeCluster(c)
registerDoParallel(cl)



ptm <- proc.time() 
foreach(i = 1:length(us),.packages=c('purrr','dplyr','tidycensus')) %dopar%{
  
 
  
  #Pull down ACS data
d  <- get_acs(
    geography = "block group",
    variables = variables_to_get,
    state = us[i],
    geometry = TRUE,
    year = 2020)
  
  
  saveRDS(d,paste0("data/storage_tmp/extra/",us[i],".rds"))
  rm(d)
  
}
proc.time() - ptm


# Combine Data
ptm <- proc.time()
DF <- list.files(path = "./data/storage_tmp/extra/", pattern = ".rds", full.names = TRUE) %>%
  map_dfr(readRDS)
proc.time() - ptm




DF %<>%
  select(-NAME,-moe)


saveRDS(DF,"./data/extra/extra_data.rds")





#Import Cluster

usa.bg.cl <- read_parquet("./data/usa.bg.cl.type.parquet")






















#Density
library(units)
density <- DF %>%
  filter(variable == "total_population") %>%
    mutate(pop_density = as.numeric(set_units(estimate / st_area(.), "1/km2")))  %>% 
    select(GEOID,pop_density) 

#Median age of house
house_age <- DF %>%
  filter(variable == "med_year_built") %>%
  mutate(median_structure_age = 2022 - estimate)  %>% 
  select(GEOID,median_structure_age) %>%
  mutate(median_structure_age = ifelse(median_structure_age >300, NA, median_structure_age))

#Segregation
library(segregation)
segregation <- DF %>%
  select(GEOID,variable,estimate) %>%
  filter(variable %in% c('white','black','asian','hispanic')) %>%
  mutual_local( # Computes Mutual Information Index (M) decomposed by block groups (ls)
    group = "variable",
    unit = "GEOID",
    weight = "estimate", 
    wide = TRUE
  )

#Age
population <- DF %>%
  filter(grepl("_M|_F", variable)) %>%
  mutate(SEX = case_when(grepl("_M", variable) ~ 'M',
                         (grepl("_F", variable) ~ 'F')))

#Median Home Value
med_home_value <- DF %>%
  filter(variable == "med_home_value")

#Median Rooms
med_rooms <- DF %>%
  filter(variable == "med_rooms")

#Median Household Income
med_house_income <- DF %>%
  filter(variable == "med_house_income")

#Rent Burden
rent_burden <- DF %>%
  filter(variable == "rent_burden")







####Viz######
############

#Pop Pyramid

pop_plot <- population %>%
  st_drop_geometry() %>%
  mutate(estimate = ifelse(SEX == "M", -estimate, estimate)) %>% #turn males scores to negative
  mutate(variable = str_remove(variable,"_M")) %>%
  mutate(variable = str_remove(variable,"_F")) %>%
  mutate(variable = str_remove(variable,"Age ")) %>%
  mutate(variable = str_replace_all(variable,"_"," ")) %>%
  as_tibble()
  
  

ggplot(pop_plot, aes(x = estimate, y = variable, fill = SEX)) + 
  geom_col()

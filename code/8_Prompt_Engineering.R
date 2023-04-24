library(tidyverse)

#This code prepares a series of prompts for ChatGPT for each Type

Prompt_Scores <- read_csv("data/gpt/Prompt_Text_V3.csv")

Prompt_Scores %<>%
  select(prompt,"x1_1":"x7_6")





#start <- "A geodemographics company is trying to explain the characteristics of a neighborhood to a new customer. They present data comparing this neighborhood to the national average. A score of 100 means the neighborhood is equivalent to the national average, a score of 150 means the neighborhhood one and a half times the national average, a score of 200 means the neighborhood is twice the national average, a score of 50 means the neighborhood is half of the national average, a score of 300 means the neighborhood is three times the national average. Their neighborhood has the following charactersitics, described in #DATA# below. Data are presented for each charactersitic followed by a colon, and then a score. A good description of a neighborhood describes characteristics that have scores which are greater than 120 or less than 80."
start <- "A geodemographics company is trying to explain the characteristics of a neighborhood to a new customer. They present data comparing this neighborhood to the national average. A score of 100 means the neighborhood is equivalent to the national average, a score of 150 means the neighborhhood one and a half times the national average, a score of 200 means the neighborhood is twice the national average, a score of 50 means the neighborhood is half of the national average, a score of 300 means the neighborhood is three times the national average. Their neighborhood has the following charactersitics, described in #DATA# below. Data are presented for each charactersitic followed by a colon, and then a score. The description of the neighborhood should focus on characteristics that have scores which are greater than 120 or less than 80. If 'housing units that are Renter occupied' have scores that are less than 80, then don't describe any details about the rent prices."



end <- "In the third person, write a description of the neighborhood in no more than 500 words. Don't mention the specific scores from the #DATA#, instead use descriptive words to illustrate rates that are above or below the national average. \n\n Also create a short summary name that describes he area in no more than four words, focusing on those characteristics with the highest scores."




#Create a list of Types
Types <- colnames(Prompt_Scores)[-1]

# Data Printing Function
print_two_columns <- function(data, col1,col2) {
  
  selected_data <- data %>%
    select(!!as.symbol(col1), !!as.symbol(col2)) %>%
    mutate(combined_columns = paste0(!!as.symbol(col1), ":", !!as.symbol(col2))) %>%
    select(combined_columns) %>% pull()
  
  selected_data %>%   paste(collapse = '\n') %>% cat()
  
}


# Write Prompts

sink("./data/gpt/prompt_GPT.txt")

for (i in 1:length(Types)){
  cat(paste(Types[i],"\n\n"))
  cat(paste(start,"\n\n"))

  cat(paste("#DATA#","\n"))
Prompt_Scores %>%
  print_two_columns("prompt",Types[i])

cat("\n\n")

cat(paste(end,"\n\n"))

cat(paste("######################","\n\n"))
  
}
  
sink()
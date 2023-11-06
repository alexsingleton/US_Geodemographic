# US Geodemographic Classification

# Introduction / Aim

This repo presents the code used to build a new geodmeographic classification for the US, following previous work within this geographic [context](https://github.com/geoss/acs_demographic_clusters/) and [published paper](https://doi.org/10.1080/00045608.2015.1052335).

# Structure of the Repository

The repo is organised into a series of folders and files:

| Folder          | Contents                                                                         |
|-----------------|----------------------------------------------------------------------------------|
| code            | All of the code used to create the classification; mostly in R with some Python. |
| > 10_Evaluation | Sub directory containing the internal and external validation code               |
| data            | All of the input data / lookups and data outputs from the created model.         |
| > evaluation    | External evaluation data.                                                        |
| > gpt           | GPT prompts and inputs.                                                          |
| > outputs       | Outputs from the classification, including lookups and descriptions.             |
| graphs          | Created graphs used in writing the accompanying paper.                           |
| maps            | Created maps used in writing the accompanying paper.                             |
| photos          | Some georeferenced photos used as part of the ground truth of the classification.|
|                 |                                                                                  |


## Code

The code is organised into a series of numbered files, which should be run in order. The code is also commented to explain what each section does. The numbered files are:

* 1_Initial_Variable_Selection.R - This file selects the variables to be used in the classification. It also creates a series of lookups for the variables, including the variable names, descriptions and the source of the data.
* 2_Download_ACS_Data_and_Prep.R - This file downloads the ACS data from the Census API and prepares it for use in the classification.
* 3_Correlation_Final_Variable_Selection.R - This file selects the final variables to be used in the classification, based on correlation and other exploratory analysis.
* 4A_Clustergram_Groups.ipynb - This file creates the clustergram for the Group level of the classification. This uses the Python package [clustergram](https://github.com/martinfleis/clustergram).
* 4B_Cluster_Analysis_Groups.R - This file creates the cluster analysis for the Group level of the classification. This uses H20.ai's [H2O](https://www.h2o.ai/) R implementation.
* 5_Description_Groups.R - This file creates the descriptions for the Group level of the classification.
* 6A_Clustergram_Types.ipynb - This file creates the clustergram for the Type level of the classification.
* 6B_Cluster_Analysis_Types.R 0 This file creates the cluster analysis for the Type level of the classification.
* 7_Description_Types.R - This file creates the descriptions for the Type level of the classification.
* 8_Map_Classification.R - This file creates the maps for the classification.
* 9_Prompt_Engineering.R - This file creates the prompts that were fed in GPT4 to create descriptions for the classification.
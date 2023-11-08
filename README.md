# US Geodemographic Classification

The classification is stored in two parquet files, each has a column which stores the US Census Bureau's block group Identifier and a column that stores the [group](data/usa.bg.cl.group.parquet) and [type](data/usa.bg.cl.type.parquet) levels of the classification.  For mapping purposes this file file has to be joined to [a boundary file containing the 2020 US Census Block Group outlines](https://www.census.gov/cgi-bin/geo/shapefiles/index.php).

# Introduction

This repo presents the code used to build a new geodmeographic classification for the US, following previous work within this geographic [context](https://github.com/geoss/acs_demographic_clusters/) and [published paper](https://doi.org/10.1080/00045608.2015.1052335). 

# Structure of the Repository

The repo is organised into a series of folders and files:

| Folder          | Contents                                                                         |
|-----------------|----------------------------------------------------------------------------------|
| [code](/code/)          | All of the code used to create the classification; mostly in R with some Python. |
| > [10_Evaluation](/code/10_Evaluation) | Sub directory containing the internal and external validation code               |
| [data](/data/)            | All of the input data / lookups and data outputs from the created model.         |
| > [evaluation](/data/evaluation/)    | External evaluation data.                                                        |
| > [gpt](/data/gpt/)           | GPT prompts and inputs.                                                          |
| > [outputs](/data/outputs)       | Outputs from the classification, including lookups and descriptions.             |
| [graphs](graphs)          | Created graphs used in writing the accompanying paper.                           |
| [maps](maps)            | Created maps used in writing the accompanying paper.                             |
| [photos](photos)          | Some georeferenced photos used as part of the ground truth of the classification.|
|                 |                                                                                  |


## Code

The code is organised into a series of numbered files, which should be run in order. The code is also commented to explain what each section does. The numbered files are:

* [1_Initial_Variable_Selection.R](/code/1_Initial_Variable_Selection.R) - This file selects the variables to be used in the classification. It also creates a series of lookups for the variables, including the variable names, descriptions and the source of the data.
* [2_Download_ACS_Data_and_Prep.R](/code/2_Download_ACS_Data_and_Prep.R) - This file downloads the ACS data from the Census API and prepares it for use in the classification.
* [3_Correlation_Final_Variable_Selection.R](/code/3_Correlation_Final_Variable_Selection.R) - This file selects the final variables to be used in the classification, based on correlation and other exploratory analysis.
* [4A_Clustergram_Groups.ipynb](/code/4A_Clustergram_Groups.ipynb) - This file creates the clustergram for the Group level of the classification. This uses the Python package [clustergram](https://github.com/martinfleis/clustergram).
* [4B_Cluster_Analysis_Groups.R](/code/4B_Cluster_Analysis_Groups.R) - This file creates the cluster analysis for the Group level of the classification. This uses H20.ai's [H2O](https://www.h2o.ai/) R implementation.
* [5_Description_Groups.R](code/5_Description_Groups.R) - This file creates the descriptions for the Group level of the classification.
* [6A_Clustergram_Types.ipynb](/code/6A_Clustergram_Types.ipynb) - This file creates the clustergram for the Type level of the classification.
* [6B_Cluster_Analysis_Types.R](/code/6B_Cluster_Analysis_Types.R) This file creates the cluster analysis for the Type level of the classification.
* [7_Description_Types.R](/code/7_Description_Types.R) - This file creates the descriptions for the Type level of the classification.
* [8_Map_Classification.R](/code/8_Map_Classification.R) - This file creates the maps for the classification.
* [9_Prompt_Engineering.R](/code/9_Prompt_Engineering.R) - This file creates the prompts that were fed in GPT4 to create descriptions for the classification.

## Classification Outputs

If you want to simply view the classification; you can download a lookup [here](/data/outputs/lookup.csv); or geopackage [here](https://pcwww.liv.ac.uk/~ucfnale/us_geodemographic_lfs/BG_SF.gpkg). If you are interested in the labels and descriptions, these can be found [here](/data/outputs/Names_Descriptions_Groups_Types.pdf).

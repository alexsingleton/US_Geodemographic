#Setup

#Terminal - Setup the Python Version
pipenv --python 3.7

#R - Setup the virtual environment

#venv <- system("pipenv --venv", inter = TRUE)
#reticulate::use_virtualenv(venv, required = TRUE)

#Install Tensorflow & Kerras

#install.packages("tensorflow")
#install.packages("keras")
#install_keras(tensorflow = "gpu")


wget 'https://repo.anaconda.com/archive/Anaconda3-2021.05-Linux-x86_64.sh'
bash Anaconda3-2021.05-Linux-x86_64.sh



library(devtools)
devtools::install_github("h2oai/h2o4gpu", subdir = "src/interface_r")

#Setup Virtual Environment
reticulate::use_virtualenv("/home/rstudio/anaconda3/envs/h2o4gpuenv")


library(h2o4gpu)

#................Environment Installation................
# This R script is for downloading and loading the relevant packages for the analysis.   

#Run this in console:
install.packages("here")
install.packages("tidyverse")
install.packages("janitor")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("data.table")
install.packages("rredlist")
devtools::install_github("IUCN-UK/iucnredlist")

#Load packages 
library(here)
library(tidyverse)
library(janitor)
library(ggplot2)
library(dplyr)
library(data.table)
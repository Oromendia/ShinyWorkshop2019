#
# This is R script is run first, before server.r or ui.r files. 
#

library(shiny)
library(tidyverse)
library(scales)
library(lubridate)
library(lazyeval)
library(gridExtra)
library(shinythemes)
#library(DT)

# Load data
load(file="dat_merged_2019_0109.Rdata") # dat_merged
data_dictionary <- read_csv("dataDictionary_2018_12_07_raw.csv")

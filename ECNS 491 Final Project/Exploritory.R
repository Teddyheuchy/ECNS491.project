################################################################################
#Wyatt Sigafoos
#ECNS 491
#Exploritory Analysis
#Final Project: Observe the recent behavior of domestic and foriegn gas prices
#November 10, 2022
################################################################################
#Step 1: Download and import both data files
setwd("/Users/wyattsig/Desktop/ECNS 491 Final Project")
library(readr)
library(readxl)
library(tidyverse)
us = read_csv("RetailUS.csv")
world1 = read_csv("WorldPrices/API_EP.PMP.SGAS.CD_DS2_en_csv_v2_4538231.csv")
world2 = read_csv("WorldPrices/Metadata_Country_API_EP.PMP.SGAS.CD_DS2_en_csv_v2_4538231.csv")
world3 = read_csv("WorldPrices/Metadata_Indicator_API_EP.PMP.SGAS.CD_DS2_en_csv_v2_4538231.csv")
################################################################################
#Step 2: See what data looks like
#the first 5 rows are dataset info and row 5 and lower need date and price separated
head(us)
(us[5,])
head(world1)
head(world2)
head(world3)
usable_data_us = us[5:359,]
cleanUS = usable_data_us |> 
  str_split(pattern = ',')
clean_data = cleanUS[[1]]
usAreas = read_xls("fullHistoryGas.xls",sheet = "Data 1")


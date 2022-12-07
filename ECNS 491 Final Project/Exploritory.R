################################################################################
#Wyatt Sigafoos
#ECNS 491
#Exploritory Analysis
#Final Project: Observe the recent behavior of domestic and foriegn gas prices
#November 10, 2022
################################################################################
#Step 1: Download and import both data files
#setwd("/Users/wyattsig/Desktop/ECNS 491 Final Project")
library(readr)
library(readxl)
library(tidyverse)
library(sf)
library(tmap)
library(ggplot2)
library(usmap)
library(lubridate)
library(stringr)
# us = read_csv("RetailUS.csv")
# world1 = read_csv("WorldPrices/API_EP.PMP.SGAS.CD_DS2_en_csv_v2_4538231.csv")
# world2 = read_csv("WorldPrices/Metadata_Country_API_EP.PMP.SGAS.CD_DS2_en_csv_v2_4538231.csv")
# world3 = read_csv("WorldPrices/Metadata_Indicator_API_EP.PMP.SGAS.CD_DS2_en_csv_v2_4538231.csv")
################################################################################
#Step 2: See what data looks like
#the first 5 rows are dataset info and row 5 and lower need date and price separated
# head(us)
# (us[5,])
# head(world1)
# head(world2)
# head(world3)
# usable_data_us = us[5:359,]
# cleanUS = usable_data_us |> 
#   str_split(pattern = ',')
# clean_data = cleanUS[[1]]

regular_conventional = read_xls("fullHistoryGas.xls",sheet = "Data 1", col_types = c("date",rep("numeric",20)),skip = 2)
regular_reformulated = read_xls("fullHistoryGas.xls",sheet = "Data 2", col_types = c("date",rep("numeric",18)),skip = 2)
regular_all = read_xls("fullHistoryGas.xls",sheet = "Data 3", col_types = c("date",rep("numeric",28)),skip = 2)

midgrade_conventional = read_xls("fullHistoryGas.xls",sheet = "Data 4", col_types = c("date",rep("numeric",20)),skip = 2)
midgrade_reformulated = read_xls("fullHistoryGas.xls",sheet = "Data 5", col_types = c("date",rep("numeric",18)),skip = 2)
midgrade_all = read_xls("fullHistoryGas.xls",sheet = "Data 6", col_types = c("date",rep("numeric",28)),skip = 2)

premium_conventional = read_xls("fullHistoryGas.xls",sheet = "Data 7", col_types = c("date",rep("numeric",20)),skip = 2)
premium_reformulated = read_xls("fullHistoryGas.xls",sheet = "Data 8", col_types = c("date",rep("numeric",18)),skip = 2)
premium_all = read_xls("fullHistoryGas.xls",sheet = "Data 9", col_types = c("date",rep("numeric",28)),skip = 2)

allGrades_conventional = read_xls("fullHistoryGas.xls",sheet = "Data 10", col_types = c("date",rep("numeric",20)),skip = 2)
allGrades_reformulated = read_xls("fullHistoryGas.xls",sheet = "Data 11", col_types = c("date",rep("numeric",18)),skip = 2)
allGrades_all = read_xls("fullHistoryGas.xls",sheet = "Data 12", col_types = c("date",rep("numeric",28)),skip = 2)

# dataList = c(regular_all,regular_conventional,regular_reformulated,midgrade_all,
#          midgrade_conventional,midgrade_reformulated,premium_all,
#          premium_conventional,premium_reformulated,allGrades_all,
#          allGrades_conventional,allGrades_reformulated)
# i = 1
# for(i in range(1,length(dataList))){
#   print(dataList[[i]])
# }
# dir("USstates/s_22mr22.shp")
# states = st_read("USstates/s_22mr22.shp") |> 
#   st_make_valid()
#plot(states)
# tmap_mode("view")
# qtm(states)
#st_layers(states)
#https://jtr13.github.io/cc19/different-ways-of-plotting-u-s-map-in-r.html
plot_usmap(regions = "states") + 
  labs(title = "U.S. States",
       subtitle = "This is a blank map of the United States.") + 
  theme(panel.background=element_blank())

#preloaded regional groups
# plot_usmap(include = .south_region, labels = TRUE)
# plot_usmap(include = .east_north_central, labels = TRUE)
# plot_usmap(include = .east_south_central, labels = TRUE)
# plot_usmap(include = .midwest_region, labels = TRUE)
# plot_usmap(include = .mid_atlantic, labels = TRUE)
# plot_usmap(include = .mountain, labels = TRUE)
# plot_usmap(include = .new_england, labels = TRUE)
# plot_usmap(include = .northeast_region, labels = TRUE)
# plot_usmap(include = .north_central_region, labels = TRUE)
# plot_usmap(include = .pacific, labels = TRUE)
# plot_usmap(include = .south_atlantic, labels = TRUE)
# plot_usmap(include = .west_north_central, labels = TRUE)
# plot_usmap(include = .west_region, labels = TRUE)
# plot_usmap(include = .west_south_central, labels = TRUE)

#ridge plot, exploratory analysis 

plot(allGrades_all$Date,allGrades_all$`Weekly U.S. All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', ylim = range(0,7), col = 1) +
  points(allGrades_all$Date,allGrades_all$`Weekly California All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 2) +
  points(allGrades_all$Date,allGrades_all$`Weekly Colorado All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 3) + 
  points(allGrades_all$Date,allGrades_all$`Weekly Florida All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 4) +
  points(allGrades_all$Date,allGrades_all$`Weekly Massachusetts All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 5) +
  points(allGrades_all$Date,allGrades_all$`Weekly Minnesota All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 6) +
  points(allGrades_all$Date,allGrades_all$`Weekly New York All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 7) +
  points(allGrades_all$Date,allGrades_all$`Weekly Ohio All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 8) +
  points(allGrades_all$Date,allGrades_all$`Weekly Texas All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 9) + 
  points(allGrades_all$Date,allGrades_all$`Weekly Washington All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 10)






CA_data = allGrades_all |> 
  select(Date,`Weekly California All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  mutate(state = rep('CA',length(allGrades_all$`Weekly California All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`))) 

CO_data = allGrades_all |> 
  select(Date,`Weekly Colorado All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  mutate(state = rep('CO',length(allGrades_all$`Weekly Colorado All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`))) 

FL_data = allGrades_all |> 
  select(Date,`Weekly Florida All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  mutate(state = rep('FL',length(allGrades_all$`Weekly Florida All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`))) 

MA_data = allGrades_all |> 
  select(Date,`Weekly Massachusetts All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  mutate(state = rep('MA',length(allGrades_all$`Weekly Massachusetts All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`))) 

MN_data = allGrades_all |> 
  select(Date,`Weekly Minnesota All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  mutate(state = rep('MN',length(allGrades_all$`Weekly Minnesota All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`))) 

NY_data = allGrades_all |> 
  select(Date,`Weekly New York All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  mutate(state = rep('NY',length(allGrades_all$`Weekly New York All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`))) 

OH_data = allGrades_all |> 
  select(Date,`Weekly Ohio All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  mutate(state = rep('OH',length(allGrades_all$`Weekly Ohio All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`))) 

TX_data = allGrades_all |> 
  select(Date,`Weekly Texas All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  mutate(state = rep('TX',length(allGrades_all$`Weekly Texas All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`))) 

WA_data = allGrades_all |> 
  select(Date,`Weekly Washington All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  mutate(state = rep('WA',length(allGrades_all$`Weekly Washington All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`))) 

#all_States = rbind(CA_data,CO_data)
#FL_data,MA_data,MN_data,NY_data,OH_data,TX_data,WA_data
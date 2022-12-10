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
library(ggplot2)
library(maps)
library(mapdata)
library(gifski)
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

plot(allGrades_all$Date,allGrades_all$`Weekly California All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', ylim = range(0,7), col = 3) +
  points(allGrades_all$Date,allGrades_all$`Weekly Texas All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 6)




CA_data = allGrades_all |> 
  select(Date,`Weekly California All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  mutate(state = rep('california',length(allGrades_all$`Weekly California All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`))) |> 
  rename(week_gas_prices = `Weekly California All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  drop_na()


CO_data = allGrades_all |> 
  select(Date,`Weekly Colorado All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  mutate(state = rep('colorado',length(allGrades_all$`Weekly Colorado All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`))) |> 
  rename(week_gas_prices = `Weekly Colorado All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`)|> 
  drop_na()

FL_data = allGrades_all |> 
  select(Date,`Weekly Florida All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  mutate(state = rep('florida',length(allGrades_all$`Weekly Florida All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`))) |> 
  rename(week_gas_prices = `Weekly Florida All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`)|> 
  drop_na()

MA_data = allGrades_all |> 
  select(Date,`Weekly Massachusetts All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  mutate(state = rep('massachusetts',length(allGrades_all$`Weekly Massachusetts All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`))) |> 
  rename(week_gas_prices = `Weekly Massachusetts All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`)|> 
  drop_na()

MN_data = allGrades_all |> 
  select(Date,`Weekly Minnesota All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  mutate(state = rep('minnesota',length(allGrades_all$`Weekly Minnesota All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`))) |> 
  rename(week_gas_prices = `Weekly Minnesota All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`)|> 
  drop_na()

NY_data = allGrades_all |> 
  select(Date,`Weekly New York All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  mutate(state = rep('newyork',length(allGrades_all$`Weekly New York All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`))) |> 
  rename(week_gas_prices = `Weekly New York All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`)|> 
  drop_na()

OH_data = allGrades_all |> 
  select(Date,`Weekly Ohio All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  mutate(state = rep('ohio',length(allGrades_all$`Weekly Ohio All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`))) |> 
  rename(week_gas_prices = `Weekly Ohio All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`)|> 
  drop_na()

TX_data = allGrades_all |> 
  select(Date,`Weekly Texas All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  mutate(state = rep('texas',length(allGrades_all$`Weekly Texas All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`))) |> 
  rename(week_gas_prices = `Weekly Texas All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`)|> 
  drop_na()

WA_data = allGrades_all |> 
  select(Date,`Weekly Washington All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`) |> 
  mutate(state = rep('washington',length(allGrades_all$`Weekly Washington All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`))) |> 
  rename(week_gas_prices = `Weekly Washington All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`)|> 
  drop_na()

all_States = rbind(CA_data,CO_data,FL_data,MA_data,MN_data,NY_data,OH_data,TX_data,WA_data)

avg_prices = all_States |>
  group_by(state) |>
  summarize(Mean = mean(week_gas_prices))

plot_usmap(regions = "states", data = avg_prices, values = "Mean")

#https://conservancy.umn.edu/bitstream/handle/11299/220339/time-maps-tutorial-v2.html?sequence=3&isAllowed=y

states = map_data("state")

ggplot(data=states, aes(x=long, y=lat, group=group)) + 
  geom_polygon(color = "white", fill = "light green") + 
  guides(fill="none") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map with States') + 
  coord_fixed(1.3)

states2 = st_read("us_states_contiguous/states_contiguous.shp")
states_albers = st_transform(states2, crs = 2163)
#qtm(states_albers)

cali_map = states_albers |> 
  filter(STATE == "California") |> 
  merge(CA_data)

colo_map = states_albers |> 
  filter(STATE == "Colorado") |> 
  merge(CO_data)

flor_map = states_albers |> 
  filter(STATE == "Florida") |> 
  merge(FL_data)

mass_map = states_albers |> 
  filter(STATE == "Massachusetts") |> 
  merge(MA_data)

mini_map = states_albers |> 
  filter(STATE == "Minnesota") |> 
  merge(MN_data)

newy_map = states_albers |> 
  filter(STATE == "New York") |> 
  merge(NY_data)

ohio_map = states_albers |> 
  filter(STATE == "Ohio") |> 
  merge(OH_data)

texa_map = states_albers |> 
  filter(STATE == "Texas") |> 
  merge(TX_data)

wash_map = states_albers |> 
  filter(STATE == "Washington") |> 
  merge(WA_data)


#c("California","Colorado","Florida","Massachusetts","Minnesota","New York","Ohio","Texas","Washington")
pretty_map = tm_shape(states_albers) + tm_polygons(alpha = 0.5) +
  tm_shape(cali_map) + tm_polygons(col = "week_gas_prices") +
  # tm_shape(colo_map) + tm_polygons(col = "week_gas_prices") +
  # tm_shape(flor_map) + tm_polygons(col = "week_gas_prices") +
  # tm_shape(mass_map) + tm_polygons(col = "week_gas_prices") +
  # tm_shape(mini_map) + tm_polygons(col = "week_gas_prices") +
  # tm_shape(newy_map) + tm_polygons(col = "week_gas_prices") +
  # tm_shape(ohio_map) + tm_polygons(col = "week_gas_prices") +
  # tm_shape(texa_map) + tm_polygons(col = "week_gas_prices") +
  # tm_shape(wash_map) + tm_polygons(col = "week_gas_prices") + 
  tm_facets(by = "Date", nrow = 1, ncol = 1)
#pretty_map

# cali_shift = tm_shape(cali_map) + tm_polygons(alpha = 0.5,col = "week_gas_prices") + 
#   tm_facets(by = "Date", nrow = 1, ncol = 1)
#tmap_animation(pretty_map,delay = 5)

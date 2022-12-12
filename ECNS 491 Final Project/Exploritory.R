################################################################################
#Wyatt Sigafoos
#ECNS 491
#Exploratory Analysis
#Final Project: Observe the recent behavior of domestic gas prices
#November 10, 2022
################################################################################
# import libraries used for exploratory analysis
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
library(patchwork)
################################################################################
# import and clean data
# the original attempt to import data had extra rows in the top of the data frames,
# incorrect column names, and the wrong format for dates. specifying the col_types
# fixed the date format and setting the skip parameter removed the extra rows and 
# correctly assigned column names

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
################################################################################
# plot the difference between reformulated and conventional gas
columnNames = c("Date","Conventional","Reformulated")      #create a vector for column names 
                                                           #to use once for subset of the raw data
reg_reformulated_vs_conventional = regular_conventional |> #
  select(1:2) |>                                           #select the date and general price for 
  inner_join(regular_reformulated |> select(1:2))          #regular conventional and reformulated gas
                                                           #and inner_join them into one dataframe
colnames(reg_reformulated_vs_conventional) = columnNames   #change the column names for easier manipulation

mid_reformulated_vs_conventional = midgrade_conventional |>#repeat the process for midgrade
  select(1:2) |> 
  inner_join(midgrade_reformulated |> select(1:2))
colnames(mid_reformulated_vs_conventional) = columnNames

pre_reformulated_vs_conventional = premium_conventional |> #and again for premium
  select(1:2) |> 
  inner_join(premium_reformulated |> select(1:2))
colnames(pre_reformulated_vs_conventional) = columnNames

reg_conv_vs_ref_plot =                                     #create a plot for conventional vs
  ggplot(data = reg_reformulated_vs_conventional,          #reformulated gas for regular grade 
       aes(x = Date,
           y = Conventional,
           col = "Conventional")) + 
  geom_line() +
  geom_line(data = reg_reformulated_vs_conventional,
            aes(x = Date,
                y = Reformulated,
                col = "Reformulated")) +
  ggtitle("Regular") +
  theme_classic()

mid_conv_vs_ref_plot =                                    #create a plot for midgrade 
  ggplot(data = mid_reformulated_vs_conventional,         #of conventional vs reformulated
       aes(x = Date,
           y = Conventional,
           col = "Conventional")) +
  geom_line() +
  geom_line(data = mid_reformulated_vs_conventional,
            aes(x = Date,
                y = Reformulated,
                col = "Reformulated")) +
  ggtitle("Midgrade") +
  theme_classic()

pre_conv_vs_ref_plot =                                    #create a plot for premium
  ggplot(data = pre_reformulated_vs_conventional,
       aes(x = Date,
           y = Conventional,
           col = "Conventional")) +
  geom_line() +
  geom_line(data = pre_reformulated_vs_conventional,
            aes(x = Date,
                y = Reformulated,
                col = "Reformulated")) +
  ggtitle("Premium") +
  theme_classic()

reg_conv_vs_ref_plot/mid_conv_vs_ref_plot/pre_conv_vs_ref_plot #using the package
#                                                              #patchwork to stack the three plots
################################################################################
# plot the difference premium, midgrade and regular gas prices
columnNames2 = c("Date","Regular","Midgrade","Premium")   #initialize a column name vector for subseted data

reg_vs_mid_vs_pre = regular_all |>                        #pull price data for regular, midgrade
  select(1:2) |>                                          #and premium
  inner_join(midgrade_all |> select(1:2)) |> 
  inner_join(premium_all |> select(1:2))

colnames(reg_vs_mid_vs_pre) = columnNames2                #change column names 

reg_vs_mid_vs_pre_plot =                                  #plot all three gas varieties 
  ggplot(data = reg_vs_mid_vs_pre,                        #on one graph to see relationships
         aes(x = Date,
             y = Regular,
             col = "Regular")) +
  geom_line() +
  geom_line(data = reg_vs_mid_vs_pre,
            aes(x = Date,
                y = Midgrade,
                col = "Midgrade")) +
  geom_line() +
  geom_line(data = reg_vs_mid_vs_pre,
            aes(x = Date,
                y = Premium,
                col = "Premium")) +
  ggtitle("Regular vs. Midgrade vs. Premium") +
  theme_classic()

reg_vs_mid_vs_pre_plot                                  #display plot

################################################################################
# plot the range of gas prices for the nine states recorded by the EIA
# the data within the first plot() statement is the average price for the US
# and all following points() statements add one states average price to the 
# plot. This shows the variation range for the most and least expensive states
plot(allGrades_all$Date,allGrades_all$`Weekly U.S. All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', ylim = range(0,7), col = 1, xlab = "Date", ylab = "Price") +
  points(allGrades_all$Date,allGrades_all$`Weekly California All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 2) +
  points(allGrades_all$Date,allGrades_all$`Weekly Colorado All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 3) + 
  points(allGrades_all$Date,allGrades_all$`Weekly Florida All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 4) +
  points(allGrades_all$Date,allGrades_all$`Weekly Massachusetts All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 5) +
  points(allGrades_all$Date,allGrades_all$`Weekly Minnesota All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 6) +
  points(allGrades_all$Date,allGrades_all$`Weekly New York All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 7) +
  points(allGrades_all$Date,allGrades_all$`Weekly Ohio All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 8) +
  points(allGrades_all$Date,allGrades_all$`Weekly Texas All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 9) + 
  points(allGrades_all$Date,allGrades_all$`Weekly Washington All Grades All Formulations Retail Gasoline Prices  (Dollars per Gallon)`, type = 'l', col = 10)
################################################################################
# gather data on a by state basis to display on a map of the US
# add a column to the date and price dataframe full of the state name for grouping
# later in the code. finally get rid of any missing values
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

# combine each state into a dataframe of all states
all_States = rbind(CA_data,CO_data,FL_data,MA_data,MN_data,NY_data,OH_data,TX_data,WA_data)

avg_prices = all_States |>                #take the average price for each state
  group_by(state) |>                      #and create a dataframe for adding to a map of the us
  summarize(Mean = mean(week_gas_prices))

plot_usmap(regions = "states", data = avg_prices, values = "Mean") # add mean prices to the map of the us

#this is the website used for help ploting the US
#https://conservancy.umn.edu/bitstream/handle/11299/220339/time-maps-tutorial-v2.html?sequence=3&isAllowed=y

#all data below was in an attempt to animate the map of the US with price data 
#however due to the lack of sucess, was not used for the final report

states2 = st_read("us_states_contiguous/states_contiguous.shp")
states_albers = st_transform(states2, crs = 2163)

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

#Clear environment
rm(list=ls())
library(dplyr)
library(stringr) 
library(tidyr)
library(ggplot2)
library(XLConnect) # reads excel
library(leaflet) # creates maps
library(prettydoc) # html formatting
source("http://peterhaschke.com/Code/multiplot.R")

# Get Datasets by running other scripts
source('../data/get_county_populations.R')
source('../data/get_county_centers.R')
source('../data/get_stores.R')
source('../data/get_sales.R')


## Join conuty geocode with county population
county_details <- inner_join(county_centers,county_populations, by="County")

## Join county details with cannabis sales 
cannabis.sales <- full_join(cannabis.sales,county_details, by = c("County","year"))
cannabis.sales <- full_join(cannabis.sales,county_n.stores,by=c('County','kind'))

## Join Store addresses with zipcode to provide geocode for stores
med_stores <- left_join(med_stores,co_zipcodes)
retail_stores <- left_join(retail_stores,co_zipcodes)
combined_stores <- left_join(combined_stores,co_zipcodes)

# Count the number of medical, retail, and total stores in each county
n.med_county <- med_stores %>% group_by(County=county) %>% 
                summarise(medical=n())
n.retail_county <- retail_stores %>% group_by(County=county) %>% 
                summarise(recreational=n())
n.total_outlets <- combined_stores %>% group_by(County=county) %>% 
                summarise(Combined=n())

# Join sales dataframe with N store dataframe
#cannabis.sales <- full_join(cannabis.sales,county_n.stores, 
#                            by = c("County", "kind"))

# calculate perCapita sales and perStore sales
cannabis.sales <- cannabis.sales %>% 
                mutate(perCapita=USD/Population,
                       perStore = USD/n.stores) 


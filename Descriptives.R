# Descriptive Statistics

# Mobile
# Set 1 
# Total number of sms in the sample
SET1S_01small = read.csv("Data\\Challenge Data\\SET1\\SET1S_01small.CSV")
# SET1S_06 = read.csv("Data\\Challenge Data\\SET1\\SET1S_06.CSV")
data = SET1S_01small

colnames(data) = c("Date","in","out","SMS")
# creating datetime and date collumns in data
x= as.character(data$Date)
data$V2 = as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
data$date = as.Date(data$V2)

# new collumn in data for dates converted to integers 
date.lookup <- format(seq(as.Date("2013-01-01"), as.Date("2013-12-31"), by = "1 day")) 
data$int_date = match(as.character(data$date), date.lookup)
# 
# for (i in 1:max(data$int_date)){
#   
# }

file.names <- list.files("Data\\Challenge Data\\SET1\\", pattern ="SET1S_*", full.names = TRUE)
file.names

#Load, calculate and remove in for-loop through files

# Sum_SMS_monthly = c()
# for(i in 1:length(file.names)){
#   
#   Set = read.csv(file.names[i])
#   colnames(Set) = c("Date","in","out","SMS")
#   Sum_SMS_monthly[i] = sum(Set$SMS)
#  
# }

file.names <- list.files("Data\\Challenge Data\\SET1\\", pattern ="SET1V_*", full.names = TRUE)
Sum_calls_monthly = c()
Sum_duration_monthly = c()
for(i in 1:length(file.names)){
  
  Set = read.csv(file.names[i])
  colnames(Set) = c("Date","in","out","calls", "duration")
  Sum_calls_monthly[i] = sum(Set$calls)
  Sum_duration_monthly[i] = sum(Set$duration)
  
}

save(Sum_SMS_monthly, file = "Sum_SMS_monthly.RData")
save(Sum_calls_monthly, file= "Sum_calls_monthly.RData")
save(Sum_duration_monthly, file = "Sum_duration_monthly.RData")

plot(1:12, Sum_SMS_monthly)
plot(1:9, Sum_calls_monthly)
plot(1:9, Sum_duration_monthly)
plot(1:9, Sum_duration_monthly/Sum_calls_monthly)



# SET2 
# histogram of number of different places people visited
# on arrondisment level
# - redo for arrondisment
# - do for whole year/big datasets
# 

# only change to arrondisment
# load mobile data Set 3

library(dplyr)
library(data.table)
library(tibble)

#_________________________ loop from here __________________(go to uni, upload data files fasster to cluster, save outcome)

INDICATORS_SET3_HEADERS <- read.csv("Data/Challenge Data/SET3/INDICATORS_SET3_HEADERS.CSV", quote="'")
SET3_M01small <- read.csv("Data/Challenge Data/SET3/SET3_M01small.CSV", header=FALSE)

SET3_M01 <- read.csv("Data/Challenge Data/SET3/SET3_M01.CSV", header=FALSE)
data = SET3_M01
#data = SET2_P01

##### calculate home for arr level #####

# theory
# create matrix: 
# row: user_ids (150000)
# col: 365 days
# value: 1:123 (arr_id that is last call)


# create sleepplace tibble: each collumn holds one two week period, rows hold IDs
sleepplace_arr = as.tibble(matrix(nrow=NROW(unique(data$V1)), ncol = 365))
#sleepplace_arr = add_column(sleepplace_arr, user_id = unique(data$V1), .before = "V1")

# creating datetime and date collumns in data
x= as.character(data$V2)
data$V2 = as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
data$date = as.Date(data$V2)

# new collumn in data for dates converted to integers (MARKED: ONLY FIRST TWO WEEKS YET)
# date.lookup <- format(seq(as.Date("2013-01-01"), as.Date("2013-12-31"), by = "1 day")) 
# data$int_date = match(as.character(data$date), date.lookup)

# group data to work on separate users and separate days
data_df = tbl_df(data)
colnames(data_df) = c("user_id", "timestamp", "arr_id", "date")
data_df_grouped = group_by(data_df, user_id, date)

# extract last call from every user on every day
sleep_arr = summarize(data_df_grouped, site_of_last_call = last(arr_id))
save(sleep_arr, file = "sleep_arr_01.RData") # <-------------------------------looop the file name 
sleep_arr_df = tbl_df(sleep_arr)
sleep_by_user_arr = group_by(sleep_arr_df, user_id)

# Fill prepared sleepplace tibble
# 
# sleepplace_arr[]
#_________________________ to here______________________________________

# histogram of arr home
arr_sleep_hist = hist(sleep_arr$site_of_last_call, breaks = 123)
summary(arr_sleep_hist$counts)

# histogram of how many places people slept at (more relevant for malaria since bites only at night)
# how many unique site_of_last_call

arr_number_of_different_sleepplaces = summarize(sleep_by_user_arr, n_distinct_sleepplaces = n_distinct(site_of_last_call))

arr_number_of_different_sleepplaces_hist = hist(arr_number_of_different_sleepplaces$n_distinct_sleepplaces)


help.start()



require(rgeos)
require(rgdal)
require(raster)
# Incidence Raster (incidence rate: cases per person per year)
# 5x5km pro Pixel

GDALinfo("Data\\Malaria\\country_profiles_data\\2015_Nature_Africa_Incidence_SEN_2013.tiff")
Incidence2013_raster = raster(("Data\\Malaria\\country_profiles_data\\2015_Nature_Africa_Incidence_SEN_2013.tiff"))
plot(Incidence2013_raster)
myCRS = crs(Incidence2013_raster)

hist(Incidence2013_raster,
     main = "Distribution of Incidence values",
     xlab = "Incidence", ylab = "Frequency",
     col = "springgreen")
mean(Incidence2013_raster)


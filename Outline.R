# Similiar to Source/Sink Tatem, 2014
# malaria risk connectivity matrices  
# 
# Check to do to step:
# - towers aggregated to larger areas in urban areas defined by GRUMP-UE dataset <- not neccessary for arr level
#   - download GRUMP-UE Senegal
#   - "falling within the boundaries of urban extents"

# - calculate 'home' for people
# - construct weighted network of movements between tower areas
# - not only residents and visitors: (every night)*(risk there) .> infective after 7 days (Average time a person is infective) 
# added to the population of infected in sleeping place




# - learn how to use comunity detection algorithm mentioned
# - check whether there is a transmission season
# - split by returning residents and visitors
# - read paper 25 to understand function of infection risk by days (in malaria season) and prevalence
# - 


# load polygon shapefile of tower areas
# assign the same coordinate system to mobile towers and incidence raster
# multiply incidence value with value attached to tower
# assign tower the average of raster values in towerarea


# install.packages("raster")
# install.packages("geoRglm")
# install.packages("rgdal")
library(rgeos)
library(rgdal)
library(raster)

# load incidence data:
GDALinfo("Data\\Malaria\\country_profiles_data\\2015_Nature_Africa_Incidence_SEN_2013.tiff")
Incidence2013_raster = raster(("Data\\Malaria\\country_profiles_data\\2015_Nature_Africa_Incidence_SEN_2013.tiff"))
plot(Incidence2013_raster, axes=TRUE)
plot(SEN_arr,axes=TRUE, add = T)
plot(SEN_arr, add = T)
myCRS = crs(Incidence2013_raster)

myCRS
# load mobile data Set 2 TOWER LEVEL!!!!!!!!!!!!!!!!!
# for FIRST PART OF first two week space -> extend for other two week periods! (PLACES MARKED)
library(dplyr)
library(data.table)
library(tibble)
INDICATORS_SET2_HEADERS <- read.csv("Data/Challenge Data/SET2/INDICATORS_SET2_HEADERS.CSV", quote="'")
SET2_P01small <- read.csv("Data/Challenge Data/SET2/SET2_P01small.CSV", header=FALSE)
#SET2_P01 <- read.csv("Data/Challenge Data/SET2/SET2_P01.CSV", header=FALSE)
data = SET2_P01small
#data = SET2_P01

##### calculate home on tower level #####

# create sleepplace tibble: each collumn holds one two week period, rows hold IDs
sleepplace = as.tibble(matrix(nrow=NROW(unique(data$V1)), ncol = 25))
sleepplace = add_column(sleepplace, user_id = unique(data$V1), .before = "V1")

# creating datetime and date collumns in data
x= as.character(data$V2)
data$V2 = as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
data$date = as.Date(data$V2)

# new collumn in data for dates converted to integers (MARKED: ONLY FIRST TWO WEEKS YET)
date.lookup <- format(seq(as.Date("2013-01-07"), as.Date("2013-01-20"), by = "1 day")) 
data$int_date = match(as.character(data$date), date.lookup)

# group data to work on separate users and separate days
data_df = tbl_df(data)
colnames(data_df) = c("user_id", "timestamp", "site_id", "date", "int_date")
data_df_grouped = group_by(data_df, user_id, int_date)

# extract last call from every user on every day
sleep = summarize(data_df_grouped, site_of_last_call = last(site_id))
sleep_by_user = group_by(sleep, user_id)

# Fill prepared sleepplace tibble
# 


# Antenna site locations
antennalocations = read.csv("Data\\Challenge Data\\ContextData\\SITE_ARR_LONLAT.CSV", header = TRUE)

#Arrondisment shapefile
SEN_arr <- readOGR(dsn = "Data/Edwards Data/Input Data Edward", layer = "senegal_arr_2014_wgs")
SEN <- readOGR(dsn = "Data/Edwards Data/Input Data Edward", layer = "SEN_outline")
plot(SEN_arr)
crs(SEN_arr)

# tower area shapefile
source("voronoi_segmentation.r")
Voronoi_SPDF = voronoi_segmentation()
plot(Voronoi_SPDF)
plot(Voronoi_SPDF[500,])

# colour map
source("coloured_map.R")
example = 1:1668
colouredmapfunct(Voronoi_SPDF, example)

# averaging raster values within towerarea <<<<--------------------- How to do it for all tower polygons?
# use https://www.neonscience.org/dc-crop-extract-raster-data-r
require(maptools)
require(ggplot2)

# SEN_arr_Shapepoly = readShapePoly()

getValues(Incidence2013_raster)
coordinates(Incidence2013_raster)
length(unique(getValues(Incidence2013_raster)))

Incidence2015_raster = raster(("Data\\Malaria\\country_profiles_data\\2015_Nature_Africa_Incidence_SEN_2015.tiff"))
getValues(Incidence2015_raster)
length(unique(getValues(Incidence2015_raster)))

# what do I want
# extract the average values of incidence in the admin areas and 
# 
library(dplyr)
av_tbl = tbl_df(cbind(ad = getValues(admin), inc = getValues(incidence_new)))
# a= coordinates(admin)
# b= coordinates(incidence_new)
# identical(a,b)

du = admin
values(du)[20]=100000000
plot(du)
dummy = incidence_new
values(dummy)[520]=100000000
plot(dummy)


ja__ = av_tbl %>%
  filter(!is.na(ad)) %>%
  group_by(ad) %>%
  summarize(mean_value = mean(inc, na.rm = TRUE)) #<-------- this works
ja__

# write averaging function

  
# un = unique(admin)
# for(i in 1:123){
#   av_tbl$average[i] = mean(filter(av_tbl, av_tbl["ad"]== un[i])["inc"])
# }


# take vector of factors of admin
# 

# assigning all raster values with same admin value the average value


# what do I want
# a plot with the average malaria values for each arr

# working with ggplot, raster

# 1. add a col to ad that is the value for each pixel
# 1.1 
# put raster in a stack, group by admin variable, average, create new raster of this

# ?stackApply()
# ?clump
# library(igraph)
# unique(admin)
# st = stack(admin, incidence_new)


# 
# reclass_matrix = as.matrix(cbind(unique(admin), average_raster_arr))
# colnames(reclass_matrix) = NULL

reclass_matrix = as.matrix(cbind(ja__$ad, ja__$mean_value))
colnames(reclass_matrix) = NULL
reclass_matrix

# reclassify the raster using the reclass object - reclass_matrix
admin_with_average_malaria_values <- reclassify(admin,reclass_matrix)

admin_with_average_malaria_values



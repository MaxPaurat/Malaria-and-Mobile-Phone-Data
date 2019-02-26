library(raster)
library(rgdal)

Incidence2013_raster = raster(("Data\\Malaria\\country_profiles_data\\2015_Nature_Africa_Incidence_SEN_2013.tiff"))
SEN_arr <- readOGR(dsn = "Data/Edwards Data/Input Data Edward", layer = "senegal_arr_2014_wgs")

# the follwoing doesnt relly work (check out borders)
test_raster = raster(ncol=146, nrow=106)
extent(test_raster) <- extent(SEN_arr)
rp <- rasterize(SEN_arr, test_raster, 'ARR_ID')
plot(rp)
plot(SEN_arr, add=T)
plot(Incidence2013_raster, add=T)


# try with Ewans new level 3 files

# Goal:
# Incidence map and arrondisment pixels of rasters overlap perfectly
# average incidence values for arrondisment


Incidence2013_raster = raster(("Data\\Malaria\\country_profiles_data\\2015_Nature_Africa_Incidence_SEN_2013.tiff"))
SEN_arr_MAP <- raster("Data/Ewan Data/SEN_admin2018_3_MG_5K/SEN_admin2018_3_MG_5K.tif")
plot(SEN_arr_MAP)
plot(Incidence2013_raster, add = T)
# problem with the plot() function?

# check extents
extent(SEN_arr_MAP)
extent(Incidence2013_raster)
show(SEN_arr_MAP)
show(Incidence2013_raster)
Incidence2013_raster

# ewans approach
library(raster) 
#the rasters are not just shifted but different in shape(see dakar region)
incidence_original = raster(("Data\\Malaria\\country_profiles_data\\2015_Nature_Africa_Incidence_SEN_2013.tiff"))
admin <- raster("Data/Ewan Data/SEN_admin2018_3_MG_5K/SEN_admin2018_3_MG_5K.tif")
dummy <- admin
values(dummy) <- extract(incidence_original,coordinates(admin))
incidence_new <- dummy

# how an it be that even after this procedure the slots of the raster that are filled are not the same?


plot(admin)
plot(incidence_original)
plot(incidence_new, add = T)
plot(incidence_new)
show(incidence_new)
show(admin)


# What is to be done
# undestand what the fuck is going on with the not overlaid plot, because if they are not overlaid then the assignment 
# didn't work
# that would mean that when averaging I get fucking wrong values
# so one way to check is to see whether there are 0s in the new incidence map
# -> values seem to say no 
# how can it have no 0s but be a raster with rows and collumns that should amount to a square?
# ->> learn about the raster data structure
# so it's a fucking square with NAs
# so I will get an error if I average over NA I hope
# but it seems like they

library(ggplot2)
values(incidence_new)[1:10]

inc = as.data.frame(incidence_new, row.names=NULL, optional=FALSE, xy=TRUE, 
                    na.rm=FALSE, long=FALSE)
ad  = as.data.frame(admin, xy=TRUE)

ggplot() +
  geom_raster(data = inc , aes(x=x,y=y, fill=SEN_admin2018_3_MG_5K)) + 
  coord_quickmap()


ggplot() +
  geom_raster(data = ad , aes(x=x,y=y, fill=SEN_admin2018_3_MG_5K)) + 
  coord_quickmap()


#Map of Malaria of arr level ---> pretty good except for dakar (because of shift problem)
arr_with_average_malaria_values_df  = as.data.frame(admin_with_average_malaria_values, xy=TRUE)
ggplot() +
  geom_raster(data = arr_with_average_malaria_values_df , aes(x=x,y=y, fill=layer)) + 
  coord_quickmap()



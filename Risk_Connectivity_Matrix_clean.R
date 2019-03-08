# Risk Connectivity Matrix CLEAN (on arr level)

# Open: 
# - make a matrix and a df (where we can see connections between arr's)
# - the rasters are not just shifted but different in shape(see dakar region), 
#     how to treat this (no incidence values for parts of dakar)?
# - choose the months for the connectivity matrix which are in the malaria season

# Loading libraries
library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)

# Loading input files
Incidence2013_raster = raster(("Data\\Malaria\\country_profiles_data\\2015_Nature_Africa_Incidence_SEN_2013.tiff"))
SEN_arr_MAP <- raster("Data/Ewan Data/SEN_admin2018_3_MG_5K/SEN_admin2018_3_MG_5K.tif")

# alligning input files
incidence_original = Incidence2013_raster
admin <- SEN_arr_MAP
dummy <- admin
values(dummy) <- extract(incidence_original,coordinates(admin))
incidence_new <- dummy
rm(dummy)

# plot alligned input files
plot(admin)
plot(incidence_new)

# aggregate malaria incidence to arr level
av_tbl = tbl_df(cbind(ad = getValues(admin), inc = getValues(incidence_new)))

av_tbl2 = av_tbl %>%
  filter(!is.na(ad)) %>%
  group_by(ad) %>%
  summarize(mean_value = mean(inc, na.rm = TRUE)) 

# reclassify the raster using the reclass object - reclass_matrix
reclass_matrix = as.matrix(cbind(av_tbl2$ad, av_tbl2$mean_value))
colnames(reclass_matrix) = NULL
admin_with_average_malaria_values <- reclassify(admin,reclass_matrix)

save(reclass_matrix, file="reclass_matrix.Rda")

#Map of Malaria of arr level ---> pretty good except for dakar (because of shift problem)
arr_with_average_malaria_values_df  = as.data.frame(admin_with_average_malaria_values, xy=TRUE)
ggplot() +
  geom_raster(data = arr_with_average_malaria_values_df , aes(x=x,y=y, fill=layer)) + 
  coord_quickmap()

############## 



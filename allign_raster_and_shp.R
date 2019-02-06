library(raster)
library(rgdal)

Incidence2013_raster = raster(("Data\\Malaria\\country_profiles_data\\2015_Nature_Africa_Incidence_SEN_2013.tiff"))
SEN_arr <- readOGR(dsn = "Data/Edwards Data/Input Data Edward", layer = "senegal_arr_2014_wgs")

test_raster = raster(ncol=146, nrow=106)
extent(test_raster) <- extent(SEN_arr)
rp <- rasterize(SEN_arr, test_raster, 'ARR_ID')
plot(rp)
plot(SEN_arr, add=T)
plot(Incidence2013_raster, add=T)


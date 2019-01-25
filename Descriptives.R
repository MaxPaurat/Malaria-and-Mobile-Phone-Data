# Descriptive Statistics

# Mobile
# Grösse der files verhindert gleichzeitiges Laden

# Geht es trotzdem?

# Calculating the sleepplace


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


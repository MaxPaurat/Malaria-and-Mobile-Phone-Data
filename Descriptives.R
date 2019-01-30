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










help.start()
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


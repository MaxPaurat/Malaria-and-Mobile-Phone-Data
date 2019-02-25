# connectivity risk matrix on arr level

# calculate sleep data for whole year
# aggregate per arr
# 

# con_risk_df = data.frame(tower_id, returning_residents_risk, visitors_risk)
# ?data.frame
# 
# tower_id = 1:1666

# calculate sleepplaces on arr level from SET 3

library(dplyr)
# library(data.table)
library(tibble)

# SET3_M01small <- read.csv("Data/Challenge Data/SET3/SET3_M01small.CSV", header=FALSE)
SET3_M01 <- read.csv("Data/Challenge Data/SET3/SET3_M01.CSV", header=FALSE)
# data = SET3_M01small
data = SET3_M01

# create sleepplace tibble
# 
# sleepplace_arr_SET3 = as.tibble(matrix(nrow=NROW(unique(data$V1)), ncol = 365 ))
# sleepplace_arr_SET3 = add_column(sleepplace_arr_SET3, user_id = unique(data$V1), .before = "V1")

# loop to load all data
i = "01"
# months = formatC(1:12,width=2,format="d",flag="0")
# 
# for(i in months) {
#   infile <- paste("Data/Challenge Data/SET3/SET3_M",i,".CSV", sep = "")
#   print(infile)
#   data <- read.csv(infile, header=FALSE)
  
##### calculate home on arr level from SET 3#####

# creating datetime and date collumns in data
x= as.character(data$V2)
data$V2 = as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
data$date = as.Date(data$V2)

# new collumn in data for dates converted to integers
date.lookup <- format(seq(as.Date("2013-01-01"), as.Date("2013-12-31"), by = "1 day")) 
data$int_date = match(as.character(data$date), date.lookup)

# group data to work on separate users and separate days
data_df = tbl_df(data)
colnames(data_df) = c("user_id", "timestamp", "site_id", "date", "int_date")
data_df_grouped = group_by(data_df, user_id, int_date)

# extract last call from every user on every day
sleep = summarize(data_df_grouped, site_of_last_call = last(site_id))
ungroup(sleep)

  if(i == "01"){
    sleepplace_arr_SET3 = sleep
  } else {
      sleepplace_arr_SET3 = rbind(sleepplace_arr_SET3,sleep)
  }
# 
# }

# order by person and day to find home
sleepplace_arr_SET3[
  with(sleepplace_arr_SET3, order(user_id, int_date)),
  ]

save(sleepplace_arr_SET3,file="sleepplace_arr_SET3.Rda")

# load("sleepplace_arr_SET3.Rda")

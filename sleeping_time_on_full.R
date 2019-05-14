# sleeping_time_on_full_data
library(dplyr)
library(tibble)
library(devtools)
library(multidplyr)


n_months = 12
months = formatC(1:n_months,width=2,format="d",flag="0")
file_month_names = vector("list", n_months)
for(k in 1:n_months) {
  infile <- paste("/cluster/scratch/pauratm/Data/Challenge\ Data/SET3/SET3_M",months[k],".CSV", sep = "")
  # infile <- paste("Data/Challenge Data/SET3/SET3_M",months[k],".CSV", sep = "")

  file_month_names[[k]] = infile

}
object.size(file_month_names)

save(file_month_names, file="file_month_names.Rda")
###

data_complete = list()

for(k in 1:n_months){

  temp = read.csv(file_month_names[[k]], header = FALSE)

  data_complete[[k]] = temp

  cat(k)
}
object.size(data_complete)

save(data_complete, file="data_complete.Rda")
# load("data_complete.Rda")

for(i in 1:n_months){
  
  colnames(data_complete[[i]]) = c("V1","V2","V3")
  
}
for(i in 1:n_months){
  
  print(colnames(data_complete[[i]]))
  
}


data = do.call(rbind, data_complete)
object.size(data)


# creating datetime and date collumns in data
x= as.character(data$V2)
data$V2 = as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
data$date = as.Date(data$V2)

# new collumn in data for dates converted to integers
date.lookup <- format(seq(as.Date("2013-01-01"), as.Date("2013-12-31"), by = "1 day")) 
data$int_date = match(as.character(data$date), date.lookup)

# group data to work on separate users and separate days
data_df = tbl_df(data)
rm(data)
colnames(data_df) = c("user_id", "timestamp", "site_id", "date", "int_date")

# aggregating Dakar arrs to be new id 11, this way it can be seen easily that Dakar is aggregated
data_df$site_id[data_df$site_id %in% 1:11] <- 11

sleepplace_aggr_dakar_full = data_df %>% 
  partition(user_id, int_date) %>% 
  summarize(data_df_grouped, site_of_last_call = last(site_id)) %>%
  collect()

# extract last call from every user on every day
object.size(sleepplace_aggr_dakar_full)

# save(sleepplace_aggr_dakar_full, file="sleepplace_aggr_dakar_full.Rda")

# order by person and day to find home

homes = sleepplace_aggr_dakar_full %>% 
  partition(user_id) %>% 
  summarize (home = names(which.max(table(site_of_last_call)))) %>%
  collect()

save(sleepplace_aggr_dakar_full, file="sleepplace_aggr_dakar_months_1-12.Rda")
save(homes, file ="homes_month_1-12.Rda")
object.size(homes)

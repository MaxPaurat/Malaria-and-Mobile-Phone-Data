# sleeping_time_on_full_data


n_months = 12
months = formatC(1:n_months,width=2,format="d",flag="0")
file_month_names = vector("list", n_months)
for(k in 1:n_months) {
  infile <- paste("cluster/scratch/pauratm/Data/Challenge\ Data/SET3/SET3_M",months[k],".CSV", sep = "")
  # infile <- paste("Data/Challenge Data/SET3/SET3_M",months[k],".CSV", sep = "")
  
  file_month_names[[k]] = infile
  
}

save(file_month_names, file="file_month_names.Rda")
###

data_complete = list()

n = 12
for(k in 1:n){
  
  temp = read.csv(file_month_names[[k]], header = FALSE)
  
  data_complete[[k]] = temp
  
  cat(k)
}

save(data_complete, file="data_complete.Rda")

for(i in 1:12){
  
  colnames(data_complete[[i]]) = c("V1","V2","V3")
  
}
for(i in 1:12){
  
  print(colnames(data_complete[[i]]))
  
}


data_complete_joined = do.call(rbind, data_complete)

data = data_complete_joined

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

# aggregating Dakar arrs to be new id 11, this way it can be seen easily that Dakar is aggregated
data_df$site_id[data_df$site_id %in% 1:11] <- 11
data_df_grouped = group_by(data_df, user_id, int_date)

# extract last call from every user on every day
sleep = summarize(data_df_grouped, site_of_last_call = last(site_id))
ungroup(sleep)

sleepplace_aggr_dakar_full = sleep
save(sleepplace_aggr_dakar_full, file="sleepplace_aggr_dakar_full.Rda")
load("reclass_matrix.Rda")

# order by person and day to find home

homes = sleepplace_aggr_dakar_full %>% group_by(user_id) %>% summarize (home = names(which.max(table(site_of_last_call)))) 
save(sleepplace_aggr_dakar_full, file="sleepplace_aggr_dakar_full.Rda")

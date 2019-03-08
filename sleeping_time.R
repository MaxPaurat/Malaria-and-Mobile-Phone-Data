library(dplyr)
library(tibble)

n_months = 12
months = formatC(1:n_months,width=2,format="d",flag="0")
file_month_names = vector("list", n_months)
for(k in 1:n_months) {
  infile <- paste("Data/Challenge\ Data/SET3/SET3_M",months[k],".CSV", sep = "")
  # infile <- paste("Data/Challenge Data/SET3/SET3_M",months[k],".CSV", sep = "")
  
  file_month_names[[k]] = infile
  
}

data_complete = list()

user_list_1000 = sample(unique(SET3_M01[,1]),1000)
save(user_list_1000, file = "user_list_1000.Rda")

n = 12
for(k in 1:n){
  
  temp = read.csv(file_month_names[[k]], header = FALSE)
  
  data_complete[[k]] = temp[temp[,1] %in% user_list_1000,]
  
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

sleepplace_1000 = sleep

save(sleepplace_1000, file="sleepplace_1000.Rda")
load("reclass_matrix.Rda")

# order by person and day to find home

homes = sleepplace_1000 %>% group_by(user_id) %>% summarize (home = names(which.max(table(site_of_last_call)))) 
sleepplace_1000 <- merge(sleepplace_1000,homes,by="user_id")
arr_av_malaria_df = as.data.frame(reclass_matrix)
colnames(arr_av_malaria_df) = c("arr_id_MAP","av_malaria_inc")
arr_av_malaria_df = add_column(arr_av_malaria_df, arr_id_MAP_123 = 1:123,home = 1:123)
sleepplace_1000 = merge(sleepplace_1000, arr_av_malaria_df, by="home")


sleepplace_1000 = sleepplace_1000[order(sleepplace_1000$user_id, sleepplace_1000$int_date),]

save(sleepplace_1000, file="sleepplace_1000.Rda")

proportion_home = numeric(1000)
user_list_1000 = unique(data_complete_joined$V1)

for(i in 1:1000){
  i=1
  weights = diff(c(0,sleepplace_1000$int_date[sleepplace_1000$user_id == user_list_1000[i]]))
  at_home = as.numeric(sleepplace_1000$site_of_last_call[sleepplace_1000$user_id == user_list_1000[i]] == sleepplace_1000$home[sleepplace_1000$user_id == user_list_1000[i]])
  proportion_home[i] = sum(weights*at_home)/sum(weights)
  }

hist(proportion_home, breaks = 50)

# # what happens to hist when people with home in Dakar are not considered
# proportion_home_outside_dakar = numeric(1000)
# 
# sleepplace_1000_outside_Dakar = sleepplace_1000[sleepplace_1000$home.x %in% 11:124,]
# 
# sl = sleepplace_1000_outside_Dakar
# for(i in 1:1000){
#   weights = diff(c(0,sl$int_date[sl$user_id == user_list_1000[i]]))
#   at_home = as.numeric(sl$site_of_last_call[sl$user_id == user_list_1000[i]] == sl$home.x[sl$user_id == user_list_1000[i]])
#   proportion_home_outside_dakar[i] = sum(weights*at_home)/sum(weights)
# }
# 
# hist(proportion_home_outside_dakar, breaks = 50)
# # home in dakar
# proportion_home_in_Dakar = numeric(1000)
# sleepplace_1000_in_Dakar = sleepplace_1000[sleepplace_1000$home.x %in% 1:10,]
# 
# sl = sleepplace_1000_in_Dakar
# for(i in 1:1000){
#   weights = diff(c(0,sl$int_date[sl$user_id == user_list_1000[i]]))
#   at_home = as.numeric(sl$site_of_last_call[sl$user_id == user_list_1000[i]] == sl$home.x[sl$user_id == user_list_1000[i]])
#   proportion_home_in_Dakar[i] = sum(weights*at_home)/sum(weights)
# }
# 
# hist(proportion_home_in_Dakar, breaks = 50)




# people who spend more than 80% of time at home 
# look at the length of sequence of time in one place
# seasonal migrants should have 
# - one or more longer sequence if the sleep in the same arr as working
# - many people with similar movement
# might need to be made more robust by discounting short range trips

# separate id's which have the 80% at home criterion

proportion_home_df = cbind(proportion_home, user_list_1000)

travellers_low = proportion_home_df[proportion_home_df[,1] > 0.8,2]
hist(proportion_home_df[proportion_home_df[,2] %in% travellers_low,1], breaks = 50)

# Identify complete trips people have taken 
# from
# not_home vector and weights vector
# 
# rle and then plus weights?
# easier with full time series
# 
# how to handle times they didn't call?
# 
travellers_low_df = data.frame()
colnames(travellers_low_df) <- c(user_id, home_arr, trip destination arr., trip start (min), trip start (max), trip end (min), trip end (max)

install.packages(adehabitatLT)
ltraj

sleepplace_arr_SET3 = sleepplace_1000

#visitors
build_visitor_df = function(user_ID){
  test3=sleepplace_arr_SET3[sleepplace_arr_SET3["user_id"] == user_ID,]
  test4 = rle(test3$site_of_last_call)
  test4.1 = data.frame(lengths = test4$lengths, values =test4$values)
  test5 = test4.1[!(test4.1$values == unique(test3$home)),]
  test5.1 = as.data.frame(table(test5$values))
  test5.2 = add_column(test5.1, user_ID = user_ID)
}

# wrangle to nice dataframe
visitor_dfs = lapply(unique(sleepplace_arr_SET3$user_id), build_visitor_df)
visitor_df = do.call("rbind", visitor_dfs)
colnames(visitor_df) = c("arr_visited", "how_often", "user_id")
sleepplace_arr_SET3_2 = unique(sleepplace_arr_SET3[,c("user_id","av_malaria_inc")])
visitor_df2 = merge(visitor_df, sleepplace_arr_SET3_2, by="user_id")
visitor_df2["multiplied"] = visitor_df2$how_often*visitor_df2$av_malaria_inc

save(visitor_df2, file="visitor_df2.Rda")

arr_visitor_risk = visitor_df2 %>%
  group_by(arr_visited) %>%
  summarize(sum(multiplied, na.rm = TRUE))

save(arr_visitor_risk, file="arr_visitor_risk.Rda")
# checking whether bigmemory is enough to work step by step with big files


# sleepplace_1000$int_date[sleepplace_1000$user_id == user_list_1000[i]]







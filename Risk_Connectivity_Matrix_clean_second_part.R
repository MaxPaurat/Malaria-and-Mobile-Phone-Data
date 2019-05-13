# RisK_Connectivity_Matrix_clean Second part

library(tibble)
library(dplyr)

# create df to fill with connectivity risk measures as in Tatem, 2014
con_risk_df = data.frame(arr_id = 1:123, returning_residents_risk=NA, visitors_risk=NA)

# create list of path names to mobility data SET3 (arr level)
n_months = 2
months = formatC(1:n_months,width=2,format="d",flag="0")
file_month_names = vector("list", n_months)
for(k in 1:n_months) {
  infile <- paste("/cluster/scratch/pauratm/Data/Challenge\ Data/SET3/SET3_M",months[k],".CSV", sep = "")
  # infile <- paste("Data/Challenge Data/SET3/SET3_M",months[k],".CSV", sep = "")

  file_month_names[[k]] = infile

}

############ calculate home on arr level from SET 3 ###########

# create function to calculate sleepplace
calculate_sleepplace = function(file_month_name){

  data <- read.csv(file_month_name, header=FALSE)

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

}

# call the function <- doesn't seem to run in parallel yet on cluster
results <- lapply(file_month_names, calculate_sleepplace)

# combine elements of the results list to one dataframe
sleepplace_arr_SET3 = do.call("rbind", results)

save(sleepplace_arr_SET3,file="sleepplace_arr_SET3_test1.Rda")

###### 
load("reclass_matrix.Rda")

# some new collumns like home and average malaria incidence at persons home
homes = sleepplace_arr_SET3 %>% group_by(user_id) %>% summarize (home =names(which.max(table(site_of_last_call)))) 
sleepplace_arr_SET3 <- merge(sleepplace_arr_SET3,homes,by="user_id")
arr_av_malaria_df = as.data.frame(reclass_matrix)
colnames(arr_av_malaria_df) = c("arr_id_MAP","av_malaria_inc")
arr_av_malaria_df = add_column(arr_av_malaria_df, arr_id_MAP_123 = 1:123,home = 1:123)
sleepplace_arr_SET3 = merge(sleepplace_arr_SET3, arr_av_malaria_df, by="home")

# when person isn't home
sleepplace_arr_SET3= mutate(sleepplace_arr_SET3,not_home = sleepplace_arr_SET3$home != sleepplace_arr_SET3$site_of_last_call)

# order by person and day
sleepplace_arr_SET3[with(sleepplace_arr_SET3, order(user_id, int_date)),]

#### calculate risk due to visitors and returning residents ###

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

save(visitor_df2, "visitor_df2.Rda")

arr_visitor_risk = visitor_df2 %>%
  group_by(arr_visited) %>%
  summarize(sum(multiplied, na.rm = TRUE))

save(arr_visitor_risk, "arr_visitor_risk.Rda")

# returning residents df
  # number of days * risk of visited arr
  # based on sleepplace_arr_SET3

build_returning_residents_df = function(user_ID){
  test3 = sleepplace_arr_SET3[sleepplace_arr_SET3["user_id"] == user_ID,]
  test4 = rle(test3$site_of_last_call)
  test4.1 = data.frame(lengths = test4$lengths, values =test4$values)
  test5 = test4.1[!(test4.1$values == unique(test3$home)),] 
  test5.1.1 = test5 %>%
    group_by(values) %>%
    summarise(nights_not_home = sum(lengths))
  #test5.1 = as.data.frame(test5.1.1))
  test5.2 = add_column(test5.1.1, user_ID = user_ID)
}

returning_residents_dfs = lapply(unique(sleepplace_arr_SET3$user_id)[1:100], build_returning_residents_df)

save(returning_residents_dfs, returning_residents_dfs)

load("sleepplace")


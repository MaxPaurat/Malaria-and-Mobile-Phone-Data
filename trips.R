# for one user
# sl = dataframe of arrs, weights, int_dates, home

library(dplyr)
load("sleepplace_1000.Rda")

sleepplace_1000 = sleepplace_1000[order(sleepplace_1000$user_id),]
homes = sleepplace_1000 %>% group_by(user_id) %>% summarize (home = as.numeric(names(which.max(table(site_of_last_call)))))
sleepplace_1000 <- merge(sleepplace_1000,homes,by="user_id")
colnames(sleepplace_1000)[3] <- "arr"

# trips = data.frame(user_id = integer(), home_arr = integer(), dest = integer(), start.min = integer(), start.max = integer(), end.min = integer(), end.max = integer())
trips = data.frame(matrix(NA,10000,7))
colnames(trips) <- c("user_id","home_arr","dest","start.min","start.max","end.min","end.max")
# s = index of first home +1
# e = index of last home


# lapply(unique(sleepplace_1000$user_id), calc_trips)
# sleepplace_1000 = sleepplace_1000 %>% group_by(user_id) 


for(j in 1:length(unique(sleepplace_1000$user_id))){
  
sl = sleepplace_1000[sleepplace_1000$user_id == unique(sleepplace_1000$user_id)[j],] # loop this
sl$weights = diff(c(0,sl$int_date)) 


# q geht die rechnung am ende auf mit e-s?

h = unique(sl$home)
u = unique(sl$user_id)

s = min(which(sl$arr == h))
e = max(which(sl$arr == h))

c = 0 # trip counter
for (i in 1:(e-s)){ # over all rows that are between first and last home row (completed trips)
if(sl$arr[s-1+i] != sl$arr[s+i]){ # when there is a change of location
  
  if (sl$arr[s+i] != h) { c = c + 1 }# increase the trip counter, if we have not come home in this change of location
  
  if(sl$arr[s-1+i] == h) { # if coming from home
    trips$user_id[c] = u # assign user_id to trip
    trips$home_arr[c] = h # assign home to trip
    trips$dest[c] = sl$arr[s+i] # assign destination to trip
    trips$start.min[c] = sl$int_date[s+i] - (sl$weights[s+i] -1) # start was earliest
    trips$start.max[c] = sl$int_date[s+i]
  }
  
  if (sl$arr[s+i] == h) {# if we have come home here
    trips$end.min[c] = sl$int_date[s+i] - (sl$weights[s+i] -1) # end was earliest 
    trips$end.max[c] = sl$int_date[s+i] # end was latest
  }
  
# always with change of location
  if (sl$arr[s-1+i] != h & sl$arr[s+i] != h){
    trips$user_id[c] = u # assign user_id to trip
    trips$home_arr[c] = h # assign home to trip
    trips$dest[c] = sl$arr[s+i] # assign destination to trip
    trips$start.min[c] = sl$int_date[s+i] - (sl$weights[s+i] -1) # start was earliest 
    trips$start.max[c] = sl$int_date[s+i] # start was latest
    
    trips$end.min[c-1] = sl$int_date[s+i] - (sl$weights[s+i] -1) # end was earliest 
    trips$end.max[c-1] = sl$int_date[s+i] # end was latest
  } 
     
}  
}
cat(j)
}
head(trips)

if(!is.na(trips$user_id[10000])){
    stop("Error: trips dataframe ran out of space to hold more trips")
}

# histogram und so
trips$dur = (trips$end.min - trips$start.min)
hist(trips$dur, breaks = 100)










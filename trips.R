# for one user
sl = dataframe of arrs, weights, int_dates

sleepplace_1000 = sleepplace_1000[order(sleepplace_1000$user_id),]
sleepplace_1000$weights = diff(c(0,sleepplace_1000$int_date))


trips = data.frame(user_id = integer(), home_arr = integer(), dest = integer(), start.min = integer(), start.max = integer(), end.min = integer(), end.max = integer())
s = index of first home +1
e = index of last home

lapply(unique(sleepplace_1000$user_id), calc_trips)

trip_counter = 0
for (i in 1:(e-s)
if(arr[s+i] != arr[s-1+i]){
  
  if(trip_counter == 0) {
    trips$dest[i] = sl$arr[s]
    trips$start.min = sl$int_date[s] - (sl$weights -1)
    trips$start.max = sl$int_date[s] + sl$weights
  else{
    trips$dest[i] == sl$arr[s]
    trips$start.min[i+1] = sl$int_date[s] - (sl$weights -1)
    trips$start.max[i+1] = sl$int_date[s]
    
    trips$end.min[i] = sl$int_date[s] - (sl$weights -1)
    trips$end.max[i] = sl$int_date[s]
  }  
  if(arr[s-1+i] == trips$home){
    
  }
  }
  
}
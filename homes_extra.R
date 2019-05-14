# homes month 1-3

library(dplyr)
library(tibble)


load("sleepplace_aggr_dakar_months_1-3.Rda")

homes = sleepplace_aggr_dakar_full %>% group_by(user_id) %>% summarize (home = names(which.max(table(site_of_last_call)))) 

save(homes, file ="homes_month_1-3.Rda")
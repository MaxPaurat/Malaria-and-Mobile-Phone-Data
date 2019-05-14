# test
library(dplyr)
library(multidplyr)
# dplyr
airquality %>% group_by(Month) %>% summarize(cnt = n())

# multidplyr
airquality %>% partition(Month) %>% summarize(cnt = n()) %>% collect()

# learning spatial data manipulation
# https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf

x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap")
install.packages(x) 
lapply(x, library, character.only = TRUE)

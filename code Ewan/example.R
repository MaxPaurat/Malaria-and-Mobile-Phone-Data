### R script to demonstrate fitting of movement matrix model

setwd("C:/Users/maxpa/Documents/Malaria-and-Mobile-Phone-Data/code Ewan")

# Generate mock data
# start and end: look again
Nadmin <- 50
admin.relative.pops <- exp(rnorm(Nadmin))
admin.latlongs <- cbind(rnorm(Nadmin),rnorm(Nadmin))
admin.distMatrix <- as.matrix(dist(admin.latlongs,upper=TRUE,diag=TRUE))

Nind <- 1000
expNjourneys <- 4
expDjourneys <- 7
obs.data <- list()
for (i in 1:Nind) {
  Njourneys <- rpois(1,expNjourneys)
  home <- sample(1:Nadmin,1,prob=admin.relative.pops)
  if (Njourneys > 0) {
    journey.starts <- sample(1:364,Njourneys,replace=FALSE)
    journey.lengths <- rpois(Njourneys,expDjourneys)
    journey.ends <- journey.starts+journey.lengths
    journey.ends[journey.ends>365] <- 365
    journey.dests <- sample((1:Nadmin)[-home],Njourneys,prob=admin.relative.pops[-home]*admin.distMatrix[home,][-home]^0.5,replace=TRUE)
      if (Njourneys > 1) {
      for (j in 1:(Njourneys-1)) {
        if (journey.ends[j] > journey.starts[j+1]) {journey.ends[j] <- journey.starts[j+1]}
      }
    }
  for (j in 1:Njourneys) {
    obs.data[[length(obs.data)+1]] <- c(i,home,journey.dests[j],journey.starts[j],journey.ends[j])
  }
  }
}
obs.data <- do.call(rbind,obs.data)

# Construct Bayesian model in TMB : Ignoring temporal information for now
# install.packages("Matrix")
input.data <- list(
                  'Nadmin'=50,
                  'log_relPops'=log(admin.relative.pops),
                  'log_distMatrix'=log(admin.distMatrix+0.0000001),
                  'Nobs'=dim(obs.data)[1],
                  'journeyMatrix'=obs.data[,2:3]
              )

parameters <- list(
                   'distance_index'=0,
                   'pop_index'=0
              )
# install.packages("TMB")
library(TMB)
TMB::compile("movement_simple.cpp")
dyn.load(dynlib("movement_simple"))

obj <- MakeADFun(input.data,parameters,DLL='movement_simple')
opt <- nlminb(obj$par,obj$fn,obj$gr)
rep <- sdreport(obj)


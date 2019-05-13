#Max spatial data analysis

#raster data
#polygon data
#point data

library(malariaAtlas)
library(raster)

#load indicators
n_months = 12
months = formatC(1:n_months,width=2,format="d",flag="0")
file_month_names = vector("list", n_months)
for(k in 1:n_months) {
  infile <- paste("Data/Challenge\ Data/SET3/INDICATORS_SET3_M",months[k],".CSV", sep = "")
  # infile <- paste("Data/Challenge Data/SET3/SET3_M",months[k],".CSV", sep = "")
  
  file_month_names[[k]] = infile
  
}
data_indicators_complete = list()
n = 12
for(k in 1:n){
  
  temp = read.csv(file_month_names[[k]], header = FALSE, stringsAsFactors = FALSE)
  
  data_indicators_complete[[k]] = temp
  
  cat(k)
}
save(data_indicators_complete, file="data_indicators_complete.Rda")

data_indicators_complete_joined = do.call(rbind, data_indicators_complete)

#aggregate indicators per user 
data_indicators_av_per_user = 
  df = data_indicators_complete_joined
  n_ids = 146352
 df_matrix= matrix(as.numeric(unlist(df)), nrow=nrow(df))
 
 unique_id  =unique(df_matrix[,1])
 u2 =matrix(NA, nrow = length(unique_id), ncol= dim(df_matrix)[2])
for(i in 1:length(unique_id)){
  u2[i,] = colMeans(df_matrix[df_matrix[, 1] == unique_id[i], ])
  if(i %% 1000 == 0) print(i)
 # user_means = colMeans(df_matrix[i+((0:11)*n_ids),])
}
               
u2 = colMeans(df_matrix[df_matrix[, 1] == 480, ])
#assign average to arr where they live
# sleepplace_aggr_dakar_full are running on cluster right now to show sleepplace of all user_ids
# then just run the homes section in a separate file

#part Rohan
pop_data = read.csv("Data/Rohan Data/admin3_pops2.csv")
pop_data = pop_data[-3,]

SEN_case_data = read.csv("Data/Rohan Data/subnational_incidence.csv")
SEN_case_data = SEN_case_data[SEN_case_data$year == 2009,]
SEN_case_data = SEN_case_data[c("admin_unit_name","fdef_id","m_mean_pf","total_pop","admin_unit_level")]
SEN_case_data = SEN_case_data[SEN_case_data$admin_unit_level == "ADMIN3",]
SEN_case_data$year
#polygon data
SEN_shp <- getShp(ISO = "SEN", admin_level = "admin0")
SEN1_shp <- getShp(ISO = "SEN", admin_level = "admin1")
SEN2_shp <- getShp(ISO = "SEN", admin_level = "admin2")

SEN3_shp <- getShp(ISO = "SEN", admin_level = "admin3")
#plot polygon
plot(SEN3_shp, border = "blue")

plot(SEN2_shp,border="red")
plot(SEN1_shp,border="green",add=TRUE)
plot(SEN_shp,border="black",lwd=2,add=TRUE)
#raster data
SEN_PfPR2_10 <- getRaster(surface = "Plasmodium falciparum PR2-10",
                          shp = SEN3_shp, year = 2013)
#plot raster data
p <- autoplot_MAPraster(SEN_PfPR2_10, shp_df = SEN3_shp)
p
#how to save raster and open raster map
# writeRaster(SEN_PfPR2_10,file="PFPR2_10.tif")
# test<-raster("PFPR2_10.tif")
#getwd()

#get additional covariates
#Z:\MalariaEradicationMetrics\Senegal\Common files\Covariates
## Stack covariates 
folder='C:/Users/maxpa/Documents/Malaria-and-Mobile-Phone-Data/Data/MAP Covariates/'#name of the folder where to add the covariates
lsf=list.files(folder)
lsf=list.files(folder)[grep("*.tif$",(lsf))]
lsf=lsf[1:5]#to make things easier we keep only 5 objects
st<-stack(paste0(folder,lsf))
NAvalue(st)=-9999
#naming the covariates (be sure that the names correspond to the right covariates)
names(st)<-c("access","AI","distwater","elevation","EVImean")
st<-mask(st,SEN3_shp)#mask and crop covariates to study area
st<-crop(st,SEN3_shp)
plot(st[[1]])

#population weigthing


#extract raster data of prevalence into polygons
PRadm3 <- extract(SEN_PfPR2_10, SEN3_shp, fun=mean, na.rm=TRUE, sp = T)#sp (T: output is a spdf, F: output is vector)
head(PRadm3@data)
#extract stack of covariates into polygons
PRadm3 <- extract(st, PRadm3 , fun=mean, na.rm=TRUE, sp = T)#sp (T: output is a spdf, F: output is vector)
head(PRadm2@data)
PRadm3@data = PRadm3@data[-3,]

#plot mean of PR for each polygon
library(tmap)
qtm(shp = PRadm3, fill = "Plasmodium.falciparum.PR2.10.2013", fill.palette = "-Blues") # not shown
qtm(shp = PRadm3, fill = "access", fill.palette = "-Blues") # not shown
qtm(shp = PRadm3, fill = "elevation", fill.palette = "-Blues") # not shown

#other approach usingg ggplot2
# PRadm2df<-fortify(PRadm2,by="name_2")
# test.df <- join(PRadm2df, PRadm2@data, by="id")
# library(ggplot2)
# my_plot <- ggplot() +
# geom_polygon(data = PRadm2df, aes(long, lat, group = group, fill =Plasmodium.falciparum.PR2.10.2013))# +
# print(my_plot)

#point data (this is just for test, in reality use your response data)
randompt<-gCentroid(PRadm2,byid=TRUE)
#extract covariate values at point locations
covatresponse<-extract(st,randompt,sp=T)
covatresponsedf<-data.frame(covatresponse)
qtm(shp = covatresponse, fill = "elevation") # maybe not the best option to plot



# glm part

library(glmm)
install.packages("C:/Users/maxpa/Downloads/INLA_19.03.04.tar.gz", repos = NULL, type = "source")

fmla <- as.formula(paste("pop_data$number ~ ", paste(names(st), collapse= "+")))

g1 = glm(fmla, family = poisson, data = PRadm3@data[names(st)])

library(INLA)
g1_INLA = inla(fmla, "poisson", data = PRadm3@data[names(st)], E = pop_data$pops)

x = PRadm3@data[names(st)]
b = g1_INLA$summary.fixed$mean[2:6]
b0 = g1_INLA$summary.fixed$mean[1]

cases_pred = exp(b0 + b%*%t(x))*pop_data$pops
rate_pred = exp(b0 + b%*%t(x))

plot(pop_data$number, cases_pred)
plot(pop_data$rate, rate_pred)
abline(a=0, b=1)


# cross-validation
# leave one out
# and
# spatial/temporal leave-some-out
# 

# go for spatial model 
# read Inla 3.2 iCAR

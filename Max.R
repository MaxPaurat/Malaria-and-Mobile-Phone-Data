#Max spatial data analysis

#raster data
#polygon data
#point data

library(malariaAtlas)
library(raster)

#load indicators


#part Rohan
SEN_case_data = read.csv("Data/Rohan Data/subnational_incidence.csv")
SEN_case_data = SEN_case_data[SEN_case_data$year == 2009,]
SEN_case_data = SEN_case_data[c("admin_unit_name","fdef_id","m_mean_pf","total_pop","admin_unit_level")]
SEN_case_data = SEN_case_data[SEN_case_data$admin_unit_level == "ADMIN3",]
SEN_case_data$year
#polygon data
SEN_shp <- getShp(ISO = "SEN", admin_level = "admin0")
SEN1_shp <- getShp(ISO = "SEN", admin_level = "admin1")
SEN2_shp <- getShp(ISO = "SEN", admin_level = "admin2")
#plot polygon
plot(SEN2_shp,border="red")
plot(SEN1_shp,border="green",add=TRUE)
plot(SEN_shp,border="black",lwd=2,add=TRUE)
#raster data
SEN_PfPR2_10 <- getRaster(surface = "Plasmodium falciparum PR2-10",
                          shp = SEN2_shp, year = 2013)
#plot raster data
p <- autoplot_MAPraster(SEN_PfPR2_10, shp_df = SEN2_shp)
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
st<-mask(st,SEN2_shp)#mask and crop covariates to study area
st<-crop(st,SEN2_shp)
plot(st[[1]])

#extract raster data of prevalence into polygons
PRadm2 <- extract(SEN_PfPR2_10, SEN2_shp, fun=mean, na.rm=TRUE, sp = T)#sp (T: output is a spdf, F: output is vector)
head(PRadm2@data)
#extract stack of covariates into polygons
PRadm2 <- extract(st, PRadm2 , fun=mean, na.rm=TRUE, sp = T)#sp (T: output is a spdf, F: output is vector)
head(PRadm2@data)

#plot mean of PR for each polygon
library(tmap)
qtm(shp = PRadm2, fill = "Plasmodium.falciparum.PR2.10.2013", fill.palette = "-Blues") # not shown
qtm(shp = PRadm2, fill = "access", fill.palette = "-Blues") # not shown
qtm(shp = PRadm2, fill = "elevation", fill.palette = "-Blues") # not shown

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


# create antennasite area polygons 

voronoi_segmentation = function(){
# extract coordinates of antennas/sites
x = antennalocations$lon
y = antennalocations$lat

coords = data.frame(x,y)

#function to get Voronoi-SpatialPolygons from coordinates of antenna positions, confined by a big polygon (SpatialPolygonsDataframe)
voronoipolygons <- function(x,poly) {
  require(deldir)
  if (.hasSlot(x, 'coords')) {
    crds <- x@coords  
  } else crds <- x
  bb = bbox(poly)
  rw = as.numeric(t(bb))
  z <- deldir(crds[,1], crds[,2],rw=rw)
  w <- tile.list(z)
  w[1]
  polys <- vector(mode='list', length=length(w))
  require(sp)
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP <- SpatialPolygons(polys)
  
  voronoi <- SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1],
                                                          y=crds[,2], row.names=sapply(slot(SP, 'polygons'), 
                                                                                       function(x) slot(x, 'ID'))))
  
  return(voronoi)
  
}

#use function above
Voronoi_SPDF<-voronoipolygons(coords,SEN)
gg = gIntersection(SEN,Voronoi_SPDF,byid=TRUE)

# give default SPDF attributes
z = 1:1668
value_df= data.frame(z)

#combine to SpatialPolygonsDataFrame with values for each antenna area that can be mapped to coloures
s_poly <- SpatialPolygonsDataFrame(gg, value_df, match.ID = FALSE)

}

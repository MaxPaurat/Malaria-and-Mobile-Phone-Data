# Coloured map

# this script creates a map of senegal with Voronoi tesselation of antenna sites and
# colours the areas accourding to values variable

colouredmapfunct = function(Voronoi_SPDF,values){
  
  #plot with colour
  Voronoi_SPDF@data$z = example
  p = spplot(Voronoi_SPDF,"z")
  
  # #plot
  # p = p + layer(panel.points(SenegalHospitals$lon, SenegalHospitals$lat, col="green", pch=19), data=hospitallocations)
  print(p)
}
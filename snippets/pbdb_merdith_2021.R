# Paleobiology Database coords with Merdith et al 2021. plate model
# Internet connection is required!
# install.packages(c("rgplates", "httr2", "geojsonsf"))
library(rgplates)

# rgplates accessing the GPlates Web Service (70Ma reconstruction)
plates70 <- reconstruct("static_polygons", age=70, model="MERDITH2021")
modern70 <- reconstruct("coastlines", age=70, model="MERDITH2021")
# plotting downloaded data (with sf, WGS84 long-lat)
plot(plates70$geometry, border=NA, col="gray90")
plot(modern70$geometry, border=NA, col="gray60", add=TRUE)

# Maastrichtian Dinosaurs with PBDB API
pbdb <- read.csv(paste0(
  "https://paleobiodb.org/data1.2/occs/list.csv?",
  "base_name=Dinosauria&interval=Maastrichtian&show=coords"))
paleocoords<-reconstruct(
  pbdb[, c("lng", "lat")], age= 70, model="MERDITH2021")
points(paleocoords, pch=21, col="darkblue", bg="white", cex=0.5)


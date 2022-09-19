library(chronosphere)
library(rgplates)

model <- fetch("paleomap", "model")


# 5A. SF multipoint - no CRS assuming lng-lat
sfPoints <- st_as_sf(dmat, coords=c("lng", "lat"))
newCoord <- rgplates::reconstruct(sfPoints, model=model, age=140)
newCoord <- rgplates::reconstruct(sfPoints, model=model, age=140, plateperiod=TRUE)

# correctly changed crs to WGS 84

# 5B SF multiline WGS 84
admin <- fetch("paleomap", "adminlines")
adminSF <- st_as_sf(admin)

newLines <- rgplates::reconstruct(adminSF, model=model, age=140)

# retained original CRS?
st_crs(newLines)

# 5B 1 transformed CRS
adminSFmoll <- st_transform(adminSF, "ESRI:54009")
newLines <- rgplates::reconstruct(adminSFmoll, model=model, age=140)

# should be mollweide as adminSFmoll
st_crs(newLines)


# 5C - polygons WGS84
myers <- fetch("Myers-hotspots")
myersSF <- st_as_sf(myers)

# this should be a multi thing
newMulti <- rgplates::reconstruct(myersSF, model=model, age=140)

# no warnings!

# 5C2 crs change
myersSFmoll <- st_transform(myersSF, "ESRI:54009")
newMultiMoll <- rgplates::reconstruct(myersSFmoll, model=model, age=140)

st_crs(newMultiMoll)

# SPATIALS 
# 6. SpatialPoints - no CRS
library(sp)
spPoints <- SpatialPoints(dmat)

newCoord <- rgplates::reconstruct(spPoints, model=model, age=140)
newCoord <- rgplates::reconstruct(spPoints, model=model, age=140, plateperiod=TRUE)

# SpatialPointsDataFrame
spPointsDF <- SpatialPointsDataFrame(spPoints, data.frame(field=paste0("dataID_",1:nrow(dmat))))
newCoord <- rgplates::reconstruct(spPointsDF, model=model, age=140)
newCoord <- rgplates::reconstruct(spPointsDF, model=model, age=140, plateperiod=TRUE)

# 7. SpatialLines - WGS 84
newLines <- rgplates::reconstruct(admin, model=model, age=140)
plot(newLines)

# check match of  object type
expect_true(inherits(newLines, "Spatial"))

# 7 B. Spatial Lines - Moll
# changed crs
adminSf<- st_as_sf(admin, "sf")
adminMoll <- st_transform(adminSf, "ESRI:54009")
adminMoll <- as(adminMoll, "Spatial")

newLinesMoll <- rgplates::reconstruct(adminMoll, model=model, age=140)

# make sure that original crs is conserved!
crs(newLinesMoll)

# 8. SpatialPolygons - related - and multiple
newMulti <- rgplates::reconstruct(myers, model=model, age=140)

# should produce a warning!




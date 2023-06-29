library(tinytest)

archive <- file.path(
  system.file("extdata", package="rgplates"), 
  "paleomap_v3.zip")
# extract to temporary directory
unzip(archive, exdir=tempdir())
# path to the rotation file
rotPath <- file.path(tempdir(), "PALEOMAP_PlateModel.rot")
# path to the polygons
polPath <- file.path(tempdir(), "PALEOMAP_PlatePolygons.gpml")


######################################################

# old-style
model <- platemodel(rotation=rotPath, polygons=polPath)

platemodel(rotation=rotPath, features=c("static_polygons"=polPath))


rotPath <- "/mnt/sky/Dropbox/WorkSpace/2021-04-28_rgplates/data/SM2_X/1000_0_rotfile_Merdith_et_al.rot"
polPath <- "/mnt/sky/Dropbox/WorkSpace/2021-04-28_rgplates/data/SM2_X/shapes_static_polygons_Merdith_et_al.gpml"
contPath <- "/mnt/sky/Dropbox/WorkSpace/2021-04-28_rgplates/data/SM2_X/shapes_continents_Merdith_et_al.gpml"
coastPath <- "/mnt/sky/Dropbox/WorkSpace/2021-04-28_rgplates/data/SM2_X/shapes_coastlines_Merdith_et_al.gpml"


# old style
platemodel(rotation=rotPath, polygons=polPath)


# new style
model <- platemodel(rotation=rotPath, features=c("static_polygons"=polPath, "continents"=contPath, "coastlines"=coastPath))



## # MANUAL Testing
## # PALEOMAP - static_polygons -> good
## # PALEOMAP - points -> good

## # MERDITH2021 - static_polygons -> good
## # MERDITH2021 - coastlines -> good 
## # MERDITH2021 - continents -> good
## # MERDITH2021 - points ->

## london <- c(-0.38, 51.52)
## sydney<- c(151.17, -33.85)
## montreal<- c(-73.61, 45.52)

## # all cities in a single matrix
## x<- rbind(london, sydney, montreal)


##  x <- "static_polygons"
##  x <- "continents"
## x<- "coastlines"
## age <- 60
## path.gplates=NULL
## dir=NULL
## verbose=TRUE
## cleanup=TRUE
## plateperiod=FALSE
## gmeta=FALSE

## # new arg
## partitioning <- "static_polygons"

## # testing the reconstrutions
## getOS <- rgplates:::getOS
## testGPlates <- rgplates:::testGPlates
## randomString <- rgplates:::randomString

## fileFromPath <- function(x){
##   res <- rep(NA, length(x))
##   for(i in 1:length(x)){
## 	all <- unlist(strsplit(x[i], "/"))
## 	res[i] <- paste(all[length(all)], collapse="/")
##   }
##   names(res) <- names(x)
##   return(res)
## }

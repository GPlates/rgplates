# test whether correct errors are provided when incorrect files are given
library(tinytest)

setwd(wd)

archive <- file.path(
  system.file("extdata", package="rgplates"),
  "paleomap_v3.zip")

# extract to temporary directory
unzip(archive, exdir=tempdir())

# path to the rotation file
rotPath <- file.path(tempdir(), "PALEOMAP_PlateModel.rot")
# path to the polygons
polPath <- file.path(tempdir(), "PALEOMAP_PlatePolygons.gpml")

# load files
adminPath <- "data/Paleomap/Scotese PaleoAtlas_v3/PALEOMAP Global Plate Model/PALEOMAP_PoliticalBoundaries.gpml"

################################################################################
# 1. Character model
################################################################################

# Correct
expect_silent(model <- platemodel(rotation=rotPath, features=c("static_polygons"=polPath, "admin"=adminPath)))

# wrong rotaiton
rotPathWrong <- paste0(rotPath, "b")
expect_error(model <- platemodel(rotation=rotPathWrong, features=c("static_polygons"=polPath, "admin"=adminPath)))

# wrong feature collection
expect_error(model <- platemodel(rotation=rotPath, features=c("static_polygons"=paste0(polPath,0), "admin"=adminPath)))

################################################################################
# 2. Data.frame model
################################################################################

# Correct
feat=data.frame(
	feature_collection=c("static_polygons"=polPath, "admin"=adminPath),
	from=100,
	to=0
)

expect_silent(model <- platemodel(rotation=rotPath, features=feat))

# wrong rotation
expect_error(model <- platemodel(rotation=rotPathWrong, features=feat))

# wrong feature
featWrong=data.frame(
	feature_collection=c("static_polygons"=polPath, "admin"=paste0(adminPath,"b")),
	from=100,
	to=0
)

expect_error(model <- platemodel(rotation=rotPath, features=featWrong))

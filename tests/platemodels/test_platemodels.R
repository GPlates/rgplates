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

# load files
adminPath <- "/mnt/sky/Dropbox/WorkSpace/2021-04-28_rgplates/data/Paleomap/Scotese PaleoAtlas_v3/PALEOMAP Global Plate Model/PALEOMAP_PoliticalBoundaries.gpml"

######################################################

# old-style
expect_silent(model <- platemodel(rotation=rotPath, polygons=polPath))

# new style
expect_silent(model <- platemodel(rotation=rotPath, features=c("static_polygons"=polPath, "admin"=adminPath)))


rotPath <- "/mnt/sky/Dropbox/WorkSpace/2021-04-28_rgplates/data/SM2_X/1000_0_rotfile_Merdith_et_al.rot"
polPath <- "/mnt/sky/Dropbox/WorkSpace/2021-04-28_rgplates/data/SM2_X/shapes_static_polygons_Merdith_et_al.gpml"
contPath <- "/mnt/sky/Dropbox/WorkSpace/2021-04-28_rgplates/data/SM2_X/shapes_continents_Merdith_et_al.gpml"
coastPath <- "/mnt/sky/Dropbox/WorkSpace/2021-04-28_rgplates/data/SM2_X/shapes_coastlines_Merdith_et_al.gpml"


# old style
expect_silent(platemodel(rotation=rotPath, polygons=polPath))

# new style
expect_silent(model <- platemodel(rotation=rotPath, features=c("static_polygons"=polPath, "continents"=contPath, "coastlines"=coastPath)))




library(tinytest)
wd <- file.path(Sys.getenv("Dropbox"), "Software/rgplates")
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
adminPath <- "/mnt/sky/Dropbox/WorkSpace/2021-04-28_rgplates/data/Paleomap/Scotese PaleoAtlas_v3/PALEOMAP Global Plate Model/PALEOMAP_PoliticalBoundaries.gpml"

######################################################
# Default partitioning polygons (static_polygons)
######################################################
# new style platemodel definition
expect_silent(model <- platemodel(rotation=rotPath, features=c("static_polygons"=polPath, "admin"=adminPath)))

# Expected behavior is to reconstruct things properly

# example matrix
dmat <- as.data.frame(
	matrix(c(
	-27.44, 26.07,
	3.53, 25.44,
	16.53, 26.71,
	12.2, 45.01,
	-45.4, 43.12,
	24.58, 58.9,
	-30.53, 72.79,
	-29.29, -28.85
	), ncol=2, byrow=TRUE)
)

colnames(dmat) <-c("col1", "col2")
rownames(dmat) <-paste0("row", 1:nrow(dmat))

expect_silent(normal <- reconstruct(dmat, age=30, model=model))


######################################################
# Different name
######################################################

# new platemodel definition without the
expect_silent(diffmodel <- platemodel(rotation=rotPath, features=c("plates"=polPath, "admin"=adminPath)))

# the model should produces
expect_error(reconstruct(dmat, age=30, model=diffmodel))
expect_silent(again <- reconstruct(dmat, age=30, model=diffmodel, partitioning="plates"))

expect_equal(normal, again)

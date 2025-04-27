library(rgplates)
library(tinytest)

# wd from test main
setwd(wd)

# test data location
dir <- "data"

################################################################################
# Model definition
################################################################################

# based on a data.frame
features<-data.frame(
	feature_collection=c(
		file.path(dir, "CAO2024","static_polygons.gpmlz"),
		file.path(dir, "CAO2024","shapes_continents.gpmlz"),
		file.path(dir, "CAO2024","shapes_coasts.gpmlz"),
		file.path(dir, "CAO2024","250-0_plate_boundaries.gpml"),
		file.path(dir, "CAO2024","410-250_plate_boundaries.gpml"),
		file.path(dir, "CAO2024","1000-410_plate_boundaries.gpml"),
		file.path(dir, "CAO2024","1800-1000_plate_boundaries.gpml"),
		file.path(dir, "CAO2024","TopologyBuildingBlocks.gpml"),
		file.path(dir, "CAO2024","Paleomagnetic_poles.gpml")),
	from=c(1800, 1800, 1800, 250, 410, 1000, 1800, 1800, 1800),
	to=c(0, 0, 0, 0, 250, 410, 1000, 0, 0)
)
rownames(features) <- c(
	"static_polygons",
	"continents",
	"coastlines",
	"plate_boundaries_250",
	"plate_boundaries_410",
	"plate_boundaries_1000",
	"plate_boundaries_1800",
	"topology-building-blocks",
	"paleomagnetic-poles"
)

expect_silent(
	mod <- rgplates::platemodel(
		rotation = file.path(dir,"CAO2024","1800_0_rotfile.rot"),
		features = features
	)
)

################################################################################
# Reconstruction of basic feature collections
################################################################################
expect_silent(stat <- reconstruct("static_polygons", age=300, model=mod))
expect_true(inherits(stat, "sf"))
expect_silent(continents <- reconstruct("continents", age=300, model=mod))
expect_true(inherits(continents, "sf"))
expect_silent(coast <- reconstruct("coastlines", age=50, model=mod))
expect_true(inherits(coast, "sf"))
expect_silent(bounds <- reconstruct("plate_boundaries_410", age=300, model=mod))
expect_true(inherits(bounds, "sf"))

# example matrix
mat <- matrix(c(
  -27.44, 26.07,
  3.53, 25.44,
  16.53, 26.71,
  12.2, 45.01,
  -45.4, 43.12,
  24.58, 58.9,
  -30.53, 72.79,
  -29.29, -28.85
), ncol=2, byrow=TRUE)

# reconstruction of points
expect_silent(respoints <- reconstruct(mat, age=300, model=mod))
expect_true(inherits(respoints, "matrix"))

# known position of missing values
matMiss <- mat
matMiss[c(1, 5, 8), ] <- NA

expect_equivalent(!is.na(respoints), !is.na(matMiss))

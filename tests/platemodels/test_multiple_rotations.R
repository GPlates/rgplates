library(tinytest)
setwd(wd)

# based on a data.frame
features<-data.frame(
	feature_collection=c(
		file.path("data/CAO2024","static_polygons.gpmlz"),
		file.path("data/CAO2024","shapes_continents.gpmlz"),
		file.path("data/CAO2024","shapes_coasts.gpmlz"),
		file.path("data/CAO2024","250-0_plate_boundaries.gpml"),
		file.path("data/CAO2024","410-250_plate_boundaries.gpml"),
		file.path("data/CAO2024","1000-410_plate_boundaries.gpml"),
		file.path("data/CAO2024","TopologyBuildingBlocks.gpml"),
		file.path("data/CAO2024","Paleomagnetic_poles.gpml")),
	from=c(1800, 1800, 1800, 250, 410, 1000, 1800, 1800),
	to=c(0, 0, 0, 0, 250, 410, 0, 0)
)
rownames(features) <- c(
	"static_polygons",
	"continents",
	"coastlines",
	"plate_boundaries_250",
	"plate_boundaries_410",
	"plate_boundaries_1000",
	"topology-building-blocks",
	"paleomagnetic-poles"
)

expect_silent(
	mod <- rgplates::platemodel(
		rotation = file.path("data/CAO2024",c("1800_0_rotfile.rot","1800_1000_rotfile.rot")),
		features = features
	)
)

expect_silent(perhaps <- reconstruct("coastlines", age=50, model=mod))
plot(perhaps$geometry)


expect_silent(perhaps <- reconstruct("static_polygons", age=900, model=mod))
plot(perhaps$geometry)


expect_silent(perhaps <- reconstruct("static_polygons", age=950, model=mod))
plot(perhaps$geometry)

expect_silent(perhaps <- reconstruct("static_polygons", age=1200, model=mod))
plot(perhaps$geometry)

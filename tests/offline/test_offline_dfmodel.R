library(rgplates)

dir <- paste0(wd, "/data")

features<-data.frame(
	feature_collection=c(
		file.path(dir, "SM2_X","shapes_static_polygons_Merdith_et_al.gpml"),
		file.path(dir, "SM2_X","shapes_cratons_Merdith_et_al.gpml"),
		file.path(dir, "SM2_X","shapes_continents_Merdith_et_al.gpml"),
		file.path(dir, "SM2_X","shapes_coastlines_Merdith_et_al.gpml"),
		file.path(dir, "SM2_X","250-0_plate_boundaries_Merdith_et_al.gpml"),
		file.path(dir, "SM2_X","410-250_plate_boundaries_Merdith_et_al.gpml"),
		file.path(dir, "SM2_X","1000-410-Convergence_Merdith_et_al.gpml"),
		file.path(dir, "SM2_X","1000-410-Divergence_Merdith_et_al.gpml"),
		file.path(dir, "SM2_X","1000-410-Topologies_Merdith_et_al.gpml"),
		file.path(dir, "SM2_X","1000-410-Transforms_Merdith_et_al.gpml"),
		file.path(dir, "SM2_X","TopologyBuildingBlocks_Merdith_et_al.gpml"),
		file.path(dir, "SM2_X","1000-410_poles.gpml")),
	from=c(1000, 1000, 1000, 400, 250, 410, 1000, 1000, 1000, 1000, 1000, 1000),
	to=c(0, 0, 0, 0, 0, 250, 410, 410, 410, 410, 0, 410 )
)
rownames(features) <- c(
	"static_polygons",
	"cratons",
	"continents",
	"coastlines",
	"plate_boundaries_250",
	"plate_boundaries_410",
	"convergence",
	"divergence",
	"topologies",
	"transforms",
	"topology-building-blocks",
	"poles"
)

expect_silent(
	mod <- platemodel(
		rotation = file.path(dir,"SM2_X","1000_0_rotfile_Merdith_et_al.rot"),
		features = features
	)
)


# check for valid ages
expect_silent(valid <- reconstruct("static_polygons", age=30, model=mod))

# invalid ages
expect_error(reconstruct("static_polygons", age=-5, model=mod))
expect_error(reconstruct("static_polygons", age=1500, model=mod))

# non-existent features
expect_error(reconstruct("invalid", age=30, model=mod))

# suppress age checking
model2 <- mod
model2@features[ "static_polygons", "to"] <- 20
# expect error even with actually valid age
expect_error(reconstruct("static_polygons", age=10, model=model2))

# normal execution when checking is suppressed
expect_silent(valid <- reconstruct("static_polygons", age=10, model=model2, check=FALSE))

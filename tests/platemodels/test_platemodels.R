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




# complete model 
# based on characters
dir <- "data/"
expect_silent(
mod <- platemodel(
	rotation = file.path(dir,"SM2_X","1000_0_rotfile_Merdith_et_al.rot"),
	features = c(
		"static_polygons"= file.path(dir, "SM2_X","shapes_static_polygons_Merdith_et_al.gpml"),
		"cratons"= file.path(dir, "SM2_X","shapes_cratons_Merdith_et_al.gpml"),
		"continents"= file.path(dir, "SM2_X","shapes_continents_Merdith_et_al.gpml"),
		"coastlines"= file.path(dir, "SM2_X","shapes_coastlines_Merdith_et_al.gpml"),
		"plate_boundaries_250-0"= file.path(dir, "SM2_X","250-0_plate_boundaries_Merdith_et_al.gpml"),
		"plate_boundaries_410-250"= file.path(dir, "SM2_X","410-250_plate_boundaries_Merdith_et_al.gpml"),
		"convergence_1000-410"= file.path(dir, "SM2_X","1000-410-Convergence_Merdith_et_al.gpml"),
		"divergence_1000-410"= file.path(dir, "SM2_X","1000-410-Divergence_Merdith_et_al.gpml"),
		"topologies_1000-410"= file.path(dir, "SM2_X","1000-410-Topologies_Merdith_et_al.gpml"),
		"transforms_1000-410"= file.path(dir, "SM2_X","1000-410-Transforms_Merdith_et_al.gpml"),
		"topology-building-blocks"= file.path(dir, "SM2_X","TopologyBuildingBlocks_Merdith_et_al.gpml"),
		"poles"= file.path(dir, "SM2_X","1000-410_poles.gpml")
	),
	name="Merdith2021"
))


# based on a data.frame
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







# age =30
age <- 30
static_30 <- reconstruct("static_polygons", age=age, model=mod)
cratons_30 <- reconstruct("cratons", age=age, model=mod)
continents_30 <- reconstruct("continents", age=age, model=mod)
coastlines_30 <- reconstruct("coastlines", age=age, model=mod)
pb_30 <- reconstruct("plate_boundaries_250", age=age, model=mod)
topb_30 <- reconstruct("topology-building-blocks", age=age, model=mod)

plot(static_30$geometry, col="gray80")
plot(continents_30$geometry, col="gray70", add=TRUE)
plot(coastlines_30$geometry, col="gray40", add=TRUE)
plot(cratons_30$geometry, col="gray20", add=TRUE)
plot(pb_30$geometry, col="blue", add=TRUE)
plot(topb_30$geometry, col="green", add=TRUE)


# age 300
age <- 300

static_300 <- reconstruct("static_polygons", age=age, model=mod)
cratons_300 <- reconstruct("cratons", age=age, model=mod)
continents_300 <- reconstruct("continents", age=age, model=mod)
coastlines_300 <- reconstruct("coastlines", age=age, model=mod)
pb_300 <- reconstruct("plate_boundaries_410", age=age, model=mod)
topb_300 <- reconstruct("topology-building-blocks", age=age, model=mod)

plot(static_300$geometry, col="gray80")
plot(continents_300$geometry, col="gray70", add=TRUE)
plot(coastlines_300$geometry, col="gray40", add=TRUE)
plot(cratons_300$geometry, col="gray20", add=TRUE)
plot(topb_300$geometry, col="green", add=TRUE)


## plot(pb_300$geometry, col="blue", add=TRUE)
## for(i in 1:nrow(pb_300)){
## 	plot(pb_300[i,]$geometry, col=i, add=TRUE)
## }

## all <- list()
## for(i in 1:nrow(pb_300)){
	
## 	all[[i]] <- pb_300[i,]$geometry[[1]][[1]]
## }
## all <- c(all, list(mapedge()[[1]][[1]]))
## a<- st_multilinestring(all)
## b <- st_node(a)

## for(i in 1:length(b)){
## 	plot(st_multilinestring(b[i]), col=sample(rainbow(255),1), add=TRUE, lwd=2)
## }
## pol <- st_polygonize(b)

## for(i in 1:length(pol)){
## 	plot(pol[[i]], col=sample(rainbow(255),1), add=TRUE, lwd=2)
## }


# age 500
age <- 500
static_500 <- reconstruct("static_polygons", age=age, model=mod)
cratons_500 <- reconstruct("cratons", age=age, model=mod)
continents_500 <- reconstruct("continents", age=age, model=mod)
# coastlines_500 <- reconstruct("coastlines_400-0", age=age, model=mod)
conv_500 <- reconstruct("convergence", age=age, model=mod)
div_500 <- reconstruct("divergence", age=age, model=mod)
# top_500 <- reconstruct("topologies", age=age, model=mod)
trans_500 <- reconstruct("transforms", age=age, model=mod)
poles_500 <- reconstruct("poles", age=age, model=mod)
topb_500 <- reconstruct("topology-building-blocks", age=age, model=mod)

plot(static_500$geometry, col="gray80")
plot(continents_500$geometry, col="gray70", add=TRUE)
#plot(coastlines_500$geometry, col="gray40", add=TRUE)
plot(cratons_500$geometry, col="gray20", add=TRUE)
plot(div_500$geometry, col="blue", add=TRUE)
plot(conv_500$geometry, col="purple", add=TRUE)
plot(trans_500$geometry, col="orange", add=TRUE)
plot(topb_500$geometry, col="green", add=TRUE)
plot(poles_500$geometry, col="cyan", add=TRUE)
# plot(top_500$geometry, col="yellow", add=TRUE)


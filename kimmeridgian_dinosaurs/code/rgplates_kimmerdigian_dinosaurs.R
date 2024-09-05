# Kimmerigian dinosaurs on the PALEOMAP temperature reconstructions
# Example use of rgplates that serves illustrative purposes.
# This example was written for rgplates v0.5.0.
# The script requires the GPlates Desktop Application
# to be installed on the system.
#
# Ádám T. Kocsis, 2024-08-30

# The script uses lots of data that need to be downloaded from the web.
# The first parts are downloaded from the chronosphere (https:://chronosphere.info/r_client/)
# which is the original parent project of rgplates, and is providing
# data items that are stably deposited on scientific repositories.
# The second kind of data are accessed from services that yield continuously changing data.
# For the sake of the figure's reproducibility, these are saved as a temporary cache file.
# For the unstable data should the cached versions be used (cached=TRUE)? Otherwise
# these will be re-downloaded from the web, which might result in a different result.
cached <- TRUE

# By default the image file will be written into the TEMPORARY DIRECTORY.
# If you want to write the image somewhere else, please set its path here:
filepath <- NULL
# filepath <- "temperature_example.png" # into the current directoy
if(is.null(filepath)){
	td <- tempdir()
	filepath <- file.path(td, "temperature_example.png")
}

message(paste0("The result image will be written as:\n", filepath))

################################################################################
# Required packages (originally used version in parens)

# All available from the CRAN: install.packages(c("rgplates" ,"terra"))
library(rgplates) # (0.5.0) - will attach sf (1.0-16)
library(terra) # for raster processing (1.7-78)
library(ncdf4) # raster input (1.22)
library(via) # raster and sf organization (0.2.0)
library(chronosphere) # data download (0.6.1)
library(rphylopic) # phylopic download (1.4.0)
library(shape) # arrow geomeries (1.4.6.1)
library(png) # reading logos images (0.1-8)

################################################################################

# downloading data from the chronosphere
# tectonic model files
mod <- chronosphere::fetch(src="paleomap", ser="model", ver="v19o_r1c")

# temperature reconstruction
gmst <- chronosphere::fetch(src="paleomap", ser="gmst", ver="scotese02a_v21321")

# paleocoastlines reconstruction
pc <- chronosphere::fetch(src="paleomap", ser="paleocoastlines", ver="7")

# target age
age <- 150

# reconstruction of the static_polygons
polys <- rgplates::reconstruct("static_polygons", age=age, model=mod)

# select the appropirate time (also resample raster to avoid warnings
# because of the grid registration)
ctemp<- terra::resample(gmst[as.character(age)], terra::rast())
ccoast<- pc[as.character(age), "coast"]

# transfer everything to mollweide
proj <- "ESRI:54009"
mollTemp <- terra::project(ctemp, proj)
mollPolys<- sf::st_transform(polys, proj)
mollEdge <- sf::st_transform(mapedge(), proj)
mollCoast <- sf::st_transform(ccoast,proj)

# all the other assets are cached on GitHub
if(cached){
	load(url("https://github.com/GPlates/rgplates/raw/devel/pkgdown/assets/kimmeridgian_dinosaurs/data/cache_2024-08-24.RData"))

}else{
	# Dowload occurrences from PBDB
	ornithischia <- read.csv(paste0(
		"https://paleobiodb.org/data1.2/occs/list.csv?",
		"base_name=Ornithischia&interval=Kimmeridgian&show=coords"))

	# Dowload occurrences from PBDB
	saurischia <- read.csv(paste0(
		"https://paleobiodb.org/data1.2/occs/list.csv?",
		"base_name=Saurischia&interval=Kimmeridgian&show=coords"))

	# get the first images
	# images with rphylopic
	saurImage<- rphylopic::get_phylopic(get_uuid(name = "Saurischia", n = 1))
	ornithImage<- rphylopic::get_phylopic(get_uuid(name = "Ornithischia", n = 1))

	# Logos
	# read in the logos, and their proprtions in pixels
	# these are on github
	# Inspect these before downloading!
	location <- "https://github.com/GPlates/rgplates/raw/devel/pkgdown/assets/kimmeridgian_dinosaurs/data/images/"

	# logo files
	files <- c(
		rgplates="rgplates.png",
		pbdb="PBDB2014small.png",
		bridge="bridge.png")

	# temporary directory
	td <- tempdir()
	targets <- file.path(td, files)
	names(targets) <- names(files)

	# download them into the temporary directory
	for(i in 1:length(files)){
		download.file(
			paste0(location, files[i]), # what is downloaded
			targets[i]) # where? In the temporary directory
	}

	# read in the rgplates logo file
	rgplatesLogo <- png::readPNG(targets['rgplates'])
	rgplatesProp <- 775/895

	# Paleobiology Database
	pbdbLogo <- png::readPNG(targets['pbdb'])
	pbdbProp <-900/759

	# BRDIGE
	bridgeLogo <- png::readPNG(targets['bridge'])
	bridgeProp <-732/400

}

#' Get the coordinates to be plotted on the map
#'
#' Subsets data to collections, reconstructs paleocoordinates and transfers them to a given projection
#'
#' @param pbdb The Paleobiology Database Download
#' @param model The model object to be passed to rgplates::reconstruct()
#' @param age The target reconstruction age-> also to be passed to rgplates::reconstruct()
#' @param crs epsg.io id for the projection to return
#' @return An sf points object
PlotCoords <- function(pbdb, model, age, crs=proj){
	require(rgplates)
	require(sf)

	# get the collection data from the PBDB
	colls <- unique(pbdb[, c("collection_no", "lng", "lat")])

	# paleocoordinate reconstruction
	paleocoords<-rgplates::reconstruct(
		colls[, c("lng", "lat")], age= age, model=model)

	# Omit missing values, if there are no coordinates, because:
	# - coordinates are off the plates
	# - the target age is beyond the valid time of the plates
	paleocoords <- na.omit(paleocoords)

	# make an sf object from the paleocoordinates for CRS transformation
	sfObj <- sf::st_as_sf(as.data.frame(paleocoords), coords=c("paleolong", "paleolat"))
	st_crs(sfObj) <- "WGS84" # paleocoordinates are long-lat

	# CRS transformation
	transObj <- sf::st_transform(sfObj, crs )

	# result
	return(transObj)

}


# Calculate the coordinates
saurCoords <- PlotCoords(saurischia, model=mod, age=age)
ornithCoords <- PlotCoords(ornithischia, model=mod, age=age)

########################################----------------------------------------
# define colors
# custom color ramp
gradinv <- colorRampPalette(c("#33358a", "#76acce", "#fff99a",  "#e22c28", "#690720"))

# groups
saurCol <- "#8610cc"
ornithCol <- "#00AA00"

# start writing image file
png(filepath, width=4000, height=2000, pointsize=48)

	# plot temperature
	plot(mollTemp, axes=F, box=F, plg=list(x="bottom"), col=gradinv(256)[1:230], range=c(-15, 37))
	# outline of plates
	plot(mollPolys$geometry, col=NA, border="#aaaaaa88", add=TRUE, lwd=6, axes=F, box=F)
	# outline of landmasses
	plot(mollCoast, col="#00000044", border=NA, add=TRUE, axes=F, box=F)
	# edge of the map to cover the pixelation
	plot(mollEdge, col=NA, border="white", lwd=7, add=TRUE)
	# the Coordinates
	points(saurCoords, pch=21, col="#FFFFFF88", bg=paste0(saurCol, "55"), cex=2)
	points(ornithCoords, pch=22, col="#FFFFFF88", bg=paste0(ornithCol, "55"), cex=2)

	# reference positions for the images
	yCenter <- 7500000
	xCenter <- 16500000

	par(xpd=TRUE)
	# Captions
	text(x=xCenter*0.995, y=yCenter*0.78, label="Saurischia", cex=1.35)
	text(x=-xCenter*0.995, y=yCenter*0.78, label="Ornithischia", cex=1.35)

	# the big legend points
	points(x = xCenter*0.94, y = yCenter*0.94, pch=21, col="#FFFFFF88", bg=paste0(saurCol, "88"),  cex=5)
	points(x = -xCenter*0.94, y = yCenter*0.93, pch=22, col="#FFFFFF88", bg=paste0(ornithCol, "88"), cex=5)
	points(x = xCenter*0.94, y = yCenter*0.94, pch=21, col="#FFFFFF88", bg=paste0(saurCol, "00"),  cex=4.7)
	points(x = -xCenter*0.94, y = yCenter*0.93, pch=22, col="#FFFFFF88", bg=paste0(ornithCol, "00"), cex=4.7)

	# the silhouettes
	rphylopic::add_phylopic_base(img = saurImage, x = xCenter, y = yCenter, ysize = 2000000, fill="black")
	rphylopic::add_phylopic_base(img = ornithImage, x = -xCenter, y = yCenter, ysize = 2000000, fill="black")

	# arrows
	# Saurischians
	shape::Arrows(x1=10800000, y1=3700000, x0=xCenter*0.89, y0=yCenter*0.94, col="black", lwd=10, arr.length=1.5, arr.type="triangle")
	shape::Arrows(x1=-6500000, y1=4500000, x0=-xCenter*0.9, y0=yCenter*0.93, col="black", lwd=10, arr.length=1.5, arr.type="triangle")

	# Ornithischians
	shape::Arrows(x1=10800000, y1=3700000, x0=xCenter*0.89, y0=yCenter*0.94, col="white", lwd=6, arr.length=1.5, arr.type="triangle")
	shape::Arrows(x1=-6500000, y1=4500000, x0=-xCenter*0.9, y0=yCenter*0.93, col="white", lwd=6, arr.length=1.5, arr.type="triangle")

	# model and reconstruction labels
	text(x=-18000000, y=-yCenter*1.03, label=expression(bold("PALEOMAP")~"model"), cex=1.4, adj=0)
	text(x=-18000000, y=-yCenter*1.18, label="Kimmeridgian (150 Ma)", cex=1.4, adj=0)


	# heatmap legend
	text(x=18000000, y=-yCenter*1.25, label=expression(italic("Annual Average Air Surface Temperature ("*degree*"C)")), cex=1, adj=1)

	# adding the logos
	# rgplates
	size <- 1800000
	xCenter2 <- xCenter*0.98+size/2
	yCenter2 <- -yCenter*0.83+size/2/rgplatesProp

	rasterImage(rgplatesLogo,xleft=xCenter2-size/2,xright=xCenter2+size/2,
		ybottom=yCenter2-size/rgplatesProp/2, ytop=yCenter2+size/rgplatesProp/2)

	# PBDB
	size <- size * 1.3
	yCenter3 <- -yCenter*0.98
	rasterImage(pbdbLogo,xleft=xCenter2-size/2,xright=xCenter2+size/2,
		ybottom=yCenter3-size/pbdbProp/2, ytop=yCenter3+size/pbdbProp/2)

	# BRIDGE
	yCenter4 <- -yCenter
	xCenter3 <- xCenter2*0.85
	rasterImage(bridgeLogo,xleft=xCenter3-size/2,xright=xCenter3+size/2,
		ybottom=yCenter4-size/bridgeProp/2, ytop=yCenter4+size/bridgeProp/2)

dev.off()


message(paste0("File was written as:\n", filepath))

###########################################################################
# GPlates Web Service internals:

# IteratedPointReconstruction <- function(coords,age, chunk=200, model="MERDITH2021", reverse=FALSE, verbose=TRUE){


# Reconstruct points
# 
# Reconstruct the geographic locations from present day coordinates back to their paleo-positions. 
# Each location will be assigned a plate id and moved back in time using the chosen reconstruction model.
# 
# Adapted from GPlates Web Service (need to find out how to reference them)
# 
# @param coords are the coordinates to be reconstructed. Can be a vector with longitude and latitude representing
# a single point or a matrix/dataframe with the first column as longitude and second column as latitude
# @param age is the age in Ma at which the points will be reconstructed
# @param	model is the reconstruction model. The default is "MERDITH2021". Add more details about additional models here
# @param reverse the flag to control the direction of reconstruction. If reverse = TRUE, the function will 
# calculate the present-day coordinates of the given paleo-coordinates.
# @param verbose Should the function output urls?
#
# @return matrix with longitude and latitude	
# 
# @examples
# gplates_reconstruct_points(c(95, 54), 140)
# 
# xy <-cbind(long=c(95,142), lat=c(54, -33))
# gplates_reconstruct_points(xy, 140)
## gplates_reconstruct_points <- function(coords,age, model="MERDITH2021", reverse=FALSE, verbose=TRUE){
	
## 	url <- 'https://gws.gplates.org/reconstruct/reconstruct_points/'
	
## 	#multiple points, as matrix or dataframe
## 	if(is.matrix(coords) | is.data.frame(coords)){
## 		coords <- toString(as.vector(t(coords)))
## 	}
	
## 	#single points as vector
## 	if(is.vector(coords)){ 
## 		coords <- toString(coords)
## 	}
	
## 	#fetch data
## 	query <- sprintf('?points=%s&time=%d&model=%s',gsub(" ", "", coords),age, model)
	
## 	# for reconstruction of present day coordinates from paleocoordinates
## 	if (reverse == TRUE){
## 		query <- paste0(query, "&reverse")
## 		cols <- c("long", "lat")
## 	} else cols <- c("paleolong", "paleolat")
	
## 	fullrequest <- sprintf(paste0(url,query, "&return_null_points"))
	
## 	if(verbose) cat("Extracting coordinates from:", fullrequest, "\n")
## 	rawdata <- readLines(fullrequest, warn="F") 
	
## 	#if null
## 	rawdata <- gsub("null", "[[-9999, -9999]]", rawdata)
	
## 	#extract coordinates
## 	rcoords <- matrix(as.numeric(unlist(regmatches(rawdata, gregexpr("-?[[:digit:]]+\\.*[[:digit:]]+", rawdata)))), ncol=2, byrow = TRUE)
## 	rcoords[rcoords == -9999] <- NA #replace na values
	
## 	colnames(rcoords) <- cols
## 	return(rcoords)
## }


# New point reconstruction function
gwsReconstructPoints <- function(coords,time, model="MERDITH2021", reverse=FALSE, verbose=TRUE){
	
	# Check whether the suggested package are there
	checkSuggested(c("geojsonsf", "httr2"))

	# define a request to the GPlates web service
	re <- httr2::request("https://gws.gplates.org/reconstruct/reconstruct_points/")
	

	# the form request
	reForm <- httr2::req_body_form(
		re, 
		lats=paste(coords[,2], collapse=","), 
		lons=paste(coords[,1], collapse=","), 
		time=time, model=model, reverse=reverse
	)

	if(verbose) message(paste0("Defined request to reconstruction points to time: ", time, "Ma, reverse=", reverse, "." ))

	# the performed request
	done <- httr2::req_perform(reForm)

	if(verbose) message("Request performed. ")

	# process request
	result<- httr2::resp_body_string(done)

	# return the geojsonsf
	newsf <- geojsonsf::geojson_sf(result)

	if(verbose) message("Processed result to GeoJSON. ")

	# the column names 
	rcoords <- sf::st_coordinates(newsf)[, c("X", "Y"), drop=FALSE]

	if(verbose) message("Extracted coordinates. ")

	# the column names
	if (reverse){
		cols <- c("long", "lat")

		# during reverse reconstruction if the past positions are not assigned to plates
		# then he web service returns IDENTICAL COORDINATES as teh original ones
		exactMatch <- rcoords==c(coords[,1], coords[,2])
		if(any(exactMatch)){
			rcoords[exactMatch] <- NA
			warning("Identical coordinates returned as present-day positions: \n  - Some points are probably off the partitioning polygons.\n  - Returning NAs for these.")
		}
		
	} else {
		cols <- c("paleolong", "paleolat")
	}
	colnames(rcoords) <- cols

	# replace NA values
	rcoords[rcoords == 999.99] <- NA

	
	# return object
	return(rcoords)
}


# Reconstruct coastlines
# Retrieve reconstructed coastline polygons for defined ages
# 
# @param age is the age in Ma at which the points will be reconstructed
# @param model is the reconstruction model. The default is "PALEOMAP". Add more details about additional models here
# @param verbose Should the function output urls?
# @return SpatialPolygonsDataFrame
gplates_reconstruct_this <- function(age, this, model="MERDITH2021", verbose=TRUE){
	if(any(age%%1!=0)){
		message("Only integer ages are supported by the online method.\nRounding target age(s).")	
		age <- round(age)
	}
	if(! requireNamespace("geojsonsf", quietly=TRUE)) stop("This method requires the 'geojsonsf' package to run.")
	
	if (this=="plate_polygons"){
		this <- "topology/plate_polygons"
	}else{
		this <- paste0("reconstruct/", this)
	}
	#download and save data
	url <- paste0('http://gws.gplates.org/', this, '/')
	query <- sprintf('?time=%d&model=%s', age, model)
	
	fullrequest <- sprintf(paste0(url,query))
	if(verbose) cat("Getting data from:", fullrequest, "\n")

	r <- readLines(fullrequest, warn=FALSE)
	
	#read data
	dat <- geojsonsf::geojson_sf(r)
	
	return(dat)
}

## #  reconstructing polygons
## #  
## #  @param sp is a SpatialPolygonsDataFrame
## #  @param verbose Should the function output urls?
## #  
## gplates_reconstruct_polygon <- function(sp, age, model="PALEOMAP", verbose=TRUE){
## 	if(! requireNamespace("geojsonsf", quietly=TRUE)) stop("This method requires the 'geojsonsf' package to run.")
## 	if(! requireNamespace("sp", quietly=TRUE)) stop("This method requires the 'sp' package to run.")
	
## 	# the target url
## 	url = 'https://gws.gplates.org/reconstruct/reconstruct_feature_collection/'
	
## 	#extract coordinates
## 	polys = attr(sp,'polygons')
## 	npolys = length(polys)
## 	for (i in 1:npolys){
## 		poly = polys[[i]]
## 		polys2 = attr(poly,'Polygons')
## 		npolys2 = length(polys2)
## 		for (j in 1:npolys2){
## 			#do stuff with these values
## 			coords = sp::coordinates(polys2[[j]])
## 		}
## 	}
	
## 	js <- paste(apply(coords, 1, function (x) paste(x, collapse=",")), collapse="],[")
	
## 	fullrequest = sprintf('%s?feature_collection={"type":"FeatureCollection","features":[{"type":"Feature","geometry":{"type":"Polygon","coordinates":[[[%s]]]},"properties":{}}]}&time=%d&model=%s',url,js, age, model)
## 	if(verbose) cat("Reconstructing polygon from:",	fullrequest, "\n")
## 	rawdata <- readLines(fullrequest, warn="F") 
	
## 	rpoly <- rgdal::readOGR(rawdata, "OGRGeoJSON", verbose = FALSE)
	
## 	return(rpoly)
## }

CheckGWS <- function(x, model, age, verbose=TRUE){
	# load the relevant data
	if(verbose){
		message("Checking validity of entries for GWS.")
	}
	gws <- NULL
	# use lazy loading to get it into the memory
	data(gws, envir=environment(), package="rgplates")

	# limit to model
	gwsMod <- gws[which(gws$model==model), ]

	if(nrow(gwsMod)==0) stop("The selected model is not a registered output of theof the GPlates Web Service.")

	# limit to feature
	feat <- gwsMod[which(gwsMod$feature==x), ]
	if(nrow(feat)==0) stop(paste0("'", x, "' is not returned for model '", model, "'."))

	# check whether it is the right range
	if(!(age <= feat$from & age >= feat$to)) stop(paste0("The model '", model, "' has a valid age range of ", feat$from, " Ma to ", feat$to, " Ma. "))

}

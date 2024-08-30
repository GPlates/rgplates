gwsURL <- "https://gws.gplates.org/"
defaultGwsURL <- gwsURL 

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
gwsReconstructPoints <- function(coords,time, model="MERDITH2021", reverse=FALSE, verbose=TRUE, warn=TRUE, anchor=0, validtime=TRUE){

	
	# Check whether the suggested package are there
	checkSuggested(c("geojsonsf", "httr2"))

	# define a request to the GPlates web service
	re <- httr2::request(paste0(gwsURL, "reconstruct/reconstruct_points/"))
	
	# the form request
	reForm <- httr2::req_body_form(
		re, 
		lats=paste(coords[,2], collapse=","), 
		lons=paste(coords[,1], collapse=","), 
		time=time, model=model, reverse=reverse, anchor_plate_id=anchor
		, ignore_valid_time=!validtime
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
		# then he web service returns IDENTICAL COORDINATES as the original ones to 4 decimal places!!
		exactMatch <- round(rcoords,4)==round(c(coords[,1], coords[,2]), 4)
		if(any(exactMatch)){
			rcoords[exactMatch] <- NA
			if(warn) warning("Identical coordinates returned as present-day positions (4 digits): \n  - Some points are probably off the partitioning polygons.\n  - Returning NAs for these.")
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
gplates_reconstruct_this <- function(age, this, model="MERDITH2021", verbose=TRUE, anchor=0){
	
	if(! requireNamespace("geojsonsf", quietly=TRUE)) stop("This method requires the 'geojsonsf' package to run.")
	
	if (this=="plate_polygons"){
		this <- "topology/plate_polygons"
	}else{
		this <- paste0("reconstruct/", this)
	}
	#download and save data
	url <- paste0(gwsURL, this, '/')
	query <- sprintf('?time=%f&model=%s&anchor_plate_id=%d', age, model, anchor)
	
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

#' Return and set the remote URL for the GPlates Web Service
#'
#' This set of functions allows the configuration of the remote URL, so the R client package can be used with a different instance of the GPlates web service, including a local implementation (served on localhost).
#'
#' The \code{getws} function returns the current url of the GPLates Web Service (defaults to: \code{https://gws.gplates.org/}).
#' The \code{setws} function allows the setting of GPLates Web Service URL.
#' @param url (\code{character}) A single string specifying the URL of the GPlates Web Service (with trailing slash).
#' @param check (\code{logical}) Flag to specify whether the immediate querying of the GWS is to be performed? If this fails the url won't be set!
#' @param reset (\code{logical}) Flag to specify whether the factory default should be reset. 
#' @rdname gwstools 
#' @return \code{getws} returns a single character string with the URL of the GWS. 
#' @examples
#' # Access currently set remote URL.
#' getgws()
#' # In case you have the GWS running on localhost (default port 18000):
#' # At time of writing this, the local instance does not return version, checking
#' # does not work!
#' setgws("http://localhost:18000/", check=FALSE)
#' # To reset factory defaults
#' setgws(reset=TRUE, check=FALSE)
#' @export 
getgws<- function(){
	return(getFromNamespace("gwsURL", ns="rgplates"))
}

#' @rdname gwstools
#' @export 
setgws <- function(url="", check=TRUE, reset=FALSE, silent=FALSE){
	# the original setting
	current <- getgws()
	if(!reset){
		# set therplags
		assignInNamespace("gwsURL", url, ns="rgplates")
	}else{
		assignInNamespace("gwsURL", defaultGwsURL, ns="rgplates")
	}

	if(check){
		ver <- checkgws(silent=silent)
		# set it back if there was an error!
		if(is.na(ver)) assignInNamespace("gwsURL", current, ns="rgplates")
	}
}

#' Ping the linked instance of the GPlates Web Service
#'
#' The function will use the http get method to access the version number of the GPlates Web Service.
#' 
#' @return Invisible return, either FALSE, or a character string with the version number. 
#' @param silent Logical flag indicating wheth the output should be silent?
#' @rdname gwstools 
#' @export
checkgws <- function(silent=FALSE){

	# construct version access URL
	url <- paste0(gwsURL, "version")

	# default success
	version <- NA 
	try({
		# try to acces the URL with the version
		suppressWarnings(version <- readLines(url, warn=FALSE))
		if(!silent) message(paste0("Successfully connected to GPlates Web Service ", version, " at\n'", gwsURL, "'."))
	}, silent=TRUE)
	if(is.na(version) & !silent){
		warning(paste0("The GPlates Web Service could not be reached at\n'", gwsURL, "'."))
	}

	# return success state 
	invisible(version)
}

# Workhorse function to get the online reconstruction method's velocity data
#
# @param age The target age
# @param model The model name string
# @param domain The domain argument, structure of velocity field
# @param type format of the velocities
# @param A data.frame containng the velocity information
gwsVelocities <- function(age, model="MERDITH2021", domain="longLatGrid", type="MagAzim", verbose=FALSE){

	if(! requireNamespace("geojsonsf", quietly=TRUE)) stop("This method requires the 'geojsonsf' package to run.")

	# we need this to check what kind of plates are there
	data(gws, envir=environment(), package="rgplates")

	# what plates are available?
	currentModel <- gws[which(gws$model==model), ]

	# if there are plate_polygons, that has to be used, otherwise it has to static_polygons
	topological <- as.logical(sum(grepl("plate_polygons", currentModel$feature)))

	#check domain, type and output

	# depending on the type
	if (topological){
		this <- "velocity/plate_polygons"
	}else{
		this <- "velocity/static_polygons"
	}
	#download and save data
	url <- paste0(gwsURL, this, '/')
	query <- sprintf('?time=%f&model=%s&velocity_type=%s&domain_type=%s', age, model, type, domain)

	fullrequest <- sprintf(paste0(url,query))
	if(verbose) cat("Getting data from:", fullrequest, "\n")

	# read in the json-like Numpy Array
	r <- readLines(fullrequest, warn=FALSE)

	#read data
	dat<- ParseVeloJSON(r, type=type)
	
	return(dat)

}

###########################################################################
# GPlates Web Service internals:

# correcting the point recontstruction problem, wrapper around the point reconstruction funciton
IteratedPointReconstruction <- function(coords,age, chunk=200, model="PALEOMAP", reverse=FALSE, verbose=TRUE){
	if(any(age%%1!=0)){
		message("Only integer ages are supported by the online method.\nRounding target age(s).")	
		age <- round(age)
	}
	# number of coordinates
	coordNum <- nrow(coords)
	
	# do only when the number of coordinates is large enough
	if(coordNum>chunk){
		# batch number
		moreIndex <- rep(1:ceiling(coordNum/chunk), each=chunk)
		index<-moreIndex[1:coordNum]
		
		# new container
		newCoords <- matrix(NA, ncol=2, nrow=coordNum)
		colnames(newCoords) <- colnames(coords)
		rownames(newCoords) <- rownames(coords)

		# the number of batches 
		maxIndex <- max(index)
	 
		# iterate - for() is easier, performance impact unlikely
		for(i in 1:maxIndex){
			# index of batch number
			bIndex <- index==i
	
			# current batch 
			current <- coords[bIndex,]

			# do the reconstruction
			tryCatch({
					iterRes <- gplates_reconstruct_points(current, age=age, model=model, reverse=reverse, verbose=verbose)
				},
				error=function(cond){
					stop("Query URL is too long. Round coordinates or decrease chunk size.")
				}
			) 

			
			# store
			newCoords[bIndex,] <- iterRes
		}

	# save some time by skipping this
	}else{
		tryCatch({
			newCoords <- gplates_reconstruct_points(coords, age=age, model=model, reverse=reverse, verbose=verbose)
		}, error=function(cond){
					stop("Query URL is too long. Round coordinates or decrease chunk size.")
			 }
		)

	}

	return(newCoords)

}



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
# @param	model is the reconstruction model. The default is "PALEOMAP". Add more details about additional models here
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
gplates_reconstruct_points <- function(coords,age, model="PALEOMAP", reverse=FALSE, verbose=TRUE){
	
	url <- 'https://gws.gplates.org/reconstruct/reconstruct_points/'
	
	#multiple points, as matrix or dataframe
	if(is.matrix(coords) | is.data.frame(coords)){
		coords <- toString(as.vector(t(coords)))
	}
	
	#single points as vector
	if(is.vector(coords)){ 
		coords <- toString(coords)
	}
	
	#fetch data
	query <- sprintf('?points=%s&time=%d&model=%s',gsub(" ", "", coords),age, model)
	
	# for reconstruction of present day coordinates from paleocoordinates
	if (reverse == TRUE){
		query <- paste0(query, "&reverse")
		cols <- c("long", "lat")
	} else cols <- c("paleolong", "paleolat")
	
	fullrequest <- sprintf(paste0(url,query, "&return_null_points"))
	
	if(verbose) cat("Extracting coordinates from:", fullrequest, "\n")
	rawdata <- readLines(fullrequest, warn="F") 
	
	#if null
	rawdata <- gsub("null", "[[-9999, -9999]]", rawdata)
	
	#extract coordinates
	rcoords <- matrix(as.numeric(unlist(regmatches(rawdata, gregexpr("-?[[:digit:]]+\\.*[[:digit:]]+", rawdata)))), ncol=2, byrow = TRUE)
	rcoords[rcoords == -9999] <- NA #replace na values
	
	colnames(rcoords) <- cols
	return(rcoords)
}

# Reconstruct coastlines
# Retrieve reconstructed coastline polygons for defined ages
# 
# @param age is the age in Ma at which the points will be reconstructed
# @param model is the reconstruction model. The default is "PALEOMAP". Add more details about additional models here
# @param verbose Should the function output urls?
# @return SpatialPolygonsDataFrame
gplates_reconstruct_this <- function(age, this, model="PALEOMAP", verbose=TRUE){
	if(any(age%%1!=0)){
		message("Only integer ages are supported by the online method.\nRounding target age(s).")	
		age <- round(age)
	}
	if(! requireNamespace("geojsonsf", quietly=TRUE)) stop("This method requires the 'geojsonsf' package to run.")
	
	#download and save data
	url <- paste0('http://gws.gplates.org/reconstruct/', this, '/')
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

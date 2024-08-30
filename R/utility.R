# Get operating system 
# 
# This package returns the type of OS you use.
# 
# This function and derivates have been circling over the web, I could not trace the original source.
getOS <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}



# get directory from a paht
fileFromPath <- function(x, win=FALSE){
  res <- rep(NA, length(x))

  if(!win){
	for(i in 1:length(x)){
		all <- unlist(strsplit(x[i], "/"))
		res[i] <- paste(all[length(all)], collapse="/")
	}
  }else{
	for(i in 1:length(x)){
		all <- unlist(strsplit(x[i], "\\\\"))
		res[i] <- paste(all[length(all)], collapse="\\\\")
	}
  }
  names(res) <- names(x)
  return(res)
}

# from: https://ryouready.wordpress.com/2008/12/18/generate-random-string-name/
randomString <- function(n=1, length=12){
  # initialize vector
  randomString <- c(1:n)                  
  for (i in 1:n){
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                    length, replace=TRUE), collapse="")
  }
  return(randomString)
}

# function used by mapedge
detailedBounds <- function(x,y, xmin=-180, xmax=180, ymin=-90, ymax=90){
  rbind(
    cbind(seq(xmin, xmax, length.out=x), rep(ymax, x)),
    cbind(rep(xmax, y), seq(ymax, ymin, length.out=y)),
    cbind(seq(xmax, xmin, length.out=x), rep(ymin, x)),
    cbind(rep(xmin, y), seq(ymin, ymax, length.out=y))
  )
}


#' Function to quickly draft the edge of the equirectangular projection 
#' 
#' Function to plot the edge of a map with different projections.
#' 
#' @param x (\code{numeric}) Number of segments in the x (longitude) dimension. 
#' @param y (\code{numeric}) Number of segments in the y (latitude) dimension. 
#' @param xmin (\code{numeric}) Minimum value of x (longitude).
#' @param xmax (\code{numeric}) Minimum value of x (longitude).
#' @param ymin (\code{numeric}) Maximum value of y (latitude).
#' @param ymax (\code{numeric}) Maximum value of y (latitude).
#' @param out (\code{character}) Output format, either \code{"sf"} or \code{"sp"}. The default \code{"sf"} returns simple feature geometries, \code{"sp"} returns \code{SpatialPolygons} from the \code{sp} package.
#' 
#' @return An \code{sfc}-, or \code{SpatialPolygons}-class object.
#' @examples
#' # requires rgdal
#' edge <- mapedge()
#' molledge <- st_transform(edge, "ESRI:54009")
#' plot(molledge) 
#'
#' @export
mapedge <- function(x=360, y=180, xmin=-180, xmax=180, ymin=-90, ymax=90, out="sf"){
	# return a rectangle
  	rectangle <- detailedBounds(x, y, xmin, xmax, ymin, ymax)

	# outdefense
	if(!out%in%c("sf", "sp")) stop("Invalid 'out' argument!.")

	# old spatials
	if(out=="sp"){
		# check for the presense of spatials
		if(!requireNamespace("sp", quietly=TRUE)){
			stop("This output requires the sp package!")	
		}else{
			final <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(rectangle)), ID="0")), proj4string=sp::CRS("+proj=longlat"))
		}
	}

	# default method
	if(out=="sf"){
		# sf is a hard dependency in any case
		final<- st_geometry(st_polygon(list(rectangle)))
		# set appropriate CRS
		st_crs(final) <- "EPSG:4326"
	}


  	# return object
  	return(final)
}


checkSuggested <- function(x){
	
	# check for necessary packages
	packages <- c(geojsonsf=FALSE,httr2=FALSE)

	if(! requireNamespace("geojsonsf", quietly=TRUE)) packages["geojsonsf"] <- TRUE
	if(! requireNamespace("httr2", quietly=TRUE)) packages["httr2"] <- TRUE


	if(any(packages)){
		if(sum(packages)==1){
			error <- paste0("This method requires the \"", names(packages)[packages], "\" package.\n",
				"Please install it with:\n\ninstall.packages(\"", names(packages)[packages], "\")")

		}else{
			error <- paste0("This method requires the \"geojsonsf\" and \"httr2\" packages.\n",
				"Please install them with:\n\ninstall.packages(c(\"geojsonsf\", \"httr2\"))")

		}
	
		stop(error)
	}
}


	



# Parse the JSON velocity-download returned by the GWS 
#
# The function takes the raw character string, removes the end bits, and
# calls to the NumpyArrayParser to get a raw matrix from the output,
# then it renames the column names, and translates everything to a data.frame.
# @param x The output of GWS.
# @param type The type of velocities used.
# @return A data.frame containing the data.
ParseVeloJSON<- function(x, type){
	if(!type%in%c("MagAzim", "east_north")) stop("Wrong velocity type. Choose either 'MagAzim' or 'east_north'.")

	# remove the json-endpoints
	noBegin<- gsub("\\{\"coordinates\"\\:", "", x)
	noEnd <- gsub("\\}", "", noBegin)

	# get the raw matrix
	rawVelocityMatrix <- ParseNumpyArrayString(noEnd)

	# name the colunsn
	if(type=="MagAzim") colnames(rawVelocityMatrix) <- c("long", "lat", "magnitude", "azimuth", "plateid")
	if(type=="east_north") colnames(rawVelocityMatrix) <- c("long", "lat", "east", "north", "plateid")

	return(as.data.frame(rawVelocityMatrix))

}

# Function to parse a string containing a Numpy array (matrix).
# @param string
# @return A matrix 
ParseNumpyArrayString <- function(x){
	# remove the braces and bracket from the end
	endRemoved <- gsub("\\[\\[", "", x)
	endRemoved <- gsub("\\]\\]", "", endRemoved)

	# break up dimension 1
	broken<- strsplit(endRemoved, "\\],\\[")[[1]]

	# get rid of the spaces
	noWhiteSpace <- gsub(" ", "", broken)

	# split properly
	splitList <- strsplit(noWhiteSpace, ",")

	# the final matrix form
	mat <- matrix(NA, ncol=length(splitList[[1]]), nrow=length(splitList))

	# still the fastest, get rid of the list bullshit
	for(i in 1:length(splitList)){
		mat[i, ] <- as.numeric(splitList[[i]])
	}

	return(mat)
}

# Translate a longitude-latitude and variables dataframe into a SpatRaster
#
# Based on the default rast(type="xyz") method. Will have as many layers as many columns
# there are besides the coordinate columns.
#
# @param x A data.frame
# @param coords The two coordinate column names
# @param crs the new CRS for he raster
# @return a SpatRaster object.
SpatRastFromDF <- function(x, coords=c("long", "lat"), crs="WGS84"){

	# coords have to be part of x 
	if(any(!coords%in%colnames(x))) stop("'coords' have to be columns of 'x'.")

	# deduce the resolution and range

	# everything will be a variable if it is not a coord
	vars <- colnames(x)[!colnames(x)%in%coords]

	# loop through all variables and create a stack from it
	for(i in 1:length(vars)){
		# separate  - assuming byrow!
		oneRast <- terra::rast(x[c(coords, vars[i])])

		if(i==1){
			stack <- oneRast
		}else{
			stack <- c(stack, oneRast)
		}
	}


	# assign a CRS - default to longitude-latitude
	terra::crs(stack) <- crs

	return(stack)
}

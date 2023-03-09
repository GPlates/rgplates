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
fileFromPath <- function(x){
  all <- unlist(strsplit(x, "/"))
  paste(all[length(all)], collapse="/")
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


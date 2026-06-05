#' Plot shading over a geographic map
#'
#' An eye-candy utility function for 3d-effect of global maps
#'
#' The function plots a gradient on a geographic projection, either as a color or as an alpha gradient, the function relies on \link{\code{mapedge}} for the calculation of bands.
#' @param n The number of color bands.
#' @param left RGB color for the left side of the gradient.
#' @param right RGB color for the right side of the gradient.
#' @param crs A coordinate reference system string to be passed to \link{\code{mapedge}}.
#' @param left.alpha The alpha value (0-1) used for the left side of the plot.
#' @param right.alpha The alpha value (0-1) used for the right side of the plot.
#' @param west The \code{xmin} argument of \link{\code{mapedge}}.
#' @param east The \code{xmax} argument of \link{\code{mapedge}}.
#' @export
#' @examples
#' # basic use
#' me <- mapedge()
#' plot(me)
#' sphereshade()
#'
#' # In Robinson projection
#' me <- mapedge(crs="ESRI:54030")
#' plot(me)
#' sphereshade(left="#0f3f67", right="#1A6BB0", crs="ESRI:54030")
sphereshade <- function(n=180, left="#ffffff", right="#aaaaaa", crs="EPSG:4326", left.alpha=1, right.alpha=1, west=-180, east=180){
	# create breakpoints
	breaks <- seq(west, east, length.out=n+1)

	# create a color ramp
	colFun <- grDevices::colorRampPalette(c(left, right))
	cols <- colFun(n)

	# transparency is needed
	alphas <- seq(left.alpha, right.alpha, length.out=n)
	alphaVals <- as.character(as.hexmode(round(alphas*255)))
	alphaVals[nchar(alphaVals)==1] <- paste0("0", alphaVals[nchar(alphaVals)==1])


	# plot them sequentiallly
	for(i in 1:n){
		oneSlice <- mapedge(xmin=breaks[i],xmax=breaks[i+1],crs=crs)
		plot(oneSlice, border=NA, col=paste0(cols[i], as.character(alphaVals[i])), add=TRUE)
	}

}

#' Plot subduction lines
#'
#' @param x An \code{sf}-class object with additional class label of \code{subduction.zones}, or \code{plate.boundaries}.
#' returned by the online \code{\link[rgplates]{reconstruct}} method.
#' @param add (\code{logical}) By default the plot will be plotting on existing plots. Use \code{add=FALSE} to plot from scratch.
#' @param ... Arguments passed to the \code{\link{sawteeth}} function.
#' @return The function has no return value.
#' @name subductionlines
#' @export
subductionlines <- function(x, add=TRUE,  ... ){
	# method S3 method dispatch does not work properly currently...
	if(inherits(x, "plate.boundaries")){
		# subset to subductions
		x <- x[x$type=="SubductionZone",]
		class(x)[class(x)=="plate.boundaries"] <- "subduction.zones"
	}

	if(!inherits(x, "subduction.zones")) stop("The function requires a 'subduction.lines'-class object.")
	if(!add) {
		plot(x$geometry, col=NA, border=NA)
	}

	# break down object to individual lines
	elements <- nrow(x)

	# repeat for every segments
	for(i in 1:elements){
		# the subset
		sfElement <- x$geometry[[i]]

		# grab coordinates
		coords <- sf::st_coordinates(sfElement)

		# for every actual line
		segmentNames <- unique(coords[, "L1"])

		for(j in 1:length(segmentNames)){
			# the coordinate matrix
			coordMat <- coords[coords[, "L1"]==segmentNames[j], c("X", "Y")]

			# call for the line-specific method
			sawteeth(coordMat, left=x$polarity[i]=="left", ...)
		}

	}

}


################################################################################
# Utility plotting


#' Draw lines with sawteeth on side
#'
#' Function used for plotting subduction lines
#'
#' @param x (\code{numeric}) Two dimensional matrix of coordinates (x and y) columns.
#' @param left (\code{logical}) Switch indicating whether to put the teeth on the left- or right-hand side.
#' @param cex (\code{numeric}) Standard scaling argument, controls the size and number of sawteeth. Defaults are based on plot parameters (\code{cxy}), different device dimensions will lead to differently sized triangles.
#' @param shape (\code{numeric}) Proportion of tooth height to its base.
#' @param splineshape (\code{numeric}) Shape parameter of \code{\link[graphics]{xspline}} (called twice!).
#' @param col (\code{numeric}) The color of the teeth and the lines.
#' @param lwd (\code{numeric}) The width of the line.
#' @param ... Additional arguments passed to \code{\link[graphics]{polygon}}.
#' @return The function has no return value.
#' @examples
#' # define points
#' x <- c(0.42, 1.90, 2.06, 1.28, 1.01, 1.05, 1.34, 2.14,
#' 	4.25, 6.69, 7.96, 8.88, 9.35, 9.41, 8.84)
#'
#' y <- c(1.06, 0.86, 1.91, 2.90, 4.25, 5.52, 6.81, 8.03,
#' 	9.03, 9.25, 9.30, 8.88, 8.36, 7.00, 6.50)
#'
#' # visualize the line with a sawteeth
#' plot(x, y, xlim=c(0,10), ylim=c(0, 10))
#'
#' sawteeth(cbind(x,y), left=TRUE, col="#99000077")
#' sawteeth(cbind(x,y), left=TRUE, col="black", shape=0.5)
#'
#' # use combination of cex and shape to control the size triangles
#' plot(x, y, xlim=c(0,10), ylim=c(0, 10))
#' sawteeth(cbind(x,y), left=TRUE, col="#99000077", cex=0.5, shape=3)
#' @export
sawteeth <- function(x, left, cex=1, shape=1, splineshape=-0.5, col="black", lwd=1, ...){

	# Defense
	# Does not interpolate otherwise...
	if(splineshape==0) splineshape <- -0.0001
	if(!is.logical(left)) stop("The argument 'left' has to be a single logical value.")
	if(!is.numeric(x)) stop("The argument 'x' has to be a two-column matrix.")
	if(ncol(x)!=2) stop("The argument 'x' has to be a two-column matrix.")

	# do not allow missing values in the matrix
	if(any(is.na(x))) stop("The argument 'x' must not contain missing values.")

	if(length(shape)>1) stop("The argument 'shape' has to a single numeric value.")
	if(shape<0 ) stop("The argument 'shape' has to be a positive value.")

	# grab params
	params <- graphics::par()

	# the size of the triangles will come from here
	sizeOrig <- params$cxy[1]

	# the distance between the triangles
	delta <- cex*sizeOrig*2

	# calculate the length outlined by the line
	triangleBasis <- trianglePoints(x, delta, splineshape)


	#testing condition
	if(length(triangleBasis$base)>1){

		# the points themselves
		polyBasis <- trianglePolygons(base=triangleBasis$points, need=triangleBasis$base, shape=shape, left=left)

		# plotting
		graphics::polygon(polyBasis, border=NA,col=col, ...)
	}

	graphics::lines(triangleBasis$points, col=col, lwd=lwd)

}

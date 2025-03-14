################################################################################
# Class-methods

#' Plotting method for subduction.zones
#'
#' @param x An \code{sf}-class object with additional class label of \code{subduction.zones},
#' returned by the online \code{\link[rgplates]{reconstruct}} method.
#' @param add (\code{logical}) By default the plot will be plotting on existing plots. Use \code{add=FALSE} to plot from scratch.
#' @return The function has no return value.
#' @rdname plots
#' @exportS3Method base::plot
#' @export plot.subduction.zones
plot.subduction.zones <- function(x, add=TRUE,  ... ){
	# break down object to individual lines
	elements <- nrow(x)

	if(!add) {
		plot(x$geometry, col=NA, border=NA)
	}

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
#' @param cex (\code{numeric}) Standard scaling argument, controls the size and number of sawteeth. Defaults are based on plot parameters (\code{cxy}).
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
#' # visualize points
#' plot(x, y, xlim=c(0,10), ylim=c(0, 10))
#'
#' sawteeth(cbind(x,y), left=TRUE, col="#99000077")
#' sawteeth(cbind(x,y), left=TRUE, col="black", shape=0.5)
#' @export
sawteeth <- function(x, left, cex=1, shape=1, splineshape=-0.5, col="black", lwd=1, ...){

	# Defense
	# Does not interpolate otherwise...
	if(splineshape==0) splineshape <- -0.0001
	if(!is.logical(left)) stop("The argument 'left' has to be a single logical value.")
	if(!is.numeric(x)) stop("The argument 'x' has to be a two-column matrix.")
	if(ncol(x)!=2) stop("The argument 'x' has to be a two-column matrix.")

	# grab params
	params <- par()

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

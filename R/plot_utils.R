# Plotting utility functions.

# Function used to determine which points will be defining the triangle bases
# @param x The coordinate matrix (column, x,y)
# @param delta Minimum distance between the base points.
# @param splineshape Behavior fo xsplines.
# @return A list with the interpolated points and the base indices.
trianglePoints <- function(x, delta, splineshape=-0.5){

	# Iteration 1
	# shape parameter
	shape <- c(0,rep(splineshape, nrow(x)-2),  0)

	# the splines
	spl <- graphics::xspline(x[,1], x[,2], shape=shape, draw=FALSE)

	# Iteration 2 - cannot set interpolation density param of xsplines...
	# interpolate again
	shape <- c(0,rep(splineshape, length(spl$x)-2),  0)
	spl2 <- graphics::xspline(spl$x, spl$y, shape=shape, draw=FALSE)

	newCoords <- cbind(spl2$x, spl2$y)

	# point density is still dependent on originals
#	points(newCoords, cex=0.4, pch=16, col="blue" )

	# desired distance between points is
	half <- delta/4

	need <- regularize(newCoords, d=half)

#	points(newCoords[need, ], col="green")

	return(list(points=newCoords, base=need))

}


# Function to get points from a matrix that are more regularly sampled (almost equidistant in 2d)
#
# @param x The coordinate matrix (column, x,y)
# @param d Approximate distance between the base points.
# @return Numeric vector with indices of the which are more or ess.
regularize <- function(x, d){

	# the distances between points
	# ith value is the distance from the ith point!
	distances <- pathDists(x)

	# find the mid value
	midIndex <- round(nrow(x)/2)

	# the first Pointset (starting with the midpoint)
	firstDistances <- rev(distances[1:(midIndex-1)])
	firstID <- rev(1:midIndex)

	# the second Pointset
	secondDistances <- distances[midIndex:length(distances)]
	secondID <- midIndex:nrow(x)

	# from the midpoint to the beginning
	firstGet <- rev(getEquidistant(ds=firstDistances, id=firstID, d=d))

	# from the midpoint to the end
	secondGet <- getEquidistant(ds=secondDistances, id=secondID, d=d)

	needed <- c(firstGet, midIndex, secondGet)

	needed
#	points(x[needed, ], col="green")

}

# To actually get the 'equidistant' values
# @param ds the distance vector
# @param id the ids of the points (one more)
# @param d single distance value
# @return subset of IDs that are actually almost equidistant
getEquidistant <- function(ds, id, d){
	#ds <- firstDistances
	#id <- firstID

	# the accumulator
	cumul <- 0

	# the distance sums
	sumDists <- cumsum(ds)

	# check?
	relative <- sumDists/d
	beyond <- floor(relative)

	# which distance is just beyond the mark?
	whichBeyond <- c(FALSE, diff(beyond)==1)

	# o be used
	id[c(FALSE, whichBeyond)]


}


# Calculate the length of segments outlined by points
#
# @param x Two-column matrix (x and y)
# @return A vector of distances
pathDists <- function(x){

	# the total distance
	dists <- rep(NA, nrow(x)-1)

	for(i in 2:nrow(x)){
		dists[i-1] <- sqrt(sum((x[i,] - x[i-1, ])^2))
	}

	return(dists)
}

# Calculate the coordinate matrix that is to visualized with polygon.
#
# @param base Two-column matrix (x and y). The fine resolution points
# @param need Integer indices indicating which points form the bases of the triangles.
# @param shape Numeric value: proportion of average base length to height.
# @param left Boolean switch for the side
# @return A matrix as base, but with added tip points of the triangles, and missing values to separate subsets.
trianglePolygons <- function(base, need, shape=1, left){
	## base <- triangleBasis$points
	## need <- triangleBasis$base
	# left <- TRUE

	# the average distance
	height <- mean(pathDists(base[need,]))
	height <- height*shape


	allTriangles <- NULL

	# repeat for every triangle
	for(i in 2:length(need)){
		firstIndex <- need[i-1]
		secondIndex <- need[i]

		# the two base points
		first <- base[firstIndex,]
		second <- base[secondIndex,]

		# get the difference vector
		diffVec <- second-first

		# the midpoint
		midPoint <- first + diffVec/2

		# the angle of the connecting line (theta)
		theta <- atan(diffVec[2]/diffVec[1])

		# desired side of the triangle (d)
		halfDist <- sqrt(sum((midPoint-first)^2))

		# the degree to the	tip
		alpha <- atan(height/halfDist)

		# the side of the triangle
		side <- halfDist/cos(alpha)


		# beta
		if(left){
			beta <- theta+alpha
		}else{
			beta <- theta-alpha
		}
		# differences
		xdiff <- side*cos(beta)
		ydiff <- side*sin(beta)

		# the tip of the triangle

		if(diffVec[1]>0){
			tip <- c(first[1]+xdiff, first[2]+ydiff)
		}else{
			tip <- c(first[1]-xdiff, first[2]-ydiff)
		}

		# temporary
		# polygon(rbind(first, second, tip), col="#0000AA99")

		allTriangles <- rbind(allTriangles, base[firstIndex:secondIndex,,drop=FALSE], tip, c(NA, NA))


	}

	return(allTriangles)


}

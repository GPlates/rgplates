# Basic call-success tests of graphical function.
# This will replaced later with image-comparison based tests
library(tinytest)
library(rgplates)


# generate dummy data
x <- c(
	0.4255322,
	1.9099657,
	2.0662219,
	1.2849411,
	1.0114928,
	1.0505568,
	1.3435372,
	2.1443500,
	4.2538081,
	6.6953107,
	7.9648920,
	8.8828969,
	9.3516654,
	9.4102615,
	8.8438329
)

y <- c(
	1.0654584,
	0.8688405,
	1.9174691,
	2.9005583,
	4.2550368,
	5.5221296,
	6.8110689,
	8.0344688,
	9.0394045,
	9.2578688,
	9.3015616,
	8.8864795,
	8.3621652,
	7.0076867,
	6.5052189
)

# point matrix
mat <- cbind(x,y)

plot(NULL, NULL, xlim=c(0,10), ylim=c(0, 10))

# provide correct arguments
expect_error(sawteeth(mat))


# left orientation
plot(NULL, NULL, xlim=c(0,10), ylim=c(0, 10))
expect_silent(sawteeth(mat, left=TRUE))

# right orientation
plot(NULL, NULL, xlim=c(0,10), ylim=c(0, 10))
expect_silent(sawteeth(mat, left=FALSE))

# passing graphical arguments
plot(NULL, NULL, xlim=c(0,10), ylim=c(0, 10))
expect_silent(sawteeth(mat, left=FALSE, col="red", cex=2))

# triangle shape
plot(NULL, NULL, xlim=c(0,10), ylim=c(0, 10))
expect_silent(sawteeth(mat, left=FALSE, col="red", cex=2, shape=0.8))

# shape should be error
expect_error(sawteeth(mat, left=FALSE, col="red", cex=2, shape=-0.8))


# splineshape
# jagged line
plot(NULL, NULL, xlim=c(0,10), ylim=c(0, 10))
expect_silent(sawteeth(mat, left=FALSE, col="red", splineshape=0))

plot(NULL, NULL, xlim=c(0,10), ylim=c(0, 10))
expect_silent(sawteeth(mat, left=FALSE, col="red", splineshape=-1))


plot(NULL, NULL, xlim=c(0,10), ylim=c(0, 10))
expect_silent(sawteeth(mat, left=FALSE, col="red", splineshape=0.5))

plot(NULL, NULL, xlim=c(0,10), ylim=c(0, 10))
expect_silent(sawteeth(mat, left=FALSE, col="red", splineshape=1))


# arguments passed to polygon
plot(NULL, NULL, xlim=c(0,10), ylim=c(0, 10))
expect_silent(sawteeth(mat, left=FALSE, col="red", density=50))

################################################################################
# Missing values in the input structure
mat2 <- mat
mat2[4,] <- c(NA, NA)

plot(NULL, NULL, xlim=c(0,10), ylim=c(0, 10))
expect_error(sawteeth(mat2, left=TRUE))

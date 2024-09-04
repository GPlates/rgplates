################################################################################
# Method defintions
library(rgplates)
suppressPackageStartupMessages(library(terra))
library(via)
suppressPackageStartupMessages(library(chronosphere))

options(timeout = 5*60)

# an offline model
dems <- chronosphere::fetch("paleomap", "dem", datadir=file.path(wd, "data/chronosphere"), verbose=FALSE)
gmst <- chronosphere::fetch("paleomap", "gmst", datadir=file.path(wd, "data/chronosphere"), verbose=FALSE)
target <- rast(res=4)

# testing objects
# present-day things
smalltopo <- terra::resample(dems["0"], target)
smallgmst <- terra::resample(gmst["0"], target)
smallDouble <- c(smalltopo, smallgmst)

# old things
pasttopo <- terra::resample(dems["60"], target)
pastgmst <- terra::resample(gmst["60"], target)
pastDouble <- c(pasttopo, pastgmst)

# additional things to compare with
pasttopo30 <- terra::resample(dems["30"], target)
pasttopo120 <- terra::resample(dems["120"], target)

#' Check the general match between topographies
#'
#' It is very difficult to check whether the reconstruction is correct.
#' Reconstructed topographies should better match with a given raster that represents the same time slice, than
#' another time slice, for instance the one that was reconstructed. This comparison is done by masking the two raster
#' to match the result, and then the total mean difference is calculated between the two pairs (res & better vs. res & worse).
#'
#' @param res SpatRaster, the result of the reconstruction to be tested.
#' @param better SpatRaster, \code{res} is expected to match this raster better than \code{worse}
#' @param worse SpatRaster, \code{res} is expected to  match thiw worse.
#' @return A single logical value, whether the expectation is met.
BetterMatch <- function(res, worse, better){
	# where are the values missing
	bMask <- is.na(terra::values(res))

	# make the comparison fair
	terra::values(worse)[bMask] <- NA
	terra::values(better)[bMask] <- NA

	# expectation, the mean abosolute difference is higher for the better match
	worseDiff <- mean(abs(terra::values(res-worse)), na.rm=TRUE)
	betterDiff <- mean(abs(terra::values(res-better)), na.rm=TRUE)

	betterDiff < worseDiff

}

# CheckGWS <- rgplates:::CheckGWS
# produce error with offline method

# I. Single target age (base case)
# single raster from 0 to past
expect_silent(sol60 <- reconstruct(smalltopo, age=60, model="MERDITH2021"))
expect_equivalent(class(sol60), "SpatRaster")
expect_true(BetterMatch(sol60, worse=smalltopo, better=pasttopo))

# single raster from past to 0
expect_silent(sol60re <- reconstruct(pasttopo, from=60, age=0, model="PALEOMAP"))
expect_equivalent(class(sol60re), "SpatRaster")
expect_true(BetterMatch(sol60re, worse=pasttopo, better=smalltopo))

# single raster from older to younger
expect_silent(sol <- reconstruct(pasttopo, from=60, age=30, model="PALEOMAP"))
expect_equivalent(class(sol), "SpatRaster")
expect_true(BetterMatch(sol, worse=pasttopo, better=pasttopo30))

# single raster from younger to older
expect_silent(sol <- reconstruct(pasttopo, from=60, age=120, model="PALEOMAP"))
expect_equivalent(class(sol), "SpatRaster")
expect_true(BetterMatch(sol, worse=pasttopo, better=pasttopo120))

# multiple raster from 0 to past
expect_silent(sol <- reconstruct(smallDouble, age=60, model="MERDITH2021"))
expect_equivalent(class(sol), "SpatRaster")

# multiple raster from older to younger
expect_silent(sol <- reconstruct(pastDouble, from=60, age=30, model="MERDITH2021"))
expect_equivalent(class(sol), "SpatRaster")

# multiple raster from younger to older
expect_silent(sol <- reconstruct(pastDouble, from=60, age=120, model="MERDITH2021"))
expect_equivalent(class(sol), "SpatRaster")

# II. Multiple target age (recursive call)
# single raster from 0 to past
targetAges <- c(60, 100)
expect_silent(sol <- reconstruct(smalltopo, age=targetAges, model="MERDITH2021"))
expect_equal(class(sol), "list")
expect_equal(length(sol), length(targetAges))
expect_equal(as.numeric(names(sol)), targetAges)
expect_true(inherits(sol[[1]], "SpatRaster"))
expect_equal(sol[["60"]], sol60)

# single raster from older to younger
targetAges <- c(30, 15)
expect_silent(sol <- reconstruct(pasttopo, from=60, age=targetAges, model="PALEOMAP"))
expect_equivalent(class(sol), "list")
expect_equal(length(sol), length(targetAges))
expect_equal(as.numeric(names(sol)), targetAges)
expect_true(inherits(sol[[1]], "SpatRaster"))

# single raster from younger to older
targetAges <- c(120, 150)
expect_silent(sol <- reconstruct(pasttopo, from=60, age=targetAges, model="PALEOMAP"))
expect_equivalent(class(sol), "list")
expect_equal(length(sol), length(targetAges))
expect_equal(as.numeric(names(sol)), targetAges)
expect_true(inherits(sol[[1]], "SpatRaster"))

# multiple raster from 0 to past
targetAges <- c(60,100)
expect_silent(sol <- reconstruct(smallDouble, age=targetAges, model="MERDITH2021"))
expect_equivalent(class(sol), "list")
expect_equal(length(sol), length(targetAges))
expect_equal(as.numeric(names(sol)), targetAges)
expect_true(inherits(sol[[1]], "SpatRaster"))

# multiple raster from older to younger
targetAges <- c(30,15)
expect_silent(sol <- reconstruct(pastDouble, from=60, age=targetAges, model="MERDITH2021"))
expect_equivalent(class(sol), "list")
expect_equal(length(sol), length(targetAges))
expect_equal(as.numeric(names(sol)), targetAges)
expect_true(inherits(sol[[1]], "SpatRaster"))

# multiple raster from younger to older
targetAges <- c(120,150)
expect_silent(sol <- reconstruct(pastDouble, from=60, age=targetAges, model="MERDITH2021"))
expect_equivalent(class(sol), "list")
expect_equal(length(sol), length(targetAges))
expect_equal(as.numeric(names(sol)), targetAges)
expect_true(inherits(sol[[1]], "SpatRaster"))

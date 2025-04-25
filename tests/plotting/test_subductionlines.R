# Basic call-success tests of graphical function.
# This will replaced later with image-comparison based tests
library(tinytest)
library(rgplates)


# access something
expect_silent(MERDITH2021_coast <- reconstruct("coastlines", model="MERDITH2021", age=30, check=FALSE))
expect_silent(MERDITH2021_sub <- reconstruct("subduction_zones", model="MERDITH2021", age=30, check=FALSE))
expect_silent(MERDITH2021_boundaries <- reconstruct("plate_boundaries", model="MERDITH2021", age=30, check=FALSE))


# wrong input
expect_error(subductionlines(MERDITH2021_coast))

# basic dispatch
plot(MERDITH2021_coast$geometry, col="gray", border=NA)
expect_silent(subductionlines(MERDITH2021_sub)) # adds by default
expect_silent(subductionlines(MERDITH2021_sub, add=FALSE))

# with the plate boundaries
plot(MERDITH2021_coast$geometry, col="gray", border=NA)
expect_silent(subductionlines(MERDITH2021_boundaries)) # adds by default

# transform projection!
proj <- "ESRI:54009"
MERDITH2021_coast_proj <- sf::st_transform(MERDITH2021_coast, crs=proj)
MERDITH2021_sub_proj <- sf::st_transform(MERDITH2021_sub, crs=proj)
MERDITH2021_boundaries_proj <- sf::st_transform(MERDITH2021_boundaries, crs=proj)


plot(MERDITH2021_coast_proj$geometry, col="gray", border=NA)
expect_silent(subductionlines(MERDITH2021_sub_proj))

plot(MERDITH2021_coast_proj$geometry, col="gray", border=NA)
expect_silent(subductionlines(MERDITH2021_boundaries_proj))

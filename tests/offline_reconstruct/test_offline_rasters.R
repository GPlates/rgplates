library(rgplates)
library(terra)
library(via)
library(chronosphere)

# an offline model
dems <- chronosphere::fetch("paleomap", "dem", datadir=file.path(wd, "data/chronosphere"), verbose=FALSE)
model <- chronosphere::fetch("paleomap", "model", datadir=file.path(wd, "data/chronosphere"), verbose=FALSE)


# Testing objects.
target <- terra::rast(res=4)

# present-day things
smalltopo <- terra:resample(dems["0"], target)

# old things
pasttopo <- terra::resample(dems["60"], target)

# additional things to compare with
pasttopo30 <- terra::resample(dems["30"], target)
pasttopo120 <- terra::resample(dems["120"], target)
# This will not yet work

expect_error(reconstruct(smalltopo, age=60, model=model))

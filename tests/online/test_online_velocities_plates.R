library(tinytest)

# The current model
currModel <- "MERDITH2021"

################################################################################
# I. Longlat-domain

## MagAzim
## data.frame
expect_silent(llg <- velocities(age=30, model=currModel, type="MagAzim", domain="longLatGrid"))
expect_true(inherits(llg, "data.frame"))
expect_equal(ncol(llg),5)
expect_equal(colnames(llg),c("long", "lat", "magnitude", "azimuth", "plateid"))
expect_equivalent(rep("numeric", 5), unlist(lapply(llg, class)))
expect_equal(range(llg$long), c(-180, 180))
expect_equal(range(llg$lat), c(-90, 90))
expect_true(min(llg$azimuth)>=0)
expect_true(max(llg$azimuth)<=2*pi)
# both max magn lower than 200mm/year
expect_true(min(llg$magnitude)>=0)
expect_true(max(llg$magnitude)<=200)

## Raster - should work without attaching terra
try(detach("package:terra"), silent=TRUE)

# default
expect_silent(llgrastDef <- velocities(age=30, model=currModel, type="MagAzim",
	domain="longLatGrid", output="SpatRaster"))
expect_true(inherits(llgrastDef, "SpatRaster"))

library(terra)

### resample=TRUE (default)
expect_silent(llgrast <- velocities(age=30, model=currModel, type="MagAzim",
	domain="longLatGrid", output="SpatRaster", cellraster=TRUE))
expect_equal(llgrastDef, llgrast)
expect_equal(dim(llgrast)[3], 2)
expect_equal(names(llgrast), c("magnitude", "azimuth"))
expect_equal(terra::ext(llgrast), terra::ext(c(-180, 180, -90,90)))
expect_true(min(values(llgrast[["azimuth"]]), na.rm=TRUE)>=0)
expect_true(max(values(llgrast[["azimuth"]]), na.rm=TRUE)<=2*pi)
expect_true(min(values(llgrast[["magnitude"]]), na.rm=TRUE)>=0)
expect_true(max(values(llgrast[["magnitude"]]), na.rm=TRUE)<=200)

### resample=FALSE
expect_silent(llgrastOrig <- velocities(age=30, model=currModel, type="MagAzim",
	domain="longLatGrid", output="SpatRaster", cellraster=FALSE))
expect_equal(terra::ext(llgrastOrig), terra::ext(c(-180.5, 180.5, -90.5,90.5)))
expect_equal(dim(llgrastOrig)[3], 3)
expect_equal(names(llgrastOrig), c("magnitude", "azimuth", "plateid"))

## east_north
## data.frame
expect_silent(llg2 <- velocities(age=30, model=currModel, type="east_north", domain="longLatGrid"))

expect_true(inherits(llg2, "data.frame"))
expect_equal(ncol(llg2),5)
expect_equal(colnames(llg2),c("long", "lat", "east", "north", "plateid"))
expect_equivalent(rep("numeric", 5), unlist(lapply(llg2, class)))
expect_equal(range(llg2$long), c(-180, 180))
expect_equal(range(llg2$lat), c(-90, 90))

# both negative and positive values, max magn lower than 200mm/year
expect_equal(sign(range(llg2$east)), c(-1, 1))
expect_equal(sign(range(llg2$north)), c(-1, 1))
expect_true(max(abs(llg2$east))<=200)
expect_true(max(abs(llg2$north))<=200)

# longlat and platied identical
expect_equal(llg[c(1,2,5)], llg2[c(1,2,5)])



## Raster - output
expect_silent(llgrastENdef <- velocities(age=30, model=currModel, type="east_north",
	domain="longLatGrid", output="SpatRaster"))
expect_true(inherits(llgrastENdef, "SpatRaster"))

### resample=TRUE
expect_silent(llgrastEN <- velocities(age=30, model=currModel, type="east_north",
	domain="longLatGrid", output="SpatRaster", cellraster=TRUE))
expect_equal(llgrastEN, llgrastENdef)
expect_equal(dim(llgrastEN)[3], 2)
expect_equal(names(llgrastEN), c("east", "north"))
expect_equal(terra::ext(llgrast), terra::ext(c(-180, 180, -90,90)))

### resample=FALSE
expect_silent(llgrastENorig <- velocities(age=30, model=currModel, type="east_north",
	domain="longLatGrid", output="SpatRaster", cellraster=FALSE))
expect_equal(dim(llgrastENorig)[3], 3)
expect_equal(names(llgrastENorig), c("east", "north", "plateid"))
expect_equal(terra::ext(llgrastENorig), terra::ext(c(-180.5, 180.5, -90.5,90.5)))

# II. healpix-domain (data.frame output)

## Raster - output should provide error
expect_error(velocities(age=30, model=currModel, type="east_north",
	domain="healpix", output="SpatRaster"))

expect_error(velocities(age=30, model=currModel, type="MagAzim",
	domain="healpix", output="SpatRaster"))

## MagAzim
expect_silent(hpMA<- velocities(age=30, model=currModel, type="MagAzim",
	domain="healpix"))
expect_true(inherits(hpMA, "data.frame"))
expect_equal(ncol(hpMA),5)
expect_equal(colnames(hpMA),c("long", "lat", "magnitude", "azimuth", "plateid"))
expect_equivalent(rep("numeric", 5), unlist(lapply(hpMA, class)))
expect_true(max(hpMA$long)<=180)
expect_true(min(hpMA$long)>=-180)
expect_true(max(hpMA$lat)<=90)
expect_true(min(hpMA$lat)>=-90)
expect_true(min(hpMA$azimuth)>=0)
expect_true(max(hpMA$azimuth)<=2*pi)
# both max magn lower than 200mm/year
expect_true(min(hpMA$magnitude)>=0)
expect_true(max(hpMA$magnitude)<=200)

# east-north
expect_silent(hpEN<- velocities(age=30, model=currModel, type="east_north",
	domain="healpix"))
expect_true(inherits(hpEN, "data.frame"))
expect_equal(ncol(hpEN),5)
expect_equal(colnames(hpEN),c("long", "lat", "east", "north", "plateid"))
expect_equivalent(rep("numeric", 5), unlist(lapply(hpEN, class)))

expect_equal(sign(range(hpEN$east)), c(-1, 1))
expect_equal(sign(range(hpEN$north)), c(-1, 1))
expect_true(max(abs(hpEN$east))<=200)
expect_true(max(abs(hpEN$north))<=200)

# longlat and platied identical
expect_equal(hpMA[c(1,2,5)], hpEN[c(1,2,5)])

########################################----------------------------------------
# Misc
# Incorrect domain
expect_error(velocities(age=30, model=currModel, type="MagAzim",
	domain="garbage", output="data.frame"))

# Incorrect type
expect_error(velocities(age=30, model=currModel, type="garbage",
	domain="longLatGrid", output="data.frame"))

# Target age beyond the limit suggested by gws
velocities(age=30, model="SETON2012", type="MagAzim")
# Iterate for ages


################################################################################
# II. Iterate for ages

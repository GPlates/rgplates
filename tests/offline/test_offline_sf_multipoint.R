library(rgplates)

# the used objects
model <- chronosphere::fetch("paleomap", "model", datadir=file.path(wd, "data/chronosphere"), verbose=FALSE)

# example matrix
dmat <- as.data.frame(
	matrix(c(                
	-27.44, 26.07,
	3.53, 25.44,
	16.53, 26.71,
	12.2, 45.01,
	-45.4, 43.12,
	24.58, 58.9,
	-30.53, 72.79,
	-29.29, -28.85
	), ncol=2, byrow=TRUE) 
)

colnames(dmat) <-c("lng", "lat")
rownames(dmat) <-paste0("row", 1:nrow(dmat))
dmat$dat <- runif(rnorm(1:nrow(dmat)))
dmat$id <- paste0("id", 1:nrow(dmat))
	
################################################################################
# 1. NO CRS - assuming long and lat 
# some points with data
sfp <- st_as_sf(dmat, coords=c("lng", "lat"))

################################################################################
# Reconstructed to 0
rec0 <- reconstruct(sfp, age=0, model=model)


# should return a data.frame? - no!
expect_inherits(rec0, "sf")
expect_equal(nrow(rec0), nrow(sfp))

# These are not expected to match!
## # attributes match
## expect_equal(rownames(sfp), rownames(rec0))
expect_equal(colnames(sfp), colnames(rec0))

# expect a WGS 84
expect_equal(st_crs(rec0)[1][[1]], "WGS 84")

## ################################################################################
# Reconstructed to 100 
rec100 <- reconstruct(sfp, age=100, model=model, plateperiod=TRUE)


# the data.frame method for cross checking
recDF<- reconstruct(dmat[, c("lng", "lat")], age=100, model=model, plateperiod=TRUE)


expect_inherits(rec100, "sf")
expect_equal(nrow(rec100), sum(!is.na(recDF[, "lng"])))

# attributes match
# expect_equal(rownames(dmat), rownames(rec100))
expect_equal(colnames(sfp), colnames(rec100))

# all the reconstructable are there
expect_equal(sum(rec100$id %in% sfp$id), nrow(rec100))

# the data match
part <- rec100$dat
names(part) <- rec100$id
full <- sfp$dat
names(full) <- sfp$id
expect_equal(part, full[names(part)])

# expect a WGS 84
expect_equal(st_crs(rec100)[1][[1]], "WGS 84")

################################################################################
# Reconstructed to 100 with plateperiod=FALSE
rec100_pp <- reconstruct(sfp, age=100, model=model, plateperiod=FALSE)


expect_inherits(rec100_pp, "sf")
expect_equal(nrow(rec100_pp), nrow(sfp))

# attributes match
# expect_equal(rownames(sfp), rownames(rec100_pp))
expect_equal(colnames(sfp), colnames(rec100_pp))


# should be the same on the matching interval
keep <- which(!is.na(recDF[,"lng"]))
expect_equivalent(rec100, rec100_pp[keep, ]) # rownames differ!

# expect a WGS 84
expect_equal(st_crs(rec100_pp)[1][[1]], "WGS 84")

################################################################################
# Reconstructed to c(0,100) with listout
rec <- reconstruct(sfp, age=c(0,100), listout=TRUE,  model=model, plateperiod=TRUE)


# high-level attributes
expect_inherits(rec, "list")
expect_equal(names(rec), c("0", "100"))

# exact match 
expect_equal(rec[[1]], rec0)
expect_equal(rec[[2]], rec100)


################################################################################
# Reconstructed to c(0,100) with listout=FALSE
expect_error(
	rec <- reconstruct(sfp, age=c(0,100), listout=FALSE,  model=model, plateperiod=TRUE)
)










################################################################################
# 2. Explicit WGS 84 CRS
proj <- "EPSG:4326"
st_crs(sfp) <-proj 

################################################################################
# Reconstructed to 0
rec0 <- reconstruct(sfp, age=0, model=model)


# should return a data.frame? - no!
expect_inherits(rec0, "sf")
expect_equal(nrow(rec0), nrow(sfp))

# These are not expected to match!
## # attributes match
## expect_equal(rownames(sfp), rownames(rec0))
expect_equal(colnames(sfp), colnames(rec0))

# expect the input CRS
expect_equal(st_crs(rec0)[1][[1]], proj)

## ################################################################################
# Reconstructed to 100 
rec100 <- reconstruct(sfp, age=100, model=model, plateperiod=TRUE)


# the data.frame method for cross checking
recDF<- reconstruct(dmat[, c("lng", "lat")], age=100, model=model, plateperiod=TRUE)


expect_inherits(rec100, "sf")
expect_equal(nrow(rec100), sum(!is.na(recDF[, "lng"])))

# attributes match
# expect_equal(rownames(dmat), rownames(rec100))
expect_equal(colnames(sfp), colnames(rec100))

# all the reconstructable are there
expect_equal(sum(rec100$id %in% sfp$id), nrow(rec100))

# the data match
part <- rec100$dat
names(part) <- rec100$id
full <- sfp$dat
names(full) <- sfp$id
expect_equal(part, full[names(part)])

# expect the input CRS
expect_equal(st_crs(rec100)[1][[1]], proj)

################################################################################
# Reconstructed to 100 with plateperiod=FALSE
rec100_pp <- reconstruct(sfp, age=100, model=model, plateperiod=FALSE)


expect_inherits(rec100_pp, "sf")
expect_equal(nrow(rec100_pp), nrow(sfp))

# attributes match
# expect_equal(rownames(sfp), rownames(rec100_pp))
expect_equal(colnames(sfp), colnames(rec100_pp))


# should be the same on the matching interval
keep <- which(!is.na(recDF[,"lng"]))
expect_equivalent(rec100, rec100_pp[keep, ]) # rownames differ!

# expect the input CRS
expect_equal(st_crs(rec100_pp)[1][[1]], proj)

################################################################################
# Reconstructed to c(0,100) with listout
rec <- reconstruct(sfp, age=c(0,100), listout=TRUE,  model=model, plateperiod=TRUE)


# high-level attributes
expect_inherits(rec, "list")
expect_equal(names(rec), c("0", "100"))

# exact match 
expect_equal(rec[[1]], rec0)
expect_equal(rec[[2]], rec100)

################################################################################
# Reconstructed to c(0,100) with listout=FALSE
expect_error(
	rec <- reconstruct(sfp, age=c(0,100), listout=FALSE,  model=model, plateperiod=TRUE)
)











################################################################################
# 3. Repeat with a different CRS
proj <- "ESRI:54009"
sfp <- st_transform(sfp, proj)

################################################################################
# Reconstructed to 0
rec0 <- reconstruct(sfp, age=0, model=model)


# should return a data.frame? - no!
expect_inherits(rec0, "sf")
expect_equal(nrow(rec0), nrow(sfp))

# These are not expected to match!
## # attributes match
## expect_equal(rownames(sfp), rownames(rec0))
expect_equal(colnames(sfp), colnames(rec0))

# expect the input CRS
expect_equal(st_crs(rec0)[1][[1]], proj)

## ################################################################################
# Reconstructed to 100 
rec100 <- reconstruct(sfp, age=100, model=model, plateperiod=TRUE)


# the data.frame method for cross checking
recDF<- reconstruct(dmat[, c("lng", "lat")], age=100, model=model, plateperiod=TRUE)


expect_inherits(rec100, "sf")
expect_equal(nrow(rec100), sum(!is.na(recDF[, "lng"])))

# attributes match
# expect_equal(rownames(dmat), rownames(rec100))
expect_equal(colnames(sfp), colnames(rec100))

# all the reconstructable are there
expect_equal(sum(rec100$id %in% sfp$id), nrow(rec100))

# the data match
part <- rec100$dat
names(part) <- rec100$id
full <- sfp$dat
names(full) <- sfp$id
expect_equal(part, full[names(part)])

# expect the input CRS
expect_equal(st_crs(rec100)[1][[1]], proj)

################################################################################
# Reconstructed to 100 with plateperiod=FALSE
rec100_pp <- reconstruct(sfp, age=100, model=model, plateperiod=FALSE)


expect_inherits(rec100_pp, "sf")
expect_equal(nrow(rec100_pp), nrow(sfp))

# attributes match
# expect_equal(rownames(sfp), rownames(rec100_pp))
expect_equal(colnames(sfp), colnames(rec100_pp))


# should be the same on the matching interval
keep <- which(!is.na(recDF[,"lng"]))
expect_equivalent(rec100, rec100_pp[keep, ]) # rownames differ!

# expect the input CRS
expect_equal(st_crs(rec100_pp)[1][[1]], proj)

################################################################################
# Reconstructed to c(0,100) with listout
rec <- reconstruct(sfp, age=c(0,100), listout=TRUE,  model=model, plateperiod=TRUE)


# high-level attributes
expect_inherits(rec, "list")
expect_equal(names(rec), c("0", "100"))

# exact match 
expect_equal(rec[[1]], rec0)
expect_equal(rec[[2]], rec100)

################################################################################
# Reconstructed to c(0,100) with listout=FALSE
expect_error(
	rec <- reconstruct(sfp, age=c(0,100), listout=FALSE,  model=model, plateperiod=TRUE)
)

########################################----------------------------------------
# Additional arguments

################################################################################
# Reconstructed to c(0,100) with gmeta=TRUE
rec100_pp_meta <- reconstruct(sfp, age=100, model=model, plateperiod=FALSE, gmeta=TRUE)


expect_equal(rec100_pp, rec100_pp_meta[, -(1:4)])

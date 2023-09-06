library(rgplates)

# the used objects
model <- chronosphere::fetch("paleomap", "model", datadir=file.path(wd, "data/chronosphere"), verbose=FALSE)

# example 
admin <- st_read(file.path(wd,"data/Paleomap/paleomap_adminlines_19o/admin.shx"), quiet=TRUE)

################################################################################
# 2. Explicit WGS 84 CRS
proj <- st_crs(admin)[1][[1]]

################################################################################
# Reconstructed to 0
rec0 <- reconstruct(admin, age=0, model=model)


# should return a data.frame? - no!
expect_inherits(rec0, "sf")

# expect the input CRS
expect_equal(st_crs(rec0)[1][[1]], proj)

## ################################################################################
# Reconstructed to 100 
rec100 <- reconstruct(admin, age=100, model=model, plateperiod=TRUE)



expect_inherits(rec100, "sf")

# attributes match
# expect_equal(rownames(dmat), rownames(rec100))
expect_equal(colnames(rec0), colnames(rec100))

# expect the input CRS
expect_equal(st_crs(rec100)[1][[1]], proj)

################################################################################
# Reconstructed to 100 with plateperiod=FALSE
rec100_pp <- reconstruct(admin, age=100, model=model, plateperiod=FALSE)


expect_inherits(rec100_pp, "sf")

# attributes match
# expect_equal(rownames(sfp), rownames(rec100_pp))
expect_equal(colnames(rec0), colnames(rec100_pp))


# expect the input CRS
expect_equal(st_crs(rec100_pp)[1][[1]], proj)

################################################################################
# Reconstructed to c(0,100) with listout
rec <- reconstruct(admin, age=c(0,100), listout=TRUE,  model=model, plateperiod=TRUE)


# high-level attributes
expect_inherits(rec, "list")
expect_equal(names(rec), c("0", "100"))

# exact match 
expect_equal(rec[[1]], rec0)
expect_equal(rec[[2]], rec100)

################################################################################
# Reconstructed to c(0,100) with listout=FALSE
expect_error(
	rec <- reconstruct(admin, age=c(0,100), listout=FALSE,  model=model, plateperiod=TRUE)
)











################################################################################
# 3. Repeat with a different CRS
proj <- "ESRI:54009"
admin <- st_transform(admin, proj)

################################################################################
# Reconstructed to 0
rec0 <- reconstruct(admin, age=0, model=model)


# should return a data.frame? - no!
expect_inherits(rec0, "sf")

# expect the input CRS
expect_equal(st_crs(rec0)[1][[1]], proj)

## ################################################################################
# Reconstructed to 100 
rec100 <- reconstruct(admin, age=100, model=model, plateperiod=TRUE)



expect_inherits(rec100, "sf")

# attributes match
# expect_equal(rownames(dmat), rownames(rec100))
expect_equal(colnames(rec0), colnames(rec100))

# expect the input CRS
expect_equal(st_crs(rec100)[1][[1]], proj)

################################################################################
# Reconstructed to 100 with plateperiod=FALSE
rec100_pp <- reconstruct(admin, age=100, model=model, plateperiod=FALSE)


expect_inherits(rec100_pp, "sf")

# attributes match
# expect_equal(rownames(sfp), rownames(rec100_pp))
expect_equal(colnames(rec0), colnames(rec100_pp))


# expect the input CRS
expect_equal(st_crs(rec100_pp)[1][[1]], proj)

################################################################################
# Reconstructed to c(0,100) with listout
rec <- reconstruct(admin, age=c(0,100), listout=TRUE,  model=model, plateperiod=TRUE)


# high-level attributes
expect_inherits(rec, "list")
expect_equal(names(rec), c("0", "100"))

# exact match 
expect_equal(rec[[1]], rec0)
expect_equal(rec[[2]], rec100)

################################################################################
# Reconstructed to c(0,100) with listout=FALSE
expect_error(
	rec <- reconstruct(admin, age=c(0,100), listout=FALSE,  model=model, plateperiod=TRUE)
)


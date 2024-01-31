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

colnames(dmat) <-c("col1", "col2")
rownames(dmat) <-paste0("row", 1:nrow(dmat))
	

################################################################################
# Reconstructed to 0
rec0 <- reconstruct(dmat, age=0, model=model)

# should return a data.frame? - no!
expect_inherits(rec0, "matrix")
expect_equal(nrow(rec0), nrow(dmat))

# attributes match
expect_equal(rownames(dmat), rownames(rec0))
expect_equal(c("long", "lat"), colnames(rec0))


## ################################################################################
# Reconstructed to 100 
rec100 <- reconstruct(dmat, age=100, model=model, plateperiod=TRUE)

expect_inherits(rec100, "matrix")
expect_equal(nrow(rec100), nrow(dmat))

# attributes match
expect_equal(rownames(dmat), rownames(rec100))
expect_equal(c("paleolong", "paleolat"), colnames(rec100))

# the invalid missing values
expect_equivalent(rec100[1,], as.numeric(c(NA, NA)))
expect_equivalent(rec100[5,], as.numeric(c(NA, NA)))
expect_equivalent(rec100[8,], as.numeric(c(NA, NA)))


################################################################################
# Reconstructed to 100 with plateperiod=FALSE
rec100_pp <- reconstruct(dmat, age=100, model=model, plateperiod=FALSE)

expect_inherits(rec100_pp, "matrix")
expect_equal(nrow(rec100_pp), nrow(dmat))

# should be the same on the matching interval
keep <- which(!is.na(rec100[,1]))
expect_equivalent(rec100[keep,], rec100_pp[keep, ])

# attributes match
expect_equal(rownames(dmat), rownames(rec100_pp))
expect_equal(c("paleolong", "paleolat"), colnames(rec100_pp))

# the incorrect coordinates are there!
expect_equivalent(is.na(rec100_pp[1,]), c(FALSE, FALSE))
expect_equivalent(is.na(rec100_pp[5,]), c(FALSE, FALSE))
expect_equivalent(is.na(rec100_pp[8,]), c(FALSE, FALSE))

################################################################################
# Reconstructed to c(0,100) with listout
rec <- reconstruct(dmat, age=c(0,100), listout=TRUE,  model=model, plateperiod=TRUE)

# high-level attributes
expect_inherits(rec, "list")
expect_equal(names(rec), c("0", "100"))

# exact match 
expect_equal(rec[[1]], rec0)
expect_equal(rec[[2]], rec100)


################################################################################
# Reconstructed to c(0,100) with listout=FALSE
rec <- reconstruct(dmat, age=c(0,100), listout=FALSE,  model=model, plateperiod=TRUE)

expect_inherits(rec, "array")
expect_equal(dim(rec), c(2, 8, 2))
expect_equivalent(rec[1,,], rec0)
expect_equivalent(rec[2,,], rec100)



library(rgplates)

# the used objects
model <- chronosphere::fetch("paleomap", "model", datadir=file.path(wd, "data/chronosphere"), verbose=FALSE)

# example matrix
mat <- matrix(c(                
  -27.44, 26.07,
  3.53, 25.44,
  16.53, 26.71,
  12.2, 45.01,
  -45.4, 43.12,
  24.58, 58.9,
  -30.53, 72.79,
  -29.29, -28.85
), ncol=2, byrow=TRUE) 

# include missing values
matMiss <- mat
matMiss[c(1, 3, 8), ] <- NA
notMiss <- which(!is.na(matMiss[,1]))

################################################################################
# Reconstructed to 0
rec0 <- reconstruct(mat, age=0, model=model)


expect_inherits(rec0, "matrix")
expect_equal(nrow(rec0), nrow(mat))

# attributes match
expect_equal(rownames(mat), rownames(rec0))
expect_equal(c("long", "lat"), colnames(rec0))

################################################################################
# Reconstructed to 0 with missing
expect_silent(
	rec0miss <- reconstruct(matMiss, age=0, model=model)
)

expect_inherits(rec0miss, "matrix")
expect_equal(nrow(rec0miss), nrow(matMiss))

################################################################################
# Reconstructed to 100 
expect_silent(
	rec100 <- reconstruct(mat, age=100, model=model, validtime=TRUE)
)


expect_inherits(rec100, "matrix")
expect_equal(nrow(rec100), nrow(mat))

# attributes match
expect_equal(rownames(mat), rownames(rec100))
expect_equal(c("paleolong", "paleolat"), colnames(rec100))

# the invalid missing values
expect_equivalent(rec100[1,], as.numeric(c(NA, NA)))
expect_equivalent(rec100[5,], as.numeric(c(NA, NA)))
expect_equivalent(rec100[8,], as.numeric(c(NA, NA)))


# same with deprecated plateperiod
expect_warning(
	rec100oldpp <- reconstruct(mat, age=100, model=model, plateperiod=TRUE)
)
expect_identical(rec100, rec100oldpp)

################################################################################
# Reconstructed to 100 with validtime=FALSE
expect_silent(
	rec100_pp <- reconstruct(mat, age=100, model=model, validtime=FALSE)
)


expect_inherits(rec100_pp, "matrix")
expect_equal(nrow(rec100_pp), nrow(mat))

# attributes match
expect_equal(rownames(mat), rownames(rec100_pp))
expect_equal(c("paleolong", "paleolat"), colnames(rec100_pp))

# should be the same on the matching interval
keep <- which(!is.na(rec100[,1]))
expect_equivalent(rec100[keep,], rec100_pp[keep, ])

# the incorrect coordinates are there!
expect_equivalent(is.na(rec100_pp[1,]), c(FALSE, FALSE))
expect_equivalent(is.na(rec100_pp[5,]), c(FALSE, FALSE))
expect_equivalent(is.na(rec100_pp[8,]), c(FALSE, FALSE))

# same with deprecated plateperiod
expect_warning(
	rec100_ppOldpp <- reconstruct(mat, age=100, model=model, plateperiod=FALSE)
)

expect_identical(rec100_pp, rec100_ppOldpp)

################################################################################
# Reconstructed to 140 with validtime=FALSE, - single coordinates

# provides error without exception
expect_silent(recOne <- reconstruct(mat[1,, drop=FALSE ], age=140, model=model, validtime=TRUE))

################################################################################
# Reconstructed to c(0,100) with listout
expect_silent(
	rec <- reconstruct(mat, age=c(0,100), listout=TRUE,  model=model, validtime=TRUE)
)


expect_inherits(rec, "list")
expect_equal(names(rec), c("0", "100"))

expect_equal(rec[[1]], rec0)
expect_equal(rec[[2]], rec100)


################################################################################
# Reconstructed to c(0,100) with listout=FALSE
expect_silent(
	rec <- reconstruct(mat, age=c(0,100), listout=FALSE,  model=model, validtime=TRUE)
)


expect_inherits(rec, "array")
expect_equal(dim(rec), c(2, 8, 2))
expect_equivalent(rec[1,,], rec0)
expect_equivalent(rec[2,,], rec100)


################################################################################
# ENUMERATE=FALSE
# more target than needed with enumerate
target <- rep(1:nrow(mat)/2, each=2)*2
expect_warning(rec <- reconstruct(mat, age=target, listout=FALSE,  model=model, validtime=TRUE, enumerate=FALSE))

# Enumerate=FALSE, validtime=TRUE
target <- rep(1:(nrow(mat)/2), each=2)*2
expect_silent(recEnPP <- reconstruct(mat, age=target, listout=FALSE,  model=model, validtime=TRUE, enumerate=FALSE))
expect_equivalent(recEnPP[1,], as.numeric(c(NA, NA)))
expect_equivalent(recEnPP[5,], as.numeric(c(NA, NA)))
expect_equivalent(recEnPP[8,], as.numeric(c(NA, NA)))

# Enumerate=FALSE, validtime=FALSE
expect_silent(recEn <- reconstruct(mat, age=target, listout=FALSE,  model=model, validtime=FALSE, enumerate=FALSE))
keep <- which(!is.na(recEnPP[,1]))
expect_equivalent(recEn[keep,], recEnPP[keep, ])

# missing ages (beginning)
targetMiss <- target
targetMiss[c(1:2)] <- NA
expect_silent(recEnMiss <- reconstruct(mat, age=targetMiss, listout=FALSE,  model=model, validtime=FALSE, enumerate=FALSE))
keep <- which(!is.na(recEnMiss[,1]))
expect_equivalent(recEn[keep,], recEnMiss[keep, ])

# missing ages (middle)
targetMiss <- target
targetMiss[c(3:4)] <- NA
expect_silent(recEnMiss <- reconstruct(mat, age=targetMiss, listout=FALSE,  model=model, validtime=FALSE, enumerate=FALSE))
keep <- which(!is.na(recEnMiss[,1]))
expect_equivalent(recEn[keep,], recEnMiss[keep, ])




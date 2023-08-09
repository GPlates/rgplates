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

################################################################################
# Reconstructed to 0
rec0 <- reconstruct(mat, age=0, model=model)


expect_inherits(rec0, "matrix")
expect_equal(nrow(rec0), nrow(mat))

# attributes match
expect_equal(rownames(mat), rownames(rec0))
expect_equal(colnames(mat), colnames(rec0))

################################################################################
# Reconstructed to 100 
rec100 <- reconstruct(mat, age=100, model=model, plateperiod=TRUE)


expect_inherits(rec100, "matrix")
expect_equal(nrow(rec100), nrow(mat))

# attributes match
expect_equal(rownames(mat), rownames(rec100))
expect_equal(colnames(mat), colnames(rec100))

# the invalid missing values
expect_equivalent(rec100[1,], as.numeric(c(NA, NA)))
expect_equivalent(rec100[5,], as.numeric(c(NA, NA)))
expect_equivalent(rec100[8,], as.numeric(c(NA, NA)))


################################################################################
# Reconstructed to 100 with plateperiod=FALSE
rec100_pp <- reconstruct(mat, age=100, model=model, plateperiod=FALSE)


expect_inherits(rec100_pp, "matrix")
expect_equal(nrow(rec100_pp), nrow(mat))

# attributes match
expect_equal(rownames(mat), rownames(rec100_pp))
expect_equal(colnames(mat), colnames(rec100_pp))

# should be the same on the matching interval
keep <- which(!is.na(rec100[,1]))
expect_equivalent(rec100[keep,], rec100_pp[keep, ])

# the incorrect coordinates are there!
expect_equivalent(is.na(rec100_pp[1,]), c(FALSE, FALSE))
expect_equivalent(is.na(rec100_pp[5,]), c(FALSE, FALSE))
expect_equivalent(is.na(rec100_pp[8,]), c(FALSE, FALSE))

################################################################################
# Reconstructed to 140 with plateperiod=FALSE, - single coordinates

# provides error without exception
expect_silent(recOne <- reconstruct(mat[1,, drop=FALSE ], age=140, model=model, plateperiod=TRUE))

################################################################################
# Reconstructed to c(0,100) with listout
rec <- reconstruct(mat, age=c(0,100), listout=TRUE,  model=model, plateperiod=TRUE)


expect_inherits(rec, "list")
expect_equal(names(rec), c("0", "100"))

expect_equal(rec[[1]], rec0)
expect_equal(rec[[2]], rec100)


################################################################################
# Reconstructed to c(0,100) with listout=FALSE
rec <- reconstruct(mat, age=c(0,100), listout=FALSE,  model=model, plateperiod=TRUE)


expect_inherits(rec, "array")
expect_equal(dim(rec), c(2, 8, 2))
expect_equivalent(rec[1,,], rec0)
expect_equivalent(rec[2,,], rec100)


################################################################################
# Missing values
mat_na <- mat
mat_na[c(1, 3), ] <-NA

# to be fixed
expect_error(
	rec100 <- reconstruct(mat_na, age=100, model=model)
)


################################################################################
# ENUMERATE=FALSE
# more target than needed with enumerate
target <- rep(1:nrow(mat)/2, each=2)*2
expect_warning(rec <- reconstruct(mat, age=target, listout=FALSE,  model=model, plateperiod=TRUE, enumerate=FALSE))

# Enumerate=FALSE, platePeriod=TRUE
target <- rep(1:(nrow(mat)/2), each=2)*2
expect_silent(recEnPP <- reconstruct(mat, age=target, listout=FALSE,  model=model, plateperiod=TRUE, enumerate=FALSE))
expect_equivalent(recEnPP[1,], as.numeric(c(NA, NA)))
expect_equivalent(recEnPP[5,], as.numeric(c(NA, NA)))
expect_equivalent(recEnPP[8,], as.numeric(c(NA, NA)))

# Enumerate=FALSE, platePeriod=FALSE
expect_silent(recEn <- reconstruct(mat, age=target, listout=FALSE,  model=model, plateperiod=FALSE, enumerate=FALSE))
keep <- which(!is.na(recEnPP[,1]))
expect_equivalent(recEn[keep,], recEnPP[keep, ])

# missing ages (beginning)
targetMiss <- target
targetMiss[c(1:2)] <- NA
expect_silent(recEnMiss <- reconstruct(mat, age=targetMiss, listout=FALSE,  model=model, plateperiod=FALSE, enumerate=FALSE))
keep <- which(!is.na(recEnMiss[,1]))
expect_equivalent(recEn[keep,], recEnMiss[keep, ])

# missing ages (middle)
targetMiss <- target
targetMiss[c(3:4)] <- NA
expect_silent(recEnMiss <- reconstruct(mat, age=targetMiss, listout=FALSE,  model=model, plateperiod=FALSE, enumerate=FALSE))
keep <- which(!is.na(recEnMiss[,1]))
expect_equivalent(recEn[keep,], recEnMiss[keep, ])




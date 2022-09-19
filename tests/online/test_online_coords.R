library(rgplates)
options(timeout = 5*60)

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
rec0 <- reconstruct(mat, age=0, model="PALEOMAP")

expect_inherits(rec0, "matrix")
expect_equal(nrow(rec0), nrow(mat))


################################################################################
# Reconstructed to 100 
expect_silent(
	rec100 <- reconstruct(mat, age=100, model="PALEOMAP")
)

expect_inherits(rec100, "matrix")
expect_equal(nrow(rec100), nrow(mat))

expect_equal(colnames(rec100), c("paleolong", "paleolat"))

# the invalid missing values
expect_equivalent(rec100[1,], as.numeric(c(NA, NA)))
expect_equivalent(rec100[5,], as.numeric(c(NA, NA)))
expect_equivalent(rec100[8,], as.numeric(c(NA, NA)))


################################################################################
# Reconstructed to c(0,100) with listout
expect_silent(
	rec <- reconstruct(mat, age=c(0,100), listout=TRUE,  model="PALEOMAP")
)

expect_inherits(rec, "list")
expect_equal(names(rec), c("0", "100"))

expect_equal(rec[[1]], rec0)
expect_equal(rec[[2]], rec100)

expect_equal(colnames(rec100), c("paleolong", "paleolat"))

################################################################################
# Reconstructed to c(0,100) with listout=FALSE
expect_silent(
	rec <- reconstruct(mat, age=c(0,100), listout=FALSE,  model="PALEOMAP")
)

expect_inherits(rec, "array")
expect_equal(dim(rec), c(2, 8, 2))
expect_equal(rec[1,,], rec0)
expect_equal(rec[2,,], rec100)


################################################################################
# Missing values
mat_na <- mat
mat_na[c(1, 3), ] <-NA

# to be fixed
expect_error(
	rec100 <- reconstruct(mat_na, age=100, model="PALEOMAP")
)

################################################################################
# Test reversal
keep <- which(!is.na(rec100[,1]))
expect_silent(
	rec100_rev <- reconstruct(rec100[keep, ], age=100, model="PALEOMAP", reverse=TRUE)
)

# approximately the same
expect_true(1e-3>abs(sum(rec100_rev- mat[keep,])))


expect_equal(colnames(rec100_rev), c("long", "lat"))

################################################################################
# Test inaccurate ages! - only rounded ones are allowed
# Reconstructed to 100.4 
expect_message(
	rec100_4 <- reconstruct(mat, age=100.4, model="PALEOMAP")
)	

expect_equal(rec100_4, rec100)



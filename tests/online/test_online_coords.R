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
# Test inaccurate ages! - now rounded ones are also allowed!
# Reconstructed to 100.4 
expect_silent(
	rec100_4 <- reconstruct(mat, age=100.4, model="PALEOMAP")
)	



################################################################################
# Simple reversal - forward rotation
################################################################################
# reverse + age
keep <- which(!is.na(rec100[,1]))
expect_silent(
	rec100_rev <- reconstruct(rec100[keep, ], age=100, model="PALEOMAP", reverse=TRUE)
)

# approximately the same
expect_true(1e-3>abs(sum(rec100_rev- mat[keep,])))

expect_equal(colnames(rec100_rev), c("long", "lat"))

########################################----------------------------------------
# reverse + from  - NOT ALLOWED!
keep <- which(!is.na(rec100[,1]))
expect_error(
	reconstruct(rec100[keep, ], from=100, model="PALEOMAP", reverse=TRUE)
)

########################################----------------------------------------
# from  - should be identical to previous 
expect_silent(
	rec100re <- reconstruct(rec100[keep, ], from=100, model="PALEOMAP")
)

expect_identical(rec100_rev, rec100re)



########################################----------------------------------------
# Multiple from arguments are not allowed 
expect_error(
	reconstruct(rec100[keep, ], from=c(100,4), model="PALEOMAP")
)

########################################----------------------------------------
# reversal with explicit NAs
expect_error(
	rec100reNA <- reconstruct(rec100, from=100, model="PALEOMAP")
)

########################################----------------------------------------
# off plate coordinates  - requires manual testing!
expect_warning(
	from420 <- reconstruct(mat, from=420, model="MERDITH2021")
)

expect_equivalent(from420[1,], as.numeric(c(NA, NA)))
expect_equivalent(from420[4,], as.numeric(c(NA, NA)))
expect_equivalent(from420[5,], as.numeric(c(NA, NA)))
expect_equivalent(from420[6,], as.numeric(c(NA, NA)))
expect_equivalent(from420[7,], as.numeric(c(NA, NA)))
expect_equivalent(from420[8,], as.numeric(c(NA, NA)))

# check those that are available for backward reconcst
keep420 <- !is.na(from420[,1])

expect_silent(
	re420 <- reconstruct(from420[keep420,], age=420, model="MERDITH2021")
)

expect_true(1e-3>abs(sum(re420-mat[keep420,])))


################################################################################
# Combined reversal - forward rotation (from) and then backward rotation to (age)
################################################################################
# only valid values - single target age
expect_silent(
	rec95re <- reconstruct(rec100[keep, ], age=95, model="PALEOMAP", from=100)
)

# compare this with direct reconstruction
expect_silent(
	rec95 <- reconstruct(mat[keep, ], age=95, model="PALEOMAP")
)

# approximately the same
expect_true(1e-3>abs(sum(rec95re-rec95)))

expect_equal(colnames(rec95re), colnames(rec95))

########################################----------------------------------------
# only valid coordinates - Multiple valid ages - listout
expect_silent(
	rec95_90<- reconstruct(rec100[keep, ], age=c(95, 90), model="PALEOMAP", from=100, listout=TRUE)
)

expect_inherits(rec95_90, "list")
expect_equal(names(rec95_90), c("95", "90"))

# should be ok now
expect_equal(rec95_90[["95"]], rec95re)





# Stress test - 10000 coords? 
# enumerate with PaleobioDB

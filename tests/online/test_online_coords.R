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

# include missing values
matMiss <- mat
matMiss[c(1, 3, 8), ] <- NA
notMiss <- which(!is.na(matMiss[,1]))

################################################################################
# Reconstructed to 0
expect_silent(
	rec0 <- reconstruct(mat, age=0, model="PALEOMAP")
)

expect_inherits(rec0, "matrix")
expect_equal(nrow(rec0), nrow(mat))


################################################################################
# Reconstructed to 0 with missing
expect_silent(
	rec0miss <- reconstruct(matMiss, age=0, model="PALEOMAP")
)

expect_inherits(rec0miss, "matrix")
expect_equal(nrow(rec0miss), nrow(matMiss))

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


# the same with single coordinates
# Missing values
expect_silent(
	rec100singleMiss <- reconstruct(mat[1,, drop=FALSE ], age=100, model="PALEOMAP")
)
expect_equivalent(as.numeric(rec100singleMiss), as.numeric(c(NA, NA)))

# proper values 
expect_silent(
	rec100single <- reconstruct(mat[2,, drop=FALSE ], age=100, model="PALEOMAP")
)

# the same as what is returned by the bulk-method
expect_identical(rec100single, rec100[2, , drop=FALSE])

################################################################################
# Reconstructed to 100 with missing
expect_silent(
	rec100miss <- reconstruct(matMiss, age=100, model="PALEOMAP")
)

expect_inherits(rec100miss, "matrix")
expect_equal(nrow(rec100miss), nrow(mat))

expect_equal(colnames(rec100miss), c("paleolong", "paleolat"))

# the invalid missing values
expect_equivalent(rec100miss[1,], as.numeric(c(NA, NA)))
expect_equivalent(rec100miss[3,], as.numeric(c(NA, NA)))
expect_equivalent(rec100miss[5,], as.numeric(c(NA, NA)))
expect_equivalent(rec100miss[8,], as.numeric(c(NA, NA)))

# those that were kept should be the same, as if the data did not contain NAs
one <- rec100[notMiss,]
two <- rec100miss[notMiss, ]
expect_identical(one, two)

################################################################################
# Reconstructed to c(0,100) with listout
expect_silent(
	rec <- reconstruct(mat, age=c(0,100), listout=TRUE,  model="PALEOMAP")
)

expect_inherits(rec, "list")
expect_equal(names(rec), c("0", "100"))

expect_equal(rec[[1]], rec0)
expect_equal(rec[[2]], rec100)

expect_equal(colnames(rec[[2]]), c("paleolong", "paleolat"))

################################################################################
# Reconstructed to c(0,100) with listout - including missing values
expect_silent(
	rec <- reconstruct(matMiss, age=c(0,100), listout=TRUE,  model="PALEOMAP")
)

expect_inherits(rec, "list")
expect_equal(names(rec), c("0", "100"))

expect_equal(rec[[1]], rec0miss)
expect_equal(rec[[2]], rec100miss)

expect_equal(colnames(rec[[2]]), c("paleolong", "paleolat"))

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
# Reconstructed to c(0,100) with listout=FALSE -with missing
expect_silent(
	rec <- reconstruct(matMiss, age=c(0,100), listout=FALSE,  model="PALEOMAP")
)

expect_inherits(rec, "array")
expect_equal(dim(rec), c(2, 8, 2))
expect_equal(rec[1,,], rec0miss)
expect_equal(rec[2,,], rec100miss)


################################################################################
# Test inaccurate ages! - now rounded ones are also allowed!
# Reconstructed to 100.4 
expect_silent(
	rec100_4 <- reconstruct(mat, age=100.4, model="PALEOMAP")
)	



################################################################################
# Test inaccurate ages! - now rounded ones are also allowed! - with issing vals
# Reconstructed to 100.4 
expect_silent(
	rec100_4miss <- reconstruct(matMiss, age=100.4, model="PALEOMAP")
)	

################################################################################
# Simple reversal - forward rotation
################################################################################
# reverse + age - no missing values inside
keep <- which(!is.na(rec100[,1]))
expect_silent(
	rec100_rev <- reconstruct(rec100[keep, ], age=100, model="PALEOMAP", reverse=TRUE)
)

# approximately the same as what they were
expect_true(1e-3>abs(sum(rec100_rev- mat[keep,])))
expect_equal(colnames(rec100_rev), c("long", "lat"))

# reverse + age - with missing values 
expect_silent(
	rec100_revMiss <- reconstruct(rec100, age=100, model="PALEOMAP", reverse=TRUE)
)

# should be exactly the same as what it was without the missing values
one <- rec100_revMiss[keep,]
expect_identical(one, rec100_rev)

########################################----------------------------------------
# reverse + from  - NOT ALLOWED!
expect_error(
	reconstruct(rec100, from=100, model="PALEOMAP", reverse=TRUE)
)

########################################----------------------------------------
# from  - should be identical to previous 
expect_silent(
	rec100re <- reconstruct(rec100, from=100, model="PALEOMAP")
)

expect_identical(rec100_revMiss, rec100re)



########################################----------------------------------------
# Multiple from arguments are not allowed 
expect_error(
	reconstruct(rec100[keep, ], from=c(100,4), model="PALEOMAP")
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

# check those that are available for backward reconcst for comparison
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
# all values - relying on missing value propagation
expect_silent(
	rec95reMiss <- reconstruct(rec100, age=95, model="PALEOMAP", from=100)
)

expect_true(1e-3>abs(sum(rec95reMiss[keep,]-rec95)))

########################################----------------------------------------
# only valid coordinates - Multiple valid ages - listout
expect_silent(
	rec95_90<- reconstruct(rec100[keep, ], age=c(95, 90), model="PALEOMAP", from=100, listout=TRUE)
)

expect_inherits(rec95_90, "list")
expect_equal(names(rec95_90), c("95", "90"))

# should be ok now
expect_equal(rec95_90[["95"]], rec95re)

########################################----------------------------------------
#  valid coordinates Missing - Multiple valid ages - listout
expect_silent(
	rec95_90missing<- reconstruct(rec100, age=c(95, 90), model="PALEOMAP", from=100, listout=TRUE)
)

expect_inherits(rec95_90, "list")
expect_equal(names(rec95_90), c("95", "90"))

# should be ok now
expect_equal(rec95_90missing[["95"]], rec95reMiss)



################################################################################
# Enumerate
ages <- c(10,10,15,15,20,25,25,30 )

expect_silent(
	recEnum <- reconstruct(mat, age=ages, model="PALEOMAP", enumerate=FALSE)
)

una <- unique(ages)
for(i in 1:length(una)){
	tempRec <- reconstruct(mat[una[i]==ages,, drop=FALSE], age=una[i], model="PALEOMAP")
	bitRec <- recEnum[una[i]==ages,, drop=FALSE]
	expect_equal(tempRec, bitRec)
}


########################################----------------------------------------
# Enumerate - with missing
expect_silent(
	recEnumMiss <- reconstruct(matMiss, age=ages, model="PALEOMAP", enumerate=FALSE)
)

# should be the same as without - based on the missing 
one <- recEnumMiss[notMiss,]
two <- recEnum[notMiss,]
expect_equal(one, two)


########################################----------------------------------------
# Enumerate - with missing : two way reconstruction

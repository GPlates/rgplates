################################################################################
# Stress test - 10000 coords? 

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

expect_silent(
	rec100 <- reconstruct(mat, age=100, model="PALEOMAP")
)

########################################----------------------------------------
# 2-times original
matPow1 <- rbind(mat, mat)

expect_silent(
	matPow1rec <- reconstruct(matPow1, age=100, model="PALEOMAP")
)

matPow1recBound <- rbind(rec100, rec100)
expect_identical(matPow1rec, matPow1recBound)


########################################----------------------------------------
# 4-times original
matPow2 <- rbind(matPow1, matPow1)

expect_silent(
	matPow2rec <- reconstruct(matPow2, age=100, model="PALEOMAP")
)

matPow2recBound <- rbind(matPow1recBound, matPow1recBound)
expect_identical(matPow2rec, matPow2recBound)


########################################----------------------------------------
# 8-times original
matPow3 <- rbind(matPow2, matPow2)

expect_silent(
	matPow3rec <- reconstruct(matPow3, age=100, model="PALEOMAP")
)

matPow3recBound <- rbind(matPow2recBound, matPow2recBound)
expect_identical(matPow3rec, matPow3recBound)

########################################----------------------------------------
# 16-times original
matPow4 <- rbind(matPow3, matPow3)

expect_silent(
	matPow4rec <- reconstruct(matPow4, age=100, model="PALEOMAP")
)

matPow4recBound <- rbind(matPow3recBound, matPow3recBound)
expect_identical(matPow4rec, matPow4recBound)

########################################----------------------------------------
# 32-times original
matPow5 <- rbind(matPow4, matPow4)

expect_silent(
	matPow5rec <- reconstruct(matPow5, age=100, model="PALEOMAP")
)

matPow5recBound <- rbind(matPow4recBound, matPow4recBound)
expect_identical(matPow5rec, matPow5recBound)

########################################----------------------------------------
# 64-times original
matPow6 <- rbind(matPow5, matPow5)

expect_silent(
	matPow6rec <- reconstruct(matPow6, age=100, model="PALEOMAP")
)

matPow6recBound <- rbind(matPow5recBound, matPow5recBound)
expect_identical(matPow6rec, matPow6recBound)

########################################----------------------------------------
# 128-times original
matPow7 <- rbind(matPow6, matPow6)

expect_silent(
	matPow7rec <- reconstruct(matPow7, age=100, model="PALEOMAP")
)

matPow7recBound <- rbind(matPow6recBound, matPow6recBound)
expect_identical(matPow7rec, matPow7recBound)

########################################----------------------------------------
# 256-times original
matPow8 <- rbind(matPow7, matPow7)

expect_silent(
	matPow8rec <- reconstruct(matPow8, age=100, model="PALEOMAP")
)

matPow8recBound <- rbind(matPow7recBound, matPow7recBound)
expect_identical(matPow8rec, matPow8recBound)

########################################----------------------------------------
# 512-times original
matPow9 <- rbind(matPow8, matPow8)

expect_silent(
	matPow9rec <- reconstruct(matPow9, age=100, model="PALEOMAP")
)

matPow9recBound <- rbind(matPow8recBound, matPow8recBound)
expect_identical(matPow9rec, matPow9recBound)

########################################----------------------------------------
# 1024-times original
matPow10 <- rbind(matPow9, matPow9)

expect_silent(
	matPow10rec <- reconstruct(matPow10, age=100, model="PALEOMAP")
)

matPow10recBound <- rbind(matPow9recBound, matPow9recBound)
expect_identical(matPow10rec, matPow10recBound)


########################################----------------------------------------
# 2048-times original
matPow11 <- rbind(matPow10, matPow10)

expect_silent(
	matPow11rec <- reconstruct(matPow11, age=100, model="PALEOMAP")
)

matPow11recBound <- rbind(matPow10recBound, matPow10recBound)
expect_identical(matPow11rec, matPow11recBound)

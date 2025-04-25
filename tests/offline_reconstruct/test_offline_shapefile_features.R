# working directory of test
setwd(wd)
#setwd("/mnt/sky/Dropbox/Software/rgplates/")

# ensure presence
library(rgplates)
library(tinytest)


# Make a model to show how it works
rot <- "data/Zahirovic_etal_2022_GDJ/CombinedRotations.rot"

# the static features
stat <- "data/Zahirovic_etal_2022_GDJ/StaticGeometries/StaticPolygons/Global_EarthByte_GPlates_PresentDay_StaticPlatePolygons.shp"
coast <- "data/Zahirovic_etal_2022_GDJ/StaticGeometries/Coastlines/Global_coastlines_low_res.shp"

# basics
feat<- c("static_polygons"=stat, "coastlines"=coast)

expect_silent(original <- platemodel(
	features=feat,
	rotation=rot
))


# depending on the operatióng system
os <- rgplates:::getOS()


################################################################################
# 1. MANUAL CONVERSION
################################################################################

# developing a function to replace do the assignment and replace this with
x <- original@features["static_polygons"]
y <- original@features["coastlines"]

# this should run fine
expect_true(rgplates:::isShapefile(x))

# where to do the manual reconstruction
theDir <- file.path(tempdir(), "newgpml")

# For partitioning
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




# for unix-like
if(os=="linux"){
	# conversion
	expect_silent(newpath <- rgplates:::shp_to_gpml(x,  dir=theDir, gplatesExecutable="gplates",
		winin=FALSE, winout=FALSE, verbose=FALSE))

	# the file was actually created
	# newfile:
	newfile <- unlist(lapply(strsplit(gsub("\\.shp$", ".gpml",x), "/"), function(x) x[length(x)]))
	expect_true(newfile%in%list.files(theDir))

	# create platemodel from this
	expect_silent(
		mod <- platemodel(
			features=c("static_polygons"=newpath),
			rotation=rot
		)
	)

	# works
	expect_silent(res <- reconstruct("static_polygons", age=40, model=mod))

	# point reconstruction
	expect_silent(respoints <- reconstruct(dmat, age=40, model=mod))

	# admin
	expect_silent(newpath2 <- rgplates:::shp_to_gpml(y,  dir=theDir, gplatesExecutable="gplates",
		winin=FALSE, winout=FALSE, verbose=FALSE))


}

# for unix-like
if(os=="osx"){
	mac <- paste(rgplates:::macDefaultGplates(), collapse="/")
	# conversion
	expect_silent(newpath <- rgplates:::shp_to_gpml(x,  dir=theDir, gplatesExecutable=mac,
		winin=FALSE, winout=FALSE, verbose=FALSE))

	# the file was actually created
	# newfile:
	newfile <- unlist(lapply(strsplit(gsub("\\.shp$", ".gpml",x), "/"), function(x) x[length(x)]))
	expect_true(newfile%in%list.files(theDir))

	# create platemodel from this
	expect_silent(
		mod <- platemodel(
			features=c("static_polygons"=newpath),
			rotation=rot
		)
	)

	# works
	expect_silent(res <- reconstruct("static_polygons", age=40, model=mod))

	# point reconstruction
	expect_silent(respoints <- reconstruct(dmat, age=40, model=mod))

	# admin
	expect_silent(newpath2 <- rgplates:::shp_to_gpml(y,  dir=theDir, gplatesExecutable=mac,
		winin=FALSE, winout=FALSE, verbose=FALSE))


}


# windows
if(os=="windows"){
	x <- gsub("/","\\\\", x)

	# executable
	win <- rgplates:::winDefaultGPlates()

	# the gplates executable in unix form
	gplates <- paste(win, collapse="/")
	gplatesWin <- gsub("/","\\\\", gplates)

	# conversion
	newpathWin <- rgplates:::shp_to_gpml(x,  dir=theDir, gplatesExecutable=paste0('\"',gplatesWin,'\"'),
		winin=TRUE, winout=FALSE, verbose=FALSE)

	# the new file's position
	newfile <- unlist(lapply(strsplit(gsub("\\.shp$", ".gpml",x), "\\\\"), function(x) x[length(x)]))
	expect_true(newfile%in%list.files(theDir))

	# create platemodel from this
	expect_silent(
		mod <- platemodel(
			features=c("static_polygons"=newpathWin),
			rotation=rot
		)
	)

	# works
	expect_silent(res <- reconstruct("static_polygons", age=40, model=mod))

	# point reconstruction
	expect_silent(respoints <- reconstruct(dmat, age=40, model=mod))

}

################################################################################
# 2. AUTOMATIC CONVERSION
################################################################################
# clean the temporary directory
unlink(tempdir(), recursive=TRUE, force=TRUE)
dir.create(tempdir(), showWarnings=FALSE)


# using the original model
expect_silent(static <- reconstruct("static_polygons", age=40, model=original))
expect_equal(static, res)


# second case
expect_silent(coast <- reconstruct("coastlines", age=40, model=original, verbose=FALSE))

################################################################################
# 3. As paritioning polygons
################################################################################

# clean the temporary directory
unlink(tempdir(), recursive=TRUE, force=TRUE)
dir.create(tempdir(), showWarnings=FALSE)

# seems to work ok
expect_silent(poin <- reconstruct(dmat, age=40, model=original))
expect_equal(poin, respoints)


# run across different OSs, full test suite!

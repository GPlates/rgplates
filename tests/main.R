# ERGYAPLÉCC/Erzsiplétsz

# Needed
## install.packages(c(NULL
## 	, "ncdf4"
## 	, "divDyn"
## 	, "chronosphere"
## 	, "tinytest"
## 	, "Rcpp"
## 	, "terra"
## 	, "via"
##  , "httr2"
##  , "geojsonsf"
##  , "sf"
##  , "knitr"
##  , "rmarkdown"
##  , "sp"
## ))


library(tinytest)
library(parallel)

# enforce correct names
library(chronosphere)
library(rgplates)

if(rgplates:::getOS()=="linux") wd <- file.path(Sys.getenv("Dropbox"), "Software/rgplates")
if(rgplates:::getOS()=="windows") wd <- file.path("D:/rgplates")
if(rgplates:::getOS()=="osx") wd <- file.path("~/Desktop/rgplates")

setwd(wd)

# make a cluster of 8
## cl <- parallel::makeCluster(4, outfile="")
## parallel::clusterCall(cl, source, "rgplates/tests/source.R")

# the offline bits
utilty <- run_test_dir("rgplates/tests/utility") # L
offline_reconstruct <- run_test_dir("rgplates/tests/offline_reconstruct") # L
platemodel <- run_test_dir("rgplates/tests/platemodels") #L
plotting <- run_test_dir("rgplates/tests/plotting") #L

# the online bits - run manually
	online_velocities <- run_test_dir("rgplates/tests/online_velocities") # L

#online recontstruct
	online_attachment <- run_test_file("rgplates/tests/online_reconstruct/test_online_attachment.R")
	online_coastlines<- run_test_file("rgplates/tests/online_reconstruct/test_online_coastlines.R")
	online_coords<- run_test_file("rgplates/tests/online_reconstruct/test_online_coords.R")
	online_coords_pbdb <- run_test_file("rgplates/tests/online_reconstruct/test_online_coords_pbdb.R")
	online_coords_stress <- run_test_file("rgplates/tests/online_reconstruct/test_online_coords_stress.R")
	online_gwscheck <- run_test_file("rgplates/tests/online_reconstruct/test_online_gwscheck.R")
	online_gwscheck_manual <- run_test_file("rgplates/tests/online_reconstruct/test_online_gwscheck_manual.R")
	online_gwstools <- run_test_file("rgplates/tests/online_reconstruct/test_online_gwstools.R")
	online_plates <- run_test_file("rgplates/tests/online_reconstruct/test_online_plates.R")
	online_rasters <- run_test_file("rgplates/tests/online_reconstruct/test_online_rasters.R")

# Finish
#stopCluster(cl)

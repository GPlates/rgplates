# ERGYAPLÉCC/Erzsiplétsz

# Needed
## install.packages(c(NULL
## 	, "ncdf4"
## 	, "rgdal"
## 	, "divDyn"
## 	, "chronosphere"
## 	, "tinytest"
## 	, "terra"
## 	, "via"
##  , "httr2"
##  , "geojsonsf"
##  , "sf"
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
cl <- parallel::makeCluster(4, outfile="")
parallel::clusterCall(cl, source, "rgplates/tests/source.R")

# the online 
online_reconstruct <- run_test_dir("rgplates/tests/online_reconstruct")
offline_reconstruct <- run_test_dir("rgplates/tests/offline_reconstruct")
utilty <- run_test_dir("rgplates/tests/utility")
platemodel <- run_test_dir("rgplates/tests/platemodels")
online_velocities <- run_test_dir("rgplates/tests/online_velocities")

# Finish
stopCluster(cl)



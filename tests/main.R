# ERGYAPLÉCC/Erzsiplétsz

# Needed
## install.packages(NULL
## 	, "ncdf4"
## 	, "rgdal"
## 	, "divDyn"
## 	, "chronosphere"
## 	, "tinytest"
## 	, "terra"
## 	, "via"
## )


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
online <- run_test_dir("rgplates/tests/online")
offline <- run_test_dir("rgplates/tests/offline")
utilty <- run_test_dir("rgplates/tests/utility")
platemodel <- run_test_dir("rgplates/tests/platemodels")

# Finish
stopCluster(cl)



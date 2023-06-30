library(tinytest)
library(parallel)

# enforce correct names
library(chronosphere)
library(rgplates)

wd <- file.path(Sys.getenv("WorkSpace"), "2021-04-28_rgplates")
setwd(wd)

# make a cluster of 8
cl <- parallel::makeCluster(8, outfile="")
parallel::clusterCall(cl, source, "rgplates/tests/source.R")

# the online 
online <- run_test_dir("rgplates/tests/online")
offline <- run_test_dir("rgplates/tests/offline")
utilty <- run_test_dir("rgplates/tests/utility")
platemodel <- run_test_dir("rgplates/tests/platemodels")

# Finish
stopCluster(cl)



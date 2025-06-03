library(rgplates)

if(rgplates:::getOS()=="osx"){
	dir <- "data"
}else{
	dir <- paste0(wd, "/data")

}

# compare the Torsvick and Cocks model with different anchoring
features<- c("static_polygons"="Torsvik_Cocks_2016/Torsvik_Cocks_2016_Terranes.gpml")

# initialize
expect_silent(model <- platemodel(rotation="Torsvik_Cocks_2016/Torsvik_Cocks_HybridRotationFile.rot", features=features))


# default (0)
expect_silent(def100offline <- reconstruct("static_polygons", age=100, model=model))
plot(def100offline$geometry)

# explicit 0 anchor
expect_silent(def100offline0 <- reconstruct("static_polygons", age=100, model=model, anchor=0))
expect_identical(def100offline, def100offline0)

# explicit 1 anchor - PMAG ref frame
expect_silent(def100offline1 <- reconstruct("static_polygons", age=100, model=model, anchor=1))
expect_false(identical(def100offline0, def100offline1))

# informal comparison - difference
## plot(def100offline0$geometry, col="#FF000044", border="#FF0000")
## plot(def100offline1$geometry, col="#00FF0044", border="#00FF00", add=TRUE)

# with online - should be practically identical
## def100online1 <- reconstruct("static_polygons", age=100, model="TorsvikCocks2017", anchor=1)
## plot(def100online1$geometry, col="#0000FF44", border="#0000FF")
## plot(def100offline1$geometry, col="#00FF0044", border="#00FF00", add=TRUE)

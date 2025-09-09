library(rgplates)

if(rgplates:::getOS()=="osx"){
	dir <- "data"
}else{
	dir <- paste0(wd, "/data")

}

# compare the Torsvick and Cocks model with different anchoring
features<- c("static_polygons"=file.path(dir,"Torsvik_Cocks_2016/Torsvik_Cocks_2016_Terranes.gpml"))

# initialize
expect_silent(model <- platemodel(rotation=file.path(dir,"Torsvik_Cocks_2016/Torsvik_Cocks_HybridRotationFile.rot"), features=features))


# default (0)
expect_silent(def300offline <- reconstruct("static_polygons", age=300, model=model))

# explicit 0 anchor
expect_silent(def300offline0 <- reconstruct("static_polygons", age=300, model=model, anchor=0))
expect_identical(def100offline, def100offline0)

# explicit 1 anchor - PMAG ref frame
expect_silent(def300offline1 <- reconstruct("static_polygons", age=300, model=model, anchor=1))
expect_false(identical(def300offline0, def300offline1))

# visual comparison - difference
## plot(def300offline0$geometry, col="#FF000044", border="#FF0000")
## plot(def300offline1$geometry, col="#00FF0044", border="#00FF00", add=TRUE)

# with online - should be practically identical
## def300online1 <- reconstruct("static_polygons", age=300, model="TorsvikCocks2017", anchor=1)
## plot(def300online1$geometry, col="#0000FF44", border="#0000FF")
## plot(def300offline1$geometry, col="#00FF0044", border="#00FF00", add=TRUE)

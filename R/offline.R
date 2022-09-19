###########################################################################
# Offline interface to Gplates


#Mac examples
#	library(chronosphere)
#	x<- fetch("pared", "public")[,c("longitude", "latitude")]
#	mo <- fetch("paleomap", "model")
#	reconstruct(x, age=10, model=mo, verbose=TRUE)
#reconstruct(x, age=10, model=mo, verbose=TRUE, path.gplates="/Users/Nick/Downloads/GPlates-2.2.0/gplates.app/Contents/MacOS/gplates")

reconstructGPlates <- function(x, age, model, path.gplates=NULL,dir=NULL, verbose=FALSE, cleanup=TRUE, plateperiod=FALSE, gmeta=FALSE){
	if(!inherits(model, "platemodel")) stop("You need a GPlates tectonic model for this method.")
	
    # 1. FIND GPlates
		# A. get operating system
		os <- getOS()

		# B. should the program look for a default path for gplates?
		if(is.null(path.gplates)){
			# depending on the os
			if(os=="linux"){
				# the GPlates executable itself
				gplatesExecutable <- "gplates"

				# what the user would have entered
				path.gplates <- gplatesExecutable

				# leave the model intact in the namespace (easier debug)
				rotation <- model@rotation
				platePolygons <- model@polygons

				# separator character between directories
				dirSep <- "/"

			}
			if(os=="windows"){
				# 1. find GPLATES exectutable if possible
				# directory and executable
				gplatesPaths <- winDefaultGPlates()
				#path to executable
				path.gplates <- paste(gplatesPaths, collapse="/")
				# system call to executable
				gplatesExecutable <- paste("\"", path.gplates, "\"", sep="")

				# 2. replace model paths with \\
				rotation <- gsub("/","\\\\", model@rotation)
				platePolygons <- gsub("/","\\\\", model@polygons)

				# characters to include directory 
				dirSep <- "\\\\"

			}
			if(os=="osx"){
				# the GPlates executable itself
				# default
				# gplatesExecutable <- "/Applications/GPlates-2.2.0/gplates.app/Contents/MacOS/gplates"
				gplatesPaths <- macDefaultGPlates()
				gplatesExecutable <- paste(gplatesPaths, collapse="/")
				
				# what the user would have entered
				path.gplates <- gplatesExecutable

				# leave the model intact in the namespace (easier debug)
				rotation <- model@rotation
				platePolygons <- model@polygons

				# separator character between directories
				dirSep <- "/"
				
			}

		# look for given path
		}else{
			# separate to form a length 2 vector
			gplatesExecutable <- path.gplates
			
			# leave the model intact in the namespace (easier debug)
			rotation <- model@rotation
			platePolygons <- model@polygons

			# separator character between directories
			dirSep <- "/"
			
			# windows needs special treatment
			if(os=="windows"){
			
				# system call to executable
				gplatesExecutable <- paste("\"", path.gplates, "\"", sep="")

				# 2. replace model paths with \\
				rotation <- gsub("/","\\\\", model@rotation)
				platePolygons <- gsub("/","\\\\", model@polygons)

				# characters to include directory 
				dirSep <- "\\\\"

			}
			
		}

		# C. one moretest whether gplates was detected or not
			gpTest <- testGPlates(gplatesExecutable, verbose=verbose)

			# if gplates is not present:
			if(!gpTest) stop(paste("The GPlates executable\n	\"", path.gplates,"\"\nwas not found.", sep=""))

	# 2. Setup reconstruction environment
		# folder where files will be executed
		if(is.null(dir)) tempd <- tempdir() else tempd <- dir

		# prepare x
		# create a SpatialPointsDataFrame from long-lat matrix

		originals <- NULL 
		if(inherits(x, "matrix")){
			originals <- x
			x <- as.data.frame(x)
		}
			
		# data.frame
		if(inherits(x, "data.frame") & !(inherits(x, "sf") | inherits(x, "Spatial"))){
			# the original
			if(is.null(originals)) originals <- x
			# enforce column names
			colnames(x) <- c("lng", "lat")
			x$ID <- paste0("a", 1:nrow(x))
			theID <- x$ID

			# transform to sf
			x <- sf::st_as_sf(x, coords=c("lng", "lat"))
		}
		
	
		# spatial original, enforce sf!
		if(inherits(x, "Spatial")){
			# conserve the originals to know what to have
			originals <- x
			x <- sf::st_as_sf(x)
		}
	
		# for non -spatial/sf this is null
		originalCRS <- NULL
		# enforce projection!
		# if originally an sf!
		if(inherits(x, "sf")){
			if(is.null(originals)) originals <- x
			# the original CRS
			originalCRS <- sf::st_crs(x)
			if(!is.na(originalCRS)){
				xTransform <- sf::st_transform(x, "EPSG:4326")
			}else{
				sf::st_crs(x) <- "EPSG:4326"
				xTransform <- x
			}
		}


		# in case stat
		if(!is.character(x)){
		# write 'x' as a shapefile
			layer<- paste(randomString(length=3), age, sep="_")
			if(verbose) message(paste("Exported data identified as ", layer))
			pathToFileNoEXT <- paste(tempd, "/", layer,sep="")
			if(verbose) message("Exporting 'x' as a shapefile.")
#			rgdal::writeOGR(xTransform, dsn=paste(pathToFileNoEXT, ".shp", sep=""), layer=layer, driver="ESRI Shapefile")
			sf::st_write(xTransform, dsn=paste(pathToFileNoEXT, ".shp", sep=""),
				layer=layer, driver="ESRI Shapefile", quiet=!verbose)

		}else{
		# feature to reconstruct is the static polygons
			if(length(x)!=1) stop("Only the 'plates' can be reconstructed with this method.")
			if(x=="plates"){
				# use original one - even for windows.
				pathToFileNoEXT <- gsub(".gpml", "",model@polygons)
			}
		}

		# inheritance of appearance and disappearance dates
		if(plateperiod){
			pPer <- 1
		}else{
			pPer <- 0
		}
		
	# 3. Execute GPlates commands
		# convert to gpml
		if(!is.character(x)){
			if(verbose) message("Converting shapefile to .gpml.")
			conversion <- paste(gplatesExecutable, " convert-file-format -l \"",pathToFileNoEXT,".shp\" -e gpml", sep="")
			system(conversion, ignore.stdout=!verbose,ignore.stderr=!verbose)
		}
		# do the plate assignment
		if(!is.character(x)){
			if(verbose) message("Assigning plate IDs to .gpml file.")
			assignment <- paste(gplatesExecutable, " assign-plate-ids -e ",pPer," -p \"", platePolygons, "\" -l \"",pathToFileNoEXT,".gpml\"", sep="")
			system(assignment, ignore.stdout=!verbose,ignore.stderr=!verbose)
		}
		# do reconstruction
		if(!is.character(x)) if(verbose) message("Reconstructing coordinates.")
		if(is.character(x)) if(x=="plates") if(verbose) message("Reconstructing plates.")
			reconstruction <- paste(gplatesExecutable, " reconstruct -l \"",pathToFileNoEXT,".gpml\" -r \"", 
					rotation, "\" -e shapefile -t ", age, " -o \"", pathToFileNoEXT,"_reconstructed\" -w 1", sep="") 
			system(reconstruction, ignore.stdout=!verbose,ignore.stderr=!verbose)

	# 4. Processing output
		# Was the output multiple?
		multiple <- FALSE

		# reading coordinates
		if(!is.character(x)){
			if(verbose) message("Reading reconstructed geometries.")

			# the single file
			targetSingle <- paste(pathToFileNoEXT,"_reconstructed.shx",	sep="")
			targetSingleNoPath <- fileFromPath(targetSingle)

			# produced directory? 
			targetDir<- paste(pathToFileNoEXT,"_reconstructed",	sep="")
			targetDirNoPath <- fileFromPath(targetDir)

			# is this a single file? 
			allFiles <- list.files(tempd)

			# is the target single file created?
			if(any(allFiles==targetSingleNoPath)){
				if(verbose) message("Found single output geometry files.")
				rotated <- sf::st_read(targetSingle, quiet=!verbose)
			}

			# did GPlates produce a whole directory? 
			if(any(allFiles==targetDirNoPath)){
				# Was an output multiple?
				multiple <- TRUE

				if(verbose) message("Found multiple output geometry files.")
				# read in files from there
				allDirFiles <- list.files(targetDir)	

				# files to read in
				toRead <- allDirFiles[grep(".shx", allDirFiles)]

				# the container
				rotated <- NULL
				for(i in 1:length(toRead)){
					# read in one bit
					one <- sf::st_read(file.path(targetDir, toRead[i]), quiet=!verbose)

					# bind it to the rest
					rotated <- rbind(rotated, one)
						
				}

			}
		} 
		if(is.character(x)){
			if(x=="plates") if(verbose) message("Reading plates.")
			pathToFile <- paste(pathToFileNoEXT,"_reconstructed",dirSep ,fileFromPath(pathToFileNoEXT),"_reconstructed_polygon.shx", sep="")
			rotated <- sf::st_read(pathToFile, quiet=!verbose)
		}
		
		# if the originals were sf - nothing happens
		if(inherits(originals, "sf") | inherits(originals, "Spatial")){
			# omission of gplates metadata? 
			if(!gmeta){
				rotated <- 	rotated[, -which(colnames(rotated)%in%c("ANCHOR", "TIME", "FILE1", "RECONFILE1"))]
			}
		}
		
		# but if there was a crs, make sure to change projection to it!
		# is CRS even applicable? 
		if(!is.null(originalCRS)){
			# if it was given in the first place
			if(!is.na(originalCRS)){
				if(verbose) message("Converting back to original CRS.")
				rotated <- sf::st_transform(rotated, originalCRS)
			}
		}
		
	
		# if originals are spatial, make the output spatial
		if(inherits(originals, "Spatial")){
			if(verbose) message("Converting sf to Spatial.")
			# different treatment dependending on whether multiple files or a single one is outpu
			# sp cannot handle heterogeneous geometries
			if(!multiple){
				rotated <- sf::as_Spatial(rotated)
			}else{
				warning("There were multiple reconstruction output, returning as sf.")
			}
		}

		# if originals are not sf and not spatial
		if(!inherits(originals, "sf") & !inherits(originals, "Spatial")){

			# and they are either a matrix or a data frame transform object back to whatever it was
			if((inherits(originals, "matrix") | inherits(originals, "data.frame")) ){
				# some coordinates probably were missing
				# get the coorindates
				#  reconstructed
				rotatedSF <- rotated
				coords <- sf::st_coordinates(rotated)

				# where to put the new coordinates
				rotated <- originals
				rotated[] <- NA

				# use the IDs to make sure that things really match!
				rownames(rotated) <- theID
				rotated[rotatedSF$ID, ] <- coords

				# copy over the rownames
				rownames(rotated) <- rownames(originals)

			}else{
				if(!is.null(originals)) stop(paste("Unknown output class", class(originals)))
			}
		}


	# 5. Finish
		# remove temporary files
		if(!inherits(x, "character")){
			if(cleanup){		
				system(paste("rm -r ",tempd, "/",layer,"*", sep=""))
			}
		}
	return(rotated)
}

# function to find Gplates installation directory
winDefaultGPlates<-function(){

	# default installation paths
	basic <- c(
		"C:/Program Files/GPlates",
		"C:/Program Files (x86)/GPlates"
		
	)

	versioned <- NULL
	inWhich <- NULL

	# search both possible folders 
	for(i in 1:2){
		# enter program files
		gpver <- list.files(basic[i])
	
		found <- grep("GPlates", gpver)
	
		# grab the latest version
		if(length(found)>0){
			versioned <- gpver[found[length(found)]]
			inWhich <- i
		}
	}
	if(is.null(inWhich)) stop("Could not locate GPlates.")

	# add it 
	dir <- file.path(basic[inWhich], versioned)

	# search executable
	gpfiles <- list.files(dir)

	# grab gplates executable file
	gplat <- grep("gplat",gpfiles)
	potExe <- gpfiles[gplat]
	exe <- potExe[grep("exe",potExe)]

	return(c(dir=dir, exe=exe))
}


macDefaultGPlates <-function(){
# default installation path
	basic <- "/Applications"
	# enter program files
	gpver <- list.files(basic)

	found <- grep("GPlates", gpver)

	# grab the latest version
	if(length(found)>0){
		gpver<- gpver[found[length(found)]]
	}else{
		stop("Could not locate GPlates.")
	}
	dir <-file.path(basic,gpver, "gplates.app/Contents/MacOS")
	exe <-"gplates"
	return(c(dir=dir, exe=exe))
}



testGPlates<- function(gplatesExecutable, verbose){
# ask version
	gplatesTest <- paste(gplatesExecutable, "--v")
	
	# "\"C:/Program Files (x86)/GPlates/GPlates 2.1.0/gplates-2.1.0.exe\" --v"
	# default version
	ver <- NULL
	
	# depending on how much the user wants to see
	if(!verbose){
		opt <- options(show.error.messages = FALSE)
		# revert even if command below fails for some reason
		on.exit(options(opt))

		try(ver <- system(gplatesTest, intern=TRUE,ignore.stdout = TRUE, 
				ignore.stderr = TRUE))
	}else{
		try(ver <- system(gplatesTest, intern=TRUE))
	}
	
	# if gplates is not present
	return(!is.null(ver))
}

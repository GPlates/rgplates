###########################################################################
# Front-end wrapper function

#' Reconstruct geographic features
#' 
#' Reconstruct the geographic locations from present day coordinates and spatial objects back to their paleo-positions. 
#' Each location will be assigned a plate id and moved back in time using the chosen reconstruction model.
#'
#' The function implements two reconstruction submodules, which are selected with the \code{model} argument:
#' 
#' If \code{model} is a \code{character} entry, then the \code{reconstruct()} function uses the GPlates Web Service (\url{https://gws.gplates.org/}, remote reconstruction submodule).
#' The available reconstruction models for this submodule are (as of 2023-06-29):
#' \itemize{
#'	 \item "SETON2012" (Seton et al., 2012) for coastlines and topological plate polygons (0-200 Ma).
#'	 \item "RODINIA2013" (Li et al., 2012) for coastlines (530-1100 Ma).
#'	 \item "MULLER2016" (Muller et al., 2016) for coastlines and topological plate polygons (0-230 Ma).
#'	 \item "GOLONKA" (Wright et al. 2013) for coastlines only (0-550 Ma). 
#'	 \item "PALEOMAP" (Scotese, 2016) for coastlines only (0-1100 Ma). 
#'	 \item "MATTHEWS2016_mantle_ref" (Matthews et al., 2016) for coastlines and topological plate polygons (0-410 Ma). 
#'	 \item "MATTHEWS2016_pmag_ref" (Matthews et al., 2016) for coastlines and topological plate polygons (0-410 Ma).
#'	 \item "MULLER2019" (Müller et al., 2019) for coastlines and static plate polygons. (0-250 Ma).
#'	 \item "MERDITH2021" (Merdith et al., 2021, default) for coastlines and static plate polygons (0-1000 Ma). 
#'	 \item "MULLER2022" (Müller et al., 2022) for coastlines and static plate polygons (0-1000 Ma). 
#' }
#' 
#' If \code{model} is a \code{\link{platemodel}} class object, then the function will try to use the GPLates desktop application (\url{https://www.gplates.org/}) to reconstruct the coordinates (local reconstruction submodule).
#' Plate models are available in chronosphere with the \code{\link[chronosphere]{fetch}} function. See \code{\link[chronosphere]{datasets}} for the available models.
#' The function will try to find the main GPlates executable in its default installation directory. If this does not succeed, use \code{path.gplates} to enter the full path to the GPlates executable as a \code{character} string.
#' 
#' 
#' @section References:
#' Matthews, K. J., Maloney, K. T., Zahirovic, S., Williams, S. E., Seton, M., & Müller, R. D. (2016). Global plate boundary evolution and kinematics since the late Paleozoic. Global and Planetary Change, 146, 226–250. https://doi.org/10.1016/j.gloplacha.2016.10.002
#' \cr
#' \cr Andrew S. Merdith, Simon E. Williams, Alan S. Collins, Michael G. Tetley, Jacob A. Mulder, Morgan L. Blades, Alexander Young, Sheree E. Armistead, John Cannon, Sabin Zahirovic, R. Dietmar Müller, (2021). Extending full-plate tectonic models into deep time: Linking the Neoproterozoic and the Phanerozoic, Earth-Science Reviews, Volume 214, 2021, 103477, ISSN 0012-8252, https://doi.org/10.1016/j.earscirev.2020.103477.
#' \cr
#' \cr Müller, R. D., Seton, M., Zahirovic, S., Williams, S. E., Matthews, K. J., Wright, N. M., … Cannon, J. (2016). Ocean Basin Evolution and Global-Scale Plate Reorganization Events Since Pangea Breakup. Annual Review of Earth and Planetary Sciences, 44(1), 107–138. https://doi.org/10.1146/annurev-earth-060115-012211
#' \cr
#' \cr  Müller, R. D., Zahirovic, S., Williams, S. E., Cannon, J., Seton, M., Bower, D. J., Tetley, M. G., Heine, C., Le Breton, E., Liu, S., Russell, S. H. J., Yang, T., Leonard, J., and Gurnis, M. (2019), A global plate model including lithospheric deformation along major rifts and orogens since the Triassic. Tectonics, vol. 38, https://doi.org/10.1029/2018TC005462.
#' \cr
#' \cr Müller, R. D., Flament, N., Cannon, J., Tetley, M. G., Williams, S. E., Cao, X., Bodur, Ö. F., Zahirovic, S., and Merdith, A.: A tectonic-rules-based mantle reference frame since 1 billion years ago – implications for supercontinent cycles and plate–mantle system evolution, Solid Earth, 13, 1127–1159, https://doi.org/10.5194/se-13-1127-2022, 2022.
#' \cr
#' \cr Scotese, C. R. (2016). PALEOMAP PaleoAtlas for GPlates and the PaleoData Plotter Program. http://www.earthbyte.org/paleomap‐ paleoatlas‐for‐gplates
#' \cr
#' \cr Seton, M., Müller, R. D., Zahirovic, S., Gaina, C., Torsvik, T., Shephard, G., … Chandler, M. (2012). Global continental and ocean basin reconstructions since 200Ma. Earth-Science Reviews, 113(3–4), 212–270. https://doi.org/10.1016/j.earscirev.2012.03.002
#' \cr
#' \cr Wright, N., Zahirovic, S., Müller, R. D., & Seton, M. (2013). Towards community-driven paleogeographic reconstructions: integrating open-access paleogeographic and paleobiology data with plate tectonics. Biogeosciences, 10(3), 1529–1541. https://doi.org/10.5194/bg-10-1529-2013
#' 
#' @param x are the features to be reconstructed. Can be a vector with longitude and latitude representing
#' a single point or a matrix/dataframe with the first column as longitude and second column as latitude, or a \code{SpatialPolygonsDataFrame} class object. 
#' For the online subroutine, the character strings \code{"static_polygons"}, \code{"coastlines"}  and \code{"plate_polygons"} return static plate polygons, rotated present-day coastlines and topological plates, respectively. For the offline subroutine, it can be a name of the feature set defined in the \code{model} object.
#' @param ... arguments passed to class-specific methods.
#' @param age (\code{numeric}) is the age in Ma at which the points will be reconstructed
#' @param model (\code{character} or \code{\link{platemodel}}) The  reconstruction model. The class of this argument selects the submodule used for reconstruction, a \code{character} value will invoke the remote reconstruction submodule and will submit \code{x} to the GPlates Web Service. A \code{platemodel} class object will call the local-reconstruction submodule. The default is \code{"PALEOMAP"}. See details for available models.
#' @param reverse (\code{logical}) Argument of the remote reconstruction submodule. The flag to control the direction of reconstruction. If \code{reverse = TRUE}, the function will 
#' calculate the present-day coordinates of the given paleo-coordinates. 
#' @param path.gplates (\code{character}) Argument of the local reconstruction submodule. In case the GPlates executable file is not found at the coded default location, the full path to the executable (gplates-<ver>.exe on Windows) can be entered here. e.g. \code{"C:/gplates_2.3.0_win64/gplates.exe"}.
#' @param listout (\code{logical})If multiple ages are given, the output can be returned as a \code{list} if \code{listout = TRUE}.
#' @param verbose (\code{logical}) Should call URLs (remote submodule) or console feedback (local-submodule) be printed?
#' @param cleanup (\code{logical}) Argument of the local reconstruction submodule. Should the temporary files be deleted immediately after reconstructions?
#' @param plateperiod (\code{logical}) Argument of the local reconstuction submodule. Should the durations of the plates be forced on the partitioned feature? If these are set to \code{TRUE} and the plate duration estimates are long, then you might lose some data.
#' @param dir (\code{character}) Argument of the local reconstruction submodule. Directory where the temporary files of the reconstruction are stored (defaults to a temporary directory created by R). Remember to toggle \code{cleanup} if you want to see the files.  
#' @param gmeta (\code{logical}) Argument of the local reconstruction submodule, in the case, when \code{sf} objects are supplied. Should the metadata produced by GPlates be included in the output object?  
#' @param partitioning (\code{character}) Argument of the local reconstruction submodule, which feature collection of the tectonic model should be used to assing plate IDs to the features? It defaults to \code{"static_polygons"}. 
#' @return A \code{numeric} matrix if \code{x} is a \code{numeric}, \code{matrix} or \code{data.frame}, or \code{Spatial*} class objects, depending on input. \code{NULL} in case no model is specified.
#' @examples
#' # With the web service 
#' # simple matrices
#' # replace model with desired choice
#' reconstruct(matrix(c(95, 54), nrow=1), 140, model=NULL)
#'	
#'	# points reconstruction
#'	xy <-cbind(long=c(95,142), lat=c(54, -33))
#'	reconstruct(xy, 140, model=NULL)
#'	
#' @rdname reconstruct
#' @exportMethod reconstruct
setGeneric("reconstruct", function(x,...) standardGeneric("reconstruct"))

# have to use long function definitions for documentation.
#' @param enumerate (\code{logical}) Should be all coordinate/age combinations be enumerated and reconstructed (set to \code{TRUE} by default)? \code{FALSE} is applicable only if the number of rows in \code{x} is equal to the number elementes in \code{age}. Then a point will be reconstructed to the age that has the same index in \code{age} as the row of the coordinates in \code{x}. List output is not available in this case. 
#' @param chunk (\code{numeric}) Argument of the remote reconstruction submodule. Single integer, the number of coordinates that will be queried from the GPlates in a single go. 
#' @param check (\code{logical}) Should the validity of the entries for the GWS checked with the information stored in \code{\link{gws}}? (default: \code{TRUE}) 
#' @rdname reconstruct
setMethod(
	"reconstruct", 
	signature="matrix", 
	function(x,age, model="MERDITH2021", listout=TRUE, verbose=FALSE, enumerate=TRUE, chunk=200, reverse=FALSE, path.gplates=NULL, cleanup=TRUE, dir=NULL,plateperiod=TRUE, partitioning="static_polygons", check=TRUE){

	if(any(is.na(x))) stop("Missing values (NAs) detected. Remove these before reconstruction.")

	if(is.null(model)){
		message("No model was specified.")
		x <- NULL
		return(x)
	}
	
		# Check long lat!
		if(!is.numeric(age)) age <- as.numeric(age)

		# depending on length
		if(length(age)>1){
		 
			# base condition of enumerate=FALSE
				if(!enumerate & length(age)!=nrow(x)){
						enumerate <- TRUE
						warning("Enumerating coordinate-age combinations. \n enumerate = FALSE is possible only if the number of coordinates matches the number of ages.")
			} 

			# if the function is allowed to enumerate
			if(enumerate){
				# depending on output
				if(listout){
					container<- list()
			 
				# 3d matrix
				}else{
					container <- array(NA, dim=c(length(age), dim(x)))
				}
	
				# iterate over ages
				for(i in 1:length(age)){
                    if(is.character(model)){
						if(check) CheckGWS("coastlines", model, age=age[i], verbose=verbose)
						fresh <- IteratedPointReconstruction(coords=x, chunk=chunk,
							age=age[i], model=model, reverse=reverse, verbose=verbose)
                    }else{
                        fresh <- reconstructGPlates(x=x, age=age[i], model=model,
							path.gplates=path.gplates, dir=dir, verbose=verbose, 
							cleanup=cleanup, plateperiod=plateperiod, partitioning=partitioning, check=check)
                    }
					# attribute copy, if there is anything
					if(!is.null(colnames(x))) colnames(fresh) <- colnames(x)
					rownames(fresh) <- rownames(x)
					# list
					if(listout){
						container[[i]] <- fresh
					# 3d matrix
					}else{
						container[i,,] <- fresh
					}
				}
	
				# name the	output
				# list
				if(listout){
					names(container) <- age
				# matrix
				}else{
					dimnames(container) <- c(list(age), dimnames(fresh))
				}

			# used vectorized age implementation, no enumeration
			}else{
				# empty container
				container <- x
				container[]<-NA

				# reconstruction is performance-capped, for loop should be enough
				ageLevs <- unique(age)

				# for all different age values
				for(i in 1:length(ageLevs)){
					# which rows apply
					index <- which(ageLevs[i]==age)
					current <- x[index, , drop=FALSE]
					# do reconstruction and store
                    if(is.character(model)){
						if(check) CheckGWS("static_polygons", model, age=ageLevs[i], verbose=verbose)
						container[index,] <- IteratedPointReconstruction(coords=current,
							chunk=chunk, age=ageLevs[i], model=model, reverse=reverse, 
							verbose=verbose)
				    }else{
                        container[index,] <- reconstructGPlates(x=current,
							age=ageLevs[i], model=model, path.gplates=path.gplates, 
							dir=dir, verbose=verbose, cleanup=cleanup, plateperiod=plateperiod, partitioning=partitioning, check=check)
                    }
                }
			}

		# single target
		}else{
            if(is.character(model)){
				if(check) CheckGWS("coastlines", model, age=age, verbose=verbose)
                container <- IteratedPointReconstruction(coords=x, chunk=chunk,
					age=age, model=model, reverse=reverse, verbose=verbose)
            }else{
                container <- reconstructGPlates(x=x, age=age, model=model,
					path.gplates=path.gplates, dir=dir, verbose=verbose, 
					cleanup=cleanup, plateperiod=plateperiod, partitioning=partitioning, check=check)
            }
			# if everything returned i just missing value
			# return original structure with missing
			if(all(is.na(container))){
				container <- x
				container[] <- NA

			}
		}

		# and return
		return(container)
	} 
)


#' @rdname reconstruct
setMethod(
	"reconstruct", 
	signature="data.frame", 
	function(x,... ){
		reconstruct(as.matrix(x), ...)
})

#' @rdname reconstruct
setMethod(
	"reconstruct", 
	signature="numeric", 
	function(x,... ){
		if(length(x)==2) reconstruct(matrix(x, nrow=1), ...) else stop("Only 2 element vectors are allowed!")
})

#' @rdname reconstruct
setMethod(
	"reconstruct", 
	signature="character", 
	function(x,age, model="MERDITH2021", listout=TRUE, verbose=FALSE,path.gplates=NULL, cleanup=TRUE, dir=NULL, plateperiod=FALSE, partitioning="static_polygons", check=TRUE){

	if(is.null(model)){
			message("No model was specified.")
			x <- NULL
			return(x)
		}

		# vectorized
		if(length(age)>1){
			
			# list
			if(listout){
				container<- list()
			# SpArray to be
			}else{
				stop("Noooo, not yet!")
			}
			# iterate over ages
			for(i in 1:length(age)){
				# what is needed?
				if(is.character(model)){
					if(check) CheckGWS(x, model, age=age[i], verbose=verbose)
					feature <- gplates_reconstruct_this(age=age[i], this=x, model=model, verbose=verbose)
				}else{
					feature <- reconstructGPlates(x=x, age=age[i], model=model,
						path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup, partitioning=partitioning, check=check)
				}

				# save it
				container[[i]] <- feature
			}
			# list output
			if(listout){
			 names(container) <- age
			}

		# single entry
		}else{
			# what do you want?
			if(is.character(model)){
				if(check) CheckGWS(x, model, age=age, verbose=verbose)
				container <- gplates_reconstruct_this(age=age, this=x, model=model, verbose=verbose)
			}else{
				container <- reconstructGPlates(x=x, age=age, model=model,
					path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup, partitioning=partitioning, check=check)
			}
		}
		# return container
		return(container)
	} 
)


#' @rdname reconstruct
setMethod(
	"reconstruct",
	"Spatial", 
	function(x, age, model, listout=TRUE, verbose=FALSE,path.gplates=NULL, cleanup=TRUE, dir=NULL, plateperiod=FALSE, partitioning="static_polygons", check=TRUE){
		if(is.null(model)){
			message("No model was specified.")
			x <- NULL
			return(x)
		}
		
		# vectorized implementation
		if(length(age)>1){
			# list output
			if(listout){
				container <- list()

			# SpArray
			}else{
				stop("Nooo, not yet!")	
			}

			# iterate
			for(i in 1:length(age)){
				if(is.character(model)){
					stop("Use the offline method to recontsruct Spatial* objects!")
#					container[[i]] <- gplates_reconstruct_polygon(sp=x, age=age[i], model=model, verbose=verbose)
				}else{
					container[[i]] <- reconstructGPlates(x=x, age=age[i], model=model,
						path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup, plateperiod=plateperiod, partitioning=partitioning, check=check)
				}
			}

			# list output
			if(listout){
				names(container) <- age
			}

		# single entry
		}else{
			if(is.character(model)){
				stop("Use the offline method to recontsruct Spatial* objects!")
#				container <- gplates_reconstruct_polygon(sp=x, age, model=model, verbose=verbose)
			}else{
				container <- reconstructGPlates(x=x, age=age, model=model,
					path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup, plateperiod, partitioning=partitioning, check=check)
			}
			
		}

		return(container)

	}
)



#' @rdname reconstruct
setMethod(
	"reconstruct",
	"sf", 
	function(x, age, model, listout=TRUE, verbose=FALSE,path.gplates=NULL, cleanup=TRUE, dir=NULL, plateperiod=FALSE, gmeta=FALSE, partitioning="static_polygons", check=TRUE){
		if(is.null(model)){
			message("No model was specified.")
			x <- NULL
			return(x)
		}
		
		# vectorized implementation
		if(length(age)>1){
			# list output
			if(listout){
				container <- list()

			# SpArray
			}else{
				stop("This will return an SfArray. Not yet!")	
			}

			# iterate
			for(i in 1:length(age)){
				if(is.character(model)){
					stop("Use the offline method to recontsruct sf objects!")
#					container[[i]] <- gplates_reconstruct_polygon(sp=x, age=age[i], model=model, verbose=verbose)
				}else{
					container[[i]] <- reconstructGPlates(x=x, age=age[i],
						model=model, path.gplates=path.gplates, dir=dir, verbose=verbose, 
						cleanup=cleanup,plateperiod=plateperiod, gmeta=gmeta, partitioning=partitioning, check=check)
				}
			}

			# list output
			if(listout){
				names(container) <- age
			}

		# single entry
		}else{
			if(is.character(model)){
				stop("Use the offline method to recontsruct sf objects!")
#				container <- gplates_reconstruct_polygon(sp=x, age, model=model, verbose=verbose)
			}else{
				container <- reconstructGPlates(x=x, age=age, model=model,
						path.gplates=path.gplates, dir=dir, verbose=verbose, 
						cleanup=cleanup, plateperiod=plateperiod, gmeta=gmeta, partitioning=partitioning, check=check)
			}
			
		}

		return(container)

	}
)
	


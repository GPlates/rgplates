###########################################################################
# Front-end wrapper function

#' Reconstruct geographic features
#' 
#' Reconstruct the geographic locations from present day coordinates and spatial objects back to their paleo-positions. 
#' Each location will be assigned a plate id and moved back in time using the chosen reconstruction model.
#' #' The function implements two reconstruction submodules, which are selected with the \code{model} argument:
#' 
#' If \code{model} is a \code{character} entry, then the \code{reconstruct()} function uses the GPlates Web Service (\url{https://gws.gplates.org/}, remote reconstruction submodule).
#' The available reconstruction models for this submodule are:
#' \itemize{
#'	 \item "SETON2012" (Seton et al., 2012) for coastlines and plate polygons.
#'	 \item "MULLER2016" (Muller et al., 2016) for coastlines and plate polygons.
#'	 \item "GOLONKA" (Wright et al. 2013) for coastlines only. 
#'	 \item "PALEOMAP" (Scotese and Wright, 2018) for coastlines and plate polygons. 
#'	 \item "MATTHEWS2016" (Matthews et al., 2016) for coastlines and plate polygons. 
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
#' \cr Müller, R. D., Seton, M., Zahirovic, S., Williams, S. E., Matthews, K. J., Wright, N. M., … Cannon, J. (2016). Ocean Basin Evolution and Global-Scale Plate Reorganization Events Since Pangea Breakup. Annual Review of Earth and Planetary Sciences, 44(1), 107–138. https://doi.org/10.1146/annurev-earth-060115-012211
#' \cr
#' \cr Scotese, C., & Wright, N. M. (2018). PALEOMAP Paleodigital Elevation Models (PaleoDEMS) for the Phanerozoic PALEOMAP Project. Retrieved from https://www.earthbyte.org/paleodem-resource-scotese-and-wright-2018/
#' \cr
#' \cr Seton, M., Müller, R. D., Zahirovic, S., Gaina, C., Torsvik, T., Shephard, G., … Chandler, M. (2012). Global continental and ocean basin reconstructions since 200Ma. Earth-Science Reviews, 113(3–4), 212–270. https://doi.org/10.1016/j.earscirev.2012.03.002
#' \cr
#' \cr Wright, N., Zahirovic, S., Müller, R. D., & Seton, M. (2013). Towards community-driven paleogeographic reconstructions: integrating open-access paleogeographic and paleobiology data with plate tectonics. Biogeosciences, 10(3), 1529–1541. https://doi.org/10.5194/bg-10-1529-2013
#' 
#' @param x are the features to be reconstructed. Can be a vector with longitude and latitude representing
#' a single point or a matrix/dataframe with the first column as longitude and second column as latitude, or a \code{SpatialPolygonsDataFrame} class object. 
#' The character strings \code{"plates"} and \code{"coastlines"} return static plates and rotated present-day coastlines, respectively.
#' @param ... arguments passed to class-specific methods.
#' @param age (\code{numeric})is the age in Ma at which the points will be reconstructed
#' @param model (\code{character} or \code{\link{platemodel}}) The  reconstruction model. The class of this argument selects the submodule used for reconstruction, a \code{character} value will invoke the remote reconstruction submodule and will submit \code{x} to the GPlates Web Service. A \code{platemodel} class object will call the local-reconstruction submodule. The default is \code{"PALEOMAP"}. See details for available models.
#' @param reverse (\code{logical}) Argument of the remote reconstruction submodule. The flag to control the direction of reconstruction. If \code{reverse = TRUE}, the function will 
#' calculate the present-day coordinates of the given paleo-coordinates. 
#' @param path.gplates (\code{character}) Argument of the local reconstruction submodule. In case the GPlates executable file is not found at the coded default location, the full path to the executable (gplates-<ver>.exe on Windows) can be entered here.
#' @param listout (\code{logical})If multiple ages are given, the output can be returned as a \code{list} if \code{listout = TRUE}.
#' @param verbose (\code{logical}) Should call URLs (remote submodule) or console feedback (local-submodule) be printed?
#' @param cleanup (\code{logical}) Argument of the local reconstruction submodule. Should the temporary files be deleted immediately after reconstructions?
#' @param plateperiod (\code{logical}) Argument of the local reconstuction submodule. Should the durations of the plates be forced on the partitioned feature? If these are set to \code{TRUE} and the plate duration estimates are long, then you might lose some data.
#' @param dir (\code{character}) Argument of the local reconstruction submodule. Directory where the temporary files of the reconstruction are stored (defaults to a temporary directory created by R). Remember to toggle \code{cleanup} if you want to see the files.  
#' @param gmeta (\code{logical}) Argument of the local reconstruction submodule, in the case, when \code{sf} objects are supplied. Should the metadata produced by GPlates be included in the output object?  
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
#' @rdname reconstruct
setMethod(
	"reconstruct", 
	signature="matrix", 
	function(x,age, model="PALEOMAP", listout=TRUE, verbose=FALSE, enumerate=TRUE, chunk=200, reverse=FALSE, path.gplates=NULL, cleanup=TRUE, dir=NULL,plateperiod=FALSE){

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
						fresh <- IteratedPointReconstruction(coords=x, chunk=chunk,
							age=age[i], model=model, reverse=reverse, verbose=verbose)
                    }else{
                        fresh <- reconstructGPlates(x=x, age=age[i], model=model,
							path.gplates=path.gplates, dir=dir, verbose=verbose, 
							cleanup=cleanup, plateperiod=plateperiod)
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
						container[index,] <- IteratedPointReconstruction(coords=current,
							chunk=chunk, age=ageLevs[i], model=model, reverse=reverse, 
							verbose=verbose)
				    }else{
                        container[index,] <- reconstructGPlates(x=current,
							age=ageLevs[i], model=model, path.gplates=path.gplates, 
							dir=dir, verbose=verbose, cleanup=cleanup, plateperiod=plateperiod)
                    }
                }
			}

		# single target
		}else{
            if(is.character(model)){
                container <- IteratedPointReconstruction(coords=x, chunk=chunk,
					age=age, model=model, reverse=reverse, verbose=verbose)
            }else{
                container <- reconstructGPlates(x=x, age=age, model=model,
					path.gplates=path.gplates, dir=dir, verbose=verbose, 
					cleanup=cleanup, plateperiod=plateperiod)
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
	function(x,age, model="PALEOMAP", listout=TRUE, verbose=FALSE,path.gplates=NULL, cleanup=TRUE, dir=NULL, plateperiod=FALSE){

	if(is.null(model)){
			message("No model was specified.")
			x <- NULL
			return(x)
		}

	if(!any(x==c("plates", "coastlines"))) stop("Invalid 'x' argument.\nThe only valid character input are \"plates\" and \"coastlines\"")
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
				if(x=="coastlines"){
					feature <- gplates_reconstruct_coastlines(age=age[i], model=model, verbose=verbose)
				}

				if(x=="plates"){
					if(is.character(model)){
						feature <- gplates_reconstruct_static_polygons(age=age[i], model=model, verbose=verbose)
					}else{
						feature <- reconstructGPlates(x="plates", age=age[i], model=model,
							path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup)
					}
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
			if(x=="coastlines"){
				container <- gplates_reconstruct_coastlines(age=age, model=model, verbose=verbose)
			}

			if(x=="plates"){
				if(is.character(model)){
					container <- gplates_reconstruct_static_polygons(age=age, model=model, verbose=verbose)
				}else{
					container <- reconstructGPlates(x="plates", age=age, model=model,
						path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup)
				}
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
	function(x, age, model, listout=TRUE, verbose=FALSE,path.gplates=NULL, cleanup=TRUE, dir=NULL, plateperiod=FALSE){
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
						path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup, plateperiod=plateperiod)
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
					path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup, plateperiod)
			}
			
		}

		return(container)

	}
)



#' @rdname reconstruct
setMethod(
	"reconstruct",
	"sf", 
	function(x, age, model, listout=TRUE, verbose=FALSE,path.gplates=NULL, cleanup=TRUE, dir=NULL, plateperiod=FALSE, gmeta=FALSE){
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
						cleanup=cleanup,plateperiod=plateperiod, gmeta=gmeta)
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
						cleanup=cleanup, plateperiod=plateperiod, gmeta=gmeta)
			}
			
		}

		return(container)

	}
)
	


###########################################################################
# Front-end wrapper functions

#' Reconstruct geographic features
#' 
#' Reconstruct the geographic locations from present day coordinates and spatial objects back to their paleo-positions. 
#' Each location will be assigned a plate id and moved back in time using the chosen reconstruction model.
#'
#' The function implements two reconstruction submodules, which are selected with the \code{model} argument:
#' 
#' If \code{model} is a \code{character} entry, then the \code{reconstruct()} function uses the GPlates Web Service (\url{https://gwsdoc.gplates.org/}, remote reconstruction submodule).
#' The available reconstruction models for this submodule are (as of 2024-02-02):
#' \itemize{
#'	 \item "TorsvikCocks2017" (Torsvik and Cocks, 2017) for coastlines (0-540 Ma). Uses a mantle reference frame by default. For climatically sensitive analyses use a paleomagnetic reference frame, which you can toggle by setting the \code{anchor} parameter to \code{1} from the default \code{0}.
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
#' \cr Torsvik and Cocks (2017). Earth History and Palaeogeography. Cambridge University Press, 317 pp.
#' \cr
#' \cr Wright, N., Zahirovic, S., Müller, R. D., & Seton, M. (2013). Towards community-driven paleogeographic reconstructions: integrating open-access paleogeographic and paleobiology data with plate tectonics. Biogeosciences, 10(3), 1529–1541. https://doi.org/10.5194/bg-10-1529-2013
#' 
#' @param x The features to be reconstructed. Can be a vector with longitude and latitude representing
#' a single point or a matrix/dataframe with the first column as longitude and second column as latitude.
#' For the online subroutine, the character strings \code{"static_polygons"}, \code{"coastlines"}  and \code{"plate_polygons"} return static plate polygons, rotated present-day coastlines and topological plates, respectively. For the offline subroutine, it can be a name of the feature set defined in the \code{model} object. Some \code{Spatial*}, \code{sf} and \code{SpatRaster} classes are also accepted, although this input is still experimental.
#' @param ... arguments passed to class-specific methods.
#' @param age (\code{numeric}) is the target age in Ma at which the feature will be reconstructed. Defaults to 0 Ma. 
#' @param model (\code{character} or \code{\link{platemodel}}) The  reconstruction model. The class of this argument selects the submodule used for reconstruction, a \code{character} value will invoke the remote reconstruction submodule and will submit \code{x} to the GPlates Web Service. A \code{platemodel} class object will call the local-reconstruction submodule. The default is \code{"PALEOMAP"}. See details for available models.
#' @param from (\code{numeric}) The original age of the features to be reconstructed. A single value, defaults to 0Ma. Only used with the online reconstruction module.
#' @param reverse (\code{logical}) Argument of the remote reconstruction submodule. The flag to control the direction of reconstruction. If \code{reverse = TRUE}, the function will 
#' calculate the present-day coordinates of the given paleo-coordinates, with age setting the target. Not recommended, kept only for compatibility with the GPlates Web Service. Using \code{from} instead of \code{age} will automatically trigger reverse reconstruction. 
#' @param path.gplates (\code{character}) Argument of the local reconstruction submodule. In case the GPlates executable file is not found at the coded default location, the full path to the executable (gplates-<ver>.exe on Windows) can be entered here. e.g. \code{"C:/gplates_2.3.0_win64/gplates.exe"}.
#' @param listout (\code{logical})If multiple ages are given, the output can be returned as a \code{list} if \code{listout = TRUE}.
#' @param verbose (\code{logical}) Should call URLs (remote submodule) or console feedback (local-submodule) be printed?
#' @param cleanup (\code{logical}) Argument of the local reconstruction submodule. Should the temporary files be deleted immediately after reconstructions?
#' @param validtime (\code{logical}) Argument of the local reconstuction submodule. Should the durations of the plates be forced on the partitioned feature? If these are set to \code{TRUE} and the plate duration estimates are long, then you might lose some data. This is the inverse of the \code{ignore.valid.time} argument of the GWS.
#' @param plateperiod (\code{logical}) Deprecated argument, renamed to \code{validtime} for higher compatibility with the GPlates Web Service. 
#' @param dir (\code{character}) Argument of the local reconstruction submodule. Directory where the temporary files of the reconstruction are stored (defaults to a temporary directory created by R). Remember to toggle \code{cleanup} if you want to see the files.  
#' @param gmeta (\code{logical}) Argument of the local reconstruction submodule, in the case, when \code{sf} objects are supplied. Should the metadata produced by GPlates be included in the output object?  
#' @param partitioning (\code{character}) Argument of the local reconstruction submodule, which feature collection of the tectonic model should be used to assing plate IDs to the features? It defaults to \code{"static_polygons"}. 
#' @param warn (\code{character}) Argument of the online reconstruction submodule, used in reverse-reconstructions (calculation of present-day coordinates from paleocoordinates). If set to `TRUE` (default), the function will produce a warning when paleocoordinates are not assigned to any of the paritioning polygons (missing values are returned for these). When set to `FALSE`, the warnings will not be displayed.
#' @param anchor (\code{character}) Argument of the online reconstruction submodule. The Plate ID of the anchored plate. This is the 'anchored_plate_id' parameter of the GPlates Web Service.
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
#' @param chunk (\code{numeric}) Deprected argument of the online reconstruction method. Ignored.
#' @param check (\code{logical}) Should the validity of the entries for the GWS checked with the information stored in \code{\link{gws}}? (default: \code{TRUE}) 
#' @rdname reconstruct
setMethod(
	"reconstruct", 
	signature="matrix", 
	function(x,age=0, model="MERDITH2021", from=0, listout=TRUE, verbose=FALSE, enumerate=TRUE, 
		chunk=NULL, reverse=FALSE, path.gplates=NULL, cleanup=TRUE, dir=NULL,plateperiod=NULL, partitioning="static_polygons", check=TRUE, warn=TRUE, anchor=0, validtime=TRUE){
		if(!is.null(plateperiod)){
			warning("This argument was renamed to 'validtime'. Use that instead, 'plateperiod' is deprecated.")
			validtime <- plateperiod
		}

		# provide some feedback for users
		if(!is.null(chunk)) warning("The 'chunk' argument is deprecated and is now unnecessary.")

#		if(any(is.na(x))) stop("Missing values (NAs) detected. Remove these before reconstruction.")

		# identify the missing values
		bPresent <- !( is.na(x[,1]) | is.na(x[,2]))

		# return null if no model is specified
		if(is.null(model)){
			message("No model was specified.")
			x <- NULL
			return(x)
		}
	
		# Check long lat!
		if(!is.numeric(age)) age <- as.numeric(age)
		if(!is.numeric(from)) from <- as.numeric(from)
		if(length(from)>1) stop("Only one 'from' argument is allowed.")

	
		# forking for recursion
		# A. past coordinates are given
		if(from!=0){

			if(reverse) stop("The argument 'reverse=TRUE' is only allowed when the past coordinates are given in 'age'.\n  Using 'from' automatically sets the direction of reconstruction.")

			# how many ages are given
			ageNumber <- length(age)

			# basic forward reconstruction (one from one age [target])
			# find present-day positions
			if(ageNumber==1){
				if(age==0){
					if(verbose) message("Calculating present-day coordinates from past ones.")
					# do the reconstruction
					if(is.character(model)){
						if(check) CheckGWS("coastlines", model, age=from, verbose=verbose)
						immediate <- gwsReconstructPoints(coords=x[bPresent, , drop=FALSE], 
							time=from, model=model, reverse=TRUE, verbose=verbose, warn=warn, anchor=anchor, validtime=validtime)
						# make it the same as it was
						fresh <- x

						# replace all values with missing
						fresh[] <- NA

						# replace the bits
						fresh[bPresent, ] <- immediate
						colnames(fresh) <- colnames(immediate)
							
					}else{
						stop("Calculation of present-day coordinates from past ones\n  is not available with the offline method.")
					}

					# attribute copy, if there is anything
					# enforce attributes! - present day attributes
					colnames(fresh) <- c("long", "lat")
					
					rownames(fresh) <- rownames(x)

					# return the present-day coordinates
					return(fresh)
				}
			}
			# if the function did not exit, then there is at least one meaningful target age 
			# 1. calculate the present-day coordinates from past ones, by recursively calling the previous chunk
			presentCoords <- reconstruct(x=x, age=0, from=from, model=model, verbose=verbose, check=check, warn=warn, anchor=anchor)

			if(verbose) message("Calculating past coordinates from present-day ones.")
			# 2. calculate the different past coordinates by using the present coordinates calculated above
			pastCoords <- reconstruct(presentCoords, age=age, from=0, model=model, verbose=verbose, enumerate=enumerate, check=check, listout=listout, warn=warn, anchor=anchor, validtime=validtime)
			# recursive case ends
			return(pastCoords)

		# normal backward reconstruction
		# present coordinates given 
		}else{
	
			# depending on length
			if(length(age)>1){

				if(reverse) stop("The argument `reverse=TRUE` is not allowed with multiple ages.")

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
							immediate <- gwsReconstructPoints(coords=x[bPresent,, drop=FALSE], 
								time=age[i], model=model, reverse=reverse, verbose=verbose, warn=warn, anchor=anchor, validtime=validtime)

						}else{
							immediate <- reconstructGPlates(x=x[bPresent, , drop=FALSE], age=age[i], model=model,
								path.gplates=path.gplates, dir=dir, verbose=verbose, 
								cleanup=cleanup, plateperiod=validtime, partitioning=partitioning, check=check)
						}

						# make it the same as it was
						fresh <- x

						# replace all values with missing
						fresh[] <- NA

						# replace the bits
						fresh[bPresent,] <- immediate
						if(age[i]!=0){
							colnames(fresh) <- c("paleolong", "paleolat")
						}else{
							colnames(fresh) <- c("long", "lat")
						}

						# attribute copy, if there is anything
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
						dimnames(container) <- c(list(age), list(rownames(fresh), c("paleolong", "paleolat")))
					}

				# used vectorized age implementation, no enumeration
				}else{
					# empty container
					fresh <- x[bPresent, , drop=FALSE]
					fresh[]<-NA

					# if missing coordinates are there, the given ages need to be remoed too!
					age <- age[bPresent]

					# filtered
					screened <- x[bPresent, , drop=FALSE]

					# reconstruction is performance-capped, for loop should be enough
					ageLevs <- unique(age)

					# for all different age values
					for(i in 1:length(ageLevs)){
						# which rows apply
						index <- which(ageLevs[i]==age)
						current <- screened[index, , drop=FALSE]
						# do reconstruction and store
						if(is.character(model)){
							if(check) CheckGWS("coastlines", model, age=ageLevs[i], verbose=verbose)
							immediate <- gwsReconstructPoints(coords=current,
								time=ageLevs[i], model=model, reverse=reverse, 
								verbose=verbose, warn=warn, anchor=anchor, validtime=validtime)
							fresh[index,] <- immediate
						}else{
							immediate <- reconstructGPlates(x=current,
								age=ageLevs[i], model=model, path.gplates=path.gplates, 
								dir=dir, verbose=verbose, cleanup=cleanup, plateperiod=validtime, partitioning=partitioning, check=check)
							fresh[index,] <- immediate
						}
					}

					# create new final container
					# make it the same as it was
					container <- x

					# replace all values with missing
					container[] <- NA

					# replace the bits
					container[bPresent, ] <-fresh 
					colnames(container) <- c("paleolong", "paleolat")
				}

			# single target
			}else{

				if(is.character(model)){
					if(check) CheckGWS("coastlines", model, age=age, verbose=verbose)
					fresh <- gwsReconstructPoints(coords=x[bPresent, , drop=FALSE],
						time=age, model=model, reverse=reverse, verbose=verbose, warn=warn, anchor=anchor, validtime=validtime)
				}else{
					fresh <- reconstructGPlates(x=x[bPresent, , drop=FALSE], age=age, model=model,
						path.gplates=path.gplates, dir=dir, verbose=verbose, 
						cleanup=cleanup, plateperiod=validtime, partitioning=partitioning, check=check)
				}
				# if everything returned i just missing value
				# return original structure with missing
				# create new final container
				# make it the same as it was
				container <- x

				# replace all values with missing
				container[] <- NA

				# replace the bits
				container[bPresent, ] <- fresh 
				# depending on whether this is truly paleo
				if(age!=0 & !reverse){
					colnames(container) <- c("paleolong", "paleolat")
				}else{
					colnames(container) <- c("long", "lat")
				}
			}

			# and return
			return(container)
		} 
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
	function(x,age, model="MERDITH2021", listout=TRUE, verbose=FALSE,path.gplates=NULL, cleanup=TRUE, dir=NULL, partitioning="static_polygons", check=TRUE, anchor=0){

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
					feature <- gplates_reconstruct_this(age=age[i], this=x, model=model, verbose=verbose, anchor=anchor)
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
				container <- gplates_reconstruct_this(age=age, this=x, model=model, verbose=verbose, anchor=anchor)
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
	function(x, age, model, listout=TRUE, verbose=FALSE,path.gplates=NULL, cleanup=TRUE, dir=NULL, plateperiod=NULL, partitioning="static_polygons", check=TRUE, validtime=TRUE){

		if(!is.null(plateperiod)){
			warning("This argument was renamed to 'validtime'. Use that instead, 'plateperiod' is deprecated.")
			validtime <- plateperiod
		}
	
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
					stop("Use the offline method to reconstruct Spatial* objects!")
#					container[[i]] <- gplates_reconstruct_polygon(sp=x, age=age[i], model=model, verbose=verbose)
				}else{
					container[[i]] <- reconstructGPlates(x=x, age=age[i], model=model,
						path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup, plateperiod=validtime, partitioning=partitioning, check=check)
				}
			}

			# list output
			if(listout){
				names(container) <- age
			}

		# single entry
		}else{
			if(is.character(model)){
				stop("Use the offline method to reconstruct Spatial* objects!")
#				container <- gplates_reconstruct_polygon(sp=x, age, model=model, verbose=verbose)
			}else{
				container <- reconstructGPlates(x=x, age=age, model=model,
					path.gplates=path.gplates, dir=dir, verbose=verbose, cleanup=cleanup, plateperiod=validtime, partitioning=partitioning, check=check)
			}
			
		}

		return(container)

	}
)



#' @rdname reconstruct
setMethod(
	"reconstruct",
	"sf", 
	function(x, age, model, listout=TRUE, verbose=FALSE,path.gplates=NULL, cleanup=TRUE, dir=NULL, plateperiod=NULL, gmeta=FALSE, partitioning="static_polygons", check=TRUE, validtime=TRUE){

		if(!is.null(plateperiod)){
			warning("This argument was renamed to 'validtime'. Use that instead, 'plateperiod' is deprecated.")
			validtime <- plateperiod
		}
	
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
					stop("Use the offline method to reconstruct sf objects!")
#					container[[i]] <- gplates_reconstruct_polygon(sp=x, age=age[i], model=model, verbose=verbose)
				}else{
					container[[i]] <- reconstructGPlates(x=x, age=age[i],
						model=model, path.gplates=path.gplates, dir=dir, verbose=verbose, 
						cleanup=cleanup,plateperiod=validtime, gmeta=gmeta, partitioning=partitioning, check=check)
				}
			}

			# list output
			if(listout){
				names(container) <- age
			}

		# single entry
		}else{
			if(is.character(model)){
				stop("Use the offline method to reconstruct sf objects!")
#				container <- gplates_reconstruct_polygon(sp=x, age, model=model, verbose=verbose)
			}else{
				container <- reconstructGPlates(x=x, age=age, model=model,
						path.gplates=path.gplates, dir=dir, verbose=verbose, 
						cleanup=cleanup, plateperiod=validtime, gmeta=gmeta, partitioning=partitioning, check=check)
			}
			
		}

		return(container)

	}
)
	


#' @rdname reconstruct
setMethod(
	"reconstruct",
	"SpatRaster",
	function(x, age, model, from=0, listout=TRUE, verbose=FALSE, plateperiod=NULL, check=TRUE, validtime=TRUE){
		if(!requireNamespace("terra", quietly=TRUE)) stop("This method requires the 'terra' package!")

		if(!is.null(plateperiod)){
			warning("This argument was renamed to 'validtime'. Use that instead, 'plateperiod' is deprecated.")
			validtime <- plateperiod
		}

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
				stop("This will return a RasterArray. Not yet!")
			}

			# iterate (recursive!)
			for(i in 1:length(age)){
				if(!is.character(model)){
					stop("'SpatRaster' objects, are not yet supported by the offline method.\n  Please use the online method (GWS) instead.")
				}else{
					container[[i]] <- reconstruct(x, from=from, age=age[i],  model=model, validtime=validtime)
				}
			}

			# list output
			if(listout){
				names(container) <- age
			}

		# single entry
		}else{
			if(!is.character(model)){
				stop("'SpatRaster' objects, are not yet supported by the offline method.\n  Please use the online method (GWS) instead.")
			}else{
				if(check) CheckGWS("coastlines", model, age=age, verbose=verbose)
				# extract the cells from the SpatRaster
				xy <- terra::xyFromCell(x, 1:terra::ncell(x))

				# reverse reconstruct with the matrix-method. It is easier to sample (extract) from an existing raster
				# then to create a new raster from points - likely contiaining missing values
				newPresent <- reconstruct(xy, from=age,age=from, model=model, warn=FALSE, validtime=validtime)

				# now the magic: extract with the reverse reconstructed coordinates
				vals <- terra::extract(x, newPresent, method="bilinear")

				# if there are multiple layers
				layers <- dim(x)[3]

				# copy over the original
				container <- x

				# repeat for every
				for(i in 1:layers){
					# replace values
					terra::values(container[[i]]) <- vals[[i]]
				}
			}

		}
		return(container)

	}
)

#' Calculate velocities of plate tectonic movements
#'
#' Queries to return meshes of tectonic plate velocities.
#'
#' The function returns a mesh of velocities: two variables, either magnitude (mm/year) and azimuth (rad): \code{type="MagAzim"} or easting and northing velocity vectors (mm/year): \code{type="east_north"}.
#' Currently only the online method is supported using the GPlates Web Service (internet connection is required).
#' Available models are in the \code{\link{gws}} object, and can be provided with arguments similar to \code{\link{reconstruct}}.
#'
#' @param x \code{character}: What should the velocities be reconstructed for? If nothing is given (i.e. \code{signature(x="missing")} the argument defaults to the only currently working feature collection, the \code{"static_polygons"}\ - expected to be expanded in the future.
#' @param age \code{numeric}: The age in millions of years at which the velocities are to be returned. Can be a vector of ages for multiple target ages.
#' @param model \code{character}: The name of the tectonic model. Similar to that of \code{\link{reconstruct}}.
#' @param domain \code{character}: Either \code{"longLatGrid"} or \code{"healpix"}. \code{"longLatGrid"} returns the velocites with the domain of a regular, one-by-one degree longitude-latitude grid.
#' \code{"healpix"} will return velocities with the domain of an icosahedral, nearly equidistant grid.
#' @param type \code{character}: The type of velocity format that is to be returned, either magnitude and azimuth (\code{type="MagAzim"}) or easting and northing velocity vectors (\code{type="east_north"}).
#' Both result in two variables.
#' @param output \code{character}: The class name of the output to be returned. Either \code{data.frame} or \code{SpatRaster}. The latter requires the \code{terra} extension (suggested) and is only available with \code{domain="longLatGrid"}.
#' @param polecrop \code{logical}: Only applicable if \code{output="SpatRaster"}. The original velocity values are provided as a grid-registered raster,
#' which forces the extent of the raster to be beyond the regular \code{[-180, 180]} longitude and \code{[-90, 90]} domain, producing warnings when the \code{SpatRaster} is used.
#' The default \code{cellraster=TRUE} resamples this raster to a native, cell-registered grid.
#' This is an issue only with latitudes, so they get cropped by default. Setting this argument to \code{FALSE} will skip cropping.
#' @param verbose \code{logical}: Are you interested in more messages?
#' @param listout \code{logical}: If multiple ages are queried, then should the results be organized in a list? (only option currently)
#' @param check (\code{logical}) Should the validity of the entries for the GWS checked with the information stored in \code{\link{gws}}? (default: \code{TRUE})
#' @return Velocities of tectonic movements. If \code{output="data.frame"} then the function returns a \code{data.frame} with the longitude, latitude, the two velocity variables and the plate ids they belong to.
#' If \code{output="SpatRaster"} then the output will be a multilayered \code{SpatRaster} object.
#' @examples
#' # dummy example,
#' # set model to the desired model string, e.g. model="MERDITH2021"
#' velocities("static_polygons", age=45, model=NULL)
#' @rdname velocities
#' @exportMethod velocities
setGeneric("velocities", function(x,...) standardGeneric("velocities"))


#' @rdname velocities
setMethod(
	"velocities",
	signature(x="missing"),
	function(x, ...){
		# fall back to the static polygons
		velocities(x="static_polygons", ...)
	}

)

#' @rdname velocities
setMethod(
	"velocities",
	signature(x="character"),
	function(x, age, model, domain="longLatGrid", type="MagAzim", output="data.frame", polecrop=TRUE, verbose=FALSE, listout=TRUE, check=TRUE){

		# basic argumentation check
		veloDefend(type=type, domain=domain)

		if(is.null(model)){
			message("No model was specified.")
			x <- NULL
			return(x)
		}

		# if output is SpatRaster, then terra needs to be there
		if(output=="SpatRaster"){
			if(!requireNamespace("terra", quietly=TRUE)) stop("This method requires the 'terra' package!")
			if(domain!="longLatGrid") stop("You need longitude-latitude domain to have 'SpatRaster' output!")
		}

		if(!is.numeric(age)) age <- as.numeric(age)

		# recursive call
		if(length(age)>1){
			stop("Not yet!")
			if(!listout) stop("Only list output is available at this point.")

		# base case: one Age
		}else{
			# online method
			if(inherits(model,"character")){

				if(check) if(x=="plate_polygons") stop("Velocities on the topological plates are not yet supported. ")
				if(check) if(x!="static_polygons") stop("Only 'static_polygons' are supported at this point. ")

				# extract the data
				velo <- gwsVelocitiesThis(x, age=age, model=model, domain=domain, type=type, verbose=verbose, check=check)

				if(output=="SpatRaster"){
					# translate the standard output to a terra-raster
					rasts <- SpatRastFromDF(velo, coord=c("long", "lat"), crs="WGS84")

					# if the rasters are to be resampled - wrong extent
					if(polecrop){
						if(verbose) message("Cropping the grid to valid latitudes.\n  Use 'polecrop=FALSE' to skip")

						# create the standard-extent raster
						extent <- terra::ext(-180, 180, -89.5,89.5)
						rasts <- terra::crop(rasts, extent)
					}

					# the returned object
					velo <- rasts

				}

			# offline methods
			}else{
				stop("Velocity calculations are not yet supported by the offline method.\n  Please use the online method (GWS) instead.")
			}


		}

		return(velo)

	}
)

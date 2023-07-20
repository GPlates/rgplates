setClassUnion("chardat", c("character", "data.frame"))

# tectonic - models

#' Class of objects representing plate tectonic models
#' 
#' Meta-object containing paths to a unique plate tectonic model
#' 
#' @rdname platemodel
#' @exportClass platemodel
platemodel <- setClass(
	"platemodel",
	slots=list(name="character", rotation="character", features="chardat"))

#' @param .Object Constructor argument (not needed).
#' @param rotation (\code{character}) The path to the rotation file.
#' @param features (\code{character}) Named vector of features with the paths to the individual files.
#' @param name (\code{character}) (Optional) name of the model. 
#' @param polygons (\code{character}) (Deprecated) The path to the static plate polygon file.
#' @rdname platemodel 
#' @return A \code{platemodel} class object.
#' @export platemodel
#' @examples
#' # path to provided archive
#' archive <- file.path(
#'   system.file("extdata", package="rgplates"), 
#'   "paleomap_v3.zip")
#' # extract to temporary directory
#' unzip(archive, exdir=tempdir())
#' # path to the rotation file
#' rotPath <- file.path(tempdir(), 
#'   "PALEOMAP_PlateModel.rot")
#' # path to the polygons
#' polPath <- file.path(tempdir(), 
#'   "PALEOMAP_PlatePolygons.gpml")
#' # register in R - to be used in reconstruct()
#' model <- platemodel(rotation=rotPath, features=c("static_polygons"=polPath))
setMethod("initialize",signature="platemodel",
	definition=function(.Object, rotation=NULL, features=NULL, name=NULL, polygons=NULL){
			if(is.null(rotation)) stop("You have to provide a rotation file.")
			.Object@rotation <- rotation
			if(!is.null(features)){
				.Object@features <- features
			}

			if(!is.null(polygons)){
				if(is.null(features)){
					.Object@features <- ""
					names(.Object@features) <- "static_polygons"

				}
				.Object@features["static_polygons"] <- polygons
			}

			if(!is.null(name)){
				.Object@name <- name
			}else{
			
				.Object@name <- ""
			}

		return(.Object)
	}
)


setMethod("show",signature="platemodel",
	definition=function(object){
		cat("GPlates plate tectonic model.\n")
		if(object@name!="") cat(object@name, "\n - ")

		cat("rotation:                     ", paste("\"", fileFromPath(object@rotation),"\"", sep=""), "\n")
		if(is.character(object@features)){
			for(i in 1:length(object@features)){
				current <- object@features[i]
				namLength <- nchar(names(current))
				reps <- 30-namLength
				if(reps<0) reps <- 0
				cat(paste0(names(current), ":", paste(rep(" ",reps ), collapse=""), paste("\"", fileFromPath(current),"\"", sep=""), "\n"))

			}
		}
		if(is.data.frame(object@features)){
			for(i in 1:nrow(object@features)){
				current <- object@features[i,]
				namLength <- nchar(rownames(current))
				reps <- 30-namLength
				if(reps<0) reps <- 0
				cat(paste0(rownames(current), ":", paste(rep(" ",reps ), collapse=""), paste("\"", fileFromPath(current[,"feature_collection"]),"\"", sep=""), "\n"))

			}

		}

	}
)

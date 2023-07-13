#' Valid reconstructable feature collections of the GPlates Web Service
#'
#' The object contains valid returns as of 2023-07-13.
#'
#' The valid return combinations for selected models and feature collections are copied from \url{https://gwsdoc.gplates.org/reconstruction-models}. 
#'
#' \describe{
#' 	\item{\code{model}}{The name of the reconstruction model.}
#' 	\item{\code{feature}}{The name of the feature collection.}
#' 	\item{\code{from}}{The oldest reconstruction age accepted by the model to return the feature collection.}
#' 	\item{\code{to}}{The youngest reconstruction age accepted by the model to return the feature collection.}
#' }
#' @format A \code{data.frame} with 4 variables and 17 observations. 
#' @usage data(gws)
"gws"

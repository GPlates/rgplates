#' Valid reconstructable feature collections of the GPlates Web Service
#'
#' This is version 1.2. The object contains valid returns of GWS v1.0.0, as of 2025-03-14.
#'
#' The valid return combinations for selected models and feature collections are copied from \url{https://gwsdoc.gplates.org/models}. 
#'
#' \describe{
#' 	\item{\code{model}}{The name of the reconstruction model.}
#' 	\item{\code{feature}}{The name of the feature collection.}
#' 	\item{\code{from}}{The oldest reconstruction age accepted by the model to return the feature collection.}
#' 	\item{\code{to}}{The youngest reconstruction age accepted by the model to return the feature collection.}
#' 	\item{\code{description}}{The short description of the feature collection.}
#' }
#' @format A \code{data.frame} with 5 variables and 26 observations.
#' @usage data(gws)
"gws"

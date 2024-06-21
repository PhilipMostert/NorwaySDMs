#' @title obtainCovariate: Function to obtain a covariate later for a specified country.
#' @description
#' Function to obtain covariate layers from _WorldClim_ around a specified area.
#' @param covariates A vector of covariate names to obtain.
#' @param res Resolution of the worldclim variable. Valid options are: \code{10}, \code{5}, \code{2.5} or \code{0.5} (minutes of a degree).
#' @param projection Coordinate reference system to use in analysis.
#' @param path The path where the covariate will be saved.
#'
#' @import geodata
#' @import terra
#'
#' @return A \code{spatialRaster} object of the covariates across the specified area.

obtainCovariate <- function(covariates,
                            res, projection, path) {

  ##Do for other covariate layers

  covariateLayers <- try(geodata::worldclim_global(res = res,
                                               var = covariates,
                                               path = path), silent = FALSE)

  if (inherits(covariateLayers, 'try-error')) stop('Could not download covariate layers. Please try again later.')

  covariateLayers <- terra::project(covariateLayers, projection)

  covariateLayers

  }

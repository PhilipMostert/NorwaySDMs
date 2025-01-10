#' @title obtainCovariate: Function to obtain a covariate later for a specified country.
#' @description
#' Function to obtain covariate layers from _WorldClim_ or _ESA_ around a specified area.
#' @param covariates A vector of covariate names to obtain.
#' @param type Which type of covariates to download.
#' @param res Resolution of the worldclim variable. Valid options are: \code{10}, \code{5}, \code{2.5} or \code{0.5} (minutes of a degree).
#' @param type Type of covariate to include. Must be one of: \code{'worldclim'} or \code{'landcover'}.
#' @param projection Coordinate reference system to use in analysis.
#' @param path The path where the covariate will be saved.
#'
#' @import geodata
#' @import terra
#'
#' @return A \code{spatialRaster} object of the covariates across the specified area.

obtainCovariate <- function(covariates, type,
                            res, projection, path) {

  ##Do for other covariate layers

  #Make this into a switch
  if (type == 'worldclim') {
  covariateLayers <- try(geodata::worldclim_global(res = res,
                                               var = covariates,
                                               path = path), silent = FALSE)
  }
  else
    if (type == 'landcover') {

      covariateLayers <- try(geodata::landcover(var = covariates, path = path), silent = FALSE)

    }

  if (inherits(covariateLayers, 'try-error')) stop('Could not download covariate layers. Please try again later.')

  covariateLayers <- terra::project(covariateLayers, projection)

  covariateLayers

  }

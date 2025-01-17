% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obtainCovariate.R
\name{obtainCovariate}
\alias{obtainCovariate}
\title{obtainCovariate: Function to obtain a covariate later for a specified country.}
\usage{
obtainCovariate(covariates, type, res, projection, path)
}
\arguments{
\item{covariates}{A vector of covariate names to obtain.}

\item{type}{Type of covariate to include. Must be one of: \code{'worldclim'} or \code{'landcover'}.}

\item{res}{Resolution of the worldclim variable. Valid options are: \code{10}, \code{5}, \code{2.5} or \code{0.5} (minutes of a degree).}

\item{projection}{Coordinate reference system to use in analysis.}

\item{path}{The path where the covariate will be saved.}
}
\value{
A \code{spatialRaster} object of the covariates across the specified area.
}
\description{
Function to obtain covariate layers from \emph{WorldClim} or \emph{ESA} around a specified area.
}

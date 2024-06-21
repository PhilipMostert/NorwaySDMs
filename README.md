
<!-- README.md is generated from README.Rmd. Please edit that file -->

# intSDM

<!-- badges: start -->

[![R-CMD-check](https://github.com/PhilipMostert/intSDM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PhilipMostert/intSDM/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/PhilipMostert/intSDM/branch/main/graph/badge.svg)](https://app.codecov.io/gh/PhilipMostert/intSDM?branch=main)

<!-- badges: end -->

The goal of *intSDM* is to assist users in creating a reproducible
workflow for large-scale integrated species distribution models. The
package does this by providing the tools and methods to obtain species’
occurrence data from [GBIF](https://www.gbif.org) and environmental
covariates from [WorldClim](https://www.worldclim.org). The package then
estimates the integrated species distribution model using a Bayesian
framework with the integrated nested Laplace approximation method.

## Installation

You can install the development version of this package from
[GitHub](https://github.com/) with:

``` r
#install.packages('devtools')
devtools::install_github("PhilipMostert/intSDM")
```

or directly through CRAN:

``` r
install.packages('intSDM')
```

## Functionality

The package contains two main functions: `startWorkflow` which
initializes the workflow, and `sdmWorkflow`, which estimates one of the
specified outcomes of the workflow. `startWorkflow` produces an
[*R6*](https://r6.r-lib.org), which has a multitude of different *slot*
functions to help customize the workflow. These include:

| Function name         | Function use                                                                                                                                                          |
|-----------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `.$plot()`            | Plot data and other objects required for the model.                                                                                                                   |
| `.$addStructured()`   | Add data not available on GBIF.                                                                                                                                       |
| `.$addMesh()`         | Create an *inla.mesh* object.                                                                                                                                         |
| `.$addGBIF()`         | Add data from GBIF.                                                                                                                                                   |
| `.$addArea()`         | Specify sampling domain.                                                                                                                                              |
| `.$addCovariates()`   | Add spatial covariates.                                                                                                                                               |
| `.$crossValidation()` | Specify the cross-validation method.                                                                                                                                  |
| `.$modelOptions()`    | Add [*R-INLA*](https://www.r-inla.org), [*inlabru*](https://inlabru-org.github.io/inlabru/)and [*PointedSDMs*](https://github.com/PhilipMostert/PointedSDMs) options. |
| `.$specifySpatial()`  | Add penalizing complexity priors to the spatial effects.                                                                                                              |
| `.$biasFields()`      | Specify an additional spatial effect for a dataset.                                                                                                                   |
| `.$workflowOutput()`  | Specify the output of the workflow.                                                                                                                                   |
| `.$obtainMeta()`      | Obtain metadata for the occurrence records.                                                                                                                           |

An example of the package in-use is provided as a vignette within the
package.

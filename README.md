
<!-- README.md is generated from README.Rmd. Please edit that file -->

# *intSDM*

<!-- badges: start -->

[![R-CMD-check](https://github.com/PhilipMostert/intSDM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PhilipMostert/intSDM/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/PhilipMostert/intSDM/branch/main/graph/badge.svg)](https://app.codecov.io/gh/PhilipMostert/intSDM?branch=main)

<!-- badges: end -->

The goal of *intSDM* is to assist users in creating a reproducible
workflow for large-scale integrated species distribution models (ISDM).
The package does this by providing the tools and functions to obtain
species’ occurrence data from [GBIF](https://www.gbif.org) and
environmental covariates from [WorldClim](https://www.worldclim.org),
specify the components of an ISDM, estimate the model, and produce
useful outputs. The package estimates the ISDM using a Bayesian
framework with the [integrated nested Laplace
approximation](https://rss.onlinelibrary.wiley.com/doi/full/10.1111/j.1467-9868.2008.00700.x)
method, which is computationally efficient in comparison to MCMC
methods. Furthermore, the package builds wrapper functions around
[inlabru](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13168)
and
[PointedSDMs](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.14091),
two *R* packages designed to simplify modelling spatial processes and
integrated species distribution models respectively. As a result, good
knowledge of both these packages is required before using *intSDM*.

The outputs of the model are objects which are typically useful in
species distribution modelling analyses. These include: models,
predictions, maps of predictions and cross-validation scores. The
default of *intSDM* is to produce single-species models and outputs;
however the package does also allow multi-species models to assist the
user in obtaining estimates of species richness.

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

| Function name         | Function use                                                                                                                                                           |
|-----------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `.$plot()`            | Plot data and other objects required for the model.                                                                                                                    |
| `.$addStructured()`   | Add data not available on GBIF.                                                                                                                                        |
| `.$addMesh()`         | Create an *inla.mesh* object.                                                                                                                                          |
| `.$addGBIF()`         | Add data from GBIF.                                                                                                                                                    |
| `.$addArea()`         | Specify sampling domain.                                                                                                                                               |
| `.$addCovariates()`   | Add spatial covariates.                                                                                                                                                |
| `.$crossValidation()` | Specify the cross-validation method.                                                                                                                                   |
| `.$modelOptions()`    | Add [*R-INLA*](https://www.r-inla.org), [*inlabru*](https://inlabru-org.github.io/inlabru/) and [*PointedSDMs*](https://github.com/PhilipMostert/PointedSDMs) options. |
| `.$specifySpatial()`  | Add penalizing complexity priors to the spatial effects.                                                                                                               |
| `.$biasFields()`      | Specify an additional spatial effect for a dataset.                                                                                                                    |
| `.$workflowOutput()`  | Specify the output of the workflow.                                                                                                                                    |
| `.$specifyPriors()`   | Specify the priors of the model.                                                                                                                                       |
| `.$modelFormula`      | Add a formula for the covariates and bias of the model.                                                                                                                |
| `.$obtainMeta()`      | Obtain metadata for the occurrence records.                                                                                                                            |

Documentation and examples for each of these slot functions may easily
be obtained using `.$help()`.

Examples of the package being used is provided as vignettes within the
package.

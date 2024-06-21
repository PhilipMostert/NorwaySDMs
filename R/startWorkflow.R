#' @title \code{startWorkflow}: function to commence the integrated species distribution model workflow.
#' @description
#' Function to initialize the reproducible workflow using integrated species distribution models. The arguments for this function are used to specify which species and countries are to be studied, as well as how the results of the model should be saved (either as an R object or saved to some directory). This function outputs an \code{R6} object with additional slot functions to help further customize the model specification. See \code{?species_model} for more details on these functions.
#' @param Countries A vector of country names to complete the analysis over. If missing, a boundary object (of class \code{Spatial} or \code{sf}) has to be added to the model using \code{.$addArea} before any analysis is completed.
#' @param Species A vector of Species names (scientific) to include in the analysis. Names should be given carefully since the names provided will be used to obtain _GBIF_ observations.
#' @param Projection The coordinate reference system used in the workflow.
#' @param Save Logical argument indicating if the model objects and outputs should be saved as .rds files. Defaults to \code{TRUE}. If \code{FALSE} then the output of the workflow will be a list of objects at each step of the workflow.
#' @param Richness Logical option to create maps for each species individually, or create a species richness model. Defaults to \code{FALSE}.
#' @param saveOptions A list containing two items: \code{projectDirectory} indicating where the objects should be saved (defaults to \code{NULL}), and \code{projectName} which indicates the name for the folder in the relevant directory. The latter argument is required, regardless of the value given to \code{Save}.
#' @param Quiet Logical argument indicating if the workflow should provide the user messages during the setup and estimation process. Defaults to \code{TRUE}.
#'
#' @returns An R6 object of class \code{species_model}. This object contains a collection of slot functions to assist the user in customizing their workflow.
#'
#' @examples
#' ##Start a workflow without saving objects
#'
#' workflow <- startWorkflow(Species = 'Fraxinus excelsior',
#'                           Projection = "+proj=longlat +ellps=WGS84",
#'                           Save = FALSE,
#'                           saveOptions = list(projectName = 'example'))
#' @export

startWorkflow <- function(Countries, Species,
                          Projection,
                          Save = TRUE,
                          Richness = FALSE,
                          saveOptions = list(
                          projectDirectory = NULL,
                          projectName = NULL),
                          Quiet = FALSE) {

  if (Save) {

    if (!is.character(saveOptions$projectName)) stop('Please provide projectName in the saveOptions list.')
    if (!all(names(saveOptions) %in% c('projectName', 'projectDirectory'))) stop('saveOptions needs to be a list containing objects with names: `projectDirectory` and `projectName`.')

    if (is.null(saveOptions$projectDirectory)) saveOptions$projectDirectory <- getwd()

  }

  if (missing(Species)) stop('At least one species name needs to be provided.')

  #if(!inherits(Projection, 'CRS')) stop('Projection needs to be a CRS object.')

  if (!Quiet) {

    cat('Initializing workflow for integrated species distribution model:\n\n')

    cat('Studied species:', paste(Species, collapse = ', '), '\n')

    if (!missing(Countries)) {

      if (length(Countries) == 1) cat('Studied country:', Countries, '\n\n')
      else cat('Studied countries:', paste(Countries, collapse = ', '), '\n\n')


    } else message('Countries not specified. You can add a sampling region with the `.$addArea` function.')


    cat('This function creates an object with "slot" functions to help you customize the ISDM for your workflow. These may be accessed by using the "$" after the name of the object created with this function.\n\n')
    cat('The following slot functions are availble with this object: \n')

    descriptionSlots <- data.frame(Name = c('---------------','plot',
                                            'addStructured',
                                            'addMesh',
                                            'addGBIF',
                                            'addArea',
                                            'addCovariates',
                                            'crossValidation',
                                            'modelOptions',
                                            'specifySpatial',
                                            'biasFields',
                                            'modelFormula',
                                            'workflowOutput',
                                            'obtainMeta',
                                            'specifyPriors'),
                                   Description = c('---------------','-> Plot data',
                                                   '-> Add structured data',
                                                   '-> Create an inla.mesh object',
                                                   '-> Add data from GBIF',
                                                   '-> Specify sampling region',
                                                   '-> Add spatial covariates',
                                                   '-> Specify CV method',
                                                   '-> Add INLA model options',
                                                   '-> Specify spatial effects',
                                                   '-> Add bias field',
                                                   '-> Change Model formula',
                                                   '-> Output of workflow',
                                                   '-> Summary of metadata',
                                                   '-> Specify priors'))
    print.data.frame(descriptionSlots, right = FALSE, row.names = FALSE)

    cat('\nUse .$help() to find documentation for the slot functions.')


    cat('\n\nThe workflow may then be estimated using the "sdmWorkflow" function.\n\n')



  }

  if (Save) {

    dir.create(path = paste0(saveOptions$projectDirectory, '/', saveOptions$projectName))

    if (!Quiet) cat('Directory for model outputs is:\n', paste0(saveOptions$projectDirectory, '/', saveOptions$projectName))

  }

  modelSetup <- species_model$new(Countries = Countries,
                                  Species = Species,
                                  Projection,
                                  Save = Save,
                                  Quiet = Quiet,
                                  Richness = Richness,
                                  nameProject = saveOptions$projectName,
                                  Directory = paste0(saveOptions$projectDirectory, '/', saveOptions$projectName))

}

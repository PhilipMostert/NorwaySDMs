#' @title R6 class for creating a \code{species_model} object.
#' @description An object containing the data, covariates  and other relevant information to be used in the reproducible workflow. The function \link[intSDM]{startWorkflow} acts as a wrapper in creating one of these objects. This object has additional slot functions within, which allow for further specification and customization of the reproducible workflow.
#' @export
#' @importFrom R6 R6Class
#'
species_model <- R6::R6Class(classname = 'species_model', lock_objects = FALSE, public = list(

#' @description Obtain documentation for a \code{species_model} object.
#' @param ... Not used

  help = function(...) {

   ?intSDM:::species_model

 }
,
#' @description initialize the species_model object.
#' @param Countries Name of the countries to include in the workflow.
#' @param Species Name of the species to include in the workflow.
#' @param nameProject Name of the project for the workflow.
#' @param Save Logical argument indicating if the model outputs should be saved.
#' @param Richness Logical create a species richness model or not.
#' @param Directory Directory where the model outputs should be saved.
#' @param Projection The coordinate reference system used in the workflow.
#' @param Quiet Logical variable indicating if the workflow should provide messages throughout the estimation procedure.
#'

initialize = function(Countries, Species, nameProject, Save,
                      Richness,
                      Directory, Projection, Quiet = TRUE) {

  private$Projection <- Projection
  private$Quiet <- Quiet

  private$richnessEstimate <- Richness

  if (!Richness) private$optionsInla[['pointsSpatial']] <- 'copy'
  else private$optionsInla[['pointsSpatial']] <- NULL

  if (!missing(Countries)) {

    #private$Countries <- Countries
    countryAdded <- self$addArea(countryName = Countries)

  }

  private$Species <- Species
  private$timeStarted <- Sys.time()
  private$Directory <- Directory
  private$Project <- nameProject
  private$Save <- Save



},

#' @param Object An \code{sf} object of the study area. If \code{NULL} then \code{countryName} needs to be provided.
#' @param countryName Name of the countries to obtain a boundary for. This argument will then use the \link[giscoR]{gisco_get_countries} function from the \code{giscoR} package to obtain a boundary.
#' @param ... Additional arguments passed to \link[giscoR]{gisco_get_countries}.
#'
#' @import sf
#' @examples
#' \dontrun{
#' workflow <- startWorkflow(Species = 'Fraxinus excelsior',
#'                           Projection = '+proj=longlat +ellps=WGS84',
#'                           Save = FALSE,
#'                           saveOptions = list(projectName = 'example'))
#'
#' #Add boundary
#' workflow$addArea(countryName = 'Sweden')
#' }

addArea = function(Object = NULL,
                   countryName = NULL,
                   ...) {

  if (all(is.null(Object),
          is.null(countryName))) stop ('One of object or countryName is required.')

  if (!is.null(private$Area)) {

    warning('Area already specified. Deleting all species occurance reccords added to the model.')

    private$dataStructured <- list()
    private$dataGBIF <- list()

  }

  if (!is.null(countryName)) {

    private$Area <- obtainArea(name = countryName, projection =  private$Projection, ...)
    private$Countries <- countryName

  }
  else {

    if (!inherits(Object, 'Spatial') && !inherits(Object, 'sf')) stop('Object needs to be a sp or sf object.')
    if (inherits(Object, 'Spatial')) Object <- as(Object, 'sf')

    Object <- sf::st_transform(Object, as.character(private$Projection))

    private$Area <- Object

  }

  if (!private$Quiet) message('Boundry object added successfully.')

}
,

#' @description Prints the datasets, their data type and the number of observations, as well as the marks and their respective families.
#' @param ... Not used.
#' @import stats
#' @examples
#' workflow <- startWorkflow(Species = 'Fraxinus excelsior',
#'                           Projection = '+proj=longlat +ellps=WGS84',
#'                           Save = FALSE,
#'                           saveOptions = list(projectName = 'example'))
#'
#' workflow$print()

  print = function(...) {

    cat('intSDM reproducible workflow summary:\n\n')

    cat('Research area: ')

    if (is.null(private$Area)) {

      cat('No research area provided: please add one using `.$addArea`.\n\n')

    } else {

      if (!is.null(private$Countries)) cat(private$Countries, '\n\n')
      else cat('Own boundry specified, research area unknown.\n\n')

      }

    cat('Datasets added: ')

    if (length(private$dataGBIF) == 0 && length(private$dataStructured) == 0) {

    cat('No species have been added to the workflow. Please add structured datasets using `.$addStructured` and data from GBIF using `.$addGBIF`.\n\n')


    } else {

      datasetNames <- unique(c(unlist(lapply(private$dataGBIF, function(x) names(x))),
                      unlist(lapply(private$dataStructured, function(x) names(x)))))

      cat(datasetNames, '\n\n') #DO an unlist



    }

    cat('Environmental covariates added: ')

    if (is.null(private$Covariates)) {

      cat('No covariates have been added to the workflow. Please add covariates using `.$addCovariates`.\n\n')

    }
    else cat(names(private$Covariates), '\n\n')

    if (!is.null(private$covariateFormula)) cat('Model formula: ', deparse1(private$covariateFormula), '\n\n')
    if (!is.null(private$biasFormula)) cat('Bias formula:', deparse1(private$biasFormula), '\n\n')

    if (is.null(private$CVMethod)) cat('No Cross-validation specified. Please specify using `.$crossValidation`.\n\n')
    else cat('Cross-validation:', paste(private$CVMethod, collapse = ', '),'\n\n')

    if (is.null(private$Output)) cat('No Output has been specified. Please specify using `.$workflowOutput`.\n\n')
    else cat('Model output:', paste(private$Output, collapse = ', '), '\n\n')

    cat('Use .$help() to find documentation for the slot functions.')


   }
  ,

#' @description Makes a plot of the features used in the integrated model.
#' @param Mesh Add the mesh to the plot.
#' @param Boundary Add the boundary to the plot.
#' @param Species Add the species location data to the plot.
#' @param Covariates Add the spatial covariates to the plot.
#' @return A ggplot object.
#' @import ggplot2
#' @import inlabru
#' @importFrom tidyterra geom_spatraster
#' @examples
#' \dontrun{
#' workflow <- startWorkflow(Species = 'Fraxinus excelsior',
#'                           Projection = '+proj=longlat +ellps=WGS84',
#'                           Save = FALSE,
#'                           saveOptions = list(projectName = 'example'))
#'
#' #Add boundary
#' workflow$addArea(countryName = 'Germany')
#' workflow$plot(Boundary = TRUE)
#' }

  plot = function(Mesh = FALSE,
                  Boundary = TRUE,
                  Species = FALSE,
                  Covariates = FALSE) {

    if (sum(Mesh, Boundary, Species, Covariates) == 0) stop('At least one of Mesh, Boundary, Species or Covariates needs to be chosen.')

    if (Mesh) {

      if (is.null(private$Mesh)) stop('No mesh object provided. Please add one using `.$addMesh()`.')

      meshComponent <- inlabru::gg(private$Mesh)

    } else meshComponent <- NULL

    if (Boundary) {

      if (is.null(private$Area)) stop('No boundary object proived. Please add one using `.$addArea()`.')

      boundaryComponent <- geom_sf(data = sf::st_boundary(private$Area))

    } else boundaryComponent <- NULL

    if (Species) {

      if (all(is.null(private$dataGBIF), is.null(private$dataStructured))) stop('No data objects provided. Please add using either `.$addGBIF` or `.$addStructured`.')

      if (!private$richnessEstimate) {

      spatData <- vector(mode = 'list', length = length(private$Species))
      names(spatData) <-  sub(' ', '_', private$Species)

      for (species in sub(' ', '_', private$Species)) {

        speData <- lapply(append(private$dataGBIF[[species]],
                                 private$dataStructured[[species]]), function(x) x$geometry)

        speData <- speData[sapply(speData, length) > 0]

        namesspeData <- names(speData)
        namesTimes <- rep(namesspeData, times = unlist(lapply(speData, length)))

        if (length(speData) > 0) {

        spatData[[species]] <- sf::st_as_sf(do.call(c, speData))
        spatData[[species]]$.__species_index_var <- species
        spatData[[species]]$.__names_index_var <- namesTimes

         }

      }

      plotSpecies <- do.call(rbind, spatData)
      speciesComponent <- geom_sf(data = plotSpecies, aes(col = .__names_index_var))
      guidesComponent <- guides(col = guide_legend(title="Dataset Name")) ##Need to convert this speciesName arg for all species
      facetComponent <- facet_wrap( ~ .__species_index_var)
      }

      else {

        speData <- lapply(append(unlist(private$dataGBIF, recursive = FALSE),
                                 unlist(private$dataStructured, recursive = FALSE)), function(x) x[, c('geometry', private$speciesName)])
        speData <- do.call(rbind, speData)

        speData$.__species_index_var <- speData[[private$speciesName]]
        speciesComponent <- geom_sf(data = speData, aes(col = .__species_index_var))
        guidesComponent <- guides(col = guide_legend(title="Species Name")) ##Need to convert this speciesName arg for all species
        facetComponent <- NULL


      }

    }

    else {

        speciesComponent <- NULL
        guidesComponent <- NULL
        facetComponent <- NULL

      }

    if (Covariates) {

      if (is.null(private$Covariates)) stop('No covariates object provided. Please add using `.$addCovariates()`.')

      plotList <- list()

      for (cov in names(private$Covariates)) {


        plotList[[cov]] <- ggplot() +
                           tidyterra::geom_spatraster(data = private$Covariates[[cov]]) +
                           boundaryComponent +
                           meshComponent +
                           speciesComponent +
                           guidesComponent + ggtitle(cov)



      }

      if (length(plotList) > 1) inlabru::multiplot(plotlist = plotList)
      else return(plotList[[1]])



    }
    else {


      plotStr <- ggplot() +
                 boundaryComponent +
                 meshComponent +
                 speciesComponent +
                 guidesComponent +
                 facetComponent

      return(plotStr)


      }



  }
  ,
#' @description Function to specify the workflow output from the model. This argument must be at least one of: \code{'Model'}, \code{'Prediction'}, \code{'Maps'} \code{'Cross-validation'}, \code{Bias} and \code{'Summary'}.
#' @param Output The names of the outputs to give in the workflow. Must be at least one of: \code{'Model'}, \code{'Prediction'}, \code{'Maps'}, \code{'Bias'}, \code{Summary} and \code{'Cross-validation'}.
#' @examples
#' workflow <- startWorkflow(Species = 'Fraxinus excelsior',
#'                           Projection = '+proj=longlat +ellps=WGS84',
#'                           Save = FALSE,
#'                           saveOptions = list(projectName = 'example'))
#' workflow$workflowOutput('Predictions')
workflowOutput = function(Output) {

  if (!all(Output %in% c('Model',
                         'Predictions',
                         'Maps',
                         'Bias',
                         'Summary',
                         'Cross-validation'))) stop('Output needs to be at least one of: Model, Predictions, Maps, Bias, Summary or Cross-validation.')

  if (private$richnessEstimate & 'Cross-validation' %in% Output) stop('Cross-validation cannot work with the richness model.')
  ##Add extra defence if 'Richness' in output and Cross-validation stop
  private$Output <- Output

},

#' @description The function is used to convert structured datasets into a framework which is usable by the model. The three types of structured data allowed by this function are presence-absence (PA), presence-only (PO) and counts/abundance datasets, which are controlled using the \code{datasetType} argument. The other arguments of this function are used to specify the appropriate variable (such as response name, trial name, species name and coordinate name) names in these datasets.
#'
#' @param dataStructured The dataset used in the model. Must be either a \code{data.frame}, \code{sf} or \code{SpatialPoints*} object, or a \code{list} containing multiples of these classes.
#' @param datasetType A vector which gives the type of dataset. Must be either \code{'count'}, \code{'PO'} or \code{'PA'}.
#' @param datasetName An optional argument to create a new name for the dataset. Must be the same length as \code{dataStructured} if that is provided as a \code{list}.
#' @param responseName Name of the response variable in the dataset. If \code{dataType} is \code{'PO'}, then this argument may be missing.
#' @param trialsName Name of the trial name variable in the \code{PA} datasets.
#' @param speciesName Name of the species variable name in the datasets.
#' @param coordinateNames Names of the coordinate vector in the dataset. Only required if the datasets added are \code{data.frame} objects.
#' @param generateAbsences Generates absences for \code{'PA'} data. This is done by combining all the sampling locations for all the species in a given dataset, and creating an absence where each of the species do not occur. Requires \code{datasetType = 'PA'}.
#' @import methods
#' @import sf
#' @examples
#' \dontrun{
#' workflow <- startWorkflow(Species = 'Fraxinus excelsior',
#'                           Projection = '+proj=longlat +ellps=WGS84',
#'                           Save = FALSE,
#'                           saveOptions = list(projectName = 'example'))
#'
#' #Add boundary
#' workflow$addArea(countryName = 'Sweden')
#'
#' #Generate random species
#' speciesData <- data.frame(X = runif(1000, 12, 24),
#'                           Y = runif(1000, 56, 68),
#'                Response = sample(c(0,1), 1000, replace = TRUE),
#'                Name = 'Fraxinus_excelsior')
#' workflow$addStructured(dataStructured = speciesData, datasetType = 'PA',
#'                        datasetName = 'xx', responseName = 'Response',
#'                        speciesName = 'Name', coordinateNames = c('X', 'Y'))
#'                        }

  addStructured = function(dataStructured, datasetType,
                           responseName, trialsName,
                           datasetName = NULL, speciesName,
                           coordinateNames, generateAbsences = FALSE) {

    if (missing(dataStructured)) stop('dataStructured needs to be provided')

    if (!is.null(datasetName)) {

      if (inherits(dataStructured, 'list') && length(dataStructured) != length(datasetName)) stop('datasetName needs to be the same length as dataStructured.')
      else if (length(datasetName) > 1) stop ('datasetName needs to contain only one name')

      dataStructured <- list(dataStructured)
      names(dataStructured) <- datasetName


    } else if (inherits(dataStructured, 'list')) {

      if (!is.null(names(dataList))) datasetName <- names(dataList)
      else {

        objectName <- as.character(as.list(match.call())$dataStructured)
        datasetName <- paste0(objectName, seq(1, length(dataStructured)))
        names(dataStructured) <- datasetName

        }

    } else {


      datasetName <- as.character(as.list(match.call())$dataStructured)
      dataStructured <- list(dataStructured)
      names(dataStructured) <- datasetName

    }

    for (dataAdd in datasetName) {

    if (!private$Quiet) message(paste('Adding dataset', dataAdd, 'to the model.'))

    if (dataAdd %in% names(private$dataStructured)) {

      warning('Dataset object already added to the model. Removing the previous version.')
      private$dataStructured[[dataAdd]] <- NULL

      }

    if (!inherits(dataStructured[[dataAdd]],c('Spatial',
                  'data.frame','sf'))) stop('dataStructured needs to be either a SpatialPoints*, data.frame or sf object')

    if (missing(speciesName)) stop('speciesName cannot be missing.')

    if (missing(coordinateNames) && all(class(dataStructured[[dataAdd]]) == 'data.frame')) stop('coordinateNames cannot be missing if dataStructured is a data.frame object.')
    else {

      if (inherits(dataStructured[[dataAdd]], 'Spatial')) coordinateNames <- colnames(dataStructured[[dataAdd]]@coords)
      if (inherits(dataStructured[[dataAdd]], 'sf')) coordinateNames <- colnames(st_coordinates(dataStructured[[dataAdd]]))

    }

    if (is.null(private$Area)) stop('An area needs to be provided before adding species. This may be done with the `.$addArea` function.')

    if (missing(datasetType) || !datasetType %in% c('PO', 'PA', 'Counts')) stop('datasetType needs to be one of "PO", "PA" or "Counts".')

    if (missing(responseName) && datasetType %in% c('PA', 'Counts')) stop('responseName cannot be missing for PA and counts datasets.')

    if (datasetType == 'PA') responseNew <- private$responsePA
    if (datasetType == 'Counts') responseNew <- private$responseCounts
    if (datasetType == 'PO') responseNew <- '.__PORESP.__' #NULL

    if (missing(trialsName)) trialsName <- NULL
    if (missing(responseName)) responseName <- NULL
    #if (missing(speciesName)) speciesName <- NULL

    dataStructured[[dataAdd]][[speciesName]] <- sub(" ", "_", dataStructured[[dataAdd]][[speciesName]])

    uniqueSpecies <- unique(dataStructured[[dataAdd]][[speciesName]])

    if (identical(uniqueSpecies, character(0))) stop('Dataset has 0 rows. Please remove the dataset.')

    if (!all(uniqueSpecies %in% sub(" ", "_", private$Species))) {

      warning('Species found in dataset not specified in the original startWorkflow call. Removing observations for those species.')

      dataStructured[[dataAdd]] <- dataStructured[[dataAdd]][dataStructured[[dataAdd]][[speciesName]] %in% sub(" ", "_", private$Species), ]

      uniqueSpecies <- unique(dataStructured[[dataAdd]][[speciesName]])

      if (nrow(dataStructured[[dataAdd]]) == 0) stop('All species removed from this dataset. Please specify all the species name using the "Species" argument in startWorkflow.')

    }

if (!private$richnessEstimate) {

  for (species in uniqueSpecies) {
##Do a if not Richness model
    #If richness then don't filter
    strucData <- formatStructured(data = dataStructured[[dataAdd]][data.frame(dataStructured[[dataAdd]])[speciesName] == species,],
                                                                     type =  datasetType,
                                                                     varsOld = list(trials = trialsName,
                                                              response = responseName,
                                                              species = speciesName,
                                                              coordinates = coordinateNames),
                                               varsNew = list(coordinates = private$Coordinates,
                                                              response = responseNew,
                                                              trials = private$trialsName,
                                                              species = private$speciesName),
                                               projection = private$Projection,
                                               boundary = private$Area)

    if (nrow(strucData) > 0) {

      private$dataStructured[[species]][[dataAdd]] <- strucData
      nmRM <- FALSE

    } else nmRM <- TRUE

  }

} else {

strucData <- formatStructured(data = dataStructured[[dataAdd]],
                              type =  datasetType,
                              varsOld = list(trials = trialsName,
                              response = responseName,
                              species = speciesName,
                              coordinates = coordinateNames),
                              varsNew = list(coordinates = private$Coordinates,
                              response = responseNew,
                              trials = private$trialsName,
                              species = private$speciesName),
                              projection = private$Projection,
                              boundary = private$Area)
if (nrow(strucData) > 0) {

  private$dataStructured[[dataAdd]][[dataAdd]] <- strucData
  nmRM <- FALSE

} else nmRM <- TRUE

}

    if (datasetType == 'PA') {

    if (generateAbsences) {

      if (length(uniqueSpecies) == 1) warning("Can't generate absences if only one species is specified.")

#Fix this for Richness model
      if (length(private$dataStructured) > 0) {

      private$dataStructured <- generateAbsences(dataList = private$dataStructured, speciesName = speciesName,
                                                 datasetName = dataAdd, responseName = responseName,
                                                 Projection = private$Projection, Richness = private$richnessEstimate)

      }


    }
  }

    }

    if(!nmRM) private$datasetName <- c(datasetName, private$datasetName)



  }
  ,

#' @description Function to add an \code{inla.mesh} object to the workflow. The user may either add their own mesh to the workflow, or use the arguments of this function to help create one.
#' @param Object An \code{inla.mesh} object to add to the workflow.
#' @param ... Additional arguments to pass to \code{INLA}'s \code{inla.mesh.2d}. Use \code{?inla.mesh.2d} to find out more about the different arguments.
#' @examples
#' if (requireNamespace('INLA')) {
#' \dontrun{
#' workflow <- startWorkflow(Species = 'Fraxinus excelsior',
#'                           Projection = '+proj=longlat +ellps=WGS84',
#'                           Save = FALSE,
#'                           saveOptions = list(projectName = 'example'))
#'
#' #Add boundary
#' workflow$addArea(countryName = 'Sweden')
#' workflow$addMesh(cutoff = 20000,
#'                  max.edge=c(60000, 80000),
#'                  offset= 100000)
#'
#' }
#' }

  addMesh = function(Object,
                     ...) {

    if (missing(Object) &&
        is.null(private$Area)) stop ('Please provide an inla.mesh object or use the ... argument to specify a mesh.')

    if (missing(Object)) {

      meshArgs <- list(...)
      if (length(meshArgs) == 0) stop('Please provide ... to specify the mesh construction. See ?inla.mesh.2d for more details.')

      meshObj <- INLA::inla.mesh.2d(boundary = fmesher::fm_as_segm(private$Area[1]),
                                    crs = inlabru::fm_crs(private$Projection),
                                         ...
                                          )

      private$Mesh <- meshObj

    }
    else {

      if (!inherits(Object, 'inla.mesh')) stop('Object provided is not an inla.mesh object.')
#Ensure CRS is the same
      private$Mesh <- Object

    }

    if (!private$Quiet) message('INLA mesh added successfully.')

    }
  ,

#' @description Function to add species occurrence records from GBIF (using the \code{rgbif} package) to the reproducible workflow. The arguments for this function are used to either filter the GBIF records, or to specify the characteristics of the observation model.
#' @param Species The names of the species to include in the workflow (initially specified using \link[intSDM]{startWorkflow}). Defaults to \code{All}, which will find occurrence records for all specie specified in \link[intSDM]{startWorkflow}.
#' @param datasetName The name to give the dataset obtained from GBIF. Cannot be \code{NULL}.
#' @param datasetType The data type of the dataset. Defaults to \code{PO}, but may also be \code{PA} or \code{Counts}.
#' @param removeDuplicates Argument used to remove duplicate observations for a species across datasets. May take a long time if there are many observations obtained across multiple datasets. Defaults to \code{FALSE}.
#' @param generateAbsences Generates absences for \code{'PA'} data. This is done by combining all the sampling locations for all the species, and creating an absence where a given species does not occur.
#' @param ... Additional arguments to specify the \link[rgbif]{occ_data} function from \code{rgbif}. See \code{?occ_data} for more details.
#' @examples
#' \dontrun{
#' workflow <- startWorkflow(Species = 'Fraxinus excelsior',
#'                           Projection = '+proj=longlat +ellps=WGS84',
#'                           Save = FALSE,
#'                           saveOptions = list(projectName = 'example'))
#' workflow$addArea(countryName = 'Sweden')
#'
#' workflow$addGBIF(datasetName = 'exampleGBIF',
#'                  datasetType = 'PA',
#'                  limit = 10000,
#'                  coordinateUncertaintyInMeters = '0,50')
#' }

addGBIF = function(Species = 'All', datasetName = NULL,
                   datasetType = 'PO',
                   removeDuplicates = FALSE,
                   generateAbsences = FALSE, ...) {

  if (is.null(private$Area)) stop('An area needs to be provided before adding species. This may be done with the `.$addArea` function.')

  if (is.null(datasetName)) stop('Please provide a name to give your dataset using datasetName.')

  if (Species == 'All') Species <- private$Species
  else if (!all(Species %in% private$Species)) stop ('Species provided not specified in startWorkflow().')

  if (datasetName %in% names(private$dataGBIF)) warning ('datasetName already provided before. The older dataset will therefore be removed.')

  ##To do here:
   #Keep only the relevant PA/Counts variables

  if (!datasetType %in% c('PO', 'PA', 'Counts')) stop('datasetType needs to be one of PO, PA and Counts.')

  ##Change the speciesName argument to private$speciesName somewhere
   #Are we changing for Name or scientific name?

  for (speciesName in Species) {

  if (!private$Quiet) message(paste('Finding GBIF observations for:', speciesName,'\n'))

    #Change this if Richness
  try(GBIFspecies <- obtainGBIF(query = speciesName,
                                                #datasetName = datasetName,
                            geometry = private$Area,
                                                #country = private$Countries,
                            projection = private$Projection,
                                                #varsKeep = c(responseCounts, responsePA),
                            datasettype = datasetType,
                            ...))

  if (!inherits(GBIFspecies, 'try-error')) {

  if (removeDuplicates) {

    if (!private$richnessEstimate) {

  if (length(private$dataGBIF[[sub(" ", '_', speciesName)]]) > 0) {

    anySame <- st_equals_exact(do.call(c, lapply(private$dataGBIF[[sub(" ", '_', speciesName)]], function(x) st_geometry(x))),
                               GBIFspecies,
                               par = 0)

    #anySame <- GBIFspecies$key %in% unique(lapply(private$dataGBIF[[sub(" ", '_', speciesName)]], function(x) x$key))
    #if (sum(anySame > 0)) {
    if (!identical(unlist(anySame), integer(0))) {

      warning('Removing duplicate observations obtained from previous calls of `.$addGBIF`')

      GBIFspecies <- GBIFspecies[-c(unlist(anySame)),]


    }


  }

    }

    }

  if (nrow(GBIFspecies) == 0) warning('All species observations were removed due to duplicates')
  else {
#Change if Richness
    ##Subset variables here: then if Richness do.call(rbind)
    #Need response variables + species Name + datasetKey
    GBIFspecies <- GBIFspecies[,names(GBIFspecies) %in% c('datasetKey', 'sampleSizeValue',
                                                          private$responsePA,
                                                          private$responseCounts,
                                                          private$trialsName)]
    GBIFspecies$speciesName <- speciesName

    if (!private$richnessEstimate) {

    private$dataGBIF[[sub(" ", '_', speciesName)]][[datasetName]] <- GBIFspecies
    private$classGBIF[[sub(" ", '_', speciesName)]][[datasetName]] <- datasetType

    } else {

      private$dataGBIF[[datasetName]][[datasetName]] <- rbind(private$dataGBIF[[datasetName]][[datasetName]], GBIFspecies)
      private$classGBIF[[datasetName]][[datasetName]] <- datasetType #Need to do something here?

    }

  }
}

  }

  if (datasetType == 'PA') {

    if (generateAbsences) {

      if (length(Species) == 1) warning("Can't generate absences if only one species is specified.")

      private$dataGBIF <- generateAbsences(dataList = private$dataGBIF, speciesName = 'species', datasetName = datasetName,
                                           responseName = private$responsePA, Projection = private$Projection, Richness = private$richnessEstimate)


    }
  }

  private$datasetName <- c(datasetName, private$datasetName)

  }
  ,

#' @description Function to add spatial covariates to the workflow. The covariates may either be specified by the user, or they may come from worldClim obtained with the \code{geodata} package.
#' @param Object A object of class: \code{spatRaster}, \code{SpatialPixelsDataFrame} or \code{raster} containing covariate information across the area. Note that this function will check if the covariates span the boundary area, so it may be preferable to add your own boundary using \code{`.$addArea`} if this argument is specified.
#' @param worldClim Name of the worldClim to include in the model. See \code{?worldclim_country} from the \code{geodata} package for more information.
#' @param Months The months to include the covariate for. Defaults to \code{All} which includes covariate layers for all months.
#' @param res Resolution of the worldclim variable. Valid options are: \code{10}, \code{5}, \code{2.5} or \code{0.5} (minutes of a degree).
#' @param Function The function to aggregate the temporal data into one layer. Defaults to \code{mean}.
#' @param ... Not used.
#'
#' @import terra
#' @examples
#' \dontrun{
#' if (requireNamespace('INLA')) {
#'
#' workflow <- startWorkflow(Species = 'Fraxinus excelsior',
#'                           Projection = '+proj=longlat +ellps=WGS84',
#'                           Save = FALSE,
#'                           saveOptions = list(projectName = 'example'))
#'
#' #Add boundary
#' workflow$addArea(countryName = 'Sweden')
#' workflow$addCovariates(worldClim = 'tavg', res = '10')
#'
#' }
#'}

  addCovariates = function(Object = NULL,
                           worldClim = NULL,
                           res = 2.5,
                           Months = 'All',
                           Function = 'mean', ...) {

    if (is.null(private$Area)) stop("Area is required before obtaining covariates. Please use `.$addArea()`.")

    if (all(is.null(Object),
            is.null(worldClim))) stop ('One of object or worldClim is required..')


    if (length(worldClim) > 1) stop ('Please only add one worldClim variable at a time.')


  if (is.null(Object)) {

    if (!worldClim %in% c("tavg", "tmin", "tmax",
                          "prec", "bio", "bioc",
                          "elev", "wind", "vapr", "srad")) stop('worldClim argument is not a valid option.')

    if (is.null(Months) && is.null(Function)) stop('Both of Months or Function need to be specified.')

    months <- c('January', 'February', 'March', 'April', 'May', 'June',
                'July', 'August', 'September', 'October', 'November', 'December')

    if (!any(Months %in% c(months, 'All'))) stop ('Month provided is not valid.')

    if (all(Months == 'All')) covIndex <- 1:12
    else covIndex <- match(Months, months)

    covDirectory <- paste0(private$Directory, '/Covariates')
    dir.create(covDirectory, showWarnings = FALSE)
    if (!dir.exists(covDirectory)) covDirectory <- getwd()#dir.create(covDirectory)
    if (!private$Quiet) message(paste('Saved covariate objects may be found in', covDirectory))

    #if (is.null(private$Countries)) stop('Please specify a country first before obtaining a covariate layer. This may be done using either startWorkflow or through `.$addArea`.')

    covRaster <- obtainCovariate(covariates = worldClim,
                                 res = res,
                                 as.character(private$Projection),
                                 path = covDirectory)

    covRaster <- terra::mask(terra::crop(covRaster, private$Area), private$Area)

    if (deparse(substitute(Function)) %in% c('mean', 'median', 'sd')) covRaster <- terra::app(covRaster[[covIndex]], fun = Function)
    else covRaster <- terra::app(terra::app(covRaster[[covIndex]], fun = mean), fun = Function)

    names(covRaster) <- worldClim

    private$Covariates[[worldClim]] <- covRaster


  }
    else {

      if (!inherits(Object, c('SpatRaster', 'Spatial', 'Raster'))) stop('Object needs to be either a SpatRaster, SpatialPixelsDataFrame or raster object.')

      if (!inherits(Object, 'SpatRaster')) Object <- as(Object, 'SpatRaster')

      Object <- terra::project(Object, private$Projection)

      #if (length(names(Object)) > 1) stop('Please provide each covariate into the workflow as their own object.')

      for (cov in names(Object)) {

      #Check this for all classes
      maskedDF <- terra::crop(terra::mask(Object[cov][[1]], private$Area), private$Area)

      if (all(is.na(terra::values(maskedDF)))) stop('The covariate provided and the area specified do not match.')

      private$Covariates[[cov]] <- Object[cov]

}
      #else get name of object and then save it

    }

    }
  ,

#' @description Function to add a spatial cross validation method to the workflow.
#' @param Method The spatial cross-validation methods to use in the workflow. May be at least one of \code{spatialBlock} or \code{Loo} (leave-one-out). See the \code{PointedSDMs} package for more details.
#' @param blockOptions A list of options to specify the spatial block cross-validation. Must be a named list with arguments specified for: \code{k}, \code{rows_cols}, \code{plot}, \code{seed}. See \code{blockCV::cv_spatial} for more information.
#' @param blockCVType The cross-validation method to complete if \code{Method = 'spatialBlock'}. May be one of \code{'DIC'} (default) which will iteratively return the DIC scores for each block, or \code{'Predict'}. This method return scores of marginal likelihood for each combination of dataset across all blocks, by fitting a model on all blocks but one, and predicting on the left out block. The prediction dataset is automatically chosen as the first PA dataset added to the model. See \link[PointedSDMs]{blockedCV} for more information. Note that this may take a long time to estimate if there are many datasets included in the model.
#'
#' @import blockCV
#' @examples
#' workflow <- startWorkflow(Species = 'Fraxinus excelsior',
#'                           Projection = '+proj=longlat +ellps=WGS84',
#'                           Save = FALSE,
#'                           saveOptions = list(projectName = 'example'))
#'
#' workflow$crossValidation(Method = 'Loo')

  crossValidation = function(Method, blockOptions = list(k = 5, rows_cols = c(4,4), plot = FALSE, seed = 123),
                             blockCVType = 'DIC') {

   #Make new argument called predictOptions

    if (!all(Method %in% c('spatialBlock', 'Loo'))) stop('Method needs to be at least one of: spatialBlock, Loo.')

    if ('spatialBlock' %in% Method) {

      if (!blockCVType %in% c('DIC', 'Predict')) stop('blockCVType must be one of "DIC" or "Predict"')

      if (blockCVType == 'Predict') {

        paStructured <- any(unlist(lapply(unlist(private$dataStructured, recursive = FALSE), function(x) {

          private$responsePA %in% names(x)

        })))

        if (!'PA' %in% unique(unlist(private$classGBIF)) & !paStructured) warning('At least one PA dataset is needed before using blockCVType as "Predict".')

      }

      private$blockCVType <- blockCVType

     if (is.null(blockOptions$seed)) blockOptions$seed <- round(abs(rnorm(n = 1, mean = 100000, sd = 100000)))

     if (is.null(blockOptions$k) || is.null(blockOptions$rows_cols)) stop('Please provide both k and rows_cols in blockOptions.')
     if (is.null(blockOptions$plot)) blockOptions$plot <- FALSE

     private$blockOptions <- blockOptions

     if (blockOptions$plot) {

     boundData <- vector(mode = 'list', length =  length(private$Species))
     spatPlot <- vector(mode = 'list', length =  length(private$Species))
     names(spatPlot) <- paste('Printing plot for',private$Species)


       for (species in gsub(' ', '_', private$Species)) { #1:length

       spatData <- lapply(append(private$dataGBIF[[species]], private$dataStructured[[species]]), function(x) x$geometry)

       boundData <- st_as_sf(do.call(c, spatData))


       blocks <- R.devices::suppressGraphics(blockCV::cv_spatial(x = boundData,
                                                                 k = blockOptions$k, rows_cols = blockOptions$rows_cols,
                                                                 progress = FALSE, seed = blockOptions$seed,
                                                                 report = FALSE, plot = TRUE))

       boundData$.__block_index <- blocks$folds_ids

       blocksPlot <- blockCV::cv_plot(blocks)

        spatPlot[[species]] <-  ggplot() +
         geom_sf(data = blocksPlot$data, aes(col = as.character(folds)), size = 2) +
         geom_sf_text(data = blocksPlot$data, aes(label = folds)) +
         geom_sf(data = sf::st_boundary(private$Area)) +
         geom_sf(data = boundData, aes(col = as.character(.__block_index))) +
         ggtitle(paste('Cross-validation blocking for', species)) +
         guides(col=guide_legend(title="Folds"))



       }

     private$CVMethod <- Method
     return(spatPlot)


     }


    }

    private$CVMethod <- Method

  }
  ,

#' @description Function to specify model options for the \code{INLA} and \code{PointedSDMs} parts of the model.
#' @param ISDM Arguments to specify in \link[PointedSDMs]{startISDM} from the \code{PointedSDMs} function. This argument needs to be a named list of the following options:
#' \enumerate{\item{\code{pointCovariates}: non-spatial covariates attached to the data points to be included in the model.} \item{\code{pointsIntercept}: Logical: intercept terms for the dataset. Defaults to \code{TRUE}} \item{\code{pointsSpatial}: Choose how the spatial effects are included in the model. If \code{'copy'} then the spatial effects are shared across the datasets, if \code{'individual'} then the spatial effects are created for each dataset individually, and if \code{'correlate'} then the spatial effects are correlated. If \code{NULL}, then spatial effects are turned off for the datasets.} \item{\code{Offset}: The name of the offset variable.}} See \code{?PointedSDMs::startISDM} for more details on these choices.
#' @param Richness Options to specify the richness model. This argument needs to be a named list of the following options:
#' \enumerate{\item{\code{predictionIntercept}: The name of the dataset to use as the prediction intercept in the richness model. The sampling size of the protocol must be known.} \item{\code{samplingSize}: The sample area size for the dataset provided in \code{predictionIntercept}. The units should be the same as specified in \link{startWorkflow}} \item{\code{speciesSpatial}: Specify the species spatial model. If \code{'replicate'} then create a spatial effect for each species with shared hyperparameters, if \code{'copy'} create a spatial effect for each species. If \code{NULL} then the spatial effects for the species will be turned off.}\item{\code{speciesIntercept:} If \code{TRUE} (default) incorporate a random intercept for the species, if \code{FALSE} use a fixed intercept and if \code{NULL} include no intercept for the species.}}
#' @examples
#' workflow <- startWorkflow(Species = 'Fraxinus excelsior',
#'                           Projection = '+proj=longlat +ellps=WGS84',
#'                           Save = FALSE,
#'                           saveOptions = list(projectName = 'example'))
#'
  modelOptions = function(ISDM = list(),
                          Richness = list()) {

    if (!is.list(ISDM)) stop('ISDM needs to be a list of arguments to specify the model.')

    if (any(!names(ISDM) %in% c('pointCovariates', 'pointsIntercept', #Remove pointCovariates perhaps?
                                'pointsSpatial', 'Offset'))) stop('ISDM needs to be a named list with at least one of the following options: "pointCovariates", "pointsIntercept", "pointsSpatial" or "Offset".')

    if (private$richnessEstimate) {

    if (any(!names(Richness) %in% c('predictionIntercept', 'speciesSpatial', 'samplingSize', 'speciesIntercept'))) stop('Richness needs to be a named list with at least one of the following options: "predictionIntercept".')

    if ('predictionIntercept' %in% names(Richness)) {

      if (length(Richness[['predictionIntercept']] ) > 1) stop('predictionIntercept needs to contain only one element.')

      if (!Richness[['predictionIntercept']] %in% private$datasetName) stop('predictionIntercept needs to be a name of one of the datasets in the model.')

    }

    }

    if ('speciesIntercept' %in% names(Richness)) private$speciesIntercept <- Richness[['speciesIntercept']]

    if ('samplingSize' %in% names(Richness)) private$samplingSize <- Richness[['samplingSize']]

    if (!'speciesSpatial' %in% names(Richness)) Richness[['speciesSpatial']] <- 'shared'
    else
      if (!Richness[['speciesSpatial']] %in% c('shared', 'replicate', 'copy') && !is.null(Richness[['speciesSpatial']])) stop ('speciesSpatial must be either: NULL, "replicate", "copy" or "shared.')

    if (length(ISDM) > 0) private$optionsISDM <- ISDM
    if (length(Richness) > 0) private$optionsRichness <- Richness

  }
  ,

#' @description Function to specify pc priors for the shared random field in the model. See \code{?INLA::inla.spde2.pcmatern} for more details.
#' @param ... Arguments passed on to \link[INLA]{inla.spde2.pcmatern}.
#' @examples
#' \dontrun{
#' if (requireNamespace('INLA')) {
#' workflow <- startWorkflow(Species = 'Fraxinus excelsior',
#'                           Projection = '+proj=longlat +ellps=WGS84',
#'                           Save = FALSE,
#'                           saveOptions = list(projectName = 'example'))
#'
#' #Add boundary
#' workflow$addArea(countryName = 'Sweden')
#' workflow$addMesh(cutoff = 20000,
#'                  max.edge=c(60000, 80000),
#'                  offset= 100000)
#' workflow$specifySpatial(prior.range = c(200000, 0.05),
#'                         prior.sigma = c(5, 0.1))
#' }
#' }
  specifySpatial = function(...) {


    anyIn <- list(...)
    if (length(anyIn) == 0) stop('Please provide arguments to customize the INLA spde object using the ... argument.')

    if (is.null(private$Mesh)) stop('Please add an INLA mesh before customizing the spatial fields. This may be done with the `.$addMesh` function.')

    private$sharedField <- INLA::inla.spde2.pcmatern(mesh = private$Mesh, ...)

  }
  ,
#' @description Function to specify priors for the fixed effects in the model. The priors of the fixed effects are assumed to be Gaussian; this function alows the user to specify the parameters of this distribution.
#' @param effectNames The name of the effects to specify the prior for. Must be the name of any of the covariates incldued in the model, or 'Intercept' to specify the priors for the intercept terms.
#' @param Mean The mean of the prior distribution. Defaults to \code{0}.
#' @param Precision The precision (inverse variance) of the prior distribution. Defaults to \code{0.01}.
#' @param copyModel List of model specifications given to the hyper parameters for the \code{"copy"} model. Defaults to \code{list(beta = list(fixed = FALSE))}.
#' @param priorIntercept Prior for the precision parameter for the random intercept in the species richness model. Needs \code{Output = "Richness"}. Defaults to the default \emph{INLA} prior.
#' @param priorGroup Prior for the precision for the \emph{iid} effect in the species spatial effect in the richness model. Needs \code{Output = "Richness"} and \code{speciesSpatial = "replicate"} in the richness options. Defualts to the default \emph{INLA} prior.
#
#' @examples
#' \dontrun{
#' if (requireNamespace('INLA')) {
#' workflow <- startWorkflow(Species = 'Fraxinus excelsior',
#'                           Projection = '+proj=longlat +ellps=WGS84',
#'                           Save = FALSE,
#'                           saveOptions = list(projectName = 'example'))
#'
#' #Add boundary
#' workflow$addArea(countryName = 'Sweden')
#' workflow$addMesh(cutoff = 20000,
#'                  max.edge=c(60000, 80000),
#'                  offset= 100000)
#' workflow$specifyPriors(effectName = 'Intercept', mean = 0, Precision = 0.1)
#' }
#' }
specifyPriors = function(effectNames, Mean = 0, Precision = 0.01,
                         copyModel = list(beta = list(fixed = FALSE)),
                         priorIntercept = list(prior="loggamma", param = c(1, 5e-5)),
                         priorGroup = list(model = "iid", hyper = list(prec = list(prior = "loggamma", param = c(1, 5e-5))))) {

  if (!missing(effectNames)) {

  if (!all(effectNames %in% c('Intercept', unlist(lapply(private$Covariates, names))))) stop('effectNames must be the names of the effects added with .$addCovariates or "Intercept".')

  for (effect in effectNames) {

    private$priorsFixed[[effect]] <- c(Mean = Mean, Prec = Precision)

  }

  }

  private$priorIntercept <- priorIntercept#deparse1(priorIntercept)
  private$priorGroup <- priorGroup#deparse1(priorGroup)
  private$copyModel <- copyModel#deparse1(copyModel)
  ##Something here for the random effects prior for the random iid model



}
,
#' @description Function to add bias fields to the model.
#' @param datasetName Name of the dataset to add a bias field to.
#' @param copyModel Create copies of the biasField across the different datasets. Defaults to \code{FALSE}.
#' @param shareModel Share a bias field across the datasets specified with \code{datasetNames}. Defaults to \code{FALSE}.
#' @param ... Additional arguments passed on to \link[INLA]{inla.spde2.pcmatern} to customize the priors for the pc matern for the bias fields.
#' @examples
#' \dontrun{
#' if(requireNamespace('INLA')) {
#'
#' workflow <- startWorkflow(Species = 'Fraxinus excelsior',
#'                           Projection = '+proj=longlat +ellps=WGS84',
#'                           Save = FALSE,
#'                           saveOptions = list(projectName = 'example'))
#' workflow$addArea(countryName = 'Sweden')
#'
#' workflow$addGBIF(datasetName = 'exampleGBIF',
#'                  datasetType = 'PA',
#'                  limit = 10000,
#'                  coordinateUncertaintyInMeters = '0,50')
#' workflow$biasFields(datasetName = 'exampleGBIF')
#' }
#' }

  biasFields = function(datasetName,
                        copyModel = FALSE,
                        shareModel = FALSE,
                        ...) {

    if (!all(datasetName %in% private$datasetName)) stop('Dataset specified for bias field not included in the workflow.')

    if (is.null(private$Mesh)) stop('Please add an INLA mesh before customizing the spatial fields. This may be done with the `.$addMesh` function.')

    if (copyModel && shareModel) stop('Only one of copyModel and shareModel may be TRUE.')

    private$biasNames <- unique(c(datasetName, private$biasNames))

    if (length(list(...)) > 0) {

      biasModels <- INLA::inla.spde2.pcmatern(mesh = private$Mesh, ...)

      } else biasModels <- INLA::inla.spde2.matern(mesh = private$Mesh)

      for (dataset in datasetName) {

      if (shareModel) private$biasFieldsSpecify[['sharedBias']] <- biasModels
      else private$biasFieldsSpecify[[dataset]] <- biasModels
      private$biasFieldsCopy <- copyModel
      private$biasFieldsShare <- shareModel

      }




  }
  ,
#' @description Add a formula to the model
#' @param covariateFormula Change the covariate formula of the model.
#' @param biasFormula Change the bias formula of the model
#' @examples
#' \dontrun{
#'
#' workflow <- startWorkflow(Species = 'Fraxinus excelsior',
#'                           Projection = '+proj=longlat +ellps=WGS84',
#'                           Save = FALSE,
#'                           saveOptions = list(projectName = 'example'))
#' workflow$addArea(countryName = 'Sweden')
#'
#' workflow$addCovariate(rasterStack)
#'
#' workflow$addFormula(covariateFormula = ~ covariate)
#' workflow$addFormula(biasFormula = ~ biasFormula)
#'
#'
#' }

modelFormula = function(covariateFormula, biasFormula) {

  if (!missing(covariateFormula)) private$covariateFormula <- covariateFormula

  #Check if all variables in here are in the covariates

  if (!missing(biasFormula)) private$biasFormula <- biasFormula


}
  ,

#' @description Obtain metadata from the workflow.
#' @param Number Print the number of observations per dataset. Defaults to \code{TRUE}.
#' @param Citations Print the citations for the GBIF obtained datasets. Defaults to \code{TRUE}.
#' @examples
#' \dontrun{
#' workflow <- startWorkflow(Species = 'Fraxinus excelsior',
#'                           Projection = '+proj=longlat +ellps=WGS84',
#'                           Save = FALSE,
#'                           saveOptions = list(projectName = 'example'))
#' workflow$addArea(countryName = 'Sweden')
#'
#' workflow$addGBIF(datasetName = 'exampleGBIF',
#'                  datasetType = 'PA',
#'                  limit = 10000,
#'                  coordinateUncertaintyInMeters = '0,50')
#' workflow$obtainMeta()
#' }

obtainMeta = function(Number = TRUE,
                      Citations = TRUE) {

  cat('Metadata for the workflow:\n')

  cat('Time started:\n'); cat(as.character(private$timeStarted), '\n')

  if (Number) {

  cat('Number of observations in dataset:\n')

  for (species in sub(" ", "_", private$Species)) {

    cat(paste0(species, ':'),'\n')

    if (length(private$dataGBIF) > 0) speciesNumGBIF <- unlist(lapply(private$dataGBIF[[species]], nrow))
    else speciesNumGBIF <- NULL

    if (length(private$dataStructured) > 0) speciesNumStructured <- unlist(lapply(private$dataGBIF[[species]], nrow))
    else speciesNumStructured <- NULL

    speciesNum <- c(speciesNumGBIF, speciesNumStructured)

    numData <- data.frame(Dataset = names(speciesNum),
                          Place = rep('-', length(speciesNum)),
                          Number = speciesNum)
    colnames(numData) <- c('Dataset', '', '#')
    print.data.frame(numData, row.names = FALSE, right = TRUE)
    cat('Sum:');cat('',sum(speciesNum))
    cat('\n\n')

  }

  }


  if (Citations) {

  if (any(names(private$Covariates) %in% c("tavg", "tmin", "tmax",
                                            "prec", "bio", "bioc",
                                            "elev", "wind", "vapr", "srad"))) {

    cat('Citation for WorldClim Data:\n')
    cat('Fick, S.E. and R.J. Hijmans, 2017. WorldClim 2: new 1km spatial resolution climate surfaces for global land areas. \nInternational Journal of Climatology 37 (12): 4302-4315.\n\n')

  }

  if (length(private$dataGBIF) == 0) stop('Please call .$addGBIF() to obtain data from GBIF.')

  cat('Citations for GBIF data:\n')
  datasetKeys <- c(na.omit(unique(unlist(lapply(unlist(private$dataGBIF, recursive = FALSE), function(x) x$datasetKey)))))

  listKeys <- vector(mode = 'list', length = length(datasetKeys))
  names(listKeys) <- datasetKeys

  for (key in datasetKeys) listKeys[[key]] <- suppressWarnings(rgbif::gbif_citation(x = key))

  print(listKeys)

  }


}
  ))

species_model$set('private', 'Area', NULL)
species_model$set('private', 'Covariates', list())
species_model$set('private', 'biasCovariates', list())
species_model$set('private', 'Mesh', NULL)
species_model$set('private', 'optionsISDM', list())
species_model$set('private', 'optionsINLA', list())
species_model$set('private', 'optionsRichness', list())
species_model$set('private', 'optionsIpoints', list())
species_model$set('private', 'CVMethod', NULL)
species_model$set('private', 'Species', NULL)
species_model$set('private', 'Countries', NULL)
species_model$set('private', 'Output', NULL)
species_model$set('private', 'Projection', NULL)
species_model$set('private', 'dataStructured', list())
species_model$set('private', 'dataGBIF', list())
species_model$set('private', 'Quiet', TRUE)
species_model$set('private', 'Directory', getwd())
species_model$set('private', 'Project', NULL)
species_model$set('private', 'sharedField', NULL)
species_model$set('private', 'biasFieldsCopy', FALSE)
species_model$set('private', 'biasFieldsShare', FALSE)
species_model$set('private', 'timeStarted', NULL)
#species_model$set('private', 'datasetFieldsSpecify', list())
species_model$set('private', 'biasFieldsSpecify', list())
species_model$set('private', 'datasetName', NULL)
species_model$set('private', 'Save', TRUE)
species_model$set('private', 'biasNames', NULL)
species_model$set('private', 'biasCovNames', NULL)
species_model$set('private', 'blockOptions', list())
species_model$set('private', 'classGBIF', list())
species_model$set('private', 'priorsFixed', list())
species_model$set('private', 'priorGroup', list(model = "iid", hyper = list(prec = list(prior = 'loggamma', param = c(1, 5e-5)))))
species_model$set('private', 'priorIntercept', list(prior="loggamma", param = c(1, 5e-5)))
species_model$set('private', 'copyModel', list(beta = list(fixed = FALSE)))
species_model$set('private', 'samplingSize', NULL)
species_model$set('private', 'covariateFormula', NULL)
species_model$set('private', 'biasFormula', NULL)
species_model$set('private', 'richnessEstimate', FALSE)
species_model$set('private', 'speciesIntercept', TRUE)
species_model$set('private', 'blockCVType', 'DIC')



##Change all the variable names for the GBIF data too
species_model$set('private', 'Coordinates', c('Longitude', 'Latitude'))
species_model$set('private', 'responseCounts', 'individualCount')
species_model$set('private', 'responsePA', 'occurrenceStatus')
species_model$set('private', 'trialsName', 'numTrials')
species_model$set('private', 'speciesName', 'speciesName')


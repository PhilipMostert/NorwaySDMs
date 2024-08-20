#' @title \code{sdmWorkflow}: Function to compile the reproducible workflow.
#' @description This function is used to compile the reproducible workflow from the \code{R6} object created with \code{startFunction}. Depending on what was specified before, this function will estimate the integrated species distribution model, perform cross-validation, create predictions from the model and plot these predictions.
#' @param Workflow The \code{R6} object created from \code{startWorkflow}. This object should contain all the data and model information required to estimate and specify the model.
#' @param predictionDim The pixel dimensions for the prediction maps. Defaults to \code{c(150, 150)}.
#' @param predictionData Optional argument for the user to specify their own data to predict on. Must be a \code{sf} or \code{SpatialPixelsDataFrame} object. Defaults to \code{NULL}.
#' @param initialValues Find initial values using a GLM before the model is estimated. Defaults to \code{FALSE}.
#' @param inlaOptions Options to specify in \link[INLA]{inla} from the \code{inla} function. See \code{?inla} for more details.
#' @param ipointsOptions Options to specify in \link[inlabru]{fm_int}'s \code{int.args} argument. See \code{?fmesher::fm_int} for more details.

#'
#' @import PointedSDMs
#'
#' @return The return of the function depends on the argument \code{Save} from the \code{startWorkflow} function. If this argument is \code{FALSE} then the objects will be saved to the specidfied directory. If this argument is \code{TRUE} then a list of different outcomes from the workflow will be returned.
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace('INLA')) {
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
#' workflow$addMesh(cutoff = 20000,
#'                  max.edge=c(60000, 80000),
#'                  offset= 100000)
#' workflow$workflowOutput('Model')
#'
#' Model <- sdmWorkflow(workflow)
#'
#' }
#' }

sdmWorkflow <- function(Workflow = NULL,
                        predictionDim = c(150, 150),
                        predictionData = NULL,
                        initialValues = FALSE,
                        inlaOptions = list(),
                        ipointsOptions = NULL) {

  modDirectory <- Workflow$.__enclos_env__$private$Directory
  saveObjects <- Workflow$.__enclos_env__$private$Save
  Quiet <- Workflow$.__enclos_env__$private$Quiet

  if (is.null(Workflow$.__enclos_env__$private$Mesh)) stop('An inla.mesh object is required before any analysis is completed. Please add using the `.$addMesh` function.')
  if (is.null(Workflow$.__enclos_env__$private$Output)) stop('A model output needs to be specified before any analysis is completed. Please add using the `.$workflowOutput` function.')

  Oputs <- Workflow$.__enclos_env__$private$Output
  outputList <-list()

  if ('Bias' %in% Oputs &&
      all(is.null(Workflow$.__enclos_env__$private$biasNames),
          is.null(Workflow$.__enclos_env__$private$biasFormula))) stop('Bias specified as output but no bias fields were added. Please do this with .$biasFields')

  if (is.null(Workflow$.__enclos_env__$private$CVMethod) && 'Cross-Validation' %in% Workflow$.__enclos_env__$private$Output) stop('Cross-validation specified as model output but no method provided. Please specify cross-validation method using the `.$crossValidation` function.')

  ##Need to do it on a species by species basis:

  if (length(Workflow$.__enclos_env__$private$optionsISDM) > 0) {

    .__pointsSpatial.__ <- Workflow$.__enclos_env__$private$optionsISDM$pointsSpatial

    if (!is.null(Workflow$.__enclos_env__$private$optionsISDM[['pointsIntercept']])) .__pointsIntercept.__ <- Workflow$.__enclos_env__$private$optionsISDM$pointsIntercept
    else .__pointsIntercept.__ <- TRUE

    if (!is.null(Workflow$.__enclos_env__$private$optionsISDM[['pointCovariates']])) .__pointCovariates.__ <- Workflow$.__enclos_env__$private$optionsISDM$pointsIntercept
    else .__pointCovariates.__ <- NULL

    if (!is.null(Workflow$.__enclos_env__$private$optionsISDM[['Offset']])) .__Offset.__ <- Workflow$.__enclos_env__$private$optionsISDM$Offset
    else .__Offset.__ <- NULL


  } else {

    if (!Workflow$.__enclos_env__$private$richnessEstimate) .__pointsSpatial.__ <- 'copy'
    else  .__pointsSpatial.__ <- NULL
    .__pointsIntercept.__ <- TRUE
    .__Offset.__ <- NULL
    .__pointCovariates.__ <- NULL

  }

  if (!is.null(Workflow$.__enclos_env__$private$copyModel)) .__copyModel.__ <- Workflow$.__enclos_env__$private$copyModel
  else .__copyModel.__ <- deparse1('list(beta = list(fixed = FALSE))')

  .__mesh.__ <- Workflow$.__enclos_env__$private$Mesh
  .__proj.__ <- Workflow$.__enclos_env__$private$Projection
  .__coordinates.__ <- Workflow$.__enclos_env__$private$Coordinates
  .__responsePA.__ <- Workflow$.__enclos_env__$private$responsePA
  .__responseCounts.__ <- Workflow$.__enclos_env__$private$responseCounts
  .__trialsName.__ <- Workflow$.__enclos_env__$private$trialsName

  if (length(Workflow$.__enclos_env__$private$Covariates) > 0) spatCovs <- terra::rast(Workflow$.__enclos_env__$private$Covariates)
  else spatCovs <- NULL

  spatCovs <- do.call(c, unlist(list(spatCovs, Workflow$.__enclos_env__$private$biasCovariates), recursive = FALSE))

  IPS <- fm_int(domain = .__mesh.__, samplers = Workflow$.__enclos_env__$private$Area,
                int.args = ipointsOptions)
  st_geometry(IPS) <- 'geometry'

  if (!Workflow$.__enclos_env__$private$richnessEstimate) {

  for (species in unique(c(names(Workflow$.__enclos_env__$private$dataGBIF),
                           names(Workflow$.__enclos_env__$private$dataStructured)))) {

   speciesNameInd <- sub(' ', '_', species)

   if (saveObjects) dir.create(path = paste0(modDirectory, '/', speciesNameInd))

   speciesDataset <- append(Workflow$.__enclos_env__$private$dataGBIF[[species]],
                            Workflow$.__enclos_env__$private$dataStructured[[species]])

   speciesDataset <- speciesDataset[sapply(speciesDataset, nrow) > 0]

  if (length(speciesDataset) == 0)  {

    warning(paste('No data added to the model for species', paste0(species,'.'), 'Skipping run.'))

  }
else {

  if (!Quiet) {

    message(paste('\nStarting model for', species, '\n\n'))
    message('\nInitializing model', '\n\n')

  }

   if (length(speciesDataset) == 1) {

     initializeModel <- specifyISDM$new(data = speciesDataset,
                            projection = .__proj.__,
                            Inlamesh = .__mesh.__,
                            initialnames = names(speciesDataset),
                            responsecounts = .__responseCounts.__,
                            responsepa = .__responsePA.__,
                            pointcovariates = .__pointCovariates.__,
                            trialspa = .__trialsName.__,
                            spatial = 'individual',
                            intercepts = .__pointsIntercept.__,
                            spatialcovariates = spatCovs,
                            boundary = Workflow$.__enclos_env__$private$Area,
                            ips = IPS,
                            temporal = NULL,
                            temporalmodel = NULL,
                            offset =  .__Offset.__,
                            copymodel = .__copyModel.__,
                            formulas = list(covariateFormula = Workflow$.__enclos_env__$private$covariateFormula,
                                            biasFormula = Workflow$.__enclos_env__$private$biasFormula))
   }
   else {

  initializeModel <- PointedSDMs::startISDM(speciesDataset, Mesh = .__mesh.__, Projection = .__proj.__,
                                            responsePA = .__responsePA.__, responseCounts = .__responseCounts.__,
                                            trialsPA = .__trialsName.__, pointsSpatial = .__pointsSpatial.__,
                                            pointsIntercept = .__pointsIntercept.__ , IPS = IPS,
                                            Offset =  .__Offset.__,
                                            pointCovariates = .__pointCovariates.__,
                                            Boundary = Workflow$.__enclos_env__$private$Area,
                                            spatialCovariates = spatCovs,
                                            Formulas = list(covariateFormula = Workflow$.__enclos_env__$private$covariateFormula,
                                                            biasFormula = Workflow$.__enclos_env__$private$biasFormula))

   }

  if (!is.null(Workflow$.__enclos_env__$private$priorsFixed)) {

    for (var in names(Workflow$.__enclos_env__$private$priorsFixed)) {

      initializeModel$priorsFixed(Effect = var, mean.linear = Workflow$.__enclos_env__$private$priorsFixed[[var]][1], prec.linear = Workflow$.__enclos_env__$private$priorsFixed[[var]][2])


    }

  }

  ##Here priors for the random effects
  if (!is.null(Workflow$.__enclos_env__$private$copyModel)) initializeModel$specifyRandom(copyModel = .__copyModel.__)
  #Workflow$specifyRandom(copyModel = x)

  if (!is.null(.__pointsSpatial.__)) {

    if (!is.null(Workflow$.__enclos_env__$private$sharedField)) {

    if (.__pointsSpatial.__ %in% c('shared', 'correlate')) initializeModel$spatialFields$sharedField$sharedField <- Workflow$.__enclos_env__$private$sharedField
    else {

      for (data in names(initializeModel$spatialFields$datasetFields)) {

        initializeModel$spatialFields$datasetFields[[data]] <- Workflow$.__enclos_env__$private$sharedField

      }

    }

    }

  }

  if (!is.null(Workflow$.__enclos_env__$private$biasNames)) {

    if (!any(names(speciesDataset) %in% Workflow$.__enclos_env__$private$biasNames)) {

      warning('Bias fields specified for datasets not in the model. Turning bias off.')

      biasIn <- FALSE

      }

    else {

    biasIn <- TRUE

    biasSubset <- Workflow$.__enclos_env__$private$biasNames[Workflow$.__enclos_env__$private$biasNames %in% names(speciesDataset)]

    initializeModel$addBias(datasetNames = biasSubset, copyModel = Workflow$.__enclos_env__$private$biasFieldsCopy, shareModel = Workflow$.__enclos_env__$private$biasFieldsShare)

    if (!is.null(Workflow$.__enclos_env__$private$biasFieldsSpecify)) {

      if (any(names(speciesDataset) %in% names(Workflow$.__enclos_env__$private$biasFieldsSpecify))) {

        specifySubset <- names(Workflow$.__enclos_env__$private$biasFieldsSpecify)[names(Workflow$.__enclos_env__$private$biasFieldsSpecify) %in% names(speciesDataset)]

      for (biasName in specifySubset) {

        initializeModel$spatialFields$biasFields[[biasName]] <- Workflow$.__enclos_env__$private$biasFieldsSpecify[[biasName]]

      }

    }

    }

    }



  } else biasIn <- FALSE

  if (!is.null(Workflow$.__enclos_env__$private$biasFormula)) biasIn <- TRUE
  else
    if (!biasIn) biasIn <- FALSE
    else biasIn <- TRUE

  if ('Cross-validation' %in% Oputs && 'spatialBlock' %in%Workflow$.__enclos_env__$private$CVMethod) {

    initializeModel$spatialBlock(k = Workflow$.__enclos_env__$private$blockOptions$k,
                                 rows_cols = Workflow$.__enclos_env__$private$blockOptions$rows_cols,
                                 seed = Workflow$.__enclos_env__$private$blockOptions$seed)

  }

  if (initialValues) Workflow$.__enclos_env__$private$optionsINLA[['bru_initial']] <- initValues(data = initializeModel, formulaComponents = initializeModel$.__enclos_env__$private$spatcovsNames)

  message('\nEstimating ISDM:\n\n')

  PSDMsMOdel <- try(PointedSDMs::fitISDM(initializeModel,
                                     options = inlaOptions))

  if (inherits(PSDMsMOdel, 'try-error')) warning(paste0('Model estimation failed for ', species,'. Will skip the rest of the outputs.'))

  if ('Model' %in% Oputs) {

    if (saveObjects) {

    if (!Quiet) message('\nSaving Model object:', '\n\n')
    saveRDS(object = PSDMsMOdel, file = paste0(modDirectory,'/', speciesNameInd, '/intModel.rds'))

    } else outputList[[speciesNameInd]][['Model']] <- PSDMsMOdel

  }

  if ('Summary' %in% Oputs) {

    if ('Fixed__Effects__Comps' %in% names(PSDMsMOdel$summary.random)) {

      fixedRandom <-PSDMsMOdel$summary.random$Fixed__Effects__Comps
      row.names(fixedRandom) <- fixedRandom$ID
      fixedRandom$ID <- NULL

    } else fixedRandom <- data.frame()

    if ('Bias__Effects__Comps' %in% names(PSDMsMOdel$summary.random)) {

      biasFixed <-PSDMsMOdel$summary.random$Bias__Effects__Comps
      row.names(biasFixed) <- biasFixed$ID
      biasFixed$ID <- NULL

    } else biasFixed <- data.frame()


    summariesIndex <- list(Fixed = rbind(PSDMsMOdel$summary.fixed, fixedRandom, biasFixed),
                           Hyper = PSDMsMOdel$summary.hyperpar)

    if (saveObjects) {

      if (!Quiet) message('\nSaving Model summaries:', '\n\n')

      saveRDS(object = summariesIndex, file = paste0(modDirectory,'/', speciesNameInd, '/modelSummary.rds'))

    } else outputList[[speciesNameInd]][['Summary']] <- summariesIndex

  }

  if ('Cross-validation' %in% Oputs && !inherits(PSDMsMOdel, 'try-error')) {


    if ('spatialBlock' %in% Workflow$.__enclos_env__$private$CVMethod) {

      if (!Quiet) message('\nEstimating spatial block cross-validation:\n\n')

      if (Workflow$.__enclos_env__$private$blockCVType == 'DIC') spatialBlockCV <- PointedSDMs::blockedCV(initializeModel, options = inlaOptions)
      else {

        blockCVPredName <- names(PSDMsMOdel$dataType)[PSDMsMOdel$dataType != 'Present only'][1]
        spatialBlockCV <- PointedSDMs::blockedCV(initializeModel, options = inlaOptions, method = 'Predict', predictName = blockCVPredName)
      }


      if (saveObjects) {

      if (!Quiet) message('\nSaving spatial blocked cross-validation object:', '\n\n')
      saveRDS(object = spatialBlockCV, file = paste0(modDirectory,'/', speciesNameInd, '/spatialBlock.rds'))

      } else outputList[[speciesNameInd]][['spatialBlock']] <- spatialBlockCV

      rm(spatialBlockCV)
    }

    if ('Loo' %in% Workflow$.__enclos_env__$private$CVMethod && !inherits(PSDMsMOdel, 'try-error')) {

      if (!Quiet) message('\nEstimating leave-one-out cross-validation:\n\n')
      LooCV <- PointedSDMs::datasetOut(model = PSDMsMOdel)

      if (saveObjects) {

      if (!Quiet) message('\nSaving leave-one-out cross-validation object:', '\n\n')
      saveRDS(object = LooCV, file = paste0(modDirectory,'/', speciesNameInd, '/LooCV.rds'))

      } else outputList[[speciesNameInd]][['LooCV']] <- LooCV

      rm(LooCV)

    }

  }

    if (any(c('Predictions', 'Maps') %in% Oputs) && !inherits(PSDMsMOdel, 'try-error')) {

      if (!Quiet) message('\nPredicting model:\n\n')

      if (is.null(predictionData)) {

        .__mask.__ <- as(Workflow$.__enclos_env__$private$Area, 'Spatial')
        predictionData <- inlabru::fm_pixels(mesh = .__mesh.__,
                                       mask = .__mask.__,
                                       dims = predictionDim)

      }



      Predictions <- predict(PSDMsMOdel, data = predictionData, predictor = TRUE)

      if (saveObjects) {

      if (!Quiet)  message('\nSaving predictions object:', '\n\n')
      saveRDS(object = Predictions, file = paste0(modDirectory,'/', speciesNameInd, '/Predictions.rds'))

      } else outputList[[speciesNameInd]][['Predictions']] <- Predictions

    }

    if ('Maps' %in% Oputs && !inherits(PSDMsMOdel, 'try-error')) {

      if (!Quiet) message("\nPlotting Maps:\n\n")

      if (saveObjects) {

      if (!Quiet) message('\nSaving plots object:', '\n\n')
       plot(Predictions)
       ggsave(filename = paste0(modDirectory,'/', speciesNameInd, '/Map.png'))

      }
      else outputList[[speciesNameInd]][['Maps']] <- plot(Predictions, plot = FALSE)

      rm(Predictions)


    }

  if ('Bias' %in% Oputs && !inherits(PSDMsMOdel, 'try-error')) {

    if (biasIn) {

    if (!Quiet) message('\nProducing bias predictions:\n\n')
      if (is.null(predictionData)) {

        .__mask.__ <- as(Workflow$.__enclos_env__$private$Area, 'Spatial')
        predictionData <- inlabru::fm_pixels(mesh = .__mesh.__,
                                             mask = .__mask.__,
                                             dims = predictionDim)

      }
    biasPreds <- predict(PSDMsMOdel,
                         data = predictionData,
                         bias = TRUE)

    if (saveObjects) {

      if (!Quiet)  message('\nSaving predictions object:', '\n\n')
      saveRDS(object = biasPreds, file = paste0(modDirectory,'/', speciesNameInd, '/biasPreds.rds'))

    } else outputList[[speciesNameInd]][['Bias']] <- biasPreds

    }

  }

}

  }


  }
  else {

    if (is.null(Workflow$.__enclos_env__$private$optionsRichness[['predictionIntercept']])) stop('predictionIntercept needs to be provided. This can be done using .$modelOptions(Richness = list(predictionIntercept = "DATASETNAME")).')

    .__predIntercept.__ <- paste0(Workflow$.__enclos_env__$private$optionsRichness[['predictionIntercept']],'_intercept')


    .__spatModel.__ <- Workflow$.__enclos_env__$private$optionsRichness[['speciesSpatial']]

    ##Fix this
     #Need to get the datasets back together
    spNames <- c(names(Workflow$.__enclos_env__$private$dataGBIF),
                 names(Workflow$.__enclos_env__$private$dataStructured))

    spData <- append(unlist(Workflow$.__enclos_env__$private$dataGBIF, recursive = FALSE),
                     unlist(Workflow$.__enclos_env__$private$dataStructured, recursive = FALSE))

    names(spData) <- spNames

    wMain <- which(spNames == Workflow$.__enclos_env__$private$optionsRichness[['predictionIntercept']])
    spData <- spData[c(spNames[wMain], spNames[-wMain])]

    message('Setting up richness model:', '\n\n')

    richSetup <- PointedSDMs::startSpecies(spData, Mesh = .__mesh.__, Projection = .__proj.__,
                                       responsePA = .__responsePA.__, responseCounts = .__responseCounts.__,
                                       trialsPA = .__trialsName.__, Boundary = Workflow$.__enclos_env__$private$Area,
                                       pointsIntercept = .__pointsIntercept.__,
                                       IPS = IPS,
                                       pointCovariates = .__pointCovariates.__,
                                       Offset =  .__Offset.__,
                                       speciesIntercept = Workflow$.__enclos_env__$private$speciesIntercept,
                                       speciesName = Workflow$.__enclos_env__$private$speciesName,
                                       speciesSpatial = .__spatModel.__,
                                       pointsSpatial = .__pointsSpatial.__, # Make this an argument
                                       spatialCovariates = spatCovs,
                                       Formulas = list(covariateFormula = Workflow$.__enclos_env__$private$covariateFormula,
                                                       biasFormula = Workflow$.__enclos_env__$private$biasFormula))

    ##Redo this
    if (!is.null(Workflow$.__enclos_env__$private$priorsFixed)) {

      for (var in names(Workflow$.__enclos_env__$private$priorsFixed)) {

        richSetup$priorsFixed(Effect = var, mean.linear = Workflow$.__enclos_env__$private$priorsFixed[[var]][1], prec.linear = Workflow$.__enclos_env__$private$priorsFixed[[var]][2])


      }

    }

    if (!is.null(Workflow$.__enclos_env__$private$sharedField)) {

      richSetup$spatialFields$speciesFields$speciesField <- Workflow$.__enclos_env__$private$sharedField

      if (!is.null(.__pointsSpatial.__)) {

        if (.__pointsSpatial.__ == 'copy') {

          for (dtt in names(richSetup$spatialFields$datasetFields)) {

        richSetup$spatialFields$datasetFields[[dtt]] <- Workflow$.__enclos_env__$private$sharedField

          }

        }

      }

      }

    #Add copy here use specifySpatial?

    if (!is.null(Workflow$.__enclos_env__$private$biasNames)) {

      richSetup$addBias(datasetNames = Workflow$.__enclos_env__$private$biasNames, copyModel = Workflow$.__enclos_env__$private$biasFieldsCopy, shareModel = Workflow$.__enclos_env__$private$biasFieldsShare)

      if (!is.null(Workflow$.__enclos_env__$private$biasFieldsSpecify)) {

        for (biasName in names(Workflow$.__enclos_env__$private$biasFieldsSpecify)) {

          richSetup$spatialFields$biasFields[[biasName]] <- Workflow$.__enclos_env__$private$biasFieldsSpecify[[biasName]]

        }


      }

    }

    #Redo this with specifyRandom
    if (!is.null(Workflow$.__enclos_env__$private$priorIntercept)) richSetup$specifyRandom(speciesIntercepts = Workflow$.__enclos_env__$private$priorIntercept)

    if (!is.null(Workflow$.__enclos_env__$private$optionsRichness$speciesSpatial)) {

    if (!is.null(Workflow$.__enclos_env__$private$priorGroup) && Workflow$.__enclos_env__$private$optionsRichness$speciesSpatial == 'replicate') richSetup$specifyRandom(speciesGroup = Workflow$.__enclos_env__$private$priorGroup)

    }
    if (initialValues)  Workflow$.__enclos_env__$private$optionsINLA[['bru_initial']] <- initValues(data = richSetup, formulaComponents = richSetup$.__enclos_env__$private$spatcovsNames)

    message('Estimating richness model:', '\n\n')

      richModel <- try(PointedSDMs::fitISDM(data = richSetup,
                                        options = inlaOptions))

      if (inherits(richModel, 'try-error')) stop('Richness model failed to estimate. Will skip the rest of the outputs.')
      else
        if ('Model' %in% Oputs) {

          if (saveObjects) {

            if (!Quiet)  message('\nSaving richness model:', '\n\n')
            saveRDS(object = richModel, file = paste0(modDirectory, '/richnessModel.rds')) #Add project name here

          } else outputList[['RichnessModel']] <- richModel


        }

      if (any(c('Predictions', 'Maps') %in% Oputs) && !inherits(richModel, 'try-error')) {

      if (is.null(predictionData)) {

        .__mask.__ <- as(Workflow$.__enclos_env__$private$Area, 'sf')
        predictionData <- inlabru::fm_pixels(mesh = .__mesh.__,
                                             mask = .__mask.__,
                                             dims = predictionDim)


      }

      if (is.null(.__spatModel.__)) .__predSpat.__ <- NULL
      else .__predSpat.__ <- '+speciesShared'
      #If copy then add here too

      if (is.null(Workflow$.__enclos_env__$private$optionsISDM$pointsSpatial)) .__pointSpat.__ <- NULL
      else
        if (Workflow$.__enclos_env__$private$optionsISDM$pointsSpatial == 'shared') .__pointSpat.__ <- '+shared_spatial'
      else .__pointSpat.__ <- paste0('+',Workflow$.__enclos_env__$private$optionsRichness[['predictionIntercept']], '_spatial')

      if (!is.null(Workflow$.__enclos_env__$private$samplingSize)) predictionData$sampSize <- Workflow$.__enclos_env__$private$samplingSize
      else predictionData$sampSize <- 1

      .__species.__ <- sort(unique(unlist(richModel[['species']][['speciesIn']])))

      .__covs.__ <- richModel[['spatCovs']][['name']]

      if (!is.null(Workflow$.__enclos_env__$private$biasCovNames)) .__covs.__ <- .__covs.__[!.__covs.__ %in%  names(Workflow$.__enclos_env__$private$biasCovariates)]

      .__speciesEffects.__ <- list()

      for (indexSp in 1:length(.__species.__)) {

        if (paste0(.__species.__[indexSp], '_Fixed__Effects__Comps') %in% names(richModel$summary.random)) .__covsSP.__ <- paste('+', paste0(.__species.__[indexSp], '_Fixed__Effects__Comps'))
        else {

          if (!is.null(.__covs.__)) .__covsSP.__ <- paste('+', paste0(.__species.__[indexSp],'_',.__covs.__, collapse = '+'))
          else .__covsSP.__ <- NULL

        }

        if (!is.null(Workflow$.__enclos_env__$private$speciesIntercept)) {

          if (Workflow$.__enclos_env__$private$speciesIntercept) .__specIntercept.__ <- paste0('+',Workflow$.__enclos_env__$private$speciesName,'_intercepts')
          else .__specIntercept.__ <- paste0('+',.__species.__[indexSp], '_intercept')

        } else .__specIntercept.__ <- NULL

        .__speciesEffects.__[[indexSp]] <- paste(.__species.__[indexSp], '= INLA::inla.link.cloglog(log(sampSize) +',  .__predIntercept.__, .__covsSP.__, .__specIntercept.__, .__pointSpat.__,.__predSpat.__,', inverse = TRUE)')

      }

      .__speciesFormulas.__ <- paste(do.call(paste0, list(.__speciesEffects.__, sep = ';')), collapse = '')

      .__speciesEval.__ <- paste('Richness = list(', paste(.__species.__,'=',.__species.__, collapse = ' , '),')')

      .__thin.__ <- paste0(paste(paste0(.__species.__, '[!1:length(',.__species.__,') %in% seq(', 1:length(.__species.__),',length(',.__species.__,'),', length(.__species.__), ')] <- FALSE'), collapse=';'),';')


      predictionFormula <- paste('{',
                                 .__speciesFormulas.__,
                                 .__thin.__,
                                 .__speciesEval.__ ,'}')

      if (!inherits(richModel, 'try-error')) {

      message('Creating richness maps:', '\n\n')

      richPredicts <- PointedSDMs:::predict.bruSDM(richModel, predictionData, #predictionDataSP?
                                                   formula = parse(text = predictionFormula))

      speciesProb <- mapply(function(x, seq) {

        if (!is.null(x[[Workflow$.__enclos_env__$private$speciesName]])) prob <- x[x[[Workflow$.__enclos_env__$private$speciesName]] == seq,]
        else prob <- x[x[['speciesSpatialGroup']] == seq,]
        prob <- prob[, c('mean', 'sd', 'q0.025', 'q0.5', 'q0.975', 'median',
                         Workflow$.__enclos_env__$private$speciesName)]
        list(prob)

      }, richPredicts[[1]], seq = 1:length(richPredicts[[1]]))

      predictionData$mean <- Reduce(`+`, lapply(speciesProb, function(x) x$mean))
      predictionData$q0.025 <- Reduce(`+`, lapply(speciesProb, function(x) x$q0.025))
      predictionData$q0.5 <- Reduce(`+`, lapply(speciesProb, function(x) x$q0.5))
      predictionData$q0.975 <- Reduce(`+`, lapply(speciesProb, function(x) x$q0.975))

      ##Filter predData + speciesProbs
       #For the richness we need mean, median and quantiles
       #For the prediction data we need stats + species ID and name
      predictionData <- predictionData[, c('mean', 'q0.025', 'q0.5', 'q0.975')]

      richOutput <- list(Richness = predictionData, Probabilities = speciesProb)

      if (saveObjects) {

        if (!Quiet)  message('\nSaving richness predictions:', '\n\n')
        saveRDS(object = richOutput, file = paste0(modDirectory, '/richnessPredictions.rds')) #Add project name here

      } else outputList[['Richness']] <- richOutput#richPredicts

      }

      }

      if ('Summary' %in% Oputs) {

        removeList <- grepl('spatial', names(richModel$summary.random)) | names(richModel$summary.random) == 'speciesShared'
        if (paste0(richModel$species$speciesVar,'_intercepts') %in% names(richModel$summary.random)) richModel$summary.random[[paste0(richModel$species$speciesVar,'_intercepts')]]$ID <- paste0(row.names(richModel$summary.random[[paste0(richModel$species$speciesVar,'_intercepts')]]), '_intercept')

        richnessSummary <- list(Fixed = richModel$summary.fixed,
                                Random = do.call(rbind, richModel$summary.random[!removeList]),
                                Hyperparameters = richModel$summary.hyperpar)
        row.names(richnessSummary$Random) <- NULL

        if (saveObjects) {

          if (!Quiet)  message('\nSaving richness summaries:', '\n\n')
          saveRDS(object = richnessSummary, file = paste0(modDirectory, '/richnessSummaries.rds')) #Add project name here

        } else outputList[['Summary']] <- richnessSummary

      }

      if ('Bias' %in% Oputs) {

        if (!Quiet) message('\nProducing bias predictions:\n\n')
        if (is.null(predictionData)) {

          .__mask.__ <- as(Workflow$.__enclos_env__$private$Area, 'Spatial')
          predictionData <- inlabru::fm_pixels(mesh = .__mesh.__,
                                               mask = .__mask.__,
                                               dims = predictionDim)

        }
        biasPreds <- predict(richModel,
                             data = predictionData,
                             bias = TRUE)

        if (saveObjects) {

          if (!Quiet)  message('\nSaving predictions object:', '\n\n')
          saveRDS(object = biasPreds, file = paste0(modDirectory,'/', '/biasRichnessPreds.rds')) #Add project name here

        } else outputList[['BiasRichness']] <- biasPreds

      }

  }

  if (!saveObjects) {

  if (!exists('outputList')) stop('Workflow did not run for any species.')
  return(outputList)

  }


  }

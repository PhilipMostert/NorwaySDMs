#' @title \code{obtainRichness}: Function to obtain richness estimates from a \code{\link[PointedSDMs]{fitISDM}} object.
#' @description This function is used to obtain richness estimates for a multi-species ISDM.
#' @param modelObject A \code{\link[PointedSDMs]{fitISDM}} object of class \code{modSpeceis}.
#' @param predictionData An \code{sf} data.frame object of containing the locations and covariates that are predicted on.
#' @param predictionIntercept The name of the prediction dataset to use in the model.
#' @param sampleSize The size of the sampling area for the prediction intercept dataset. Defaults to \code{1}.
#' @param inclProb Include the individual probabilities for each species. Defaults to \code{TRUE}
#' @import PointedSDMs
#'
#' @return An \code{sf} data.frame object of richness at each sampling location.
#'

obtainRichness = function(modelObject, predictionData,
                          predictionIntercept, sampleSize = 1,
                          inclProb = TRUE) {

  if (!inherits(modelObject, 'modSpecies')) stop('modelObject needs to be a modSpecies object obtained from the PointedSDMs function fitISDM.')

  if (!inherits(predictionData, 'sf')) stop('predictionData needs to be an sf data.frame object.')

  if (missing(predictionIntercept)) stop('predictionIntercept cannot be missing.')

  if (!predictionIntercept %in% modelObject$source) stop('predictionIntercept needs to be the name of a dataset included in modelObject.')

#Get from model Object
    if (is.null(modelObject$spatial$species)) .__predSpat.__ <- NULL
    else .__predSpat.__ <- '+speciesShared'
    #If copy then add here too

    #Get from modelObject

    if (is.logical(modelObject$spatial$points)) {

    if (!modelObject$spatial$points) .__pointSpat.__ <- NULL

    }
    else {

      if (modelObject$spatial$points == 'shared') .__pointSpat.__ <- '+shared_spatial'
      else .__pointSpat.__ <- paste0('+', predictionIntercept, '_spatial')

    }

      if (!is.null(sampleSize)) predictionData$sampSize <- sampleSize
      else predictionData$sampSize <- 1

      .__species.__ <- sort(unique(unlist(modelObject[['species']][['speciesIn']])))

      .__covs.__ <- modelObject[['spatCovs']][['name']]

      #Get from model Object
      #Remove from bias formula
      if (!is.null(modelObject$spatCovs$biasFormula)) .__covs.__ <- .__covs.__[!.__covs.__ %in% labels(terms(modelObject$spatCovs$biasFormula))]

      .__speciesEffects.__ <- list()

      .__predIntercept.__ <- paste0(predictionIntercept,'_intercept')

      for (indexSp in 1:length(.__species.__)) {

        if (paste0(.__species.__[indexSp], '_Fixed__Effects__Comps') %in% names(modelObject$summary.random)) .__covsSP.__ <- paste('+', paste0(.__species.__[indexSp], '_Fixed__Effects__Comps'))
        else {

          if (!is.null(.__covs.__)) .__covsSP.__ <- paste('+', paste0(.__species.__[indexSp],'_',.__covs.__, collapse = '+'))
          else .__covsSP.__ <- NULL

        }

        #Get from model object
        if (!is.null(modelObject$species$speciesEffects$Intercepts)) {

          if (modelObject$species$speciesEffects$Intercepts) .__specIntercept.__ <- paste0('+',modelObject$species$speciesVar,'_intercepts')
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

      if (!inherits(modelObject, 'try-error')) {

        message('Creating richness maps:', '\n\n')

        richPredicts <- PointedSDMs:::predict.bruSDM(modelObject, predictionData,
                                                     formula = parse(text = predictionFormula))

        speciesProb <- mapply(function(x, seq) {

          #Get from model object
          if (!is.null(x[[modelObject$species$speciesVar]])) prob <- x[x[[modelObject$species$speciesVar]] == seq,]
          else prob <- x[x[['speciesSpatialGroup']] == seq,]
          prob <- prob[, c('mean', 'sd', 'q0.025', 'q0.5', 'q0.975', 'median',
                           modelObject$species$speciesVar)]
          list(prob)

        }, richPredicts[[1]], seq = 1:length(richPredicts[[1]]))

        predictionData$mean <- Reduce(`+`, lapply(speciesProb, function(x) x$mean))
        predictionData$q0.025 <- Reduce(`+`, lapply(speciesProb, function(x) x$q0.025))
        predictionData$q0.5 <- Reduce(`+`, lapply(speciesProb, function(x) x$q0.5))
        predictionData$q0.975 <- Reduce(`+`, lapply(speciesProb, function(x) x$q0.975))

        predictionData <- predictionData[, c('mean', 'q0.025', 'q0.5', 'q0.975')]

        if (inclProb) richOutput <- list(Richness = predictionData, Probabilities = speciesProb)
        else richOutput <- list(Richness = predictionData)

        richOutput
  }

}

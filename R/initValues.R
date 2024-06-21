#' @title \code{initValues}: Function to obtain initial values from a \code{\link[PointedSDMs]{intModel}} object.
#' @description This function is used to obtain initial values by running a separate linear model on each sub-likelihood of the model to find maximum likelihood estimates, and averaging the estimates across all likelihoods.
#' @param data A \code{\link[PointedSDMs]{intModel}} object.
#' @param formulaComponents A vector of fixed effects for which to find initial values.
#' @import PointedSDMs
#'
#' @return The return of the function depends on the argument \code{Save} from the \code{startWorkflow} function. If this argument is \code{FALSE} then the objects will be saved to the specified directory. If this argument is \code{TRUE} then a list of different outcomes from the workflow will be returned.

initValues <- function(data, formulaComponents) {

##How does this work for factor covariates?
  dataNames <- names(lapply(data$.__enclos_env__$private$modelData, names))

  responsePA = data$.__enclos_env__$private$responsePA
  trialName = data$.__enclos_env__$private$trialsPA
  responseCounts = data$.__enclos_env__$private$responseCounts

  if (!is.null(data$.__enclos_env__$private$speciesName)) {

    if (data$.__enclos_env__$private$speciesEnvironment) formulaComponents <- apply(expand.grid(paste0(unique(unlist(data$.__enclos_env__$private$speciesIn)),'_'), formulaComponents), MARGIN = 1, FUN = paste0, collapse = '')

    intercepts <- TRUE

    if (is.null(data$.__enclos_env__$private$speciesIntercepts)) intTerm <- FALSE
    else intTerm <- data$.__enclos_env__$private$speciesIntercepts

    if (data$.__enclos_env__$private$Intercepts) dataIntercepts <- TRUE
    else dataIntercepts <- FALSE
    #if (intTerm) dataIntercepts <- TRUE
    #else dataIntercepts <- FALSE

  } else if (data$.__enclos_env__$private$Intercepts) {

    intercepts <- TRUE
    dataIntercepts <- TRUE

  } else intercepts <- FALSE

  if (intercepts) {

    species <- data$.__enclos_env__$private$speciesIn

    for (dataset in 1:length(dataNames)) {

      if (!is.null(species)) {

        if (dataIntercepts) {

          data$.__enclos_env__$private$modelData[[dataset]][[1]][paste0(dataNames[dataset], '_intercept')] <- 1
          formulaComponents <- unique(c(formulaComponents, paste0(dataNames[dataset], '_intercept')))

        }

        if (!intTerm) {

        for (sp in species[[dataset]]) {

          data$.__enclos_env__$private$modelData[[dataset]][[paste0(dataNames[dataset], '_', sp)]][paste0(sp, '_intercept')] <- 1
          formulaComponents <- unique(c(formulaComponents, paste0(sp, '_intercept')))

        }

        }


      }
      else {

        data$.__enclos_env__$private$modelData[[dataset]][[1]][paste0(dataNames[dataset], '_intercept')] <- 1
        formulaComponents <- unique(c(formulaComponents, paste0(dataNames[dataset], '_intercept')))


      }


    }

  }
  modelData <- lapply(unlist(data$.__enclos_env__$private$modelData, recursive = FALSE), as.data.frame)


  # for (dataset in names(modelData)) {

  #    modelData[[dataset]]$datasetName <- dataset

  #  }

  IPS <- as.data.frame(data$.__enclos_env__$private$IPS)
  IPS$PORESPONSE__MOCK <- 0 #exp(0)?

  varsNotIPS <- formulaComponents[!formulaComponents %in% names(IPS)]
  for (var in varsNotIPS) IPS[[var]] <- 0

  #changedResponse <- lapply(modelData, function(x) {

  #  x$.__response__ <- x[,c("PO_resp", 'PA_resp', 'Countsresp')]
  #  x$.__response__ <- link.transform(x$.__response__)
  #  x

  #  })

  #allData <- Reduce(function(x, y) merge(x, y, all=TRUE), append(changedResponse, IPS.__response__))

  #  if (type == 'datasets') allData <- allData[sort(allData$dataName),]
  #  else allData <- allData[sort(allData$speciesName),]

  #if (intercepts) {

  #    if (type == 'datasets') nameIndex <- paste0(sort(unique(allData$dataName)), '_intercept')
  #    else nameIndex <- paste0(sort(unique(allData$speciesName)), '_intercept')

  # }

  #THINGS TO DO:
  #CHECK INTERCEPT TERMS
  #SHOULD I STACK ALL DATASETS TOGETHER, OR IS IT ENOUGH TO DO IT ON A SPECIES BASIS

  modelResults <- lapply(modelData, function(x) {

    formComps <- formulaComponents[formulaComponents %in% names(x)]

    if (responsePA %in% names(x)) {

      if (!is.null(trialName) && trialName %in% names(x)) form <- formula(paste('cbind(',responsePA, ',', trialName,') ~' , paste(formComps, sep = '+')))
      else form <- formula(paste(responsePA ,'~' , paste(formComps, collapse = '+'), '- 1'))

      mod <- tryCatch(stats::glm(form, data = x, family=binomial(link=cloglog)), warning=function(w) w)

    }
    else
      if (responseCounts %in% names(x)) {

        form <- formula(paste(responseCounts ,'~' , paste(formComps, collapse = '+'), '- 1'))

        mod <- tryCatch(stats::glm(form, data = x, family = 'poisson'), warning=function(w) w)

      }
    else {

      x$PORESPONSE__MOCK <- 1
      x <- rbind(x[,c('PORESPONSE__MOCK', formComps)],
                 IPS[,c('PORESPONSE__MOCK', formComps)])

      form <- formula(paste('PORESPONSE__MOCK ~' , paste(formComps, collapse = '+'), '- 1'))

      mod <- tryCatch(stats::glm(form, data = x, family = 'poisson'), warning=function(w) w)

    }

    mod$coefficients

  })

  coefAverage <- vector(mode = 'list', length = length(formulaComponents))
  names(coefAverage) <- formulaComponents

  for (component in formulaComponents) {

    resModel <- sapply(modelResults, function(x) x[component])
    coefAverage[component] <- mean(resModel, na.rm = TRUE)

  }

  coefAverage
  #sqCalc <- function(x, optimCovFrame, linValues) {
  #  sqDiff <- (as.matrix(optimCovFrame) %*% x - linValues)^2
  #  sum(sqDiff, na.rm = TRUE)
  #}
  #initVec <- stats::optim(testVec, sqCalc, optimCovFrame = optimCovFrame, linValues = linValues)$par


}

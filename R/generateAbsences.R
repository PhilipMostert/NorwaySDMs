#' @title Function to generate absences for data that comes from checklist data.
#' @description Function used to generate absences for data coming from lists. This function takes all the sampling locations from all the species obtained from a given dataset, and generates an absence if a species does not occur at a given location.
#' @param dataList A List of data objects for the dataset.
#' @param datasetName The name of the dataset.
#' @param speciesName The name of the species variable name.
#' @param responseName The name of the response variable name.
#' @param Projection Coordinate reference system used.
#' @param Richness Generate absences for the richness model.
#'
generateAbsences <- function(dataList, datasetName,
                             speciesName, responseName,
                             Projection,
                             Richness) {


  datasetData <- lapply(dataList, function(x) x[[datasetName]])
  datasetData <- datasetData[!sapply(datasetData, is.null)]
  #allCoords <- unique(do.call(rbind, lapply(unlist(dataList, recursive = FALSE), st_coordinates)))
  allCoords <- unique(do.call(rbind, lapply(datasetData, st_coordinates)))

  allCoords <- unique(st_as_sf(data.frame(allCoords),
                        coords = colnames(allCoords),
                        crs = Projection))

  if (!Richness) {

  for (species in names(dataList)) {

    absences <- allCoords[-unlist(st_equals(dataList[[species]][[datasetName]], allCoords)),]

    if (nrow(absences) > 0) {
      nms <- names(dataList[[species]][[datasetName]])[!names(dataList[[species]][[datasetName]]) %in% 'geometry']
      absences[nms] <- NA
      absences[,'speciesName'] <- species #<- sub(" ", '_', species) # <- absences[speciesName]
      absences[responseName] <- 0

      dataList[[species]][[datasetName]] <- rbind(dataList[[species]][[datasetName]], absences)

    }

  }

  }
  else {

    for (species in unique(datasetData[[1]][['speciesName']])) {

      absences <- allCoords[-unlist(st_equals(datasetData[[1]][datasetData[[1]][['speciesName']] == species,], allCoords)),]

      if (nrow(absences) > 0) {
        nms <- names(datasetData[[1]])[!names(datasetData[[1]]) %in% 'geometry']
        absences[nms] <- NA
        absences[,'speciesName'] <- species#sub(" ", '_', species) # <- absences[speciesName]
        absences[responseName] <- 0

        dataList[[datasetName]][[datasetName]] <- rbind(dataList[[datasetName]][[datasetName]], absences)

      }


      }



  }

  dataList

}

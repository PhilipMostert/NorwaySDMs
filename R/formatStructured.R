#' @title formatStructured: Function to add structured data into the workflow.
#' @description
#' Function used to format structure data into a coherent framework.
#'
#' @param dataOCC The species occurrence data. May be either a \code{SpatialPointsDataFrame}, \code{sf} or \code{data.frame} object.
#' @param type The type of observation model for the data. May be either: \code{PO}, \code{PA} or \code{Counts}.
#' @param varsOld The names of the old variables in the model which need to be converted to something new.
#' @param varsNew The name of the new variables in the model which will be used in the full model.
#' @param projection The CRS object to add to the species occurrence data.
#' @param boundary An sf object of the boundary of the study area, used to check if the data points are over the region.
#'
#' @import sf
#'
#' @return An \code{sf} object containing the locations of the species.

formatStructured <- function(dataOCC, type, varsOld, varsNew, projection, boundary) {

  #varsOld <- unlist(varsOld)
  #varsNew <- unlist(varsNew)

  if (inherits(dataOCC, 'Spatial')) dataOCC <- as(dataOCC, 'sf')

  if (!inherits(dataOCC, 'sf')) {

    dataOCC <- as.data.frame(dataOCC)

    if (ncol(dataOCC) <= 2) dataOCC$.__PORESP.__ <- 1

    colnames(dataOCC)[names(dataOCC) %in% varsOld$coordinates] <-  c('Longitude', 'Latitude')

    dataOCC <- sf::st_as_sf(x = dataOCC[, c('Longitude', 'Latitude', names(dataOCC)[names(dataOCC) %in% unlist(varsOld)])],
                    coords = c('Longitude', 'Latitude'),
                    crs = projection)
  }

  initRows <- nrow(dataOCC)

  #dataOCC <- dataOCC[, names(dataOCC)[names(dataOCC) %in% unlist(varsOld)]]

  st_geometry(dataOCC) <- 'geometry'
  dataOCC <- sf::st_transform(dataOCC, as.character(projection))

  namesData <- colnames(dataOCC)[!colnames(dataOCC) %in% c('geometry', '.__PORESP.__')]

  oldNames <- unlist(varsOld)[match(namesData, unlist(varsOld))]
  newNames <- unlist(varsNew)[match(names(oldNames), names(unlist(varsNew)))]

  colnames(dataOCC)[!colnames(dataOCC) %in% c('geometry', '.__PORESP.__')] <- newNames

  dataOCC <- dataOCC[unlist(st_intersects(boundary, dataOCC)),]

  if (nrow(dataOCC) == 0) warning('Dataset provided has no reccords over the boundary.')
  if (initRows > nrow(dataOCC)) warning('Some of the records provided are not over the boundary, and will therefore be removed.')

  dataOCC

}

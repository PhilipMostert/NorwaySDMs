#' @title obtainGBIF: Function to obtain occurrence data from GBIF.
#' @description
#' Function used to obtain species observations from _GBIF_.
#'
#' @param query The scientific name of the species for which observations need to be obtained.
#' @param geometry An \code{sf} object surrounding the study area where observations need to be obtained.
#' @param projection The coordinate reference system used for the observations and geometry.
#' @param datasettype The type of dataset that is obtained from _GBIF_. Can be one of: \code{PO}, \code{PA}, \code{Counts}.
#' @param ... Additional arguments to pass to \link[rgbif]{occ_download}.
#'
#' @import rgbif
#' @import sf
#'
#' @return An \code{sf} object containing the locations and other relevant information of the species obtained from _GBIF_.


obtainGBIF <- function(query,
                       geometry,
                       projection,
                       datasettype,
                       ...) {

  ##Add something here to create absences in lists where species are not.

  if (!inherits(geometry, 'sf')) geometry <- st_as_sf(geometry)#geometry <- as(geometry, 'sf')

  boundaryCheck <- sf::st_transform(geometry, crs = "+proj=longlat +ellps=WGS84")

  speciesList <- vector(mode = 'list', length = length(query))
  names(speciesList) <- query

  #if (!is.null(country)) {

    #countryCode <- rgbif::enumeration_country()
    #countryCode <- countryCode[countryCode$title %in% country, 'iso2']

  #}

  for (species in query) {

  #speciesOCC <- spocc::occ(query = species,
  #                      geometry = boundaryCheck,
  #                      gbifopts = gbifopts,
  #                      ...)

  if (datasettype == 'PA') {

  PresSpeciesOCC <- rgbif::occ_data(scientificName = species,
                                # country = countryCode, #Country codes
                                geometry = st_bbox(boundaryCheck),
                                hasCoordinate = TRUE,
                                occurrenceStatus = 'PRESENT',
                                ...)

  if (!all(names(PresSpeciesOCC) %in% c('meta', 'data'))) {

    namesData <- Reduce(intersect, lapply(PresSpeciesOCC, function(x) names(x$data)))
    speciesOCC <- data.frame(do.call(rbind, lapply(PresSpeciesOCC, function(x) x$data[, namesData])))

  } else PresSpeciesOCC <- data.frame(PresSpeciesOCC$data)

  AbsSpeciesOCC <- try(rgbif::occ_data(scientificName = species,
                                    #country = countryCode, #Country codes
                                    geometry = st_bbox(boundaryCheck),
                                    hasCoordinate = TRUE,
                                    occurrenceStatus = 'ABSENT',
                                    ...), silent = FALSE)

  if (inherits(AbsSpeciesOCC, 'try-error')) stop('Could not download data from GBIF. Please change your search query or try again later.')

  if (!all(names(AbsSpeciesOCC) %in% c('meta', 'data'))) {

    namesData <- Reduce(intersect, lapply(AbsSpeciesOCC, function(x) names(x$data)))
    AbsSpeciesOCC <- data.frame(do.call(rbind, lapply(AbsSpeciesOCC, function(x) x$data[, namesData])))

  } else AbsSpeciesOCC <- data.frame(AbsSpeciesOCC$data)

  notPres <- nrow(PresSpeciesOCC) == 0
  notAbs <- nrow(AbsSpeciesOCC) == 0

  if (!notPres && !notAbs) {

    presCols <- colnames(PresSpeciesOCC)
    absCols <- colnames(AbsSpeciesOCC)

    speciesOCC <- data.frame(rbind(PresSpeciesOCC[, presCols %in% absCols],
                                   AbsSpeciesOCC[, absCols %in% presCols]))


  }

  if (notAbs) {

   warning('No absences found for the specified species.')
   speciesOCC <- data.frame(PresSpeciesOCC)

  }

  if (notPres) {


    warning('No presences found for the specified species.')
    speciesOCC <- data.frame(AbsSpeciesOCC)

  }

  if (notPres && notAbs) stop ('Species provided not available in specified area.')

  speciesOCC[speciesOCC$occurrenceStatus == 'PRESENT', 'occurrenceStatus'] <- 1
  speciesOCC[speciesOCC$occurrenceStatus == 'ABSENT', 'occurrenceStatus'] <- 0
  speciesOCC$occurrenceStatus <- as.numeric(speciesOCC$occurrenceStatus)


  } else {


    speciesOCC <- rgbif::occ_data(scientificName = species,
                                  #country = countryCode,
                                  geometry = st_bbox(boundaryCheck),
                                  hasCoordinate = TRUE,
                                  ...) #Multiple countries

   if (!all(names(speciesOCC) %in% c('meta', 'data'))) {

     namesData <- Reduce(intersect, lapply(speciesOCC, function(x) names(x$data)))
     speciesOCC <- data.frame(do.call(rbind, lapply(speciesOCC, function(x) x$data[, namesData])))

   } else speciesOCC <- data.frame(speciesOCC$data)

    #speciesOCC <- data.frame(speciesOCC$data)
    if (nrow(speciesOCC) == 0) stop ('Species provided not available in specified area.')

  }


  #speciesOCC <- data.frame(speciesOCC$gbif$data[[1]])

  #Something here: if datasetType = PA

  if (datasettype == 'Counts') {

    #Remove all rows with NA
    rowsOCC <- nrow(speciesOCC)
    speciesOCC <- speciesOCC[!is.na(speciesOCC$individualCount),]

    if (nrow(speciesOCC) == 0) stop('All reccords had NA for individualCount.')
    if (rowsOCC > nrow(speciesOCC)) warning('Removing reccords with NA individualCount values')

  }

  speciesOCC[, c('decimalLongitude', 'decimalLatitude')] <- sf::sf_project(pts = speciesOCC[, c('decimalLongitude', 'decimalLatitude')],
                                                             to = as.character(projection),
                                                             from = "+proj=longlat +ellps=WGS84")

  speciesSF <- sf::st_as_sf(x = speciesOCC,
                        coords = c('decimalLongitude', 'decimalLatitude'),
                        crs = as.character(projection))

  speciesSF <- sf::st_transform(speciesSF, crs = as.character(projection))

  speciesIn <- speciesSF[unlist(st_intersects(geometry, speciesSF)),]

  if (nrow(speciesIn) == 0) warning(paste(species, 'provided no occurrence reccords over the specified region.'))

  if (datasettype !=  'PA') speciesIn$occurrenceStatus <- NULL
  if (datasettype != 'Counts') speciesIn$individualCount <- NULL

  speciesList[[species]] <- speciesIn

  }

  speciesList <- do.call(rbind, speciesList); speciesList

  }

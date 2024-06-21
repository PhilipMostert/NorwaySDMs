testthat::test_that('generateAbsences correctly creates absences for the data.', {

  ##First set up workflow
  skip_on_cran()

  proj <- '+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
  species <- c('Fraxinus excelsior', 'Ulmus glabra', 'Arnica montana')
  workflow <- startWorkflow(Species = species,
                            saveOptions = list(projectName = 'testthatexample'),
                            Projection = proj,
                            Countries = c('Sweden', 'Norway'),
                            Quiet = TRUE, Save = FALSE)


  if (is.null(workflow$.__enclos_env__$private$Area)) {

    map <- st_as_sf(geodata::world(path = tempdir()))
    map <- map[map$NAME_0 == 'Norway',]
    map <- st_transform(map, proj)

    workflow$addArea(Object = map)

  }

  workflow$addGBIF(datasetType = 'PO', limit = 50, datasetName = 'PO')

  workflow$addGBIF(datasetType = 'PA', datasetName = 'PA', generateAbsences = FALSE)

  paData <- lapply(workflow$.__enclos_env__$private$dataGBIF, function(x) x[['PA']])

  workflow$addGBIF(datasetType = 'PA', datasetName = 'PA', generateAbsences = TRUE)

  expect_true(all(names(workflow$.__enclos_env__$private$dataGBIF) %in% sub(" ", '_', species)))

  expect_true(all(unlist(lapply(workflow$.__enclos_env__$private$dataGBIF, function(x) names(x))) %in% c('PO', 'PA')))

  expect_true(nrow(paData$Fraxinus_excelsior) < nrow(workflow$.__enclos_env__$private$dataGBIF$Fraxinus_excelsior$PA))
  expect_true(nrow(paData$Ulmus_glabra) < nrow(workflow$.__enclos_env__$private$dataGBIF$Ulmus_glabra$PA))
  expect_true(nrow(paData$Arnica_montana) < nrow(workflow$.__enclos_env__$private$dataGBIF$Arnica_montana$PA))

  expect_true(all(is.na(data.frame(workflow$.__enclos_env__$private$dataGBIF$Fraxinus_excelsior$PA)[(nrow(paData$Fraxinus_excelsior) +1: nrow(workflow$.__enclos_env__$private$dataGBIF$Arnica_montana$PA)),
  !names(workflow$.__enclos_env__$private$dataGBIF$Fraxinus_excelsior$PA) %in% c('speciesName','species', 'networkKeys','occurrenceStatus', 'geometry')])))

  expect_true(all(is.na(data.frame(workflow$.__enclos_env__$private$dataGBIF$Ulmus_glabra$PA)[(nrow(paData$Ulmus_glabra) +1: nrow(workflow$.__enclos_env__$private$dataGBIF$Ulmus_glabra$PA)),
                                                                                                    !names(workflow$.__enclos_env__$private$dataGBIF$Ulmus_glabra$PA) %in% c('speciesName','species', 'networkKeys','occurrenceStatus', 'geometry')])))

  expect_true(all(is.na(data.frame(workflow$.__enclos_env__$private$dataGBIF$Arnica_montana$PA)[(nrow(paData$Arnica_montana) +1: nrow(workflow$.__enclos_env__$private$dataGBIF$Arnica_montana$PA)),
                                                                                              !names(workflow$.__enclos_env__$private$dataGBIF$Arnica_montana$PA) %in% c('speciesName','species', 'networkKeys','occurrenceStatus', 'geometry')])))

  ##Test Richness = TRUE
  proj <- '+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
  species <- c('Fraxinus excelsior', 'Ulmus glabra', 'Arnica montana')
  workflow <- startWorkflow(Species = species,
                            saveOptions = list(projectName = 'testthatexample'),
                            Projection = proj,
                            Richness = TRUE,
                            Countries = c('Sweden', 'Norway'),
                            Quiet = TRUE, Save = FALSE)


  if (is.null(workflow$.__enclos_env__$private$Area)) {

    map <- st_as_sf(geodata::world(path = tempdir()))
    map <- map[map$NAME_0 == 'Norway',]
    map <- st_transform(map, proj)

    workflow$addArea(Object = map)

  }

  workflow$addGBIF(datasetType = 'PO', limit = 50, datasetName = 'PO')

  workflow$addGBIF(datasetType = 'PA', datasetName = 'PA', generateAbsences = FALSE)

  paData <- lapply(workflow$.__enclos_env__$private$dataGBIF, function(x) x[['PA']])

  workflow$addGBIF(datasetType = 'PA', datasetName = 'PA', generateAbsences = TRUE)

  expect_true(all(names(workflow$.__enclos_env__$private$dataGBIF) %in% c('PO', 'PA')))

  expect_true(all(unlist(lapply(workflow$.__enclos_env__$private$dataGBIF, function(x) names(x))) %in% c('PO', 'PA')))

  expect_true(nrow(paData$PA[paData$PA$speciesName == 'Fraxinus excelsior',]) < nrow(workflow$.__enclos_env__$private$dataGBIF$PA$PA[workflow$.__enclos_env__$private$dataGBIF$PA$PA$speciesName == 'Fraxinus excelsior',]))
  expect_true(nrow(paData$PA[paData$PA$speciesName == 'Ulmus glabra',]) < nrow(workflow$.__enclos_env__$private$dataGBIF$PA$PA[workflow$.__enclos_env__$private$dataGBIF$PA$PA$speciesName == 'Ulmus glabra',]))
  expect_true(nrow(paData$PA[paData$PA$speciesName == 'Arnica montana',]) < nrow(workflow$.__enclos_env__$private$dataGBIF$PA$PA[workflow$.__enclos_env__$private$dataGBIF$PA$PA$speciesName == 'Arnica montana',]))



})

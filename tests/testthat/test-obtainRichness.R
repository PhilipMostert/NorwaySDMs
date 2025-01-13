testthat::test_that('obtainRichness can produce an sf object of species richness', {

  skip_on_cran()
  library(R.utils)

  proj <- '+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
  countries <- st_as_sf(geodata::world(path = tempdir()))
  countries <- countries[countries$NAME_0 %in% c('Norway'),]
  countries <- st_transform(countries, proj)
  species <- c('Fraxinus excelsior')
  workflow <- try(startWorkflow(Species = species,
                                saveOptions = list(projectName = 'testthatexample', projectDirectory = './'),
                                Projection = proj, Countries = 'Norway', Richness = TRUE,
                                Quiet = TRUE, Save = FALSE))

  if (inherits(workflow, 'try-error')) {


    workflow <- startWorkflow(Species = species,
                              saveOptions = list(projectName = 'testthatexample', projectDirectory = './'),
                              Projection = proj, Quiet = TRUE, Save = FALSE, Richness = TRUE)


    workflow$addArea(Object = countries)

  }

  workflow$addGBIF(datasetName = 'GBIF_data', limit = 50) #Get less species
  workflow$addGBIF(datasetName = 'GBIF_data2', limit = 50, datasetType = 'PA')
  workflow$workflowOutput(c('Model'))
  try(withTimeout(workflow$addCovariates(worldClim = 'tmax', res = 10), timeout = 60, onTimeout = 'silent'))
  workflow$addMesh(max.edge = 500000) #200000
  workflow$modelOptions(Richness = list(predictionIntercept = 'GBIF_data'))
  model <- sdmWorkflow(Workflow = workflow)

  ##Try wrong modelObject
  Rich <- expect_error(obtainRichness(modelObject = model), 'modelObject needs to be a modSpecies object obtained from the PointedSDMs function fitISDM.')
  Rich <- expect_error(obtainRichness(modelObject = model$RichnessModel,
                                      predictionData = fm_pixels(workflow$.__enclos_env__$private$Mesh)),'predictionIntercept cannot be missing.')
  Rich <- expect_error(obtainRichness(modelObject = model$RichnessModel,
                                      predictionData = fm_pixels(workflow$.__enclos_env__$private$Mesh),
                                      predictionIntercept = 'wrong'),'predictionIntercept needs to be the name of a dataset included in modelObject.')

  Rich <- obtainRichness(modelObject = model$RichnessModel,
                       predictionData = fm_pixels(workflow$.__enclos_env__$private$Mesh),
                       predictionIntercept = 'GBIF_data')

  expect_identical(class(Rich), 'list')
  expect_setequal(names(Rich), c('Richness', 'Probabilities'))
  expect_setequal(names(Rich$Probabilities), gsub(' ', '_', species))

  })

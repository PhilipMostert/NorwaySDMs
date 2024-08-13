testthat::test_that('sdmWorkflow produces the correct output given different Workflow situations.', {

  skip_on_cran()

  ##Create different workflows here:
   #1. Just GBIF data
  proj <- '+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
  countries <- st_as_sf(geodata::world(path = tempdir()))
  countries <- countries[countries$NAME_0 %in% c('Norway'),]
  countries <- st_transform(countries, proj)
  species <- c('Fraxinus excelsior')
  workflow <- try(startWorkflow(Species = species,
                            saveOptions = list(projectName = 'testthatexample', projectDirectory = './'),
                            Projection = proj, Countries = 'Norway',
                            Quiet = TRUE, Save = TRUE))

  if (inherits(workflow, 'try-error')) {


    workflow <- startWorkflow(Species = species,
                  saveOptions = list(projectName = 'testthatexample', projectDirectory = './'),
                  Projection = proj, Quiet = TRUE, Save = TRUE)


    workflow$addArea(Object = countries)

  }

  workflow$addGBIF(datasetName = 'GBIF_data', limit = 50) #Get less species
  workflow$addGBIF(datasetName = 'GBIF_data2', limit = 50, datasetType = 'PA')
  expect_error(sdmWorkflow(Workflow = workflow)) #Test no output given
  workflow$workflowOutput('Model')
  expect_error(sdmWorkflow(Workflow = workflow)) #Test no mesh provided
  workflow$addMesh(max.edge = 500000) #200000
  #Test something about CV-method -- none specified but given as output
  #Need to test a lot of the copy model; points spatial; points intercept parts
  workflow$modelOptions(ISDM = list(pointsSpatial = 'shared'))
  sdmWorkflow(Workflow = workflow)
  expect_true(all(c(dir.exists('./testthatexample/Fraxinus_excelsior'))))

  expect_true(all(c(file.exists('./testthatexample/Fraxinus_excelsior/intModel.rds'))))

  Fraxinus_excelsior <- readRDS(file = './testthatexample/Fraxinus_excelsior/intModel.rds')
  expect_setequal(rownames(Fraxinus_excelsior$summary.fixed), c("GBIF_data_intercept", "GBIF_data2_intercept"))
  expect_equal(as.character(Fraxinus_excelsior$componentsJoint)[2],
               "-1 + shared_spatial(main = geometry, model = shared_field) + GBIF_data_intercept(1) + GBIF_data2_intercept(1)")
  rm(Fraxinus_excelsior)
  biasCopyonCRAN <- FALSE
  if (biasCopyonCRAN) {

  biasWorkflow <- startWorkflow(Species = species,
                            saveOptions = list(projectName = 'testthatexample', projectDirectory = './tests'),
                            Projection = proj,
                            Quiet = TRUE, Save = FALSE)

  biasWorkflow$addArea(Object = countries)


  biasWorkflow$addGBIF(datasetName = 'GBIF_data') #Get less species
  biasWorkflow$addGBIF(datasetName = 'GBIF_data2', limit = 50, datasetType = 'PA')
  biasWorkflow$workflowOutput('Model')
  biasWorkflow$addMesh(max.edge = 500000) #200000
  biasWorkflow$biasFields('GBIF_data')
  biasWorkflow$modelOptions(ISDM = list(pointsSpatial = 'shared'))
  biasMod <- sdmWorkflow(biasWorkflow)

  expect_setequal(names(biasMod$Fraxinus_excelsior$Model$summary.random), c("shared_spatial", "GBIF_data_biasField"))
  expect_setequal(class(biasMod$Fraxinus_excelsior$Model), c("modISDM", "bru", "iinla", "inla"))
  rm(biasWorkflow)
}
  copyWorkflow <- startWorkflow(Species = species,
                                saveOptions = list(projectName = 'testthatexample', projectDirectory = './tests'),
                                Projection = proj,
                                Quiet = TRUE, Save = FALSE)

  copyWorkflow$addArea(Object = countries)

  copyWorkflow$addGBIF(datasetName = 'GBIF_data') #Get less species
  copyWorkflow$addGBIF(datasetName = 'GBIF_data2', limit = 50, datasetType = 'PA')
  copyWorkflow$workflowOutput('Model')
  copyWorkflow$addMesh(max.edge = 500000) #200000
  copyWorkflow$modelOptions(ISDM = list(pointsSpatial = 'copy'))
  copyWorkflow$specifyPriors(copyModel = list(beta = list(fixed = TRUE)))
  copyMod <- sdmWorkflow(copyWorkflow)

  expect_setequal(names(copyMod$Fraxinus_excelsior$Model$summary.random), c("GBIF_data_spatial", "GBIF_data2_spatial"))
  expect_equal(as.character(copyMod$Fraxinus_excelsior$Model$componentsJoint)[2],
               "-1 + GBIF_data_spatial(main = geometry, model = GBIF_data_field) + GBIF_data2_spatial(main = geometry, copy = \"GBIF_data_spatial\", hyper = list(beta = list(fixed = TRUE))) + GBIF_data_intercept(1) + GBIF_data2_intercept(1)")

  unlink('./tests/testthatexample', recursive = TRUE)

  ##Test Richness model
  proj <- '+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
  countries <- st_as_sf(geodata::world(path = tempdir()))
  countries <- countries[countries$NAME_0 %in% c('Norway'),]
  countries <- st_transform(countries, proj)
  species <- c('Fraxinus excelsior')
  workflow <- try(startWorkflow(Species = species,
                                saveOptions = list(projectName = 'testthatexample', projectDirectory = './'),
                                Projection = proj, Countries = 'Norway', Richness = TRUE,
                                Quiet = TRUE, Save = TRUE))

  if (inherits(workflow, 'try-error')) {


    workflow <- startWorkflow(Species = species,
                              saveOptions = list(projectName = 'testthatexample', projectDirectory = './'),
                              Projection = proj, Quiet = TRUE, Save = TRUE)


    workflow$addArea(Object = countries)

  }

  workflow$addGBIF(datasetName = 'GBIF_data', limit = 50) #Get less species
  workflow$addGBIF(datasetName = 'GBIF_data2', limit = 50, datasetType = 'PA')
  expect_error(sdmWorkflow(Workflow = workflow)) #Test no output given
  workflow$workflowOutput(c('Model', 'Predictions'))
  expect_error(sdmWorkflow(Workflow = workflow)) #Test no mesh provided
  workflow$addMesh(max.edge = 500000) #200000
  #Test something about CV-method -- none specified but given as output
  #Need to test a lot of the copy model; points spatial; points intercept parts
  workflow$modelOptions(ISDM = list(pointsSpatial = 'shared'))
  expect_error(sdmWorkflow(Workflow = workflow))
  workflow$modelOptions(Richness = list(predictionIntercept = 'GBIF_data'))
  sdmWorkflow(Workflow = workflow)

  expect_true(all(c(file.exists('./testthatexample/richnessModel.rds'))))

  expect_true(all(c(file.exists('./testthatexample/richnessPredictions.rds'))))

  RichModel <- readRDS(file = './testthatexample/richnessModel.rds')
  expect_setequal(rownames(RichModel$summary.fixed), c("GBIF_data_intercept", "GBIF_data2_intercept"))
  expect_equal(deparse1(RichModel$componentsJoint),
               "~-1 + shared_spatial(main = geometry, model = shared_field) + speciesShared(main = geometry, model = speciesField) + GBIF_data_intercept(1) + GBIF_data2_intercept(1) + speciesName_intercepts(main = speciesName, model = \"iid\", constr = TRUE, hyper = list(prec = list(prior = \"loggamma\", param = c(1, 5e-05))))")
  rm(RichModel)
  unlink('./testthatexample', recursive = TRUE)

})

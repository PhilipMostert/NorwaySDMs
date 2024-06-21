testthat::test_that('initValues can estimate a glm to obtain initial values', {

  skip_on_cran()

  projection <- '+proj=tmerc'

  #Make random shape to generate points on
  x <- c(16.48438,  17.49512,  24.74609, 22.59277, 16.48438)
  y <- c(59.736328125, 55.1220703125, 55.0341796875, 61.142578125, 59.736328125)
  xy <- cbind(x, y)
  SpatialPoly <- st_sfc(st_polygon(list(xy)), crs = projection)

  ##Old coordinate names
  #Make random points
  #Random presence only dataset
  PO <- st_as_sf(st_sample(SpatialPoly, 100, crs = projection))
  st_geometry(PO) <- 'geometry'
  PO$species <- sample(x = c('fish'), size = nrow(PO), replace = TRUE)
  #Random presence absence dataset
  PA <- st_as_sf(st_sample(SpatialPoly, 100, crs = projection))
  st_geometry(PA) <- 'geometry'
  PA$PAresp <- sample(x = c(0,1), size = nrow(PA), replace = TRUE)
  PA$species <- sample(x = c('bird'), nrow(PA), replace = TRUE)
  mesh <- INLA::inla.mesh.2d(boundary = INLA::inla.sp2segment(SpatialPoly),
                             max.edge = 2, crs = inlabru::fm_crs(projection))
  #Random Counts dataset
  Counts <- st_as_sf(st_sample(SpatialPoly, 100, crs = projection))
  Counts$count <- rpois(n = nrow(Counts), lambda = 5)
  Counts$species <- sample(x = c('fish', 'bird'), nrow(Counts), replace = TRUE)

  iPoints <- inlabru::fm_int(samplers = SpatialPoly, domain = mesh)

  coordnames <- c('long', 'lat')
  responseCounts <- 'count'
  responsePA <- 'PAresp'
  speciesName <- 'species'

  cov <- terra::rast(st_as_sf(SpatialPoly), crs = projection)
  terra::values(cov) <- rgamma(n = terra::ncell(cov), shape = 2)
  names(cov) <- 'covariate'

  ##Find initial values with species
  obj <- startSpecies(PO, PA, Counts, Projection = projection, Mesh = mesh,
                  IPS = iPoints, responseCounts = responseCounts,
                  responsePA = responsePA, speciesSpatial = 'replicate',
                  speciesName = speciesName, spatialCovariates = cov)


  speciesVals <- initValues(data = obj, formulaComponents = 'covariate')
  expect_equal(class(speciesVals), 'list')
  ##FIX THIS
  expect_setequal(names(speciesVals), c( "fish_covariate", "bird_covariate", "PO_intercept", "PA_intercept", "Counts_intercept"))

  ##Find initial values with species but fixed environment
  obj2 <- startSpecies(PO, PA, Counts, Projection = projection, Mesh = mesh,
                  IPS = iPoints, responseCounts = responseCounts,
                  responsePA = responsePA, speciesSpatial = 'replicate',
                  speciesName = speciesName, spatialCovariates = cov, speciesEnvironment = FALSE, speciesIntercept = TRUE)
  speciesVals2 <- initValues(data = obj2, formulaComponents = 'covariate')
  expect_setequal(names(speciesVals2), c( "covariate", "PO_intercept", "PA_intercept", 'Counts_intercept'))

  #Find initial values no species
  obj3 <- startISDM(PO, PA, Counts, Projection = projection, Mesh = mesh,
                   IPS = iPoints, responseCounts = responseCounts,
                   responsePA = responsePA,
                   spatialCovariates = cov)

  datasetVals <- initValues(data = obj3, formulaComponents = 'covariate')
  expect_setequal(names(datasetVals), c( "covariate", "PO_intercept", "PA_intercept", 'Counts_intercept'))

  #Find initial values no species and no intercept
  obj4 <- startISDM(PO, PA, Counts, Projection = projection, Mesh = mesh,
                   IPS = iPoints, responseCounts = responseCounts,
                   responsePA = responsePA, pointsIntercept = FALSE,
                   spatialCovariates = cov)

  datasetVals2 <- initValues(data = obj4, formulaComponents = 'covariate')

  expect_setequal(names(datasetVals2), c( "covariate"))

  #Try species fixed intercept + data intercept
  obj5 <- startSpecies(PO, PA, Counts, Projection = projection, Mesh = mesh,
                       IPS = iPoints, responseCounts = responseCounts,
                       responsePA = responsePA, speciesSpatial = 'replicate',
                       speciesName = speciesName, spatialCovariates = cov, speciesEnvironment = TRUE, speciesIntercept = FALSE)
  speciesVals5 <- initValues(data = obj5, formulaComponents = 'covariate')
  expect_setequal(names(speciesVals5), c('fish_covariate', 'bird_covariate', 'PO_intercept', 'fish_intercept', 'PA_intercept', 'bird_intercept', 'Counts_intercept'))

})

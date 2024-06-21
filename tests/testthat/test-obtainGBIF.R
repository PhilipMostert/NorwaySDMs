testthat::test_that('obtainGBIF can correctly obtain observations of species in the correct boundary, and transform it to the desired projection', {

  skip_on_cran()

  #args
  speciesIn <- 'Fraxinus excelsior'

  proj <- '+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

  map <- try(obtainArea(names = c('Norway'), projection = proj))

  if (inherits(map, 'try-error')) {

    map <- st_as_sf(geodata::world(path = tempdir()))
    map <- map[map$NAME_0 == 'Norway',]
    map <- st_transform(map, proj)
  }

  species <- obtainGBIF(query = speciesIn,
                        datasettype = 'PO', country = 'NO',
                        coordinateUncertaintyInMeters = 50,
                        geometry = map, projection = proj)

  expect_equal(class(species), c('sf', 'data.frame'))
  expect_true(all(species$coordinateUncertaintyInMeters <= 50))
  expect_identical(st_crs(species)[2], st_crs(proj)[2])

  speciesPA <- obtainGBIF(query = speciesIn,
                          datasettype = 'PA',country = 'NO',
                          geometry = map, projection = proj)

  expect_setequal(unique(speciesPA$occurrenceStatus), c(0,1))

  speciesIn2 <- 'Ceratotherium simum'

  expect_error(obtainGBIF(query = speciesIn2, datasettype = 'PA',
                          country = 'NO',
                          geometry = map, projection = proj), 'Species provided not available in specified area.')

  ##Multiple years

  speciesTime <- obtainGBIF(query = speciesIn,
                            datasettype = 'PO',country = 'NO',
                            geometry = map, projection = proj, year = 2010:2012)

  expect_setequal(unique(speciesTime$year), c(2010, 2011, 2012))

})

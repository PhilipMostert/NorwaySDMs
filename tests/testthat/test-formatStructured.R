testthat::test_that('formatStructured can correctly convert a dataset into the correct format required by the model.', {
  set.seed(1)
  skip_on_cran()

  proj <- '+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
  map <-  try(obtainArea(names = c('Norway'), projection = proj))

  if (inherits(map, 'try-error')) {

    map <- st_as_sf(geodata::world(path = tempdir()))
    map <- map[map$NAME_0 == 'Norway',]
    map <- st_transform(map, proj)
  }

  box <- st_bbox(map)

  dataChange <- data.frame(x = rnorm(100, mean = (box$xmax + box$xmin)/2, sd = 100000),
                           y = rnorm(100, mean = (box$ymax + box$ymin)/2, sd = 100000),
                           num = rpois(n = 100, lambda = 5))
  model = 'Counts'
  old <- list(trials = 'trialsName',
              response = 'num',
              species = 'species',
              coordinates = c('x', 'y'))
  new <- list(coordinates = c('Longitude', 'Latitude'),
              response = 'individualCount',
              trials = 'numTrials',
              speciesName = 'speciesName')

  coords <- c('x', 'y')


  dataStructured <- formatStructured(dataOCC = dataChange, type = model,
                                     varsOld = old, varsNew = new,
                                     projection = proj, boundary = map)

  ##Test that the function converts the data.frame of a counts dataset into an sf with the correct variable names.
  expect_setequal(class(dataStructured), c('sf', 'data.frame'))
  expect_true(all(names(dataStructured) %in% c("individualCount", "geometry")))
  expect_identical(st_crs(dataStructured)[2], st_crs(proj)[2])

  ##Test that the function converts the data.frame of a PA dataset into an sf with the correct variable names.
  model2 <- 'PA'
  dataChange$trialsName <- 100
  old2 <- list(trials = 'trialsName',
               response = 'num',
               species = 'species',
               coordinates = c('x', 'y'))

  new2 <- list(coordinates = c('Longitude', 'Latitude'),
               response = 'occurrenceStatus',
               trials = 'numTrials',
               speciesName = 'speciesName')

  dataStructured2 <- formatStructured(dataOCC = dataChange, type = model2,
                                     varsOld = old2, varsNew = new2, projection = proj, boundary = map)

  expect_setequal(class(dataStructured2), c('sf', 'data.frame'))
  expect_true(all(names(dataStructured2) %in% c("occurrenceStatus", 'numTrials', "geometry")))
  expect_identical(st_crs(dataStructured2)[2], st_crs(proj)[2])
  ##Test that if some points are not over the region, the function will give a warning.

  dataChange2 <- data.frame(x = rnorm(100, mean = (box$xmax + box$xmin)/2, sd = 200000),
                           y = rnorm(100, mean = (box$ymax + box$ymin)/2, sd = 200000),
                           num = rpois(n = 100, lambda = 5))

  expect_warning(formatStructured(dataOCC = dataChange2, type = model,
                                      varsOld = old, varsNew = new, projection = proj, boundary = map),
                 'Some of the records provided are not over the boundary, and will therefore be removed.')

  #Test that if all points are not over the region, the function will give an error.
  dataChange3 <- data.frame(x = rnorm(100, mean = 1, sd = 2),
                            y = rnorm(100, mean = 1, sd = 2),
                            num = rpois(n = 100, lambda = 5))

  expect_warning(formatStructured(dataOCC = dataChange3, type = model,
               varsOld = old, varsNew = new,
               projection = proj, boundary = map), 'Dataset provided has no reccords over the boundary.')


})

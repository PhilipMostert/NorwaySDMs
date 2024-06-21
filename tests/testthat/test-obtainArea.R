testthat::test_that('obtainArea can correctly obtain the boundary layer of a country, and transform it to the desired projection', {

  skip_on_cran()

  #args
  country <- c('Norway', 'Sweden')
  proj <- '+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

  map <- try(obtainArea(country, proj))

  skip_if(inherits(map, 'try-error'))

  expect_equal(class(map), c('sf', 'data.frame'))
  expect_identical(st_crs(map)[2], st_crs(proj)[2])

  proj2 <- 'EPSG:4326'

  map2 <- obtainArea(country, proj2)

  expect_equal(class(map2), c('sf', 'data.frame'))
  expect_identical(st_crs(map2)[2], st_crs(proj2)[2])


})

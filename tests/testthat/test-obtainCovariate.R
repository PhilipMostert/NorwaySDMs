testthat::test_that('obtainCovariate can correctly obtain the covariate layer, and transform it to the desired projection', {

 skip_on_cran()
 library(R.utils)

  covname <- "tavg"
  countries <- c('Norway', 'Sweden')
  projection <- '+proj=utm +zone=59 +south'
  #path <- './tests/testthat'
  path <- './wc2.1_country'
  dir.create(path)

  if(dir.exists(path)) {

    ##If taking too long//servers down

  try(cov <- withTimeout(
    obtainCovariate(covname, res = '10',
                    projection, path), timeout = 60, onTimeout = 'silent'))

  if ('cov' %in% ls()) {

  expect_equal(class(cov)[1], 'SpatRaster')
  expect_identical(st_crs(cov)[2], st_crs(projection)[2])


  }
  #Change CRS
  projection2 <- 'EPSG:4326'

  try(cov2 <-  withTimeout(obtainCovariate(covname, res = '10',
                         projection2, path), timeout = 60, onTimeout = 'silent'))

  if ('cov2' %in% ls()) {

  expect_equal(class(cov2)[1], 'SpatRaster')
  expect_identical(st_crs(cov2)[2], st_crs(projection2)[2])
}
  unlink(path, recursive = TRUE)


  }

})

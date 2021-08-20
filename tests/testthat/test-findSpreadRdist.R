test_that("length of spread equal to response variables", {
  data("met")
  data("physg")
  best.spread<-findSpreadRdist(physg,met,10,"euclidean",scale=TRUE)
  expect_equal(length(best.spread), ncol(met))
})

test_that("error of the input 'fun'", {
  data("met")
  data("physg")
  expect_error(findSpreadRdist(physg,met,10,euclidean,scale=TRUE))
})

test_that("wrong distance funcion", {
  data("met")
  data("physg")
  expect_error(findSpreadRdist(physg,met,10,"bray",scale=TRUE))
})

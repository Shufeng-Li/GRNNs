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

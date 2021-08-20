test_that("length of spread equal to response variables", {
  data("met")
  data("physg")
  best.spread<-findSpread(physg,met,10,"bray",scale=TRUE)
  expect_equal(length(best.spread), ncol(met))
})

test_that("error of the input 'fun'", {
  data("met")
  data("physg")
  expect_error(findSpread(physg,met,10,bray,scale=TRUE))
})

test_that("wrong distance funcion", {
  data("met")
  data("physg")
  physg<-physg[-1,]
  best.spread<-findSpread(physg,met,10,"BRAY",scale=TRUE)
  expect_output(str(best.spread), "The distance funcion is not included in GRNNs, please try other functions.")
})

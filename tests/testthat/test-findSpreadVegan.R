test_that("length of spread equal to response variables", {
  data("met")
  data("physg")
  met.test<-met[1:30,1]
  met.test<-as.data.frame(met.test)
  physg.test<-physg[1:30,]
  best.spread<-findSpreadVegan(physg.test,met.test,3,"bray",scale=TRUE)
  expect_equal(length(best.spread), ncol(met.test))
})

test_that("error of the input 'fun'", {
  data("met")
  data("physg")
  expect_error(findSpreadVegan(physg,met,10,euclidean,scale=TRUE))
})

test_that("wrong distance funcion", {
  data("met")
  data("physg")
  expect_error(findSpreadVegan(physg,met,10,"minkowski",scale=TRUE))
})


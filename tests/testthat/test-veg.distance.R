test_that("row and column of results equal to train and test dataset", {
  data("physg")
  physg.train<-physg[1:10,]
  physg.test<-physg[11:30,]
  distance<-veg.distance(physg.test,physg.train,"bray")
  expect_equal(nrow(distance), nrow(physg.test))
  expect_equal(ncol(distance), nrow(physg.train))
})

test_that("error of input datasets", {
  data("physg")
  physg.train<-physg[1:10]
  physg.test<-physg[11:30]
  expect_error(veg.distance(physg.test,physg.train,"bray"))
})

test_that("invalid distance method", {
  data("physg")
  physg.train<-physg[1:10,]
  physg.test<-physg[11:30,]
  expect_error(veg.distance(physg.test,physg.train,"minkowski"))
})

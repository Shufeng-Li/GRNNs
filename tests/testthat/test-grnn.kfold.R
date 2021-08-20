test_that("row and column of results equal to train and test dataset", {
  data("met")
  data("physg")
  results_kfold<-grnn.kfold(physg,met,10,"euclidean",scale=TRUE)
  expect_equal(nrow(results_kfold), 7)
  expect_equal(ncol(results_kfold), ncol(met))
})

test_that("error of input k-folds", {
  data("met")
  data("physg")
  expect_error(grnn.kfold(physg,met,0,"euclidean",scale=TRUE))
  expect_error(grnn.kfold(physg,met,1,"euclidean",scale=TRUE))
})

test_that("error of input in 'fun'", {
  data("met")
  data("physg")
  expect_error(grnn.kfold(physg,met,10,euclidean,scale=TRUE))
})

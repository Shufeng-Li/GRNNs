test_that("length of prediction equal to response variables", {
  data("met")
  data("physg")
  best.spread<-c(0.33,0.33,0.31,0.34,0.35,0.35,0.32,0.31,0.29,0.35,0.35)
  predict<-physg[1,]
  physg.train<-physg[-1,]
  met.train<-met[-1,]
  prediction<-grnn(predict,physg.train,met.train,fun="euclidean",best.spread,scale=TRUE)
  expect_equal(length(prediction), ncol(met))
})

test_that("error of spread", {
  data("met")
  data("physg")
  best.spread<-c(0.33,0.33,0.31,0.34,0.35,0.35,0.32,0.31,0.29,0.35)
  predict<-physg[1,]
  physg.train<-physg[-1,]
  met.train<-met[-1,]
  expect_error(prediction<-grnn(predict,physg.train,met.train,fun="euclidean",best.spread,scale=TRUE))
})

test_that("variables in predict not equal to training dataset", {
  data("met")
  data("physg")
  best.spread<-c(0.33,0.33,0.31,0.34,0.35,0.35,0.32,0.31,0.29,0.35,0.35)
  predict<-physg[1,-1]
  physg.train<-physg[-1,]
  met.train<-met[-1,]
  expect_error(prediction<-grnn(predict,physg.train,met.train,fun="euclidean",best.spread,scale=TRUE))
})

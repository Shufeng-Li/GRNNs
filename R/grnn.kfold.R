#' General Regression Neural Networks (GRNNs)
#'
#' @param x The dataframe of training predictor dataset
#' @param y The dataframe of training response variables
#' @param k The numeric number of k folds
#' @param fun The distance function
#' @param scale The logic statements (TRUE/FALSE)
#'
#' @importFrom cvTools cvFolds
#' @importFrom scales rescale
#' @importFrom stats sd
#' @importFrom stats cor
#' @importFrom stats cor.test
#' @return rmse,stdae,stdev,mae,r,pvalue,best spread
#' @export
#'
#' @examples
#' data("met")
#' data("physg")
#' \donttest{results_kfold<-grnn.kfold(physg,met,10,"euclidean",scale=TRUE)}
grnn.kfold <- function(x,y,k,fun,scale=TRUE) {
  x<-as.data.frame(x)
  y<-as.data.frame(y)
  # the follow code are to find the optimal spread
  n_p<-ncol(y)
  n_s<-nrow(y)
  pred_it<-numeric(n_p)
  spread.end<-NULL

  cvr1<-cvTools::cvFolds(n_s, K = k)
  predict_end<-NULL
  for (i in 1:k) {
    subcvr1<-cvr1$subsets
    tv.x <- x[subcvr1[cvr1$which != i],,drop=FALSE]
    test.x <- x[subcvr1[cvr1$which == i],,drop=FALSE]
    tv.y <- y[subcvr1[cvr1$which != i],,drop=FALSE]
    test.y <- y[subcvr1[cvr1$which == i],,drop=FALSE]
    best.spread<-findSpread (tv.x,tv.y,k,fun)
    spread.end<-rbind(spread.end,best.spread)
    #end for find optimal spread
    #begining to use the optimal spread to calculate the predict parameters
    pred_it<-matrix(NA,nrow(test.y),ncol(test.y))
    if (scale==TRUE){
      tv.x<-scales::rescale(as.matrix(tv.x), to=c(-1,1))
      test.x<-scales::rescale(as.matrix(test.x), to=c(-1,1))
    }
    w.input<-grnn.distance(tv.x,test.x,fun)
    for (k in 1:n_p) {
      b.input<-w.input*sqrt(-log(0.5))/best.spread[k]
      a1<-exp(-b.input^2)
      weight_all<-colSums(a1)
      for (h in 1:ncol(a1)) {if (weight_all[h]==0) {weight_all[h]<-1}}
      pred_it[,k]<-(t(a1) %*% as.matrix(tv.y[,k]))/weight_all
    }
    row.names(pred_it) <- row.names(test.y)
    predict_end<-rbind(predict_end,pred_it)
  }
  predict_all<-predict_end[order(rownames(predict_end)),]
  y_all<-y[order(rownames(y)),]
  res<-predict_all-y_all
  res<-as.data.frame(res)
  stdev<-numeric(ncol(res))
  stdae<-numeric(ncol(res))
  mae<-numeric(ncol(res))
  rmse<-sqrt(colSums(res^2)/nrow(res))
  stdev<-sqrt(colSums(res^2)/(nrow(res)-2))
  r<-numeric(ncol(res))
  pvalue<-numeric(ncol(res))

  for (n in 1:ncol(res)) {
    mae[n]<-mean(abs(res[,n]))
    stdae[n]<-stats::sd(abs(res[,n]))
    r[n]<-stats::cor(y_all[,n],predict_all[,n])
    corv<-stats::cor.test(y_all[,n],predict_all[,n])
    pvalue[n] <- 2*stats::pt(corv$statistic,  df = corv$parameter, lower.tail=FALSE)
  }
  spread.mean<-colMeans(spread.end)
  results<-rbind(rmse,stdae,stdev,mae,r,pvalue,spread.mean)
  print(results)
}

#' Find best spread using vegan function
#'
#' @param x The dataframe of training predictor dataset
#' @param y The dataframe of training response variables
#' @param k The numeric number of k folds
#' @param fun The distance function
#' @param scale The logic statements (TRUE/FALSE)
#'
#' @importFrom cvTools cvFolds
#' @importFrom scales rescale
#' @return The vector of best spreads
#' @export
#'
findSpreadVegan <- function(x,y,k,fun,scale=TRUE) {
  x<-as.data.frame(x)
  y<-as.data.frame(y)
  spread_all<-NULL
  rmse_all<-NULL
  cvr<-cvTools::cvFolds(nrow(x), K = k)  # Generate the index of random k folds for the data
  subcvr<-cvr$subsets   # Assign the index to subsvr
  for (spread in seq(0.01, 2, 0.01)) {
    predict<-NULL
    #print(spread)
    for (i in 1:k) {
      train.x <- x[subcvr[cvr$which != i],,drop=FALSE]       # k-1 folds of physiognomic data used for training
      validation.x <- x[subcvr[cvr$which == i],,drop=FALSE]  # 1 folds of physiognomic data used as validation data
      train.y <- y[subcvr[cvr$which != i],,drop=FALSE]         # k-1 folds of meteorological data used for training
      validation.y <- y[subcvr[cvr$which == i],,drop=FALSE]    # 1 folds of meteorological data used as validation data
      if (scale==TRUE){
        train.x<-scales::rescale(as.matrix(train.x), to=c(-1,1))
        validation.x<-scales::rescale(as.matrix(validation.x), to=c(-1,1))
      }
      w.input<-veg.distance(train.x,validation.x,fun)    # Calculating the distance between the train data and the validation data
      b.input<-w.input*sqrt(-log(.5))/spread
      a1<-exp(-b.input^2)                                      # Use Gaussian kernel function to transform the input data
      weight_all<-colSums(a1)
      for (h in 1:ncol(a1)) {if (weight_all[h]==0) {weight_all[h]<-1}}
      pred_it<-(t(a1) %*% as.matrix(train.y))/weight_all     # Calculating the prediction values
      row.names(pred_it) <- row.names(validation.y)
      predict<-rbind(predict,pred_it)
    }
    predict1<-predict[order(rownames(predict)),]
    y1<-y[order(rownames(y)),]
    res<-predict1-y1          # Calculating the errors between the prediction values and the validation values
    res<-as.data.frame(res)
    rmse<-sqrt(colSums(res^2)/nrow(res))  # root-mean-square error
    spread_all<-rbind(spread_all,spread)
    rmse_all<-rbind(rmse_all,rmse)
  }
  best.spread<-numeric(ncol(y))
  test<-cbind(as.matrix(spread_all),rmse_all) # Combine the spread and rmse_all values together
  for (m in 1:ncol(y)) {
    best_num<-test[which(test[,m+1]==min(test[,m+1])),1]
    best.spread[m]<-as.matrix(best_num[1])   # Find the optimal spread with minimum value of rmse
  }
  best.spread
}

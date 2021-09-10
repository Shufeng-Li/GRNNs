#' General Regression Neural Networks (GRNNs)
#'
#' @description This GRNNs uses various distance functions including: "euclidean", "minkowski",
#' "manhattan", "maximum", "canberra", "angular", "correlation", "absolute_correlation", "hamming",
#' "jaccard","bray", "kulczynski", "gower", "altGower", "morisita", "horn", "mountford", "raup",
#' "binomial", "chao", "cao","mahalanobis".
#' @param p_input The dataframe of input predictors
#' @param p_train The dataframe of training predictor dataset
#' @param v_train The dataframe of training response variables
#' @param fun The distance function
#' @param best.spread The vector of best spreads
#' @param scale The logic statements (TRUE/FALSE)
#'
#' @importFrom scales rescale
#' @return The predictions
#' @export
#'
#' @examples
#' data("met")
#' data("physg")
#' best.spread<-c(0.33,0.33,0.31,0.34,0.35,0.35,0.32,0.31,0.29,0.35,0.35)
#' predict<-physg[1,]
#' physg.train<-physg[-1,]
#' met.train<-met[-1,]
#' prediction<-grnn(predict,physg.train,met.train,fun="euclidean",best.spread,scale=TRUE)
grnn<- function(p_input,p_train,v_train,fun="euclidean",best.spread,scale=TRUE){
  p_input<-as.data.frame(p_input)
  p_train<-as.data.frame(p_train)
  v_train<-as.data.frame(v_train)
  n_col<-ncol(v_train)
  n_row<-nrow(p_input)
  pred_it<-numeric(n_col)
  spread.end<-NULL
  predict<-NULL
  if (scale==TRUE){
    p_train<-scales::rescale(as.matrix(p_train), to=c(-1,1))
    p_input<-scales::rescale(as.matrix(p_input), to=c(-1,1))
  }
  for (i in 1:n_row){
    p_input.e<-data.frame(as.list(p_input[i,]))
    w.input<-grnn.distance(p_train,p_input.e,fun)
    for (j in 1:n_col) {
      b.input<-w.input*sqrt(-log(0.5))/best.spread[j]
      a1<-exp(-b.input^2)
      weight_all<-colSums(a1)
      for (h in 1:ncol(a1)) {if (weight_all[h]==0) {weight_all[h]<-1}}
      pred_it[j]<-(t(a1) %*% as.matrix(v_train[,j]))/weight_all
    }
    predict<-rbind(predict,pred_it)
  }
colnames(predict) <- colnames(v_train)
rownames(predict) <- rownames(p_input)
predict<-format(round(predict, 2), nsmall = 2)
return(predict)
}

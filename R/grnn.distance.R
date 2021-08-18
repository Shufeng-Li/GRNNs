#' grnn distance
#'
#' @param x The matrix of training predictor dataset
#' @param y The matrix of training response variables
#' @param fun The distance function
#'
#' @importFrom rdist cdist
#' @return The matrix of distance between a and b
#' @export
#'
#' @examples
#' data("met")
#' data("physg")
#' distance<-grnn.distance(physg[1:30,],physg[40:60,],"bray")
grnn.distance<-function(x,y,fun){
  if ( fun== "euclidean"|fun== "minkowski"|fun== "manhattan"|
       fun== "maximum"|fun== "canberra"|fun== "angular"|
       fun== "correlation"|fun== "absolute_correlation"|
       fun== "hamming"|fun== "jaccard"){
    rdist::cdist(x,y,metric = fun)
  }
  else if (fun== "bray"|fun== "kulczynski"|fun== "gower"|
           fun== "altGower"|fun== "morisita"|fun== "horn"|
           fun== "mountford"|fun== "raup"|fun== "binomial"|
           fun== "chao"|fun== "cao"|fun== "mahalanobis"){
    veg.distance(x,y,fun = fun)
  }
  else
  {print ("The distance funcion is not included in GRNNs, please try other functions.")}
}

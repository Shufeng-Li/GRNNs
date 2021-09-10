#' Find best spread
#'
#' @param p_train The dataframe of training predictor dataset
#' @param v_train The dataframe of training response variables
#' @param k The numeric number of k folds
#' @param fun The distance function
#' @param scale The logic statements (TRUE/FALSE)
#'
#' @return Best spread
#' @export
#'
#' @examples
#' data("met")
#' data("physg")
#' \dontrun{best.spread<-findSpread(physg,met,10,"bray",scale=TRUE)}
findSpread<-function(p_train,v_train,k,fun,scale=TRUE){
  if ( fun== "euclidean"|fun== "minkowski"|fun== "manhattan"|
       fun== "maximum"|fun== "canberra"|fun== "angular"|
       fun== "correlation"|fun== "absolute_correlation"|fun== "hamming"|
       fun== "jaccard"){
    if (scale==TRUE){
      findSpreadRdist(p_train,v_train,k,fun,scale=TRUE)
    }
    else
    {
      findSpreadRdist(p_train,v_train,k,fun)
    }
  }
  else if (fun== "bray"|fun== "kulczynski"|fun== "gower"|
           fun== "altGower"|fun== "morisita"|fun== "horn"|
           fun== "mountford"|fun== "raup"|fun== "binomial"|
           fun== "chao"|fun== "cao"|fun== "mahalanobis"){
    if (scale==TRUE){
      findSpreadVegan(p_train,v_train,k,fun,scale=TRUE)
    }
    else
    {findSpreadVegan(p_train,v_train,k,fun)}
  }
  else
  {print ("The distance funcion is not included in GRNNs, please try other functions.")}
}

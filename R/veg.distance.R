#' distance using vegdist
#'
#' @param a The matrix of training predictor dataset
#' @param b The matrix of validation predictor dataset
#' @param fun The distance function
#'
#' @importFrom vegan vegdist
#' @return The matrix of distance between a and b
#' @export
#'
#' @examples
#' data("physg")
#' distance<-veg.distance(physg[1:10,],physg[20:40,],"bray")
veg.distance <- function(a,b,fun="bray"){
  m <- rbind(a,b)+1
  v <- as.matrix(vegan::vegdist(m, method = fun))
  xx<-data.frame(v[1:dim(a)[1],(dim(a)[1]+1):(dim(a)[1]+dim(b)[1])])
}

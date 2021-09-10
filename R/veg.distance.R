#' distance using vegdist
#'
#' @param a The dataframe of training predictor dataset
#' @param b The dataframe of validation predictor dataset
#' @param fun The distance function
#'
#' @importFrom vegan vegdist
#' @return The matrix of distance between a and b
#' @export
#'
#' @examples
#' data("physg")
#' physg.train<-physg[1:10,]
#' physg.test<-physg[11:30,]
#' distance<-veg.distance(physg.test,physg.train,"bray")
veg.distance <- function(a,b,fun="bray"){
  m <- rbind(a,b)+1
  v <- as.matrix(vegan::vegdist(m, method = fun))
  xx<-data.frame(v[1:dim(a)[1],(dim(a)[1]+1):(dim(a)[1]+dim(b)[1])])
}

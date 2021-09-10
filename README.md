
# General Regression Neural Networks (GRNNs) Package

<!-- badges: start -->
<!-- badges: end -->

The goal of GRNNs is to build a GRNN model using different functions.
This GRNNs package uses various distance functions including: "euclidean", "minkowski", "manhattan", "maximum", "canberra", "angular", "correlation", "absolute_correlation", "hamming", "jaccard","bray", "kulczynski", "gower", "altGower", "morisita", "horn", "mountford", "raup", "binomial", "chao", "cao","mahalanobis".

## Installation

You can install the released version of GRNNs from 
[github](https://github.com/Shufeng-Li/GRNNs/) with:

``` r
library(devtools)
install_github("Shufeng-Li/GRNNs")
```

## Example

This is a basic example which shows you how to use GRNNs:

``` r
library(GRNNs)
data("met")
data("physg")
predict<-physg[1,]
physg.train<-physg[-1,]
met.train<-met[-1,]
best.spread<-findSpread(physg.train,met.train,10,"euclidean",scale=TRUE)
prediction<-grnn(predict,physg.train,met.train,fun="euclidean",best.spread,scale=TRUE)
```


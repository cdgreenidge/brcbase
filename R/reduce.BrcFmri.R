reduce <- function(x, ...) {UseMethod("reduce")}

reduce.BrcFmri <- function(x, y, func = reduce_mean){
}

reduce_mean <- function(mat){
  stopifnot(is.matrix(mat))
  
  apply(mat, 1, mean)
}

reduce_pca <- function(mat){
  stopifnot(is.matrix(mat))
  
  stats::prcomp(mat, center = T, scale. = T)$x[,1]
}
reduce <- function(x, ...) {UseMethod("reduce")}

reduce.BrcFmri <- function(x, y, func = reduce_mean){
}

reduce_mean <- function(mat){
  stopifnot(is.matrix(mat))
  
  if(ncol(mat) == 0) return(rep(0, nrow(mat)))
  
  apply(mat, 1, mean)
}

reduce_pca <- function(mat){
  stopifnot(is.matrix(mat))
  
  if(ncol(mat) == 0) return(rep(0, nrow(mat)))
  
  stats::prcomp(mat, center = T, scale. = T)$x[,1]
}

.isValid_reduction <- function(x, y){
  if(class(x) != "BrcFmri") stop("x must be class BrcFmri")
  if(class(y) != "BrcParcellation") stop("y must be class BrcParcellation")
  
  if(!all(x$parcellation$dim3d == y$dim3d)) stop("x and y must have the same 
    dim3d")
  
  if(!isValid(x)) stop("x must be a valid BrcFmri. Try isValid(x)")
  if(!isValid(y)) stop("y must be a valid BrcFmri. Try isValid(y)")
  
  TRUE
}

.reduceFmritoParcellation <- function(x, y, func){
  stopifnot(class(x) == "BrcFmri", class(y) == "BrcParcellation")
  stopifnot(class(func) == "function")
  
  parcel <- unique(y$partition); parcel <- parcel[parcel != 0]
  
  mat <- sapply(parcel, function(i){
    idx <- which(y$partition == i)
    col.x <- x$parcellation$partition[idx]
    col.x <- col.x[col.x != 0]
    func(x$data2d[,col.x,drop=FALSE])
  })
  
  if(nrow(x$data2d) == 1) mat <- matrix(mat, ncol = length(mat))
  
  if(!is.matrix(mat)) stop("func used has corrupted the reduction. Please check the
                          func.")
  
  mat
}
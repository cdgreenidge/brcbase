#' Reducing function for BrcFmri object given parcellation
#' 
#' Reduces the BrcFmri object passed in based on applying a function
#' over each partition specified by a given BrcParcellation object. 
#' Both the BrcFmri and BrcParcellation must have the same dimensions.
#'
#' @param fmri The BrcFmri object to reduce
#' @param parcellation The BrcParcellation object to use as a parcellation
#' @param func The function to apply to each partition of parcellation in
#' fmri
#'
#' @return A BrcFmri object with the reduced data2d object, the same
#' id as fmri, and the parcellation of parcellation
#' @export
reduce <- function(fmri, parcellation, func = reduce_mean){
  .isValid_reduction(fmri,parcellation)
  
  mat <- .reduceFmritoParcellation(fmri, parcellation, func)
  
  structure(list(data2d = mat, id = fmri$id, parcellation = parcellation),
            class = "BrcFmri")
}

#' Reducing using the mean for reduce.BrcFmri
#' 
#' If mat is a matrix with no columns, the all 0-vector is returned.
#'
#' @param mat The matrix where each column represents a time series with nrow(mat)
#' values.
#'
#' @return A vector with nrow(mat) values
#' @export
reduce_mean <- function(mat){
  stopifnot(is.matrix(mat))
  
  if(ncol(mat) == 0) return(rep(0, nrow(mat)))
  
  apply(mat, 1, mean)
}

#' Reducing using the first principal component for reduce.BrcFmri
#' 
#' If mat is a matrix with no columns, the all 0-vector is returned. Be warned that
#' the first principal component is not unique, as the negative of the returned
#' value is also a valid first principal component. We use the first principal
#' component decided upon by stat::princomp.
#'
#' @param mat The matrix where each column represents a time series with nrow(mat)
#' values.
#'
#' @return A vector with nrow(mat) values
#' @export
reduce_pca <- function(mat){
  stopifnot(is.matrix(mat))
  
  if(ncol(mat) == 0) return(rep(0, nrow(mat)))
  
  stats::prcomp(mat, center = T, scale. = T)$x[,1]
}

.isValid_reduction <- function(x, y){
  if(class(x) != "BrcFmri") stop("x must be class BrcFmri")
  if(class(y) != "BrcParcellation") stop("y must be class BrcParcellation")
  
  if(!all(x$parcellation$dim3d == y$dim3d)) stop(paste("x and y must have the",
   "same dim3d"))
  
  isValid(x)
  isValid(y)
  
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
  
  if(!is.matrix(mat)) stop(paste("func used has corrupted the reduction. Please",
    "check the func."))
  
  mat
}
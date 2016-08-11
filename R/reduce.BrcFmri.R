#' Reduces S3's representation
#'
#' Generic function to reduce the S3 object's representation to
#' a simpler one. Its implementation depends on the specific class
#' passed in.
#'
#' @param obj The object to reduce
#' @param template The template specifying the regions to reduce obj to
#' @param func The function specifying how to do the reduction
#'
#' @return void
#' @export
reduce <- function(obj, template, func) {UseMethod("reduce")}

#' Reducing function for BrcFmri object given parcellation
#' 
#' Reduces the BrcFmri object passed in based on applying a function
#' over each partition specified by a given BrcParcellation object. 
#' Both the BrcFmri and BrcParcellation must have the same dimensions.
#'
#' @param obj The BrcFmri object to reduce
#' @param template The BrcParcellation object to use as a template
#' @param func The function to apply to each partition of parcellation in
#' obj
#'
#' @return A BrcFmri object with the reduced data2d object, the same
#' id as obj, and the parcellation of template
#' @export
reduce.BrcFmri <- function(obj, template, func = reduce_mean){
  .isValid_reduction(obj,template)
  
  mat <- .reduceFmritoParcellation(obj, template, func)
  
  structure(list(data2d = mat, id = obj$id, parcellation = template),
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
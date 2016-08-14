#' BrcParcellation constructor
#' 
#' \code{BrcParcellation} makes a new \code{BrcParcellation} instance.
#' 
#' A \code{BrcParcellation} datatype contains all the information needed to map 
#' a two-dimensional fMRI data matrix (time x voxels) to a four-dimensional 
#' series of fMRI volumes (length x width x height x time). It is a list with 
#' two named components: \enumerate{ \item \code{dim3d}, the dimensions of the 
#' 3D fMRI volume (length x width x height) \item \code{partition}, a vector 
#' assigning columns in the 2D data matrix to voxels in the 3D fMRI volume. The 
#' purpose of the \code{partition} is to allow one to group the 3D fMRI region 
#' into "parcels", making the 2D data matrix smaller. More details on the 
#' \code{partition} below. }
#' 
#' Because \code{BrcFmri} is a dataype, not an object there are no accessor 
#' functions. You can get its components directly with the \code{$} operator.
#' 
#' \code{partition} details: \code{partition} is a vector with one entry for 
#' each voxel in the 3D fMRI volume, and with values \{0, 1, ..., \emph{n}\}, 
#' where \emph{n} is the number of columns in the two-dimensional fMRI data 
#' matrix. A "0" entry in the \code{partition} marks the corresponding 3D voxel 
#' as being empty, i.e. it is not specified in the 2D data matrix and it has an 
#' implicit value of 0. Otherwise, the (postive) number marks the column in the
#' 2D data matrix that contains the data for that 3D voxel. Since the vector is 
#' 1-dimensional and the fMRI volume is three-dimensional, we adopt R's 
#' convention of listing the voxels in column-row-depth order.
#' 
#' @param dim3d  a 3-element numeric specifiying the dimensions of the 3D fMRI
#'      volume
#' @param partition a vector specifying the partition of the 3D fMRI volume. It
#'      must have as many elements as there are 3D voxels specified in the 
#'      \code{dim3d} argument. Its values must be \{0, 1, ..., \emph{n}\} where
#'      \emph{n} is the number of columns in the two-dimensional fMRI data 
#'      matrix.
#' @return a new \code{BrcParcellation} instance.
#' @export   
BrcParcellation <- function(dim3d, partition) {
  if (!is.numeric(dim3d)) {
    stop("dim3d argument must be a 3-element numeric vector")
  } else if (length(dim3d) != 3) {
    stop("dim3d argument must be a 3-element numeric vector")
  } else if (length(partition) != prod(dim3d)) {
    stop("partition must be length equal to the product of elements in dim3d")
  } else if (!all(partition <= prod(dim3d))) {
    stop("all elements in partition must be consecutive integers starting from 0 or 1")
  }
  
  .isValid_partition(partition)

  structure(list(dim3d=dim3d, partition=partition), class="BrcParcellation")
}

#' Checking \code{BrcParcellation} instance validity.
#' 
#' \code{isValid} method for class "\code{BrcParcellation}.
#' 
#' Fails noisily with a stop message if the \code{BrcParcellation}
#' instance is invalid. Otherwise, returns TRUE.
#' 
#' @param  obj  The \code{BrcParcellation} instance to check
#' @return void
#' @export
isValid.BrcParcellation <- function(obj) {
  num3dVoxels <- Reduce("*", obj$dim3d)
  if (length(obj$partition) != num3dVoxels) {
    stop("Length of partition not equal to number of 3D voxels.")
  }
  .isValid_partition(obj$partition)
  
  TRUE
}

.isValid_partition <- function(partition) {
  if(!is.numeric(partition)) stop("partition must be a numeric vector")
  if(!all(partition >= 0)) stop("partition must be all non-negative integers")
  if(!all(diff(sort(unique(partition))) == 1)) stop("partition must be consecutive integers")
  if(!(all(partition %%1 == 0))) stop("partition all must be non-negative integers")
  if(all(partition == 0)) stop("partition must have at least one positive integer")
  
  TRUE
}

#' Printing BrcParcellation objects
#' 
#' \code{print} method for class "\code{BrcParcellation}.
#'
#' @param x a BrcParcellation instance
#' @param ... unused
#' @export
print.BrcParcellation <- function(x, ...){
  cat("BrcParcellation object of dimension", paste0(x$dim3d, collapse = " x "),
      "\n with", length(which(x$partition != 0)), "parcels\n")
  cat("----------\n\n")
  
  cat("$dim3d\n")
  print(x$dim3d)
  
  if(length(x$partition) > 10){
    cat("\n$partition (Abridged)\n")
  } else {
    cat("\n$partition\n")
  }
  print(x$partition[1:min(10, length(x$partition))])
  
  invisible() 
}

#' Summarizing BrcParcellation objects
#' 
#' \code{summary} method for class "\code{BrcParcellation}.
#' 
#' @param object a BrcParcellation instance
#' @param ... unused
#' @export
summary.BrcParcellation <- function(object, ...){
  dims <- object$dim3d
  cat(sprintf("Volume resolution:       %d x %d x %d voxels\n", dims[1], dims[2],
              dims[3]))
  cat(sprintf("Number of parcellations: %d parcels\n", 
              length(which(object$partition != 0))))
  cat(sprintf("Estimate size:           %.2f Mb\n", utils::object.size(object)/1024^2,2))
  
  invisible()
}
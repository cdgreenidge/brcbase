#' BrcParcellation constructor
#' 
#' \code{BrcParcellation} makes a new \code{BrcParcellation} instance.
#' 
#' A \code{BrcParcellation} datatype contains all the information needed to map 
#' a two-dimensional fMRI data matrix (time x voxels) to a four-dimensional 
#' series of fMRI volumes (length x width x height x time). It is a list with 
#' two named components: \enumerate{ \item \code{dim3d}, the dimensions of the 
#' 3D fMRI volume (length x width x height) \item \code{partition}, a factor 
#' assigning columns in the 2D data matrix to voxels in the 3D fMRI volume. The 
#' purpose of the \code{partition} is to allow one to group the 3D fMRI region 
#' into "parcels", making the 2D data matrix smaller. More details on the 
#' \code{partition} below. }
#' 
#' Because \code{BrcFmri} is a dataype, not an object there are no accessor 
#' functions. You can get its components directly with the \code{$} operator.
#' 
#' \code{partition} details: \code{partition} is a factor with one entry for 
#' each voxel in the 3D fMRI volume, and with levels \{0, 1, ..., \emph{n}\}, 
#' where \emph{n} is the number of columns in the two-dimensional fMRI data 
#' matrix. A "0" entry in the \code{partition} marks the corresponding 3D voxel 
#' as being empty, i.e. it is not specified in the 2D data matrix and it has an 
#' implicit value of 0. Otherwise, the (postive) number marks the column in the
#' 2D data matrix that contains the data for that 3D voxel. Since the factor is 
#' 1-dimensional and the fMRI volume is three-dimensional, we adopt R's 
#' convention of listing the voxels in column-row-depth order.
#' 
#' @param dim3d  a 3-element numeric specifiying the dimensions of the 3D fMRI
#'      volume
#' @param partition a factor specifying the partition of the 3D fMRI volume. It
#'      must have as many elements as there are 3D voxels specified in the 
#'      \code{dim3d} argument. Its levels must be \{0, 1, ..., \emph{n}\} where
#'      \emph{n} is the number of columns in the two-dimensional fMRI data 
#'      matrix.
#' @return a new \code{BrcParcellation} instance.
#' @export   
BrcParcellation <- function(dim3d, partition) {
  if (!is.numeric(dim3d)) {
    stop("dim3d argument must be a 3-element numeric vector")
  } else if (length(dim3d) != 3) {
    stop("dim3d argument must be a 3-element numeric vector")
  } else if (!is.factor(partition)) {
    stop("partition argument must be a factor whose levels are indices (>= 0)")
  } else if (any(levels(partition) < 0)) {
    stop("partition argument must be a factor whose levels are indices (>= 0)")
  }

  structure(list(dim3d=dim3d, partition=partition), class="BrcParcellation")
}

#' Checking \code{BrcParcellation} instance validity.
#' 
#' \code{isValid} method for class "\code{BrcParcellation}.
#' 
#' Fails noisily with a stop message if the \code{BrcParcellation}
#' instance is invalid. Otherwise, nothing happens.
#' 
#' @param  obj  The \code{BrcParcellation} instance to check
#' @return void
isValid.BrcParcellation <- function(obj) {
  num3dVoxels <- Reduce("*", obj$dim3d)
  if (length(obj$partition) != num3dVoxels) {
    stop("Length of partition not equal to number of 3D voxels.")
  }
}

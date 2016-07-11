#' BrcFmri Builder
#' 
#' \code{buildBrcFmri} is a helper function that quickly build a \code{BrcFmri}
#' instance. Its main benefit is that it constructs all of the BrcFmri object's
#' dependencies (a \code{BrcParcellation} and a \code{partition}) for you.
#' 
#' @param data2d, the 2D fMRI matrix (time x voxels)
#' @param dim3d, the dimensions of the 3D fMRI volume
#' @param id, an ID string
#' @param partition, a factor specifying the 3D fMRI partitions. The default 
#'   (\code{NULL}) will put every 3D fMRI voxel in its own partition. If you 
#'   wish to specify a custom partition, see \code{\link{BrcParcellation}} for 
#'   format details.
#' @return a \code{BrcFmri} instance
#' @seealso \code{\link{BrcFmri}}, \code{\link{BrcParcellation}}
#' @export
buildBrcFmri <- function(data2d, dim3d, id="", partition=NULL) {
  parcellation <- .buildParcellation(dim3d=dim3d, userPartition=partition)
  mri <- BrcFmri(data2d=data2d, id=id, parcellation=parcellation)
  isValid(mri)
  mri
}

.buildParcellation <- function(dim3d, userPartition) {
  partition <- .buildPartition(dim3d, userPartition)
  BrcParcellation(dim3d, partition)
}

.buildPartition <- function(dim3d, userPartition) {
  if (is.null(userPartition)) {
    num3dVoxels <- Reduce("*", dim3d)
    return(factor(1:num3dVoxels))
  } else {
    return(userPartition)
  }
}

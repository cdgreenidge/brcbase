#' \code{BrcFmri} Builder
#' 
#' \code{buildBrcFmri} is a helper function that quickly build a \code{BrcFmri}
#' instance from a 4-dimensional matrix. Its main benefit is that it 
#' constructs all of the \code{BrcFmri} object's dependencies 
#' (a \code{BrcParcellation} and a \code{data2d}) for you. 
#' Here, the input \code{data4d} is a 4-dimensional matrix where the 
#' first 3 dimensions 
#' refer to the x, y, z coordinates and the 4th dimension refers to time.
#' 
#' Along each 3-dimensional matrix indexed by time, each voxel corresponds to
#' an x, y, z coordinate. An non-empty voxel is a voxel whose vector along the
#' 4th-dimension (time) is all non-zero. \code{buildBrcFmri} assumes a
#' singleton parcellation where each non-empty voxel is assigned its own 
#' parcel.
#' 
#' @param data4d, the 4D fMRI matrix (x-direction x y-direction x z-direction
#' x time)
#' @param id, an ID string
#' 
#' @return a \code{BrcFmri} instance
#' @seealso \code{\link{BrcFmri}}, \code{\link{BrcParcellation}}
#' @export
buildBrcFmri <- function(data4d, id="") {
  if(!is.array(data4d)) stop("data4d must be an array")
  dim.vec <- dim(data4d)
  if(length(dim.vec) != 4) stop("data4d must have 4 dimensions")
  if(!checkMotion(data4d)) stop(paste("data4d must not display any motion.",
    "See ?checkMotion for more details."))
  
  idx <- which(data4d[,,,1] != 0)
  parcellation <- .buildParcellation(dim.vec[1:3], idx)
  data2d <- data4dTo2d(data4d)[,idx]
  
  mri <- BrcFmri(data2d=data2d, id=id, parcellation=parcellation)
  
  isValid(mri)
  mri
}

.buildParcellation <- function(dim3d, idx) {
  partition <- .buildPartition(dim3d, idx)
  BrcParcellation(dim3d, partition)
}

.buildPartition <- function(dim3d, idx) {
  partition <- rep(0, prod(dim3d))
  partition[idx] <- 1:length(idx)
  
  partition
}


#' \code{BrcParcellation} Builder
#' 
#' \code{buildBrcParcellation} is a helper function that quickly build a 
#' \code{BrcParcellation} instance from a 3-dimensional matrix. Its main 
#' benefit is that it constructs all of the \code{BrcParcellation} 
#' object's dependencies (\code{dim3d} and \code{partition}) for you. 
#' 
#' Here, the input \code{data3d} is a 3-dimensional matrix, and each
#' unique value in \code{data3d} will be assigned a unique parcel. The value
#' 0 in \code{data3d} will signal an empty voxel.
#' 
#' @param data3d a 3-dimensional matrix
#'
#' @return a \code{BrcParcellation} instance
#' @export
buildBrcParcellation <- function(data3d){
  if(!is.array(data3d)) stop("data3d must be an array")
  dim.vec <- dim(data3d)
  if(length(dim.vec) != 3) stop("data3d must have 4 dimensions")
  
  idx <- which(data3d != 0)
  .buildParcellation(dim.vec, idx)
}

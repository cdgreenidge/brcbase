#' Convert a Voxel's Coordinates (3-dimensional) to Position Index
#'
#' vec represents the 3-dimensional coordinates of a voxel and
#' dim3d represents the total number of voxels in each direction (x,y,z 
#' direction) for the fmri. Converts vec into the numeric that represents
#' the position of vec in 3-dimensional array.
#'
#' @param dim3d a numeric vector of length 3
#' @param vec a numeric vector of length 3
#'
#' @return numeric representing vec's position in an array of dimension dim3d
#' @export
voxel3DToIdx <- function(dim3d, vec){
  if(!is.numeric(vec) | !is.numeric(dim3d)) 
    stop("dim3d and vec must be numerics")
  if(length(vec) != 3 | length(dim3d) != 3) 
    stop("dim3d and vec must be have length 3")
  if(!all(vec > 0) | !all(dim3d > 0)) 
    stop("dim3d and vec must be all positive values")
  if(!all(vec %% 1 == 0) | !all(dim3d %% 1 == 0)) 
    stop("dim3d and vec must have integer values")
  if(!all(vec <= dim3d))
    stop("values in vec must be smaller than or equal to dim3d elementwise")

  vec[1] + (vec[2]-1)*dim3d[1] + (vec[3]-1)*(dim3d[1]*dim3d[2])
}

#' Convert a Voxel's Position Index to Coordinates (3-dimensional) 
#'
#' idx represents the numeric position of a voxel and
#' dim3d represents the total number of voxels in each direction (x,y,z 
#' direction) for the fmri. Converts idx into the 3-dimensional
#' coordinates of the voxel in 3-dimensional array.
#' 
#' @param dim3d a numeric vector of length 3
#' @param idx a numeric
#'
#' @return numeric of length 3 representing the 3D coordinates
#' @export
voxelIdxTo3D <- function(dim3d, idx){
  if(!is.numeric(idx) | !is.numeric(dim3d)) 
    stop("dim3d and idx must be numerics")
  if(length(dim3d) != 3) stop("dim3d must have length 3")
  if(length(idx) != 1) stop("idx must have length 1")
  if(!all(idx > 0) | !all(dim3d > 0)) 
    stop("dim3d and idx must be all positive values")
  if(!all(idx %% 1 == 0) | !all(dim3d %% 1 == 0)) 
    stop("idx and vec must have integer values")
  if(idx > prod(dim3d)) stop("idx must be less than prod(dim3d)")
  
  prod.dim <- prod(dim3d[1:2])
  z <- ceiling(idx / prod.dim)
  
  tmp <- idx %% prod.dim; if(tmp == 0) tmp <- prod.dim
  y <- ceiling(tmp / dim3d[1])
  
  x <- tmp %% dim3d[1]; if(x == 0) x <- dim3d[1]
  
  c(x,y,z)
}


#' Convert a 2D fMRI data matrix to a 4D fMRI array 
#' 
#' The 2D data matrix's dimensions are time x voxels. It is suitable for
#' statistical analysis. The 4D data matrix's dimensions are length x width x
#' height x time. it is suitable for visualization.
#' 
#' @param mat2d the 2D fMRI data matrix (which will usually come from 
#' \code{BrcFmri$data2d}.)
#' @param parcellation a \code{BrcParcellation} instance (which will usually
#' come from \code{BrcFmri$parcellation}).
#' @return a 4D array containing the series of fMRI volumes.
#' @export
#' @seealso \code{\link{data4dTo2d}, the inverse function}
data2dTo4d <- function(mat2d, parcellation) {
  part <- parcellation$partition
  expandedMatrix <- .expandMatrix(mat2d, part)
  array(data=t(expandedMatrix), dim=c(parcellation$dim3d, nrow(mat2d)))
}

#' Convert a 4D array containing a series of fMRI volumes to a 2D fMRI data 
#' matrix.
#' 
#' The 4D data matrix's dimensions are length x width x height x time. it is
#' suitable for visualization. The 2D data matrix's dimensions are time x
#' voxels. It is suitable for statistical analysis.
#' 
#' @param arr4d the 4D array containing the series of fMRI volumes
#' @return a 2D fMRI data matrix
#' @export
data4dTo2d <- function(arr4d) {
  num3dVoxels <- Reduce("*", dim(arr4d)[-4])
  scanLength <- dim(arr4d)[4]
  t(matrix(data=arr4d, ncol=scanLength, nrow=num3dVoxels))
}

.expandMatrix <- function(mat2d, partition) {
  compressedIndices <- partition[partition != 0]
  expandedIndices <- (1:length(partition))[partition != 0]

  expandedMatrix <- matrix(data=0, nrow=nrow(mat2d), ncol=length(partition))
  expandedMatrix[ , expandedIndices] <- mat2d[ , compressedIndices]

  expandedMatrix
}
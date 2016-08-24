#' Reads NIfTI fMRI data
#' 
#' \code{readNifti} reads a NIfTI formatted fMRI data file into a 
#' \code{BrcFmri} object.
#' 
#' @param file  the file path to read
#' @param id  the id for the the \code{BrcFmri} output. If left \code{NULL},
#'   the ID will be set as the file path.
#' @return a BrcFmri instance with the data from \code{file}
#' @export
readNifti <- function(file, id=NULL) {
  if (is.null(id)) {
    id <- file
  }

  nifti <- oro.nifti::readNIfTI(file)
  buildBrcFmri(nifti@.Data, id=id)
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
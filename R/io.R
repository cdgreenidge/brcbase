niftiToBrcFmri <- function(file, id=NULL) {
  if (is.null(id)) {
    id <- file
  }

  nifti <- oro.nifti::readNIfTI(file)
  mat2d <- data4dTo2d(nifti@.Data)
  buildBrcFmri(data2d=mat2d, dim3d=dim(nifti)[-4], id=id, partition=NULL)
}

data2dTo4d <- function(mat2d, parcellation) {
  part <- parcellation$partition
  expandedMatrix <- .expandMatrix(mat2d, part)
  array(data=t(expandedMatrix), dim=c(parcellation$dim3d, nrow(mat2d)))
}

data4dTo2d <- function(arr4d) {
  num3dVoxels <- Reduce("*", dim(arr4d)[-4])
  scanLength <- dim(arr4d)[4]
  t(matrix(data=arr4d, ncol=scanLength, nrow=num3dVoxels))
}

.expandMatrix <- function(mat2d, partition) {
  partition <- .factorToNumeric(partition)

  compressedIndices <- partition[partition != 0]
  expandedIndices <- (1:length(partition))[partition != 0]

  expandedMatrix <- matrix(data=0, nrow=nrow(mat2d), ncol=length(partition))
  expandedMatrix[ , expandedIndices] <- mat2d[ , compressedIndices]

  expandedMatrix
}

.factorToNumeric <- function(f) {
  as.numeric(levels(f))[f]
}

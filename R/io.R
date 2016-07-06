niftiToBrcFmri <- function(file, id=NULL) {
  if (is.null(id)) {
    id <- file
  }

  nifti <- oro.nifti::readNIfTI(file)
  mat2d <- data4dTo2d(nifti@.Data)
  buildBrcFmri(data=mat2d, dim3d=dim(nifti)[-4], id=id, partition=NULL)
}

data4dTo2d <- function(arr4d) {
  num3dVoxels <- Reduce("*", dim(arr4d)[-4])
  scanLength <- dim(arr4d)[4]
  t(matrix(data=arr4d, ncol=scanLength, nrow=num3dVoxels))
}

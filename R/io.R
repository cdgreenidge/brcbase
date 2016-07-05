data4dTo2d <- function(arr4d) {
  num3dVoxels <- Reduce("*", dim(arr4d)[-4])
  scanLength <- dim(arr4d)[4]
  t(matrix(data=arr4d, ncol=scanLength, nrow=num3dVoxels))
}

buildParcellation <- function(dim3d, userPartition) {
  partition <- buildPartition(dim3d, userPartition)
  BrcParcellation(dim3d, partition)
}

buildPartition <- function(dim3d, userPartition) {
  if (is.null(userPartition)) {
    num3dVoxels <- Reduce("*", dim3d)
    return(factor(1:num3dVoxels))
  } else {
    return(userPartition)
  }
}

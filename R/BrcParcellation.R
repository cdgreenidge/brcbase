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

isValid.BrcParcellation <- function(obj) {
  num3dVoxels <- Reduce("*", obj$dim3d)
  if (length(obj$partition) != num3dVoxels) {
    stop("Length of partition not equal to number of 3D voxels.")
  }
}

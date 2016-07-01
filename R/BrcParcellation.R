BrcParcellation <- function(dim3d, partition) {
  if (!is.numeric(dim3d)) {
    stop("dim3d argument must be a 3-element numeric vector")
  } else if (length(dim3d) != 3) {
    stop("dim3d argument must be a 3-element numeric vector")
  }

  structure(list(dim3d=dim3d, partition=partition), class="BrcParcellation")
}

dim3d.BrcParcellation <- function(obj) {
  obj$dim3d
}

isValid.BrcParcellation <- function(obj) {
  num3dVoxels <- Reduce("*", dim3d(obj))
  if (length(partition(obj)) != num3dVoxels) {
    stop("Length of partition not equal to number of 3D voxels.")
  }
}

partition.BrcParcellation <- function(obj) {
  obj$partition
}

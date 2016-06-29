BrcParcellation <- function(dim3d, partition=NULL) {
  if (!is.numeric(dim3d)) {
    stop("dim3d argument must be a 3-element numeric vector")
  } else if (length(dim3d) != 3) {
    stop("dim3d argument must be a 3-element numeric vector")
  }

  NUM_3D_VOXELS <- Reduce("*", dim3d)

  if (is.null(partition)) {
    partition <- factor(1:NUM_3D_VOXELS)
  }

  if (length(partition) != NUM_3D_VOXELS) {
    stop("partition factor length not equal to number of 3D voxels")
  }

  structure(list(dim3d=dim3d, partition=partition), class="BrcParcellation")
}

dim3d.BrcParcellation <- function(obj) obj$dim3d
partition.BrcParcellation <- function(obj) obj$partition

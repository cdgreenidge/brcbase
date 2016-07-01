BrcFmri <- function(data, parcellation) {
  if (!is.matrix(data)) {
    stop("data argument must be a matrix")
  }

  if (is.null(partition)) {
    num3dVoxels <- Reduce("*", dim3d)
    partition <- factor(1:num3dVoxels)
  }

  structure(list(data=data, parcellation=parcellation), class="BrcFmri")
}

data.BrcFmri <- function(obj) {
  obj$data
}

parcellation.BrcFmri <- function(obj) {
  obj$parcellation
}

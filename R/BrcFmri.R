BrcFmri <- function(data, parcellation) {
  if (!is.matrix(data)) {
    stop("data argument must be a matrix")
  } else if (class(parcellation) != "BrcParcellation") {
    stop("parcellation argument must be of class BrcParcellation")
  }

  structure(list(data=data, parcellation=parcellation), class="BrcFmri")
}

data.BrcFmri <- function(obj) {
  obj$data
}

isValid.BrcFmri <- function(obj) {
  num3dVoxels <- Reduce("*", dim3d(parcellation(obj)))
  if (ncol(data(obj)) != num3dVoxels) {
    stop(paste("Number of columns in data matrix does not equal number of 3D",
               "voxels specified in the parcellation. If you used the default",
               "partition, this could be caused by an incorrect dim3d argument",
               "passed to buildBrcFmri()."))

  }
  isValid(parcellation(obj))
}

parcellation.BrcFmri <- function(obj) {
  obj$parcellation
}

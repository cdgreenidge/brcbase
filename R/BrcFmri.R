BrcFmri <- function(data2d, id, parcellation) {
  if (!is.matrix(data2d)) {
    stop("data2d argument must be a matrix")
  } else if (class(parcellation) != "BrcParcellation") {
    stop("parcellation argument must be of class BrcParcellation")
  }

  structure(list(data2d=data2d, id=id, parcellation=parcellation),
            class="BrcFmri")
}

dim4d.BrcFmri <- function(obj) {
  c(dim3d(obj$parcellation), nrow(obj$data2d))
}

isValid.BrcFmri <- function(obj) {
  partition <- partition(obj$parcellation)
  num3dVoxels <- sum(levels(partition) > 0)
  if (ncol(obj$data2d) != num3dVoxels) {
    stop(paste("Number of columns in data matrix does not equal number of 3D",
               "voxels specified in the parcellation. If you used the default",
               "partition, this could be caused by an incorrect dim3d argument",
               "passed to buildBrcFmri()."))

  }
  isValid(obj$parcellation)
}

summary.BrcFmri <- function(object, ...) {
  dims <- dim4d(object)
  cat(sprintf("Id:                %s\n", id(object)))
  cat(sprintf("Volume resolution: %d x %d x %d voxels\n", dims[1], dims[2],
              dims[3]))
  cat(sprintf("Scan length:       %d volumes\n", dims[4]))
}

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

partition.BrcParcellation <- function(obj) {
  obj$partition
}

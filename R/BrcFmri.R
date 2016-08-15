#' BrcFmri constructor.
#'
#' \code{BrcFmri} makes a new \code{BrcFmri} instance.
#'
#' A BrcFmri dataype represents a functional magnetic resonance imaging (fMRI)
#' scan. It is a list with three named components:
#' \enumerate{
#'    \item \code{data2d}, the fMRI data matrix. Each column represents a voxel,
#'      or 3-dimensional pixel, in the fMRI. Each row represents a time step.
#'      Thus, one column of \code{data2d} is the time series for a single fMRI
#'      voxel, and one row of \code{data2d} is an fMRI image at one point in
#'      time.
#'    \item \code{id}, an identification string. It is not guaranteed to be
#'      unique or nonempty---those depend on how the \code{BrcFmri} object
#'      is constructed. Typically this field would be used to cross-reference
#'      the MRI with a dataframe containing phenotype information.
#'    \item \code{parcellation}, a \code{BrcParcellation} datatype. This
#'      contains the information necessary to transform the two-dimensional
#'      \code{data2d} matrix into the four-dimensional series of volumes useful
#'      for visualization.
#' }
#' Because \code{BrcFmri} is a dataype, not an object there are no accessor
#' functions. You can get its components directly with the \code{$} operator.'
#' 
#' If you are trying to make a new \code{BrcFmri} instance, the 
#' \code{buildBrcFmri} function will build the required 
#' \code{parcellation} argument for you and wire everything together. If you 
#' only have the four-dimensional fMRI data, then the \code{data4dTo2d}
#' function will transform it for you.
#' 
#' @param   data2d A matrix representing an fMRI
#' @param   id An identification string
#' @param   parcellation A \code{BrcParcellation} object
#' @return A new \code{BrcFmri} instance.
#' @seealso \code{\link{BrcParcellation}}, \code{\link{buildBrcFmri}},
#'  \code{\link{data4dTo2d}}
#' @export
BrcFmri <- function(data2d, id, parcellation) {
  if (!is.matrix(data2d)) {
    stop("data2d argument must be a matrix")
  } else if (class(parcellation) != "BrcParcellation") {
    stop("parcellation argument must be of class BrcParcellation")
  }

  obj <- structure(list(data2d=data2d, id=id, parcellation=parcellation),
            class="BrcFmri")
  isValid(obj)
  
  obj
}

#' 4D fMRI dimensions
#' 
#' \code{dim4d} gets the 4D dimensions of a \code{BrcFmri} object. 
#' 
#' The first three dimensions are the length, width, and height of each volume. 
#' The fourth dimension (time) is the scan length.
#' 
#' @param mri a \code{\link{BrcFmri}} instance
#' @return a 4-element numeric vector containing the \code{BrcFmri}'s
#'   dimensions.
#' @export
dim4d <- function(mri) {
  if (class(mri) != "BrcFmri") {
      stop("mri argument must be of class BrcFmri")
  }
  c(mri$parcellation$dim3d, nrow(mri$data2d))
}

#' Checking \code{BrcFmri} instance validity.
#' 
#' \code{isValid} method for class "\code{BrcFmri}".
#' 
#' Fails noisily with a stop message if the \code{BrcFmri}
#' instance is invalid. Otherwise, nothing happens.
#' 
#' @param  obj  The \code{BrcFmri} instance to check
#' @return void
#' @export
isValid.BrcFmri <- function(obj) {
  num3dVoxels <- numParcels(obj$parcellation)
  if (ncol(obj$data2d) != num3dVoxels) {
    stop(paste("Number of columns in data matrix does not equal number of 3D",
               "voxels specified in the parcellation. If you used the default",
               "partition, this could be caused by an incorrect dim3d argument",
               "passed to buildBrcFmri()."))

  }
  
  isValid(obj$parcellation)
}

#' Printing BrcFmri objects
#' 
#' \code{print} method for class "\code{BrcFmri}.
#'
#' @param x a BrcFmri instance
#' @param ... unused
#' @export
print.BrcFmri <- function(x, ...){
  cat("BrcFmri object of dimension", paste0(x$parcellation$dim3d, collapse = " x "),
      "\n with", ncol(x$data2d), "parcels and", nrow(x$data2d), "length\n")
  cat("----------\n\n")
  
  if(nrow(x$data2d) > 10 | ncol(x$data2d) > 10){
    cat("$data2d (Abridged)\n")
  } else {
    cat("$data2d\n")
  }
  print(utils::head(x$data2d[,1:min(10,ncol(x$data2d))]))
  
  cat("\n$id\n")
  print(x$id)
  
  cat("\n$parcellation (Object of class BrcParcellation)\n")
  cat("$$dim3d\n")
  print(x$parcellation$dim3d)
  
  if(length(x$parcellation$partition) > 10){
    cat("\n$$partition (Abridged)\n")
  } else {
    cat("\n$$partition\n")
  }
  print(x$parcellation$partition[1:min(10, length(x$parcellation$partition))])
 
  invisible() 
}

#' Summarizing BrcFmri objects
#' 
#' \code{summary} method for class "\code{BrcFmri}.
#' 
#' @param object a BrcFmri instance
#' @param ... unused
#' @export
summary.BrcFmri <- function(object, ...) {
  dims <- dim4d(object)
  cat(sprintf("Id:                      %s\n", object$id))
  cat(sprintf("Volume resolution:       %d x %d x %d voxels\n", dims[1], dims[2],
              dims[3]))
  cat(sprintf("Number of parcellations: %d parcels\n", ncol(object$data2d)))
  cat(sprintf("Scan length:             %d volumes\n", dims[4]))
  cat(sprintf("Estimate size:           %.2f Mb\n", utils::object.size(object)/1024^2,2))

  invisible()
}

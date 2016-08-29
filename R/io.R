#' Reads NIfTI fMRI data
#' 
#' \code{readNifti} reads a NIfTI formatted fMRI data file into a 
#' \code{BrcFmri} object.
#' 
#' @param file  the file path to read
#' @param id  the id for the the \code{BrcFmri} output. If left \code{NULL},
#'   the ID will be set as the file path.
#' @return a BrcFmri instance with the data from \code{file}
#' @export
readNifti <- function(file, id=NULL) {
  if (is.null(id)) {
    id <- file
  }

  nifti <- oro.nifti::readNIfTI(file)
  buildBrcFmri(nifti@.Data, id=id)
}
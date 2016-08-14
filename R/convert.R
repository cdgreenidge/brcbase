#' Convert a Voxel's Coordinates (3-dimensional) to Position Index
#'
#' vec represents the 3-dimensional coordinates of a voxel and
#' dim3d represents the total number of voxels in each direction (x,y,z 
#' direction) for the fmri. Converts vec into the numeric that represents
#' the position of vec in 3-dimensional array.
#'
#' @param dim3d a numeric vector of length 3
#' @param vec a numeric vector of length 3
#'
#' @return numeric representing vec's position in an array of dimension dim3d
#' @export
convertVoxel3DtoIndex <- function(dim3d, vec){
  if(!is.numeric(vec) | !is.numeric(dim3d)) 
    stop("dim3d and vec must be numerics")
  if(length(vec) != 3 | length(dim3d) != 3) 
    stop("dim3d and vec must be have length 3")
  if(!all(vec > 0) | !all(dim3d > 0)) 
    stop("dim3d and vec must be all positive values")
  if(!all(vec %% 1 == 0) | !all(dim3d %% 1 == 0)) 
    stop("dim3d and vec must have integer values")
  if(!all(vec <= dim3d))
    stop("values in vec must be smaller than or equal to dim3d elementwise")

  vec[1] + (vec[2]-1)*dim3d[1] + (vec[3]-1)*(dim3d[1]*dim3d[2])
}

#' Convert a Voxel's Position Index to Coordinates (3-dimensional) 
#'
#' idx represents the numeric position of a voxel and
#' dim3d represents the total number of voxels in each direction (x,y,z 
#' direction) for the fmri. Converts idx into the 3-dimensional
#' coordinates of the voxel in 3-dimensional array.
#' 
#' @param dim3d a numeric vector of length 3
#' @param idx a numeric
#'
#' @return numeric of length 3 representing the 3D coordinates
#' @export
convertVoxelIndexto3D <- function(dim3d, idx){
  if(!is.numeric(idx) | !is.numeric(dim3d)) 
    stop("dim3d and idx must be numerics")
  if(length(dim3d) != 3) stop("dim3d must have length 3")
  if(length(idx) != 1) stop("idx must have length 1")
  if(!all(idx > 0) | !all(dim3d > 0)) 
    stop("dim3d and idx must be all positive values")
  if(!all(idx %% 1 == 0) | !all(dim3d %% 1 == 0)) 
    stop("idx and vec must have integer values")
  if(idx > prod(dim3d)) stop("idx must be less than prod(dim3d)")
  
  prod.dim <- prod(dim3d[1:2])
  z <- ceiling(idx / prod.dim)
  
  tmp <- idx %% prod.dim; if(tmp == 0) tmp <- prod.dim
  y <- ceiling(tmp / dim3d[1])
  
  x <- tmp %% dim3d[1]; if(x == 0) x <- dim3d[1]
  
  c(x,y,z)
}
convertVoxel3Dto2D <- function(dim3d, vec){
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

convertVoxel2Dto3D <- function(dim3d, idx){
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
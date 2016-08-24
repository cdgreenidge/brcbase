checkMotion.array <- function(obj){
  dim.vec <- dim(obj)
  stopifnot(length(dim.vec) >= 3, length(dim.vec) <= 4)
  if(length(dim.vec) == 3) return(TRUE)
  
  idx <- which(obj[,,,1] != 0)
  data.slice <- apply(obj, c(1,2,3), function(x){
    if(any(x == 0)) 0 else 1
  })
  idx2 <- which(data.slice != 0)
  
  if(length(idx) != length(idx2)) {
    return(FALSE)
  } else {
   return(TRUE)
  }
}

checkMotion.nifti <- function(obj){
  checkMotion(obj@.Data)
}
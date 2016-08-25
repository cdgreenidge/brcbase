checkMotion.array <- function(obj){
  dim.vec <- dim(obj)
  stopifnot(length(dim.vec) >= 3, length(dim.vec) <= 4)
  if(length(dim.vec) == 3) return(TRUE)
  
  data.slice <- apply(obj, c(1,2,3), function(x){
    if(any(x == 0)){
      if(all(x == 0)) 0 else -1
    } else 1
  })

  if(any(data.slice == -1)) return(FALSE)
  idx <- which(obj[,,,1] != 0)
  idx2 <- which(data.slice == 1)
  
  if(length(idx) == length(idx2) && all(sort(idx) == sort(idx2))) {
    return(TRUE)
  } else {
   return(FALSE)
  }
}

checkMotion.nifti <- function(obj){
  checkMotion(obj@.Data)
}
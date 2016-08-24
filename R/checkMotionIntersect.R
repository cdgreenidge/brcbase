checkMotion.array <- function(data4d){
 dim.vec <- dim(data4d)
 stopifnot(length(dim.vec) >= 3, length(dim.vec) <= 4)
 if(length(dim.vec) == 3) return(TRUE)
 
 idx <- which(data4d[,,,1] != 0)
 data.slice <- apply(data4d, c(1,2,3), function(x){sum(abs(x))})
 idx2 <- which(data.slice != 0)
 
 if(length(idx) != length(idx2)) {
    return(FALSE)
 } else {
   return(TRUE)
 }
}
applyMask <- function(fmri, parcellation){
  if(class(fmri) != "BrcFmri") stop("fmri must be of class BrcFmri")
  if(class(parcellation) != "BrcParcellation") stop(paste("parcellation must",
    "be of class BrcParcellation"))
  if(!isValid(fmri)) stop("fmri must be a valid BrcFmri")
  if(!isValid(parcellation)) stop(paste("parcellation must be a valid",
    "BrcParcellation"))
  
  uniq.idx <- .findColumnIdx(fmri$parcellation, parcellation)
  mat <- .translateData2d(fmri$data2d, uniq.idx)
  partition <- .translatePartition(fmri$parcellation$partition, uniq.idx)
  
  new.parcellation <- BrcParcellation(parcellation$dim3d, partition)
  BrcFmri(data2d = mat, id = fmri$id, parcellation = new.parcellation)
}

.findColumnIdx <- function(parcellationBase, parcellationMask){
  voxel.base <- which(parcellationBase$partition != 0)
  voxel.mask <- which(parcellationMask$partition != 0)
  
  voxel.baseIdx <- which(voxel.base %in% voxel.mask)
 
  unique(parcellationBase$partition[voxel.base[voxel.baseIdx]])
}

.translateData2d <- function(data2d, idx){
  mat <- matrix(0, ncol = length(idx), nrow = nrow(data2d))
  mat[,1:length(idx)] <- data2d[,idx]
  
  mat
}

.translatePartition <- function(old.partition, idx){
  new.partition <- rep(0, length(old.partition))
  
  for(i in 1:length(idx)){
    loc <- which(old.partition == idx[i])
    new.partition[loc] <- i
  }
  
  new.partition
}
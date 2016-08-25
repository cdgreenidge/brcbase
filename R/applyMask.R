applyMask <- function(fmri, parcellation){
  if(class(fmri) != "BrcFmri") stop("fmri must be of class BrcFmri")
  if(class(parcellation) != "BrcParcellation") stop(paste("parcellation must",
    "be of class BrcParcellation"))
  if(!isValid(fmri)) stop("fmri must be a valid BrcFmri")
  if(!isValid(parcellation)) stop(paste("parcellation must be a valid",
    "BrcParcellation"))
  
  voxel.fmri <- which(fmri$parcellation$partition != 0)
  voxel.parcel <- which(parcellation$partition != 0)
  
  idx.in <- which(voxel.fmri %in% voxel.parcel)
  idx.location <- which(voxel.parcel %in% voxel.fmri)
  
  mat <- matrix(0, ncol = length(voxel.parcel), nrow = nrow(fmri$data2d))
  mat[,idx.location] <- fmri$data2d[,idx.in]
  
  new.partition <- rep(0, prod(fmri$parcellation$dim3d))
  new.partition[voxel.fmri[idx.in]] <- 1:length(idx.in)
  new.parcellation <- BrcParcellation(parcellation$dim3d, new.partition)
  
  BrcFmri(data2d = mat, id = fmri$id, parcellation = new.parcellation)
}
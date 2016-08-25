applyMask <- function(fmri, parcellation){
  if(class(fmri) != "BrcFmri") stop("fmri must be of class BrcFmri")
  if(class(parcellation) != "BrcParcellation") stop(paste("parcellation must",
    "be of class BrcParcellation"))
  if(!isValid(fmri)) stop("fmri must be a valid BrcFmri")
  if(!isValid(parcellation)) stop(paste("parcellation must be a valid",
    "BrcParcellation"))
  
  partitionTmp <- .translatePartition(fmri$parcellation$partition, 
    parcellation$partition)
 
  data2dNew <- .translateData2d(fmri$data2d, partitionTmp)
  
  partitionNew <- .reindexPartition(partitionTmp)
  
  parcellationNew <- BrcParcellation(parcellation$dim3d, partitionNew)
  BrcFmri(data2d = data2dNew, id = fmri$id, parcellation = parcellationNew)
}

.translatePartition <- function(partitionBase, partitionMask){
  voxel.base <- which(partitionBase != 0)
  voxel.zero <- which(partitionBase == 0)
  voxel.mask <- which(partitionMask != 0)

  voxel.baseIdx <- which(voxel.base %in% voxel.mask)
  voxel.zeroIdx <- which(voxel.zero %in% voxel.mask)
  
  partitionNew <- rep(0, length(partitionBase))
  
  partitionNew[voxel.base[voxel.baseIdx]] <- 
    partitionBase[voxel.base[voxel.baseIdx]]
  partitionNew[voxel.zero[voxel.zeroIdx]] <- -1
  
  partitionNew
}

.reindexPartition <- function(partition){
  idx <- which(partition > 0)
  uniq <- sort(unique(partition[idx]))
  
  for(i in 1:length(uniq)){
    partition[partition == uniq[i]] <- i
  }
  
  partition[partition == -1] <- length(uniq)+1
  
  partition
}

.translateData2d <- function(data2d, partitionNew){
  uniq <- unique(partitionNew[partitionNew != 0])
  numPar <- length(uniq)
  
  uniqIn <- uniq[uniq > 0]
  uniqIn <- sort(uniqIn)
  
  data2dNew <- matrix(0, ncol = numPar, nrow = nrow(data2d))
  if(length(uniqIn) > 0) data2dNew[,1:length(uniqIn)] <- data2d[,uniqIn]
  
  data2dNew
}
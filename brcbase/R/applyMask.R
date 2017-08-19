#' Applying a Mask (\code{BrcParcellation}) To \code{BrcFmri}
#' 
#' Given a \code{BrcFmri} instance (\code{fmri}) and \code{BrcParcellation}
#' instance (\code{parcellation}), \code{applyMask} intersects \code{fmri}
#' by \code{parcellation}. This means all the voxels in \code{parcellation}'s
#' that were 0 will be zero-ed out in \code{fmri} (i.e., the partition of
#' that voxel will be set to 0). If no elements of any partition originally 
#' in \code{fmri} exist anymore after this procedure, its corresponding 
#' column in \code{fmri} will be removed. On the flip side, if there were
#' voxel in \code{parcellation} that were non-zero, but its corresponding
#' voxel in \code{fmri} is empty, the voxel will be assigned an entirely
#' new parcellation in the output of \code{applyMask}, and its corresponding
#' column in \code{fmri$data2d} will be the column of all 0's. 
#' 
#' Here, \code{fmri} and \code{parcellation} must have the same size. 
#' 
#' We call this procedure applying a mask because this procedure does not
#' respect the partitioning in \code{parcellation}. It only takes into account
#' which voxels are zero and non-zero in \code{parcellation}. 
#'
#' @param fmri the \code{BrcFmri} instance
#' @param parcellation the \code{BrcParcellation} instance of the same 
#' dimension as \code{fmri}
#'
#' @return a new \code{BrcFmri} instance
#' @export
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
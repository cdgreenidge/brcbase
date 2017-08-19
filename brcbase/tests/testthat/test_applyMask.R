context("Test Apply Parcellation")

mat <- matrix(1:8, nrow=2, ncol=4)
dim3d <- c(2, 2, 2)
partition <- rep(1:4, each = 2)
parcellation <- BrcParcellation(dim3d, partition)
mri <- BrcFmri(data2d=mat, id="01", parcellation=parcellation)

parcellation2 <- BrcParcellation(dim3d, partition = c(0,0,0,0,1,1,1,1))
rm(list = c("mat", "dim3d", "partition", "parcellation"))

test_that("it errors if fmri is not a BrcFmri",{
  expect_error(applyMask(parcellation, parcellation2))
})

test_that("it errors if parcellation is not a BrcParcellation",{
  expect_error(applyMask(mri, parcellation2$partition))
})

test_that("it errors if fmri is not valid",{
  mri2 <- mri
  mri2$parcellation$dim3d <- c(2,2,1)
  expect_error(applyMask(mri2, parcellation2))
})

test_that("it properly counts when fmri is not a singleton parcellation",{
  res <- applyMask(mri, parcellation2)
  expect_true(numParcels(res$parcellation) == 2)
})

test_that("it properly moves when fmri is not a singleton parcellation",{
  res <- applyMask(mri, parcellation2)
  expect_true(ncol(res$data2d) == 2)
  expect_true(all(res$data2d[,1] == c(5,6)))
  expect_true(all(res$data2d[,2] == c(7,8))) 
})

test_that("it produces a 0 matrix if fmri and parcellation don't overlap",{
  mat <- matrix(1:4, nrow=2, ncol=2)
  dim3d <- c(2, 2, 2)
  partition <- c(1,1,2,2,0,0,0,0)
  parcellation <- BrcParcellation(dim3d, partition)
  mri2 <- BrcFmri(data2d=mat, id="01", parcellation=parcellation)  
  
  res <- applyMask(mri2, parcellation2)
  expect_true(ncol(res$data2d) == 1)
  expect_true(all(res$data2d[,1] == 0))
  expect_true(all(res$parcellation$partition == c(0,0,0,0,1,1,1,1)))
})

test_that("it adds zero-columns when appropriate", {
  mat <- matrix(1:4, nrow=2, ncol=2)
  dim3d <- c(2, 2, 2)
  partition <- c(0,0,1,1,2,2,0,0)
  parcellation <- BrcParcellation(dim3d, partition)
  mri2 <- BrcFmri(data2d=mat, id="01", parcellation=parcellation)  
  
  res <- applyMask(mri2, parcellation2)
  expect_true(ncol(res$data2d) == 2)
  expect_true(all(res$data2d[,1] == c(3,4)))
  expect_true(all(res$data2d[,2] == 0))
  expect_true(all(res$parcellation$partition == c(0,0,0,0,1,1,2,2)))
})

test_that("it rearranges the partitions in order",{
  mat <- matrix(1:12, nrow=2, ncol=6)
  dim3d <- c(2, 2, 2)
  partition <- c(0,0,1:6)
  parcellation <- BrcParcellation(dim3d, partition)
  mri2 <- BrcFmri(data2d=mat, id="01", parcellation=parcellation)  
  parcellation3 <- BrcParcellation(dim3d, partition = c(1,2,0,0,0,0,4,3))
  
  res <- applyMask(mri2, parcellation3)
  
  expect_true(ncol(res$data2d) == 3)
  expect_true(all(as.numeric(res$data2d) == c(9,10,11,12,0,0)))
  expect_true(all(res$parcellation$partition == c(3,3,0,0,0,0,1,2)))
})

test_that("it works when fmri and parcellation don't completely overlap",{
  mat <- matrix(1:8, nrow=2, ncol=4)
  dim3d <- c(2, 2, 2)
  partition <- c(1,0,1,2,0,3,4,2)
  parcellation <- BrcParcellation(dim3d, partition)
  mri2 <- BrcFmri(data2d=mat, id="01", parcellation=parcellation)  
  parcellation3 <- BrcParcellation(dim3d, partition = c(2,2,0,0,0,0,1,2))
  
  res <- applyMask(mri2, parcellation3)
  
  expect_true(ncol(res$data2d) == 4)
  expect_true(all(as.numeric(res$data2d) == c(1:4,7,8,0,0)))
  expect_true(all(res$parcellation$partition == c(1,4,0,0,0,0,3,2)))
})


########################

## test .translatePartition

test_that("it works normally when parcellations are the same",{
  par1 <- 1:8
  expect_true(all(.translatePartition(par1, par1) == par1))
})

test_that("it works normally for singleton parcellations, subset",{
  par1 <- 1:8
  par2 <- c(0,0,0,0,1,1,1,1)
  res <- .translatePartition(par1, par2)
  
  expect_true(all(res == c(0,0,0,0,5,6,7,8)))
})

test_that("it works normally when parcellations don't completely overlap",{
  par1 <- c(0,0,1,1,1,1,2,2)
  par2 <- c(0,0,0,0,1,1,1,1)
  res <- .translatePartition(par1, par2)
  
  expect_true(all(res == c(0,0,0,0,1,1,2,2)))
})

test_that("it works when there are missing parcels", {
  par1 <- c(0,0,0,0,1,1,2,2)
  par2 <- c(0,0,1,1,2,2,3,3)
  res <- .translatePartition(par1, par2)
  
  expect_true(all(res == c(0,0,-1,-1,1,1,2,2)))
})

test_that("it works when there is no overlap at all",{
  par1 <- c(0,0,0,0,1,1,1,1)
  par2 <- c(1,1,1,1,0,0,0,0)
  res <- .translatePartition(par1, par2)
  
  expect_true(all(res == c(-1,-1,-1,-1,0,0,0,0)))
})

test_that("index of parcellation are never 0",{
  set.seed(10)
  par1 <- sample(0:10, 100, replace = T)
  par2 <- sample(0:10, 100, replace = T)
  res <- .translatePartition(par1, par2)
  
  expect_true(length(res) == 100)
  expect_true(all(res[par2 != 0] != 0))
})

#########################

## test .translateData2d

test_that("it works on the identity",{
  mat <- matrix(1:8, ncol = 4, nrow = 2)
  partition <- c(4,4,2,2,3,3,1,1)
  res <- .translateData2d(mat, partition)
  
  expect_true(all(res == mat))
})

test_that("it works normally when partition has all positive",{
  mat <- matrix(1:8, ncol = 4, nrow = 2)
  partition <- c(0,0,0,0,3,3,4,4)
  res <- .translateData2d(mat, partition)
  
  expect_true(ncol(res) == 2)
  expect_true(all(as.numeric(res) == c(5,6,7,8)))
})

test_that("it works when there is no overlap", {
  mat <- matrix(1:8, ncol = 4, nrow = 2)
  partition <- c(0,0,0,0,-1)
  res <- .translateData2d(mat, partition)
  
  expect_true(ncol(res) == 1)
  expect_true(all(res == 0))
})

test_that("it works when there are positive and negative vals in parcellation",{
  mat <- matrix(1:8, ncol = 4, nrow = 2)
  partition <- c(0,0,4,2,-1,3)
  res <- .translateData2d(mat, partition)
  
  expect_true(ncol(res) == 4)
  expect_true(all(as.numeric(res) == c(3,4,5,6,7,8,0,0)))
})

#####################

## test .reindexPartition

test_that("it works on the identity (nothing to do)",{
  partition <- c(1:8)
  expect_true(all(.reindexPartition(partition) == partition))
})

test_that("it works on the empty vector with only -1", {
  partition <- c(0,-1)
  expect_true(all(.reindexPartition(partition) == c(0,1)))
})

test_that("it works when there are -1 involved",{
  partition <- c(0,0,0,1:5,-1)
  expect_true(all(.reindexPartition(partition) == c(0,0,0,1:6)))
})

test_that("it works when the partition does not start from 0",{
  partition <- c(0,4,3,9,-1)
  expect_true(all(.reindexPartition(partition) == c(0,2,1,3,4)))
})

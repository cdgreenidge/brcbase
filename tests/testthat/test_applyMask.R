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

#########################

## test .translateData2d

test_that("it works on the identity",{
  mat <- matrix(1:8, ncol = 4, nrow = 2)
  par <- c(4,4,2,2,3,3,1,1)
  res <- .translateData2d(mat, par)
  
  expect_true(all(res == mat))
})

test_that("it works normally when partition has all positive",{
  mat <- matrix(1:8, ncol = 4, nrow = 2)
  par <- c(0,0,0,0,3,3,4,4)
  res <- .translateData2d(mat, par)
  
  expect_true(ncol(res) == 2)
  expect_true(all(as.numeric(res) == c(5,6,7,8)))
})
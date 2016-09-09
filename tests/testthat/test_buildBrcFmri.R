context("Test buildBrcFmri.R")

mat <- array(1:2^4, rep(2,4))

# Test buildBrcFmri()

test_that("buildBrcFmri builds an fMRI object", {
  mri <- buildBrcFmri(data4d=mat, id="")
  expect_equal(class(mri), "BrcFmri")
})

test_that("it errors when given not a 4D array", {
  mat <- matrix(0, 5, 5)
  expect_error(buildBrcFmri(mat))
})

test_that("it errors when array has more than 4 dimensions", {
  mat <- array(0, c(2,5))
  expect_error(buildBrcFmri(mat))
})

test_that("it errors if there is motion detected",{
  mat <- array(0, rep(3,4))
  mat[1,1,1,] <- 1
  mat[2,2,2,] <- 2
  mat[3,3,3,] <- 3
  mat[1,2,3,3] <- -1
  
  expect_error(buildBrcFmri(mat))
})

##########################

# Test .buildParcellation()

test_that(".buildSingletonParcellation builds a singleton parcellation", {
  parcellation <- .buildSingletonParcellation(dim3d=c(2, 2, 2), idx = 1:8)
  expect_equal(class(parcellation), "BrcParcellation")
  expect_equal(parcellation$dim3d, c(2, 2, 2))
  expect_equal(parcellation$partition, 1:8)
})

########################

# Test .buildSingletonPartition()

test_that(".buildSingletonPartition puts each voxel in its own cell", {
  expect_equal(.buildSingletonPartition(c(2,2,2), 1:8), 1:8)
})

########################

## test buildBrcParcellation

test_that("it errors when given not a 3D array", {
  mat <- matrix(0, 5, 5)
  expect_error(buildBrcParcellation(mat))
})

test_that("it errors when array has more than 3 dimensions", {
  mat <- array(0, c(2,4))
  expect_error(buildBrcParcellation(mat))
})

test_that("it returns the correct parcellation", {
  mat <- array(0, rep(3,3))
  mat[c(1,5,10)] <- 1
  mat[c(2,27)] <- 2
  
  parcellation <- buildBrcParcellation(mat)
  expect_true(class(parcellation) == "BrcParcellation")
  expect_true(all(parcellation$dim3d == 3))
  expect_true(numParcels(parcellation) == 2)
  
  vec <- rep(0, 27)
  vec[c(1,5,10)] <- 1
  vec[c(2,27)] <- 2
  expect_true(all(parcellation$partition == vec))
})

test_that("it returns correct parcellation when for rectangular parcels", {
  mat <- array(1, rep(5,3))
  mat[2:4,2:4,2:4] <- 2
  mat[3,3,3] <- 3
  
  parcellation <- buildBrcParcellation(mat)
  expect_true(class(parcellation) == "BrcParcellation")
  expect_true(all(parcellation$dim3d == 5))
  expect_true(numParcels(parcellation) == 3)
  
  expect_true(length(which(parcellation$partition == 1)) == 125-27)
  expect_true(length(which(parcellation$partition == 2)) == 26)
  expect_true(length(which(parcellation$partition == 3)) == 1)
})


test_that("input data3d can have non-consecutive integers", {
  mat <- array(1, rep(5,3))
  mat[2:4,2:4,2:4] <- 5
  mat[3,3,3] <- 10
  
  parcellation <- buildBrcParcellation(mat)
  expect_true(class(parcellation) == "BrcParcellation")
  expect_true(all(parcellation$dim3d == 5))
  expect_true(numParcels(parcellation) == 3)
  
  expect_true(length(which(parcellation$partition == 1)) == 125-27)
  expect_true(length(which(parcellation$partition == 2)) == 26)
  expect_true(length(which(parcellation$partition == 3)) == 1)
})


context("Test convert.R")

## Test convertVoxel3Dto2D()

test_that("it returns the first idx correctly",{
  dim3d <- c(5,5,5)
  expect_true(convertVoxel3Dto2D(dim3d, c(1,1,1)) == 1)
})

test_that("it returns the last idx correctly",{
  dim3d <- c(5,5,5)
  expect_true(convertVoxel3Dto2D(dim3d, dim3d) == prod(dim3d))
})

test_that("it returns the middle idx correctly",{
  dim3d <- c(5,5,5)
  expect_true(convertVoxel3Dto2D(dim3d, c(3,3,3)) == (1+prod(dim3d))/2)
})


###############

## Test convertVoxel2to3D()

test_that("it returns the first coordinate correctly", {
  dim3d <- c(5,5,5)
  expect_true(all(convertVoxel2Dto3D(dim3d, 1) == c(1,1,1)))
})

test_that("it returns the last coordinate correctly", {
  dim3d <- c(5,5,5)
  expect_true(all(convertVoxel2Dto3D(dim3d, prod(dim3d)) == dim3d))
})

test_that("it returns the correct index, mass test", {
  dim3d <- rep(4,3)
  idx.vec <- 1:(4^3)
  arr <- array(idx.vec, dim = dim3d)
  res <- sapply(idx.vec, convertVoxel2Dto3D, dim3d = dim3d)
  for(i in 1:nrow(res)){
    expect_true(idx.vec[i] == arr[res[1,i], res[2,i], res[3,i]])
  }
})
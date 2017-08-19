context("Test convert.R")

## Test voxel3DToIdx()

test_that("it errors on non-numeric",{
  expect_error(voxel3DToIdx("a",c(1,1,1)))
  expect_error(voxel3DToIdx(c(1,1,1)), "a")
})

test_that("it errors when not length 3", {
  expect_error(voxel3DToIdx(rep(5,3),rep(4,2)))
  expect_error(voxel3DToIdx(rep(4,2),rep(2,3)))
})

test_that("it errors on negative values", {
  expect_error(voxel3DToIdx(rep(5,3),c(-1,3,3)))
  expect_error(voxel3DToIdx(c(-1,5,-5),c(1,3,3)))
})

test_that("it errors on non-integer", {
  expect_error(voxel3DToIdx(c(5,4.2,5), c(3,3,3)))
  expect_error(voxel3DToIdx(c(5,5,5), c(3,3.2,3)))
})

test_that("it errors when prod(vec) is larger than prod(dim3d)", {
  expect_error(voxel3DToIdx(c(5,5,5), c(5,5,6)))
})

test_that("it returns the first idx correctly",{
  dim3d <- c(5,5,5)
  expect_true(voxel3DToIdx(dim3d, c(1,1,1)) == 1)
})

test_that("it returns the last idx correctly",{
  dim3d <- c(5,5,5)
  expect_true(voxel3DToIdx(dim3d, dim3d) == prod(dim3d))
})

test_that("it returns the middle idx correctly",{
  dim3d <- c(5,5,5)
  expect_true(voxel3DToIdx(dim3d, c(3,3,3)) == (1+prod(dim3d))/2)
})


###############

## Test convertVoxel2to3D()

test_that("it errors on non-numeric",{
  expect_error(voxelIdxTo3D("a",1))
  expect_error(voxelIdxTo3D(1, "a"))
})

test_that("it errors when dim3d not length 3", {
  expect_error(voxelIdxTo3D(rep(5,2),5))
})

test_that("it errors when idx not length 1", {
  expect_error(voxelIdxTo3D(rep(5,3),c(1,1)))
})

test_that("it errors on negative values", {
  expect_error(voxelIdxTo3D(rep(5,3), -3))
  expect_error(voxelIdxTo3D(c(-1,5,-5), 15))
})

test_that("it errors on non-integer", {
  expect_error(voxelIdxTo3D(c(5,4.2,5), 15))
  expect_error(voxelIdxTo3D(c(5,5,5), 2.3))
})

test_that("it errors when idx is larger than prod(dim3d)", {
  expect_error(voxelIdxTo3D(c(5,5,5), 5*5*6))
})

test_that("it returns the first coordinate correctly", {
  dim3d <- c(5,5,5)
  expect_true(all(voxelIdxTo3D(dim3d, 1) == c(1,1,1)))
})

test_that("it returns the last coordinate correctly", {
  dim3d <- c(5,5,5)
  expect_true(all(voxelIdxTo3D(dim3d, prod(dim3d)) == dim3d))
})

test_that("it returns the correct index, mass test", {
  dim3d <- rep(4,3)
  idx.vec <- 1:(4^3)
  arr <- array(idx.vec, dim = dim3d)
  res <- sapply(idx.vec, voxelIdxTo3D, dim3d = dim3d)
  for(i in 1:nrow(res)){
    expect_true(idx.vec[i] == arr[res[1,i], res[2,i], res[3,i]])
  }
})

###########################3

## test data conversions


test_that("you can convert a 2D data matrix to a 4D data matrix", {
  mat2d <- matrix(data=c(1, 2, 3, 4, 5, 6, 7, 8), nrow=2, ncol=4)
  partition <- c(0, 1, 2, 2, 3, 3, 4, 0)
  parcellation <- BrcParcellation(c(2, 2, 2), partition)
  arr4d <- array(c(0, 1, 3, 3, 5, 5, 7, 0, 0, 2, 4, 4, 6, 6, 8, 0),
                 dim=c(2, 2, 2, 2))
  expect_equal(data2dTo4d(mat2d, parcellation), arr4d)
})

test_that("you can convert a 4D data matrix to a 2D data matrix", {
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
  arr4d <- array(data=data, dim=c(2, 2, 2, 2))
  mat2d <- t(matrix(data=data, nrow=8, ncol=2))
  expect_equal(data4dTo2d(arr4d), mat2d)
})

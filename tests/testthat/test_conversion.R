context("Test convert.R")

## Test convertVoxel3DtoIndex()

test_that("it errors on non-numeric",{
  expect_error(convertVoxel3DtoIndex("a",c(1,1,1)))
  expect_error(convertVoxel3DtoIndex(c(1,1,1)), "a")
})

test_that("it errors when not length 3", {
  expect_error(convertVoxel3DtoIndex(rep(5,3),rep(4,2)))
  expect_error(convertVoxel3DtoIndex(rep(4,2),rep(2,3)))
})

test_that("it errors on negative values", {
  expect_error(convertVoxel3DtoIndex(rep(5,3),c(-1,3,3)))
  expect_error(convertVoxel3DtoIndex(c(-1,5,-5),c(1,3,3)))
})

test_that("it errors on non-integer", {
  expect_error(convertVoxel3DtoIndex(c(5,4.2,5), c(3,3,3)))
  expect_error(convertVoxel3DtoIndex(c(5,5,5), c(3,3.2,3)))
})

test_that("it errors when prod(vec) is larger than prod(dim3d)", {
  expect_error(convertVoxel3DtoIndex(c(5,5,5), c(5,5,6)))
})

test_that("it returns the first idx correctly",{
  dim3d <- c(5,5,5)
  expect_true(convertVoxel3DtoIndex(dim3d, c(1,1,1)) == 1)
})

test_that("it returns the last idx correctly",{
  dim3d <- c(5,5,5)
  expect_true(convertVoxel3DtoIndex(dim3d, dim3d) == prod(dim3d))
})

test_that("it returns the middle idx correctly",{
  dim3d <- c(5,5,5)
  expect_true(convertVoxel3DtoIndex(dim3d, c(3,3,3)) == (1+prod(dim3d))/2)
})


###############

## Test convertVoxel2to3D()

test_that("it errors on non-numeric",{
  expect_error(convertVoxelIndexto3D("a",1))
  expect_error(convertVoxelIndexto3D(1, "a"))
})

test_that("it errors when dim3d not length 3", {
  expect_error(convertVoxelIndexto3D(rep(5,2),5))
})

test_that("it errors when idx not length 1", {
  expect_error(convertVoxelIndexto3D(rep(5,3),c(1,1)))
})

test_that("it errors on negative values", {
  expect_error(convertVoxelIndexto3D(rep(5,3), -3))
  expect_error(convertVoxelIndexto3D(c(-1,5,-5), 15))
})

test_that("it errors on non-integer", {
  expect_error(convertVoxelIndexto3D(c(5,4.2,5), 15))
  expect_error(convertVoxelIndexto3D(c(5,5,5), 2.3))
})

test_that("it errors when idx is larger than prod(dim3d)", {
  expect_error(convertVoxelIndexto3D(c(5,5,5), 5*5*6))
})

test_that("it returns the first coordinate correctly", {
  dim3d <- c(5,5,5)
  expect_true(all(convertVoxelIndexto3D(dim3d, 1) == c(1,1,1)))
})

test_that("it returns the last coordinate correctly", {
  dim3d <- c(5,5,5)
  expect_true(all(convertVoxelIndexto3D(dim3d, prod(dim3d)) == dim3d))
})

test_that("it returns the correct index, mass test", {
  dim3d <- rep(4,3)
  idx.vec <- 1:(4^3)
  arr <- array(idx.vec, dim = dim3d)
  res <- sapply(idx.vec, convertVoxelIndexto3D, dim3d = dim3d)
  for(i in 1:nrow(res)){
    expect_true(idx.vec[i] == arr[res[1,i], res[2,i], res[3,i]])
  }
})
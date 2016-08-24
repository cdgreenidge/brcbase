context("Test Motion and Intersect")

## test checkMotion.array

test_that("it errors if not dimension 3 or 4", {
  mat <- array(1,rep(2,5))
  expect_error(checkMotion(mat))
})

test_that("it returns false if there is motion", {
  mat <- array(0, rep(3,4))
  mat[1,1,1,1] <- 1
  mat[1,1,2,3] <- 1
  expect_true(!checkMotion(mat))
})

test_that("it returns true for dimension 3", {
  mat <- array(0, rep(3,3))
  expect_true(checkMotion(mat))
})

####################

## test checkMotion.nifti

test_that("it works for oro.nifti objects", {
  path <- "../assets/5002_ABIDE_segment.nii.gz"
  nifti <- oro.nifti::readNIfTI(path)
  expect_true(checkMotion(nifti))
})
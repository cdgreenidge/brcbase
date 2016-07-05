context("Test io.R")

test_that("you can construct a BrcFmri object from a NIFTI file", {
  path <- "../assets/5002_ABIDE_segment.nii.gz"
  mri <- niftiToBrcFmri(file=path)
  nifti <- oro.nifti::readNIfTI(path)
  expect_equal(dim4d(mri), c(4, 5, 4, 3))
  expect_equal(data(mri), data4dTo2d(nifti@.Data))
})

test_that("you can convert a 4D data matrix to a 2D data matrix", {
    data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
    arr4d <- array(data=data, dim=c(2, 2, 2, 2))
    mat2d <- t(matrix(data=data, nrow=8, ncol=2))
    expect_equal(data4dTo2d(arr4d), mat2d)
})

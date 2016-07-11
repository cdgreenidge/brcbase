context("Test io.R")

test_that("you can construct a BrcFmri object from a NIFTI file", {
  path <- "../assets/5002_ABIDE_segment.nii.gz"
  mri <- readNifti(file=path, id="5002_ABIDE")
  nifti <- oro.nifti::readNIfTI(path)
  expect_equal(dim4d(mri), c(4, 5, 4, 3))
  expect_equal(mri$data2d, data4dTo2d(nifti@.Data))
  expect_equal(mri$id, "5002_ABIDE")
})

test_that("the default ID is the file path", {
  path <- "../assets/5002_ABIDE_segment.nii.gz"
  mri <- readNifti(file=path)
  expect_equal(mri$id, path)
})

test_that("youc an convert a 2D data matrix to a 4D data matrix", {
  mat2d <- matrix(data=c(1, 2, 3, 4, 5, 6, 7, 8), nrow=2, ncol=4)
  partition <- factor(c(0, 1, 2, 2, 3, 3, 4, 0))
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

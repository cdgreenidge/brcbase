context("Test io.R")

test_that("you can construct a BrcFmri object from a NIFTI file", {
  path <- "../assets/5002_ABIDE_segment.nii.gz"
  mri <- readNifti(file=path, id="5002_ABIDE")
  nifti <- oro.nifti::readNIfTI(path)
  expect_equal(dim4d(mri), c(4, 5, 4, 3))
  
  idx <- which(mri$parcellation$partition != 0)
  expect_equal(mri$data2d, data4dTo2d(nifti@.Data)[,idx])
  expect_equal(mri$id, "5002_ABIDE")
})

test_that("the default ID is the file path", {
  path <- "../assets/5002_ABIDE_segment.nii.gz"
  mri <- readNifti(file=path)
  expect_equal(mri$id, path)
})

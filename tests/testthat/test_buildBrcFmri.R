context("Test buildBrcFmri.R")

mat <- array(1:2^4, rep(2,4))

# Test buildBrcFmri()

test_that("buildBrcFmri builds an fMRI object", {
  mri <- buildBrcFmri(data4d=mat, id="")
  expect_equal(class(mri), "BrcFmri")
})

# Test .buildParcellation()

test_that(".buildParcellation builds a parcellation", {
  parcellation <- .buildParcellation(dim3d=c(2, 2, 2), idx = 1:8)
  expect_equal(class(parcellation), "BrcParcellation")
  expect_equal(parcellation$dim3d, c(2, 2, 2))
  expect_equal(parcellation$partition, 1:8)
})


# Test .buildPartition()

test_that(".buildPartition puts each voxel in its own cell if partition=NULL", {
  expect_equal(.buildPartition(c(2,2,2), 1:8), 1:8)
})

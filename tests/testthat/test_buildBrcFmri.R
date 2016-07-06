context("Test buildBrcFmri.R")

mat <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
              nrow=2, ncol=8)
dim3d <- c(2, 2, 2)

# Test buildBrcFmri()

test_that("buildBrcFmri builds an fMRI object", {
  mri <- buildBrcFmri(data2d=mat, dim3d=dim3d, id="", partition=NULL)
  expect_equal(class(mri), "BrcFmri")
})

test_that("buildBrcFmri checks the MRI object for validity", {
  expect_error(buildBrcFmri(data2d=mat, dim3d=c(2, 1, 2), partition=NULL),
               "columns")
})

# Test buildParcellation()

test_that("buildParcellation builds a parcellation", {
  parcellation <- buildParcellation(dim3d=c(2, 2, 2), userPartition=NULL)
  expect_equal(class(parcellation), "BrcParcellation")
  expect_equal(dim3d(parcellation), c(2, 2, 2))
  expect_equal(partition(parcellation), factor(1:8))
})

test_that("buildParcellation uses the user-supplied partition if it exists", {
  userPartition <- factor(c(1, 1, 2, 2, 3, 3, 3, 4))
  parcellation <- buildParcellation(dim3d=c(2, 2, 2),
                                    userPartition=userPartition)
  expect_equal(class(parcellation), "BrcParcellation")
  expect_equal(dim3d(parcellation), c(2, 2, 2))
  expect_equal(partition(parcellation), userPartition)
})

# Test buildPartition()

test_that("buildPartition uses the user-supplied partition if it exists", {
  userPartition <- factor(c(1, 1, 2, 2, 3, 3, 4, 4))
  expect_equal(buildPartition(dim3d=dim3d, userPartition), userPartition)
})

test_that("buildPartition puts each voxel in its own cell if partition=NULL", {
  userPartition <- NULL
  expect_equal(buildPartition(dim3d=dim3d, userPartition), factor(1:8))
})

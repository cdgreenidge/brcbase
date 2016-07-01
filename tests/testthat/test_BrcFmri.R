context("Test BrcFmri.R")

mat <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
              nrow=2, ncol=8)
dim3d <- c(2, 2, 2)
partition <- factor(1:8)
parcellation <- BrcParcellation(dim3d, partition)

test_that("the constructor returns an object of class BrcFmri", {
  mri <- BrcFmri(data=mat, parcellation=parcellation)
  expect_equal(class(mri), "BrcFmri")
})

test_that("it contains a 2D data matrix", {
  mri <- BrcFmri(data=mat, parcellation=parcellation)
  expect_equal(data(mri), mat)
})

test_that("data must be a matrix", {
  expect_error(BrcFmri(data=numeric(), parcellation=parcellation), "data")
})

test_that("it contains a parcellation", {
  mri <- BrcFmri(data=mat, parcellation=parcellation)
  expect_equal(parcellation(mri), parcellation)
})

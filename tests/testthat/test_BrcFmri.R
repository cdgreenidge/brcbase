context("Test BrcFmri.R")

mat <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
              nrow=2, ncol=8)
dim3d <- c(2, 2, 2)
partition <- factor(1:8)
parcellation <- BrcParcellation(dim3d, partition)

test_that("the constructor returns an object of class BrcFmri", {
  mri <- BrcFmri(data=mat, id="01", parcellation=parcellation)
  expect_equal(class(mri), "BrcFmri")
})

test_that("it contains a 2D data matrix", {
  mri <- BrcFmri(data=mat, id="01", parcellation=parcellation)
  expect_equal(data(mri), mat)
})

test_that("data must be a matrix", {
  expect_error(BrcFmri(data=numeric(), parcellation=parcellation), "data")
})

test_that("it contains a parcellation", {
  mri <- BrcFmri(data=mat, id="01", parcellation=parcellation)
  expect_equal(parcellation(mri), parcellation)
})

test_that("parcellation must be of class BrcParcellation", {
  expect_error(BrcFmri(data=mat, parcellation=character()), "parcellation")
})

test_that("it contains an ID string", {
  mri <- BrcFmri(data=mat, id="ABIDE_5002", parcellation=parcellation)
  expect_equal(id(mri), "ABIDE_5002")
})

test_that("it knows how to check itself for validity", {
  dim3d <- c(2, 1, 2)
  partition <- factor(1:4)
  parcellation <- BrcParcellation(dim3d, partition)
  mri <- BrcFmri(data=mat, id="01", parcellation=parcellation)
  expect_error(isValid(mri), "columns")
})

test_that("it also checks the parcellation it contains for validity", {
  dim3d <- c(2, 2, 2)
  partition <- factor(1:4)
  parcellation <- BrcParcellation(dim3d, partition)
  mri <- BrcFmri(data=mat, id="01", parcellation=parcellation)
  expect_error(isValid(mri), "partition")
})

test_that("it can tell you its 4D dimensions", {
  mri <- BrcFmri(data=mat, id="01", parcellation=parcellation)
  expect_equal(dim4d(mri), c(2, 2, 2, 2))
})

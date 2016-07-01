context("Test BrcParcellation.R")

test_that("the constructor returns an object of class BrcParcellation", {
  parcellation <- BrcParcellation(dim3d=c(2, 2, 2), factor(1:8))
  expect_equal(class(parcellation), "BrcParcellation")
})

test_that("it contains a 3-element dimension vector", {
  parcellation <- BrcParcellation(dim3d=c(2, 2, 2), factor(1:8))
  expect_equal(dim3d(parcellation), c(2, 2, 2))
})

test_that("dim3d must be a numeric vector", {
  expect_error(BrcParcellation(dim3d="hi"), "dim3d")
})

test_that("dim3d must be 3 elements long", {
  expect_error(BrcParcellation(dim3d=c(1, 2)), "dim3d")
})

test_that("it contains a partition factor", {
  part <- factor(1:8)
  parcellation <- BrcParcellation(dim3d=c(2, 2, 2), partition=part)
  expect_equal(partition(parcellation), part)
})

test_that("it knows how to check itself for validity", {
  part <- factor(1:4)
  parcellation <- BrcParcellation(dim3d=c(2, 2, 2), partition=part)
  expect_error(isValid(parcellation), "number of 3D voxels")
})

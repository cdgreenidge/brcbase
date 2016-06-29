context("Test BrcParcellation.R")

test_that("it contains a 3-element dimension vector", {
  parcellation <- BrcParcellation(dim3d=c(2, 2, 2))
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

test_that("the default partition puts every voxel in its own parcel", {
  parcellation <- BrcParcellation(dim3d=c(2, 2, 2))
  expect_equal(partition(parcellation), factor(c(1, 2, 3, 4, 5, 6, 7, 8)))
})

test_that("the partition must contain an entry for each 3D voxel", {
  expect_error(BrcParcellation(dim3d=c(2, 2, 2), partition=c(1, 2)),
               "partition")
})

context("Test BrcParcellation.R")

test_that("the constructor returns an object of class BrcParcellation", {
  parcellation <- BrcParcellation(dim3d=c(2, 2, 2), factor(1:8))
  expect_equal(class(parcellation), "BrcParcellation")
})

test_that("it contains a 3-element dimension vector", {
  parcellation <- BrcParcellation(dim3d=c(2, 2, 2), factor(1:8))
  expect_equal(parcellation$dim3d, c(2, 2, 2))
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
  expect_equal(parcellation$partition, part)
})

test_that("the partition must be a factor", {
  expect_error(BrcParcellation(dim3d=c(2, 2, 2), partition=character()),
               "factor")
})

test_that("the partition must have levels >= 0", {
  part <- factor(-1:6)
  expect_error(BrcParcellation(dim3d=c(2, 2, 2), partition=part), "factor")
})

test_that("it knows how to check itself for validity", {
  part <- factor(1:4)
  parcellation <- BrcParcellation(dim3d=c(2, 2, 2), partition=part)
  expect_error(isValid(parcellation), "number of 3D voxels")
})


######

## test .isValid_partition

test_that("it errors when there are negative values", {
  expect_error(.isValid_partition(-1:5))
})

test_that("it errors when the values are not consecutive", {
  expect_error(.isValid_partition(c(1:10,15)))
})

test_that("it errors when the values are decimal", {
  expect_error(.isValid_partition(1.1))
  expect_error(.isValid_partition(c(1:10)+.1))
})

test_that("it errors when all the values are 0", {
  expect_error(.isValid_partition(0))
  expect_error(.isValid_partition(rep(0,10)))
})

test_that("it works as expected on valid partitions", {
  expect_true(.isValid_partition(1:100))
  expect_true(.isValid_partition(rbinom(100,1,0.5)))
  
  vec <- rbinom(100,1,0.5)
  partition <- rep(0, length(vec))
  idx <- which(vec != 0)
  partition[idx] = 1:length(idx)
  expect_true(.isValid_partition(partition))
})
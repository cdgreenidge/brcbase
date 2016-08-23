context("Test BrcParcellation.R")

test_that("the constructor returns an object of class BrcParcellation", {
  parcellation <- BrcParcellation(dim3d=c(2, 2, 2), 1:8)
  expect_equal(class(parcellation), "BrcParcellation")
})

test_that("it contains a 3-element dimension vector", {
  parcellation <- BrcParcellation(dim3d=c(2, 2, 2), 1:8)
  expect_equal(parcellation$dim3d, c(2, 2, 2))
})

test_that("dim3d must be a numeric vector", {
  expect_error(BrcParcellation(dim3d="hi", parcellation = 0))
})

test_that("dim3d must be 3 elements long", {
  expect_error(BrcParcellation(dim3d=c(1, 2)))
})

test_that("it contains a partition vector", {
  part <- 1:8
  parcellation <- BrcParcellation(dim3d=c(2, 2, 2), partition=part)
  expect_equal(parcellation$partition, part)
})

test_that("the partition must be a vector", {
  expect_error(BrcParcellation(dim3d=c(2, 2, 2), partition=character()))
})

test_that("the partition must be consecutive from 1", {
  part <- 2:9
  expect_error(BrcParcellation(dim3d=c(2, 2, 2), partition=part))
})

test_that("the partition can start from 0", {
  part <- 0:7
  parcellation <- BrcParcellation(dim3d=c(2, 2, 2), partition=part)
  expect_true(isValid(parcellation))
})


test_that("the partition must have levels >= 0", {
  part <- -1:6
  expect_error(BrcParcellation(dim3d=c(2, 2, 2), partition=part))
})

test_that("it knows how to check itself for validity", {
  part <- 1:4
  expect_error(isValid(BrcParcellation(dim3d=c(2, 2, 2), partition=part)))
})

test_that("it errors on empty parcellations", {
  expect_error(BrcParcellation(c(2,2,2),rep(0,8)))
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

#####################

## test numParcels()

test_that("numParcels errors when given not a BrcParcellation",{
  expect_error(numParcels(1:10))
})

test_that("numParcels counts correctly for singletons", {
  parcellation <- brcbase::BrcParcellation(c(2,2,2),1:8)
  expect_true(numParcels(parcellation) == 8)
})

test_that("numParcels counts correctly for non-singletons",{
  parcellation <- brcbase::BrcParcellation(c(2,2,2),rep(1:4,each=2))
  expect_true(numParcels(parcellation) == 4)
})

test_that("numParcel counts correctly when some voxels are empty",{
  parcellation <- brcbase::BrcParcellation(c(2,2,2),c(0,rep(1:3,each=2),0))
  expect_true(numParcels(parcellation) == 3)
})

#################

## test print and summary

test_that("BrcParcellation has a meaningful print statement", {
  parcellation <- BrcParcellation(dim3d=c(2, 2, 2), 1:8)
  res <- paste0(capture.output(print(parcellation)), collapse = " ")
  expect_true(grep("BrcParcellation", res) == 1)
})

test_that("BrcParcellation has a meaningful summary statement", {
  parcellation <- BrcParcellation(dim3d=c(2, 2, 2), 1:8)
  res <- paste0(capture.output(summary(parcellation)), collapse = " ")
  expect_true(grep("BrcParcellation", res) == 1)
})

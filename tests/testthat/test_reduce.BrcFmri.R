context("Test reduce.BrcFmri.R")

## test reduce_mean()

test_that("it computes the correct mean", {
  mat <- matrix(rnorm(200), ncol = 20, nrow = 10)
  mat <- t(scale(mat))
  res <- reduce_mean(mat)
  expect_true(sum(abs(res)) < 1e-5)
  expect_true(length(res) == 20)
})

test_that("it errors when passed in nothing", {
  expect_error(reduce_mean(NULL))
  expect_error(reduce_mean(NA))
  expect_error(reduce_mean())
})

test_that("it can handle matrix with no columns", {
  mat <- matrix(0, 10, 10)
  res <- reduce_mean(mat[,numeric(0)])
  expect_true(length(res) == 10)
  expect_true(all(res == 0))
})

## test reduce_pca()

test_that("it computes the right pca", {
  set.seed(10)
  x = rnorm(100); x <- scale(x)
  y = x
  mat <- cbind(x,y)
  res <- reduce_pca(mat)
  
  expect_true(sum(res) < 1e-5)
  expect_true(length(res) == 100)
  expect_true(all(order(x) == order(res)) | 
                all(order(x) == rev(order(res))))
})

test_that("it errors when passed in nothing", {
  expect_error(reduce_pca(NULL))
  expect_error(reduce_pca(NA))
  expect_error(reduce_pca())
})

test_that("it can handle matrix with no columns", {
  mat <- matrix(0, 10, 10)
  res <- reduce_pca(mat[,numeric(0)])
  expect_true(length(res) == 10)
  expect_true(all(res == 0))
})

###########

## test .isValid_reduction()

set.seed(10)
mat <- matrix(1:16, nrow=2, ncol=8)
mri <- BrcFmri(data2d = mat, id = "01", 
  parcellation = BrcParcellation(c(2,2,2), 1:8))

parcellation <- BrcParcellation(c(2,2,2), rbinom(8,1,0.5))

mat.big <- matrix(5, nrow = 2, ncol = 27)
mri.big <- BrcFmri(data2d = mat.big, id = "02", 
  parcellation = BrcParcellation(c(3,3,3), 1:27))

test_that("it errors when fmri and parcellation don't match in dimension", {
  expect_error(.isValid_reduction(mri, mri.big$parcellation))
})

test_that("works as normal", {
  expect_true(.isValid_reduction(mri, mri$parcellation))
  expect_true(.isValid_reduction(mri, parcellation))
})

test_that("errors if mri is not valid", {
  mri.fake <- mri
  mri.fake$data2d <- mri.fake$data2d[,1:7]
  expect_error(.isValid_reduction(mri.fake, parcellation))
})

test_that("errors if parcellation is not valid", {
  parcellation.fake <- parcellation
  parcellation.fake$dim3d <- c(3,2,2)
  expect_error(.isValid_reduction(mri, parcellation.fake))
})
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


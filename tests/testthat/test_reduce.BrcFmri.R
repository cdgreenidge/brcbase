context("Test reduce.BrcFmri.R")

## test reduce_mean()

test_that("it computes the correct mean", {
  mat <- matrix(rnorm(200), ncol = 20, nrow = 10)
  mat <- t(scale(mat))
  res <- reduce_mean(mat)
  expect_true(sum(abs(res)) < 1e-5)
  expect_true(length(res) == 20)
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
context("Test reduce.BrcFmri.R")

mat <- matrix(1:8, nrow=2, ncol=4)
mri <- BrcFmri(data2d = mat, id = "01",
  parcellation = BrcParcellation(c(2,2,2), c(0,0,1,1,2,2,3,4)))

mat2 <- matrix(1:4, nrow=1, ncol=4)
mri2 <- BrcFmri(data2d = mat2, id = "02",
               parcellation = BrcParcellation(c(2,2,2), c(0,0,1,1,2,2,3,4)))

parcellation <- BrcParcellation(c(2,2,2), c(0,0,1,1,1,1,2,2))

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

test_that("it errors when fmri and parcellation don't match in dimension", {
  mat.big <- matrix(5, nrow = 2, ncol = 27)
  mri.big <- BrcFmri(data2d = mat.big, id = "03", 
                     parcellation = BrcParcellation(c(3,3,3), 1:27))
  
  expect_error(.isValid_reduction(mri, mri.big$parcellation))
})

test_that("works as normal", {
  expect_true(.isValid_reduction(mri, mri$parcellation))
  expect_true(.isValid_reduction(mri, parcellation))
})

test_that("errors if mri is not valid", {
  mri.fake <- mri
  mri.fake$data2d <- mri.fake$data2d[,1:3]
  expect_error(.isValid_reduction(mri.fake, parcellation))
})

test_that("errors if parcellation is not valid", {
  parcellation.fake <- parcellation
  parcellation.fake$dim3d <- c(3,2,2)
  expect_error(.isValid_reduction(mri, parcellation.fake))
})

########################

## test .reduceFmritoParcellation()

test_that("it returns a matrix", {
  expect_true(is.matrix(.reduceFmritoParcellation(mri, parcellation, 
    reduce_mean)))
})

test_that("it returns a matrix even when the parcellation is one partition", {
  parcellation2 <- BrcParcellation(c(2,2,2), rep(1,8))
  
  expect_true(is.matrix(.reduceFmritoParcellation(mri, parcellation2, 
    reduce_mean)))
})

test_that("it turns a vector of 0's when there is no overlap", {
  parcellation2 <- BrcParcellation(c(2,2,2), c(1,1,0,0,0,0,0,0))
  res <- .reduceFmritoParcellation(mri, parcellation2, reduce_mean)
  
  expect_true(is.matrix(res))
  expect_true(length(res) == 2)
  expect_true(all(res == 0))
})

test_that("it returns a matrix for 1-time series fmri, 2 partitions", {
  parcellation2 <- BrcParcellation(c(2,2,2), c(rep(1,4), rep(2,4)))
  
  expect_true(is.matrix(.reduceFmritoParcellation(mri2, parcellation2, 
                                                  reduce_mean)))
})

test_that("it returns a matrix for 1-time series fmri, 1 partition", {
  parcellation2 <- BrcParcellation(c(2,2,2), rep(1,8))
  
  expect_true(is.matrix(.reduceFmritoParcellation(mri2, parcellation2, 
                                                  reduce_mean)))
})

test_that("it returns a matrix for 1-time series, no overlap", {
  parcellation2 <- BrcParcellation(c(2,2,2), c(1,1,0,0,0,0,0,0))
  res <- .reduceFmritoParcellation(mri2, parcellation2, reduce_mean)
  
  expect_true(is.matrix(res))
  expect_true(length(res) == 1)
  expect_true(all(res == 0))
})

test_that("it returns a column of all 0's if no overlap", {
  parcellation2 <- BrcParcellation(c(2,2,2), c(1,2,0,0,3,3,3,3))
  res <- .reduceFmritoParcellation(mri, parcellation2, reduce_mean)
  
  expect_true(all(res[,1:2] == 0))
})

test_that("it does not apply func to empty voxels", {
  parcellation2 <- BrcParcellation(c(2,2,2), c(1,2,0,0,2,2,2,2))
  res <- .reduceFmritoParcellation(mri, parcellation2, reduce_mean)
  
  expect_true(all(res[,2] == c(mean(c(3,3,5,7)), mean(c(4,4,6,8)))))
})

test_that("it errors when passed a bad function", {
  parcellation2 <- BrcParcellation(c(2,2,2), c(1,1,0,0,0,0,0,0))
  expect_error(.reduceFmritoParcellation(mri, parcellation2, mean))
})
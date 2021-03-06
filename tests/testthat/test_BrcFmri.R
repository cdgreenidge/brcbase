context("Test BrcFmri.R")

mat <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
              nrow=2, ncol=8)
dim3d <- c(2, 2, 2)
partition <- 1:8
parcellation <- BrcParcellation(dim3d, partition)

test_that("the constructor returns an object of class BrcFmri", {
  mri <- BrcFmri(data2d=mat, id="01", parcellation=parcellation)
  expect_equal(class(mri), "BrcFmri")
})

test_that("it contains a 2D data matrix", {
  mri <- BrcFmri(data2d=mat, id="01", parcellation=parcellation)
  expect_equal(mri$data2d, mat)
})

test_that("data must be a matrix", {
  expect_error(BrcFmri(data2d=numeric(), parcellation=parcellation), "data")
})

test_that("it contains a parcellation", {
  mri <- BrcFmri(data2d=mat, id="01", parcellation=parcellation)
  expect_equal(mri$parcellation, parcellation)
})

test_that("parcellation must be of class BrcParcellation", {
  expect_error(BrcFmri(data2d=mat, parcellation=character()), "parcellation")
})

test_that("it contains an ID string", {
  mri <- BrcFmri(data2d=mat, id="ABIDE_5002", parcellation=parcellation)
  expect_equal(mri$id, "ABIDE_5002")
})

test_that("it knows how to check itself for validity", {
  dim3d <- c(2, 1, 2)
  partition <- 1:4
  parcellation <- BrcParcellation(dim3d, partition)
  expect_error(isValid(BrcFmri(data2d=mat, id="01", 
    parcellation=parcellation)), "columns")
})

test_that("it knows how to check itself for validity", {
  dim3d <- c(2, 2, 2)
  partition <- 1:8
  parcellation <- BrcParcellation(dim3d, partition)
  obj <- BrcFmri(data2d=mat, id="01", parcellation=parcellation)
  expect_true(isValid(obj))
})

test_that("0 values in the partition are allowed", {
  mat <- matrix(data=c(1, 2, 3, 4, 5, 6, 7, 8), nrow=2, ncol=4)
  partition <- c(0, 1, 2, 2, 3, 3, 4, 0)
  parcellation <- BrcParcellation(dim3d=c(2, 2, 2), partition)
  mri <- BrcFmri(data2d=mat, id="01", parcellation=parcellation)
  expect_true(isValid(mri))
})

test_that("it also checks the parcellation it contains for validity", {
  dim3d <- c(2, 2, 2)
  partition <- 1:4
  expect_error(isValid(BrcFmri(data2d=mat, id="01", 
   parcellation=BrcParcellation(dim3d, partition))), "partition")
})

test_that("it can tell you its 4D dimensions", {
  mri <- BrcFmri(data2d=mat, id="01", parcellation=parcellation)
  expect_equal(dim4d(mri), c(2, 2, 2, 2))
})

test_that("dim4d checks if its object is a BrcFmri object", {
  expect_error(dim4d(character()), "BrcFmri")   
})

test_that("errors if data2d and parcellation don't match in columns",{
  mat2 <- matrix(1:4, nrow=1, ncol=8)
  expect_error(BrcFmri(data2d = mat2, id = "02",
                  parcellation = BrcParcellation(c(2,2,2), c(0,0,1,1,2,2,3,4))))
})

#################

## test print and summary

test_that("BrcFmri has a meaningful print statement", {
  fmri <- BrcFmri(data2d=mat, id="01", parcellation=parcellation)
  res <- paste0(capture.output(print(fmri)), collapse = " ")
  expect_true(grep("BrcFmri", res) == 1)
})

test_that("BrcFmri has a meaningful summary statement", {
  fmri <- BrcFmri(data2d=mat, id="01", parcellation=parcellation)
  res <- paste0(capture.output(summary(fmri)), collapse = " ")
  expect_true(grep("BrcFmri", res) == 1)
})

test_that("BrcFmri has a meaningful abridged print statement", {
  parcellation2 <- BrcParcellation(dim3d=c(3,3,3), 1:27)
  mat2 <- matrix(1:27, nrow = 1)
  fmri <- BrcFmri(data2d=mat2, id="01", parcellation=parcellation2)
  res <- paste0(capture.output(print(fmri)), collapse = " ")
  expect_true(grep("Abridged", res) == 1)
})

context("Test Apply Parcellation")

mat <- matrix(1:8, nrow=2, ncol=4)
dim3d <- c(2, 2, 2)
partition <- rep(1:4, each = 2)
parcellation <- BrcParcellation(dim3d, partition)
mri <- BrcFmri(data2d=mat, id="01", parcellation=parcellation)

parcellation2 <- BrcParcellation(dim3d, partition = c(0,0,0,0,1,1,1,1))

test_that("it errors if fmri is not a BrcFmri",{
  expect_error(applyMask(parcellation, parcellation2))
})

test_that("it errors if parcellation is not a BrcParcellation",{
  expect_error(applyMask(mri, parcellation2$partition))
})

test_that("it errors if fmri is not valid",{
  mri2 <- mri
  mri2$parcellation$dim3d <- c(2,2,1)
  expect_error(applyMask(mri2, parcellation2))
})

test_that("it properly counts when fmri is not a singleton parcellation",{
  res <- applyMask(mri, parcellation2)
  expect_true(numParcels(res$parcellation) == 2)
})

test_that("it properly moves when fmri is not a singleton parcellation",{
  res <- applyMask(mri, parcellation2)
  expect_true(ncol(res$data2d) == 2)
  expect_true(all(res$data2d[,1] == c(5,6)))
  expect_true(all(res$data2d[,2] == c(7,8))) 
})
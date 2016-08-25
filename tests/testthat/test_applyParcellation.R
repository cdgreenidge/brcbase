context("Test Apply Parcellation")

mat <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
              nrow=2, ncol=8)
dim3d <- c(2, 2, 2)
partition <- 1:8
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
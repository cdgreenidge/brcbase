test_that("you can convert a 4D data matrix to a 2D data matrix", {
    data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
    arr4d <- array(data=data, dim=c(2, 2, 2, 2))
    mat2d <- t(matrix(data=data, nrow=8, ncol=2))
    expect_equal(data4dTo2d(arr4d), mat2d)
})

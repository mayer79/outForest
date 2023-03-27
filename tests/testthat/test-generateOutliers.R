test_that("generateOutliers(p = 0.2) changes vector as expected", {
  x <- 1:10
  p <- 0.2
  x_out <- generateOutliers(x, p = p, sd_factor = 100, seed = 1L)
  expect_equal(sum(x_out != x), p * length(x))
})

test_that("generateOutliers(p = 0) does not change vector", {
  x <- 1:10
  p <- 0.0
  x_out <- generateOutliers(x, p = p, sd_factor = 100, seed = 1L)
  expect_equal(x_out, x)
})

test_that("generateOutliers(p = 0.2) changes matrix as expected", {
  x <- matrix(1:10, ncol = 2L)
  p <- 0.2
  x_out <- generateOutliers(x, p = p, sd_factor = 100, seed = 1L)
  expect_equal(sum(x_out != x), p * length(x))
})

test_that("generateOutliers(p = 0.0) changes matrix as expected", {
  x <- matrix(1:10, ncol = 2L)
  p <- 0.0
  x_out <- generateOutliers(x, p = p, sd_factor = 100, seed = 1L)
  expect_equal(x_out, x)
})

test_that("generateOutliers(p = 0.2) changes numeric cols and keeps factor", {
  p <- 0.2
  iris_out <- generateOutliers(iris, p = p, sd_factor = 100, seed = 1L)
  expect_equal(unname(colSums(iris_out != iris)), c(rep(nrow(iris) * p, 4L), 0L))
})

test_that("generateOutliers(p = 0.0) does not change anything", {
  p <- 0.0
  iris_out <- generateOutliers(iris, p = p, sd_factor = 100, seed = 1L)
  expect_equal(iris, iris_out)
})

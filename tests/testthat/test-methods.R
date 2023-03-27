x <- outForest(iris, seed = 1L, verbose = FALSE)

test_that("print() does not fail", {
  capture_output(expect_no_error(print(x)))
})

test_that("summary() does not fail", {
  capture_output(expect_no_error(summary(x)))
})

test_that("plot() does not fail", {
  expect_no_error(plot(x, what = "counts"))
  expect_no_error(plot(x, what = "scores"))
})

test_that("is.outForest() does what it should", {
  expect_true(is.outForest(x))
  expect_false(is.outForest(1))
})

test_that("Data() extracts $Data", {
  expect_equal(Data(x), x$Data)
})

test_that("Data() fails on non-outForest object", {
  expect_error(Data(1))
})

X <- data.frame(a = 1:100, b = 1:100)
X[1L, "a"] <- 1000
x <- outForest(X, seed = 1L, verbose = FALSE, min.node.size = 10L)

test_that("outliers() finds gross outlier", {
  expect_true(nrow(subset(outliers(x), row == 1L)) >= 1L)
})

test_that("outliers(1) gives error", {
  expect_error(outliers(1))
})


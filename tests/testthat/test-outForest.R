X <- data.frame(a = 1:100, b = 1:100)
X[1L, "a"] <- 1000
x <- outForest(X, seed = 1L, min.node.size = 10L, verbose = FALSE)

X_NA <- X
X_NA[2L, "a"] <- NA

test_that("outliers() finds gross outlier", {
  expect_true(nrow(subset(outliers(x), row == 1L)) >= 1L)
})

test_that("messages are suppressed with verbose=FALSE", {
  expect_silent(outForest(X, seed = 1L, min.node.size = 10L, verbose = FALSE))
})

test_that("outForest() is chatty with verbose=TRUE", {
  expect_output(outForest(X, seed = 1L, min.node.size = 10L, verbose = TRUE))
})

test_that("replace = 'NA' produces NA", {
  x <- outForest(X, seed = 1L, min.node.size = 10L, verbose = FALSE, replace = "NA")
  expect_true(all(is.na(outliers(x)$replacement)))
})

test_that("replace = 'pmm' produces values in x", {
  x <- outForest(X, seed = 1L, min.node.size = 10L, verbose = FALSE, replace = "pmm")
  expect_true(all(outliers(x)$replacement %in% unlist(X)))
})

test_that("replace = 'predictions' produces values not all in x", {
  x <- outForest(X, seed = 1L, min.node.size = 10L, verbose = FALSE, replace = "pred")
  expect_false(all(outliers(x)$replacement %in% c(X)))
})

test_that("replace = 'no' does not change input", {
  x <- outForest(X, seed = 1L, min.node.size = 10L, verbose = FALSE, replace = "no")
  expect_identical(X, Data(x))
})


test_that("impute_multivariate = TRUE/FALSE has an effect", {
  x_T <- outForest(
    X_NA,
    seed = 1L,
    min.node.size = 10L,
    verbose = FALSE,
    replace = "pred"
  )

  x_F <- outForest(
    X,
    seed = 1L,
    min.node.size = 10L,
    verbose = FALSE,
    replace = "pred",
    impute_multivariate = FALSE
  )
  expect_false(identical(outliers(x_T), outliers(x_F)))
})

test_that("setting a seed gives identical results", {
  x_1 <- outForest(
    X_NA,
    seed = 1L,
    min.node.size = 10L,
    verbose = FALSE,
    replace = "pred"
  )

  x_2 <- outForest(
    X_NA,
    seed = 1L,
    min.node.size = 10L,
    verbose = FALSE,
    replace = "pred"
  )
  expect_identical(x_1, x_2)
})

test_that("using a very large outlier will avoid finding outliers", {
  x <- outForest(
    X[-1L, ], seed = 1L, min.node.size = 10L, verbose = FALSE, threshold = 100
  )
  expect_true(nrow(outliers(x)) == 0L)
})

test_that("predict() does not work when allow_predictions = FALSE", {
  x <- outForest(X, seed = 1L, min.node.size = 10L, verbose = FALSE)
  expect_error(predict(x, x[1L, ]))
})

test_that("predict() works when allow_predictions = TRUE", {
  x <- outForest(
    X[-1L, ], seed = 1L, min.node.size = 10L, verbose = FALSE, allow_predictions = TRUE
  )
  expect_no_error(out <- predict(x, X[1L, ]))
  expect_true(nrow(outliers(out)) >= 1L)

  expect_no_error(out_na <- predict(x, X_NA[2L, ])) # predict with missings
})

test_that("predict() works when prediction data has a missing value", {
  x <- outForest(
    X[-1L, ], seed = 1L, min.node.size = 10L, verbose = FALSE, allow_predictions = TRUE
  )
  expect_no_error(out <- predict(x, X[1L, ]))
  expect_true(nrow(outliers(out)) >= 1L)
})

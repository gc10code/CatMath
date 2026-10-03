test_that("datasets are read with clean types", {
  expect_equal(dim(iris_df), c(150, 5))
  expect_equal(numeric_vars(iris_df), c("sepal_length", "sepal_width", "petal_length", "petal_width"))
  expect_equal(categorical_vars(iris_df), "species")
  expect_error(read_dataset(tempfile(fileext = ".xlsx")), "Unsupported")
})

test_that("correlated features are dropped greedily", {
  x <- data.frame(a = 1:20, b = (1:20) * 2 + 0.01 * sin(1:20), c = cos(1:20))
  dropped <- find_correlated(as.matrix(x), 0.95)
  expect_length(dropped, 1)
  expect_true(dropped %in% c("a", "b"))
  res <- select_features(iris_df, numeric_vars(iris_df), "species", cor_cutoff = 0.9)
  expect_false("petal_length" %in% names(res$data))
})

test_that("rows are aggregated by group with mean and mode", {
  df <- data.frame(id = c("a", "a", "b"), x = c(1, 3, NA), g = c("u", "u", "v"))
  out <- aggregate_rows(df, "id")$data
  expect_equal(out$x, c(2, NA))
  expect_equal(out$g, c("u", "v"))
})

test_that("missing values are imputed or removed", {
  df <- data.frame(x = c(1, NA, 3, -999), y = c("a", NA, "a", "b"))
  expect_equal(impute_numeric(df, "x", "mean")$data$x[2], mean(c(1, 3, -999)))
  expect_equal(impute_numeric(df, "x", "median", indicator = -999)$data$x, c(1, 2, 3, 2))
  expect_equal(nrow(impute_numeric(df, "x", "remove")$data), 3)
  expect_equal(impute_categorical(df, "y", "mode")$data$y, c("a", "a", "a", "b"))
  expect_equal(drop_sparse_vars(df, 0.2)$data |> names(), character(0))
})

test_that("transformations behave", {
  z <- transform_numeric(iris_df, "sepal_length", "zscore")$data$sepal_length
  expect_equal(c(mean(z), sd(z)), c(0, 1), tolerance = 1e-8)
  m <- transform_numeric(iris_df, "sepal_length", "minmax")$data$sepal_length
  expect_equal(range(m), c(0, 1))
  expect_error(transform_numeric(data.frame(x = c(-1, 2)), "x", "log"), "non-negative")
})

num <- c("sepal_length", "sepal_width", "petal_length", "petal_width")

test_that("statistical tests pick the right procedure", {
  res <- run_stat_tests(iris_df, num, "species")
  expect_setequal(unique(res$tests$family), c("pairwise", "variance", "multigroup"))
  multi <- res$tests[res$tests$family == "multigroup", ]
  expect_true(all(multi$p_value < 1e-10))
  expect_true(build_ok(stats_treemap(res, "pairwise")))
})

test_that("Brown-Forsythe matches the median-centred Levene test", {
  bf <- brown_forsythe(iris_df$sepal_width, iris_df$species)
  expect_equal(round(bf$statistic, 4), 0.5902)
})

test_that("PCA agrees with prcomp", {
  res <- run_pca(iris_df, num)
  expect_equal(res$eigen, prcomp(iris_df[num], scale. = TRUE)$sdev^2)
  expect_true(build_ok(pca_biplot(res, group = iris_df$species)))
})

test_that("NMF reconstructs non-negative data", {
  res <- run_nmf(iris_df, num, rank = 3, curve_max_rank = 4)
  expect_true(all(res$W >= 0) && all(res$H >= 0))
  expect_gt(res$ev, 0.99)
  expect_true(all(diff(res$ev_curve$ev) > -1e-3))
  expect_error(run_nmf(data.frame(a = c(-1, 2, 3), b = 1:3), c("a", "b")), "non-negative")
})

test_that("regression selects informative predictors", {
  set.seed(1)
  df <- data.frame(x1 = rnorm(200), x2 = rnorm(200), noise = rnorm(200))
  df$y <- 2 * df$x1 - df$x2 + rnorm(200, sd = 0.1)
  res <- run_lm(df, "y", c("x1", "x2", "noise"), rounds = 10)
  expect_true(all(c("x1", "x2") %in% res$predictors))
  expect_equal(unname(coef(res$model)[c("x1", "x2")]), c(2, -1), tolerance = 0.05)
})

test_that("k-NN all-k predictions equal class::knn-style brute force", {
  X <- standardize(as.matrix(iris_df[num]))$x
  y <- as.integer(factor(iris_df$species))
  tr <- seq(1, 150, by = 2)
  pred <- knn_predict_all_k(X[tr, ], y[tr], X[-tr, ], 5, 3)
  brute <- apply(X[-tr, ], 1, function(p) {
    d <- colSums((t(X[tr, ]) - p)^2)
    as.integer(names(which.max(table(y[tr][order(d)[1]]))))
  })
  expect_equal(pred[1, ], unname(brute))
  res <- run_knn(iris_df, num, "species", k_max = 15, rounds = 5)
  expect_gt(res$cv_accuracy, 0.9)
  expect_true(build_ok(knn_boundary(res, "petal_length", "petal_width")))
})

test_that("k-means and random forest run end to end", {
  km <- run_kmeans(iris_df, num, k_max = 5, rounds = 3)
  expect_true(km$best_k %in% 2:5)
  expect_true(build_ok(kmeans_silhouette_plot(km, 3)))
  rf <- run_rf(iris_df, num, "species", max_trees = 60, rounds = 2)
  expect_gt(rf$cv_accuracy, 0.9)
  expect_true(build_ok(rf_tree_plot(rf, 1)))
})

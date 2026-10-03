# Linear regression with cross-validated predictor subset selection.
#
# Every candidate subset is scored on the same random train/validation splits
# (fair comparison) with lm.fit on a pre-built design matrix, which avoids the
# formula/model.frame overhead of lm() inside the search loop.

subset_cv_scores <- function(X, y, cols, splits) {
  vapply(splits, function(tr) {
    fit <- stats::lm.fit(X[tr, c(1, cols + 1), drop = FALSE], y[tr])
    beta <- fit$coefficients
    beta[is.na(beta)] <- 0
    va <- -tr
    pred <- drop(X[va, c(1, cols + 1), drop = FALSE] %*% beta)
    yv <- y[va]
    r2 <- 1 - sum((yv - pred)^2) / sum((yv - mean(yv))^2)
    n <- length(yv); k <- length(cols)
    if (n - k - 1 > 0) 1 - (1 - r2) * (n - 1) / (n - k - 1) else r2
  }, numeric(1))
}

run_lm <- function(data, response, predictors, rounds = 30, train_frac = 0.8, seed = 123, progress = NULL) {
  progress <- progress_or_noop(progress)
  if (is.null(response) || !response %in% numeric_vars(data)) stop("Select a numeric response variable.", call. = FALSE)
  predictors <- require_vars(data, setdiff(intersect(predictors, numeric_vars(data)), response), 1, "predictor")
  require_complete(data, c(response, predictors))
  n <- nrow(data)
  if (n < length(predictors) + 5) stop("Not enough observations for the selected predictors.", call. = FALSE)

  X <- cbind(1, as.matrix(data[predictors]))
  y <- data[[response]]
  splits <- random_splits(n, rounds, train_frac, seed)
  p <- length(predictors)

  scores <- list()
  if (p <= CM_LIMITS$lm_exhaustive_max) {
    method <- "Exhaustive best subset"
    subsets <- unlist(lapply(seq_len(p), function(k) utils::combn(p, k, simplify = FALSE)), recursive = FALSE)
    for (i in seq_along(subsets)) {
      scores[[i]] <- subset_cv_scores(X, y, subsets[[i]], splits)
      if (i %% 20 == 0 || i == length(subsets)) progress(i / length(subsets), sprintf("Subset %d / %d", i, length(subsets)))
    }
  } else {
    method <- "Forward selection"
    subsets <- list()
    current <- integer(0)
    best_score <- -Inf
    repeat {
      candidates <- setdiff(seq_len(p), current)
      if (!length(candidates)) break
      cand_scores <- lapply(candidates, function(j) subset_cv_scores(X, y, c(current, j), splits))
      means <- vapply(cand_scores, mean, numeric(1))
      j <- which.max(means)
      subsets[[length(subsets) + 1]] <- c(current, candidates[j])
      scores[[length(scores) + 1]] <- cand_scores[[j]]
      progress(length(current) / p, sprintf("%d predictors", length(current) + 1))
      if (means[j] <= best_score) break
      best_score <- means[j]
      current <- c(current, candidates[j])
    }
  }

  score_matrix <- do.call(cbind, scores)
  labels <- vapply(subsets, function(s) paste(predictors[s], collapse = " + "), character(1))
  colnames(score_matrix) <- labels
  best <- which.max(colMeans(score_matrix))
  chosen <- predictors[subsets[[best]]]

  formula <- stats::reformulate(sprintf("`%s`", chosen), response = sprintf("`%s`", response))
  model <- stats::lm(formula, data = data[c(response, chosen)])
  progress(1)
  structure(list(
    type = "lm", response = response, predictors = chosen, candidates = predictors,
    model = model, method = method, scores = score_matrix, best = best,
    cv_adj_r2 = mean(score_matrix[, best])
  ), class = "cm_result")
}

lm_tables <- function(res) {
  s <- summary(res$model)
  f <- s$fstatistic
  f_p <- if (is.null(f)) NA else stats::pf(f[1], f[2], f[3], lower.tail = FALSE)
  coefs <- s$coefficients
  ranking <- data.frame(
    Predictors = colnames(res$scores),
    Size = lengths(strsplit(colnames(res$scores), " + ", fixed = TRUE)),
    `Mean CV adj. R²` = signif(colMeans(res$scores), 4),
    `SD` = signif(apply(res$scores, 2, stats::sd), 3),
    check.names = FALSE
  )
  list(
    summary = data.frame(
      Metric = c("Response", "Predictors", "Selection", "Observations", "R²", "Adjusted R²",
                 "CV adjusted R² (mean)", "Residual SE", "F-test p"),
      Value = fmt_values(res$response, paste(res$predictors, collapse = ", "), res$method, stats::nobs(res$model),
                signif(s$r.squared, 4), signif(s$adj.r.squared, 4), signif(res$cv_adj_r2, 4),
                signif(s$sigma, 4), format_p(f_p))
    ),
    coefficients = data.frame(
      Term = rownames(coefs), Estimate = signif(coefs[, 1], 4), `Std. error` = signif(coefs[, 2], 4),
      t = signif(coefs[, 3], 4), p = format_p(coefs[, 4]), check.names = FALSE, row.names = NULL
    ),
    ranking = ranking[order(-ranking[[3]]), , drop = FALSE]
  )
}

# Supervised classification: k-nearest neighbours and random forest.

# ---- k-NN -------------------------------------------------------------------

#' Predict class codes for every k in 1..k_max in one pass.
#'
#' Distances are computed block-wise with the identity |a-b|^2 = |a|^2 + |b|^2 - 2ab
#' (a single BLAS product per block), neighbours are sorted once, and votes for
#' all k are obtained with cumulative sums. Ties go to the class with the
#' nearer neighbours. Returns an integer matrix k_max x nrow(test_x).
knn_predict_all_k <- function(train_x, train_y, test_x, k_max, n_classes, chunk = CM_LIMITS$knn_chunk) {
  k_max <- min(k_max, nrow(train_x))
  train_sq <- rowSums(train_x^2)
  tie_weight <- 1 - seq_len(k_max) * 1e-9
  out <- matrix(NA_integer_, k_max, nrow(test_x))
  for (start in seq(1, nrow(test_x), by = chunk)) {
    idx <- start:min(start + chunk - 1, nrow(test_x))
    tx <- test_x[idx, , drop = FALSE]
    d <- outer(rowSums(tx^2), train_sq, "+") - 2 * tcrossprod(tx, train_x)
    nn <- matrix(apply(d, 1, function(r) order(r)[seq_len(k_max)]), nrow = k_max)
    labels <- matrix(train_y[nn], nrow = k_max)
    best <- matrix(0L, k_max, length(idx))
    best_score <- matrix(-Inf, k_max, length(idx))
    for (cl in seq_len(n_classes)) {
      votes <- (labels == cl) * tie_weight
      score <- votes
      if (k_max > 1) for (k in 2:k_max) score[k, ] <- score[k - 1, ] + votes[k, ]
      better <- score > best_score
      best[better] <- cl
      best_score[better] <- score[better]
    }
    out[, idx] <- best
  }
  out
}

run_knn <- function(data, vars, target, k_max = 30, rounds = 20, train_frac = 0.8, seed = 123, progress = NULL) {
  progress <- progress_or_noop(progress)
  vars <- require_vars(data, intersect(vars, numeric_vars(data)), 1, "numeric predictor")
  if (is.null(target) || !target %in% names(data)) stop("Select the target variable.", call. = FALSE)
  data <- data[!is.na(data[[target]]), , drop = FALSE]
  require_complete(data, vars)
  y <- factor(data[[target]])
  if (nlevels(y) < 2) stop("The target needs at least two classes.", call. = FALSE)
  raw <- as.matrix(data[vars])
  std <- standardize(raw)
  X <- std$x
  yc <- as.integer(y)
  n_classes <- nlevels(y)
  splits <- random_splits(nrow(X), rounds, train_frac, seed)
  k_max <- max(1, min(as.integer(k_max), length(splits[[1]])))

  acc <- kap <- matrix(NA_real_, rounds, k_max, dimnames = list(NULL, seq_len(k_max)))
  conf_all <- array(0, c(k_max, n_classes, n_classes))
  for (r in seq_along(splits)) {
    tr <- splits[[r]]
    pred <- knn_predict_all_k(X[tr, , drop = FALSE], yc[tr], X[-tr, , drop = FALSE], k_max, n_classes)
    truth <- yc[-tr]
    for (k in seq_len(k_max)) {
      conf <- matrix(tabulate(truth + (pred[k, ] - 1L) * n_classes, n_classes^2), n_classes)
      conf_all[k, , ] <- conf_all[k, , ] + conf
      m <- accuracy_kappa(conf)
      acc[r, k] <- m[["accuracy"]]
      kap[r, k] <- m[["kappa"]]
    }
    progress(r / rounds, sprintf("Round %d / %d", r, rounds))
  }
  mean_kappa <- colMeans(kap, na.rm = TRUE)
  mean_acc <- colMeans(acc, na.rm = TRUE)
  best_k <- order(-mean_kappa, -mean_acc)[1]
  conf <- conf_all[best_k, , ]
  dimnames(conf) <- list(Actual = levels(y), Predicted = levels(y))
  structure(list(
    type = "knn", vars = vars, target = target, levels = levels(y),
    X = X, y = y, center = std$center, scale = std$scale,
    k = best_k, accuracy = acc, kappa = kap, confusion = as.table(conf),
    cv_accuracy = mean_acc[best_k], cv_kappa = mean_kappa[best_k]
  ), class = "cm_result")
}

knn_predict <- function(res, newx) {
  pred <- knn_predict_all_k(res$X, as.integer(res$y), newx, res$k, length(res$levels))
  factor(res$levels[pred[res$k, ]], levels = res$levels)
}

knn_tables <- function(res) {
  list(
    summary = data.frame(
      Metric = c("Observations", "Predictors", "Target", "Classes", "Best k", "CV accuracy", "CV Cohen's kappa"),
      Value = fmt_values(nrow(res$X), length(res$vars), res$target, length(res$levels), res$k,
                round(res$cv_accuracy, 4), round(res$cv_kappa, 4))
    ),
    classes = class_metrics(res$confusion)
  )
}

# ---- Random forest ------------------------------------------------------------

default_mtry_grid <- function(p, max_values = 8) {
  unique(round(seq(1, p, length.out = min(p, max_values))))
}

log_loss <- function(prob, y_codes) {
  p <- prob[cbind(seq_along(y_codes), y_codes)]
  -mean(log(pmax(p, 1e-15)))
}

#' Random forest tuning over mtry and number of trees.
#'
#' One forest of `max_trees` is grown per (mtry, round); smaller forests are
#' evaluated by predicting with the first `n` trees (ranger's num.trees
#' argument), so the tree-count grid costs no extra training.
run_rf <- function(data, vars, target, max_trees = 500, tree_steps = 10, mtry_grid = NULL,
                   rounds = 5, train_frac = 0.8, seed = 123, num_threads = NULL, progress = NULL) {
  progress <- progress_or_noop(progress)
  vars <- require_vars(data, intersect(vars, names(data)), 1, "predictor")
  if (is.null(target) || !target %in% names(data)) stop("Select the target variable.", call. = FALSE)
  vars <- setdiff(vars, target)
  data <- data[!is.na(data[[target]]), , drop = FALSE]
  require_complete(data, vars)
  y <- factor(data[[target]])
  if (nlevels(y) < 2) stop("The target needs at least two classes.", call. = FALSE)
  X <- data[vars]
  for (v in categorical_vars(X)) X[[v]] <- factor(X[[v]])
  yc <- as.integer(y)
  lv <- levels(y)
  n_classes <- length(lv)

  max_trees <- max(10, as.integer(max_trees))
  trees <- unique(pmax(1, round(seq(max_trees / tree_steps, max_trees, length.out = tree_steps))))
  mtry_grid <- mtry_grid %||% default_mtry_grid(length(vars))
  mtry_grid <- sort(unique(pmin(pmax(1, mtry_grid), length(vars))))
  splits <- random_splits(nrow(X), rounds, train_frac, seed)

  dims <- c(length(mtry_grid), length(trees), rounds)
  acc <- valid_loss <- train_loss <- array(NA_real_, dims)
  conf_all <- array(0, c(length(mtry_grid), length(trees), n_classes, n_classes))
  total <- length(mtry_grid) * rounds
  step <- 0
  for (i in seq_along(mtry_grid)) {
    for (r in seq_len(rounds)) {
      tr <- splits[[r]]
      fit <- ranger::ranger(
        x = X[tr, , drop = FALSE], y = y[tr], num.trees = max_trees, mtry = mtry_grid[i],
        probability = TRUE, seed = seed + r, num.threads = num_threads, verbose = FALSE
      )
      for (j in seq_along(trees)) {
        pv <- stats::predict(fit, X[-tr, , drop = FALSE], num.trees = trees[j], num.threads = num_threads)$predictions
        pt <- stats::predict(fit, X[tr, , drop = FALSE], num.trees = trees[j], num.threads = num_threads)$predictions
        pv <- pv[, lv, drop = FALSE]
        pt <- pt[, lv, drop = FALSE]
        pred <- max.col(pv, ties.method = "first")
        truth <- yc[-tr]
        conf <- matrix(tabulate(truth + (pred - 1L) * n_classes, n_classes^2), n_classes)
        conf_all[i, j, , ] <- conf_all[i, j, , ] + conf
        acc[i, j, r] <- sum(diag(conf)) / sum(conf)
        valid_loss[i, j, r] <- log_loss(pv, truth)
        train_loss[i, j, r] <- log_loss(pt, yc[tr])
      }
      step <- step + 1
      progress(0.9 * step / total, sprintf("mtry = %d, round %d", mtry_grid[i], r))
    }
  }
  mean_valid <- apply(valid_loss, c(1, 2), mean)
  best <- which(mean_valid == min(mean_valid), arr.ind = TRUE)[1, ]
  progress(0.95, "Fitting final model")
  model <- ranger::ranger(
    x = X, y = y, num.trees = trees[best[2]], mtry = mtry_grid[best[1]], probability = TRUE,
    importance = "impurity", seed = seed, num.threads = num_threads, verbose = FALSE
  )
  conf <- conf_all[best[1], best[2], , ]
  dimnames(conf) <- list(Actual = lv, Predicted = lv)
  grid <- expand.grid(mtry = mtry_grid, trees = trees)
  grid$accuracy <- as.vector(apply(acc, c(1, 2), mean))
  grid$valid_loss <- as.vector(mean_valid)
  grid$train_loss <- as.vector(apply(train_loss, c(1, 2), mean))
  progress(1)
  structure(list(
    type = "rf", vars = vars, target = target, levels = lv, model = model,
    mtry = mtry_grid[best[1]], trees = trees[best[2]], grid = grid, accuracy = acc,
    mtry_grid = mtry_grid, tree_grid = trees, confusion = as.table(conf),
    cv_accuracy = grid$accuracy[grid$mtry == mtry_grid[best[1]] & grid$trees == trees[best[2]]],
    cv_loss = min(mean_valid)
  ), class = "cm_result")
}

rf_tables <- function(res) {
  imp <- sort(res$model$variable.importance, decreasing = TRUE)
  list(
    summary = data.frame(
      Metric = c("Observations", "Predictors", "Target", "Classes", "Trees", "mtry",
                 "CV accuracy", "CV log loss", "OOB Brier score"),
      Value = fmt_values(res$model$num.samples, length(res$vars), res$target, length(res$levels), res$trees, res$mtry,
                round(res$cv_accuracy, 4), round(res$cv_loss, 4), round(res$model$prediction.error, 4))
    ),
    importance = data.frame(Variable = names(imp), Importance = signif(imp, 4),
                            `Relative %` = round(100 * imp / sum(imp), 2), check.names = FALSE, row.names = NULL),
    grid = signif_df(stats::setNames(res$grid, c("mtry", "Trees", "CV accuracy", "Validation log loss", "Training log loss"))),
    classes = class_metrics(res$confusion)
  )
}

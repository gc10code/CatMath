# Small, dependency-free helpers shared by the analysis and UI layers.

`%||%` <- function(a, b) if (is.null(a)) b else a

numeric_vars <- function(df) {
  if (is.null(df)) return(character(0))
  names(df)[vapply(df, is.numeric, logical(1))]
}

categorical_vars <- function(df) {
  if (is.null(df)) return(character(0))
  names(df)[!vapply(df, is.numeric, logical(1))]
}

# "None" / "" / NULL coming from a picker all mean "no variable".
none_to_null <- function(x) {
  if (is.null(x) || length(x) == 0 || identical(x, "") || identical(x, "None")) NULL else x
}

mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Evaluate `code` with a fixed seed without touching the caller's RNG stream.
with_seed <- function(seed, code) {
  env <- globalenv()
  if (exists(".Random.seed", envir = env, inherits = FALSE)) {
    old <- get(".Random.seed", envir = env)
    on.exit(assign(".Random.seed", old, envir = env), add = TRUE)
  } else {
    on.exit(if (exists(".Random.seed", envir = env, inherits = FALSE)) rm(".Random.seed", envir = env), add = TRUE)
  }
  set.seed(seed)
  code
}

# Progress callback used by every compute function: progress(value in [0, 1], detail).
progress_or_noop <- function(progress) {
  if (is.null(progress)) function(value, detail = NULL) invisible(NULL) else progress
}

require_vars <- function(df, vars, min = 1, what = "variables") {
  vars <- intersect(vars, names(df))
  if (length(vars) < min) {
    stop(sprintf("Select at least %d %s.", min, what), call. = FALSE)
  }
  vars
}

require_complete <- function(df, vars) {
  bad <- vars[vapply(df[vars], anyNA, logical(1))]
  if (length(bad)) {
    stop(sprintf(
      "Missing values in: %s. Handle them in Preprocessing > Missing data first.",
      paste(utils::head(bad, 5), collapse = ", ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}

# Column-wise z-score; zero-variance columns are centred but not scaled.
standardize <- function(x) {
  x <- as.matrix(x)
  center <- colMeans(x)
  scale <- sqrt(colSums(sweep(x, 2, center)^2) / max(nrow(x) - 1, 1))
  scale[!is.finite(scale) | scale == 0] <- 1
  list(x = sweep(sweep(x, 2, center), 2, scale, "/"), center = center, scale = scale)
}

is_standardized <- function(x, tol = 1e-6) {
  x <- as.matrix(x)
  all(abs(colMeans(x)) < tol) && all(abs(apply(x, 2, stats::sd) - 1) < tol)
}

random_splits <- function(n, rounds, train_frac, seed) {
  n_train <- max(2, min(n - 1, floor(train_frac * n)))
  with_seed(seed, lapply(seq_len(rounds), function(i) sample.int(n, n_train)))
}

# Index of the "knee" of a curve: point with maximum distance from the chord.
knee_point <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) return(which(ok)[1])
  xs <- x[ok]; ys <- y[ok]
  xn <- (xs - min(xs)) / diff(range(xs))
  yn <- (ys - min(ys)) / max(diff(range(ys)), .Machine$double.eps)
  x1 <- xn[1]; y1 <- yn[1]; x2 <- xn[length(xn)]; y2 <- yn[length(yn)]
  d <- abs((y2 - y1) * xn - (x2 - x1) * yn + x2 * y1 - y2 * x1)
  which(ok)[which.max(d)]
}

# Confusion matrix with fixed levels on both axes.
confusion <- function(actual, predicted, levels) {
  table(Actual = factor(actual, levels = levels), Predicted = factor(predicted, levels = levels))
}

accuracy_kappa <- function(conf) {
  n <- sum(conf)
  if (n == 0) return(c(accuracy = NA_real_, kappa = NA_real_))
  po <- sum(diag(conf)) / n
  pe <- sum(rowSums(conf) * colSums(conf)) / n^2
  c(accuracy = po, kappa = if (pe < 1) (po - pe) / (1 - pe) else NA_real_)
}

class_metrics <- function(conf) {
  tp <- diag(conf)
  precision <- tp / pmax(colSums(conf), 1)
  recall <- tp / pmax(rowSums(conf), 1)
  f1 <- ifelse(precision + recall > 0, 2 * precision * recall / (precision + recall), 0)
  data.frame(
    Class = rownames(conf), Support = as.integer(rowSums(conf)),
    Precision = round(precision, 4), Recall = round(recall, 4), F1 = round(f1, 4),
    check.names = FALSE, row.names = NULL
  )
}

# Round numeric columns to `digits` significant digits (integers untouched).
signif_df <- function(df, digits = 4) {
  df <- as.data.frame(df, check.names = FALSE)
  for (i in seq_along(df)) {
    if (is.double(df[[i]])) df[[i]] <- signif(df[[i]], digits)
  }
  df
}

matrix_table <- function(m, row_label = "Variable", digits = 4) {
  df <- signif_df(as.data.frame(m, check.names = FALSE), digits)
  cbind(stats::setNames(data.frame(rownames(m) %||% seq_len(nrow(m))), row_label), df)
}

format_p <- function(p) {
  ifelse(is.na(p), NA_character_, ifelse(p < 0.001, "< .001", formatC(p, format = "f", digits = 3)))
}

# Format heterogeneous summary values (numbers and labels) as display strings.
fmt_values <- function(...) {
  vapply(list(...), function(x) {
    if (is.numeric(x) && length(x) == 1) {
      if (is.na(x)) "—" else if (x == round(x) && abs(x) < 1e15) format(x, big.mark = ",", scientific = FALSE)
      else format(signif(x, 4), big.mark = ",", scientific = abs(x) < 1e-4)
    } else paste(as.character(x), collapse = ", ")
  }, character(1))
}

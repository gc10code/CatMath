# Pure preprocessing operations: data.frame in, data.frame out.
# Every function returns list(data = <data.frame>, message = <character>)
# so the UI can log a human-readable history of the applied steps.

#' Greedy removal of highly correlated numeric variables.
#'
#' Repeatedly takes the most correlated pair above `cutoff` and drops the
#' member with the larger mean absolute correlation (same idea as
#' caret::findCorrelation, without the dependency).
find_correlated <- function(x, cutoff = 0.9) {
  if (ncol(x) < 2) return(character(0))
  cm <- abs(stats::cor(x, use = "pairwise.complete.obs"))
  diag(cm) <- 0
  cm[is.na(cm)] <- 0
  dropped <- character(0)
  while (ncol(cm) > 1 && max(cm) > cutoff) {
    pair <- colnames(cm)[which(cm == max(cm), arr.ind = TRUE)[1, ]]
    victim <- pair[which.max(colMeans(cm[, pair, drop = FALSE]))]
    dropped <- c(dropped, victim)
    keep <- colnames(cm) != victim
    cm <- cm[keep, keep, drop = FALSE]
  }
  dropped
}

select_features <- function(df, num_vars, cat_vars, cor_cutoff = NULL) {
  num_vars <- intersect(num_vars, numeric_vars(df))
  cat_vars <- intersect(cat_vars, names(df))
  if (length(num_vars) + length(cat_vars) == 0) stop("Select at least one variable.", call. = FALSE)
  dropped <- character(0)
  if (!is.null(cor_cutoff) && length(num_vars) > 1) {
    dropped <- find_correlated(as.matrix(df[num_vars]), cor_cutoff)
    num_vars <- setdiff(num_vars, dropped)
  }
  msg <- sprintf("Selected %d categorical + %d numeric variables", length(cat_vars), length(num_vars))
  if (length(dropped)) {
    msg <- sprintf("%s (dropped %d with |r| > %.2f: %s)", msg, length(dropped), cor_cutoff,
                   paste(utils::head(dropped, 6), collapse = ", "))
  }
  list(data = df[c(cat_vars, num_vars)], message = msg)
}

replace_pattern <- function(df, var, pattern, replacement = "") {
  if (!nzchar(pattern)) stop("Enter a regular expression.", call. = FALSE)
  ok <- tryCatch({ grepl(pattern, "test"); TRUE }, error = function(e) FALSE)
  if (!ok) stop("Invalid regular expression.", call. = FALSE)
  x <- as.character(df[[var]])
  changed <- sum(grepl(pattern, x), na.rm = TRUE)
  df[[var]] <- gsub(pattern, replacement, x)
  list(data = df, message = sprintf("Replaced /%s/ with '%s' in %s (%d values)", pattern, replacement, var, changed))
}

#' Collapse rows sharing the same group value: mean for numeric columns,
#' mode for categorical ones. Uses rowsum() so it is linear in the data size.
aggregate_rows <- function(df, group_var) {
  df <- df[!is.na(df[[group_var]]), , drop = FALSE]
  g <- factor(df[[group_var]])
  num <- setdiff(numeric_vars(df), group_var)
  cat <- setdiff(categorical_vars(df), group_var)
  out <- stats::setNames(data.frame(levels(g), stringsAsFactors = FALSE), group_var)
  if (length(num)) {
    m <- as.matrix(df[num])
    present <- !is.na(m)
    m[!present] <- 0
    sums <- rowsum(m, g, reorder = TRUE)
    counts <- rowsum(present * 1, g, reorder = TRUE)
    means <- sums / counts
    means[counts == 0] <- NA
    out[num] <- as.data.frame(means)
  }
  for (v in cat) {
    out[[v]] <- vapply(split(df[[v]], g), function(x) as.character(mode_value(x)), character(1))
  }
  list(
    data = out[c(group_var, cat, num)],
    message = sprintf("Aggregated %d rows into %d groups by %s", nrow(df), nrow(out), group_var)
  )
}

#' Drop variables whose share of missing values exceeds `threshold`,
#' overall or (when `group_var` is given) inside any group.
drop_sparse_vars <- function(df, threshold = 0.1, group_var = NULL) {
  na <- is.na(df) * 1
  if (is.null(group_var)) {
    frac <- colMeans(na)
  } else {
    g <- factor(df[[group_var]], exclude = NULL)
    frac <- apply(rowsum(na, g) / as.vector(table(g)), 2, max)
  }
  dropped <- setdiff(names(frac)[frac > threshold], group_var)
  list(
    data = df[setdiff(names(df), dropped)],
    message = sprintf("Dropped %d variables with more than %.0f%% missing values%s",
                      length(dropped), 100 * threshold,
                      if (length(dropped)) paste0(": ", paste(utils::head(dropped, 6), collapse = ", ")) else "")
  )
}

as_categorical <- function(df, vars) {
  for (v in vars) df[[v]] <- as.character(df[[v]])
  list(data = df, message = sprintf("Converted to categorical: %s", paste(vars, collapse = ", ")))
}

# Parse the "missing value indicator" typed by the user: "NA" or empty -> NA.
parse_indicator <- function(x, numeric = TRUE) {
  x <- trimws(x %||% "")
  if (!nzchar(x) || toupper(x) == "NA") return(NA)
  if (numeric) {
    v <- suppressWarnings(as.numeric(x))
    if (is.na(v)) stop("The missing value indicator must be a number or NA.", call. = FALSE)
    return(v)
  }
  x
}

impute_numeric <- function(df, vars, method = c("remove", "mean", "median", "constant", "mice"),
                           indicator = NA, constant = 0) {
  method <- match.arg(method)
  vars <- intersect(vars, numeric_vars(df))
  if (!length(vars)) stop("Select at least one numeric variable.", call. = FALSE)
  if (!is.na(indicator)) {
    for (v in vars) df[[v]][df[[v]] %in% indicator] <- NA
  }
  n_missing <- sum(is.na(df[vars]))
  if (method == "remove") {
    keep <- stats::complete.cases(df[vars])
    return(list(data = df[keep, , drop = FALSE],
                message = sprintf("Removed %d rows with missing numeric values", sum(!keep))))
  }
  if (method == "mice") {
    if (!requireNamespace("mice", quietly = TRUE)) stop("Package 'mice' is not installed.", call. = FALSE)
    predictors <- numeric_vars(df)
    sub <- df[predictors]
    meth <- mice::make.method(sub)
    meth[] <- ""
    meth[vars] <- "pmm"
    imp <- mice::mice(sub, m = 1, maxit = 10, method = meth, seed = 123, printFlag = FALSE)
    df[vars] <- mice::complete(imp)[vars]
  } else {
    fill <- switch(method,
      mean = function(x) mean(x, na.rm = TRUE),
      median = function(x) stats::median(x, na.rm = TRUE),
      constant = function(x) constant
    )
    for (v in vars) {
      x <- df[[v]]
      x[is.na(x)] <- fill(x)
      df[[v]] <- x
    }
  }
  list(data = df, message = sprintf("Imputed %d numeric values (%s)", n_missing, method))
}

impute_categorical <- function(df, vars, method = c("remove", "mode", "constant"),
                               indicator = NA, constant = "missing") {
  method <- match.arg(method)
  vars <- intersect(vars, categorical_vars(df))
  if (!length(vars)) stop("Select at least one categorical variable.", call. = FALSE)
  for (v in vars) {
    x <- as.character(df[[v]])
    if (!is.na(indicator)) x[x %in% indicator] <- NA
    df[[v]] <- x
  }
  n_missing <- sum(is.na(df[vars]))
  if (method == "remove") {
    keep <- stats::complete.cases(df[vars])
    return(list(data = df[keep, , drop = FALSE],
                message = sprintf("Removed %d rows with missing categorical values", sum(!keep))))
  }
  for (v in vars) {
    x <- df[[v]]
    x[is.na(x)] <- if (method == "mode") mode_value(x) else constant
    df[[v]] <- x
  }
  list(data = df, message = sprintf("Imputed %d categorical values (%s)", n_missing, method))
}

transform_numeric <- function(df, vars, method = c("zscore", "minmax", "sqrt", "log")) {
  method <- match.arg(method)
  vars <- intersect(vars, numeric_vars(df))
  if (!length(vars)) stop("Select at least one numeric variable.", call. = FALSE)
  if (method %in% c("sqrt", "log")) {
    negative <- vars[vapply(df[vars], function(x) any(x < 0, na.rm = TRUE), logical(1))]
    if (length(negative)) {
      stop(sprintf("%s requires non-negative values (check: %s). Apply min-max scaling first.",
                   if (method == "sqrt") "Square root" else "Log", paste(utils::head(negative, 5), collapse = ", ")),
           call. = FALSE)
    }
  }
  f <- switch(method,
    zscore = function(x) { s <- stats::sd(x, na.rm = TRUE); (x - mean(x, na.rm = TRUE)) / if (is.na(s) || s == 0) 1 else s },
    minmax = function(x) { r <- range(x, na.rm = TRUE); d <- diff(r); (x - r[1]) / if (!is.finite(d) || d == 0) 1 else d },
    sqrt = sqrt,
    log = log1p
  )
  df[vars] <- lapply(df[vars], f)
  label <- c(zscore = "Z-score standardisation", minmax = "Min-max scaling", sqrt = "Square root", log = "log(1 + x)")
  list(data = df, message = sprintf("%s on %d variables", label[[method]], length(vars)))
}

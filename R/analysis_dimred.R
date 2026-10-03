# Dimensionality reduction: PCA (prcomp) and NMF (native implementation).

# ---- PCA --------------------------------------------------------------------

run_pca <- function(data, vars, scale = TRUE, progress = NULL) {
  progress <- progress_or_noop(progress)
  vars <- require_vars(data, intersect(vars, numeric_vars(data)), 2, "numeric variables")
  require_complete(data, vars)
  x <- as.matrix(data[vars])
  if (scale) {
    flat <- vars[apply(x, 2, stats::sd) == 0]
    if (length(flat)) stop(sprintf("Constant variables cannot be scaled: %s", paste(flat, collapse = ", ")), call. = FALSE)
  }
  progress(0.3, "Decomposing")
  fit <- stats::prcomp(x, center = TRUE, scale. = scale)
  eigen <- fit$sdev^2
  progress(1)
  structure(list(
    type = "pca", vars = vars, scale = scale,
    sdev = fit$sdev, eigen = eigen, var_explained = eigen / sum(eigen),
    rotation = fit$rotation, scores = fit$x
  ), class = "cm_result")
}

pca_tables <- function(res) {
  list(
    importance = data.frame(
      Component = colnames(res$scores),
      Eigenvalue = signif(res$eigen, 4),
      `SD` = signif(res$sdev, 4),
      `Variance %` = round(100 * res$var_explained, 2),
      `Cumulative %` = round(100 * cumsum(res$var_explained), 2),
      check.names = FALSE
    ),
    loadings = matrix_table(res$rotation),
    scores = signif_df(as.data.frame(res$scores))
  )
}

# ---- NMF --------------------------------------------------------------------

#' NNDSVDa initialisation (Boutsidis & Gallopoulos, 2008): deterministic,
#' fast convergence; zeros are replaced by the data mean so that
#' multiplicative updates can move every entry.
nndsvd_init <- function(V, rank) {
  s <- svd(V, nu = rank, nv = rank)
  W <- matrix(0, nrow(V), rank)
  H <- matrix(0, rank, ncol(V))
  W[, 1] <- sqrt(s$d[1]) * abs(s$u[, 1])
  H[1, ] <- sqrt(s$d[1]) * abs(s$v[, 1])
  if (rank > 1) {
    for (j in 2:rank) {
      u <- s$u[, j]; v <- s$v[, j]
      up <- pmax(u, 0); un <- pmax(-u, 0); vp <- pmax(v, 0); vn <- pmax(-v, 0)
      np <- sqrt(sum(up^2)) * sqrt(sum(vp^2))
      nn <- sqrt(sum(un^2)) * sqrt(sum(vn^2))
      if (np >= nn) {
        a <- up / max(sqrt(sum(up^2)), 1e-12); b <- vp / max(sqrt(sum(vp^2)), 1e-12); m <- np
      } else {
        a <- un / max(sqrt(sum(un^2)), 1e-12); b <- vn / max(sqrt(sum(vn^2)), 1e-12); m <- nn
      }
      W[, j] <- sqrt(s$d[j] * m) * a
      H[j, ] <- sqrt(s$d[j] * m) * b
    }
  }
  avg <- mean(V)
  W[W < 1e-12] <- avg
  H[H < 1e-12] <- avg
  list(W = W, H = H)
}

#' Lee-Seung multiplicative updates minimising the Frobenius norm.
#' Fully vectorised: each iteration is a handful of BLAS matrix products.
nmf_fit <- function(V, rank, max_iter = 400, tol = 1e-5) {
  init <- nndsvd_init(V, rank)
  W <- init$W; H <- init$H
  eps <- 1e-10
  prev <- Inf
  for (i in seq_len(max_iter)) {
    H <- H * crossprod(W, V) / (crossprod(W) %*% H + eps)
    W <- W * tcrossprod(V, H) / (W %*% tcrossprod(H) + eps)
    if (i %% 10 == 0) {
      err <- sum((V - W %*% H)^2)
      if (is.finite(prev) && abs(prev - err) / max(prev, eps) < tol) break
      prev <- err
    }
  }
  list(W = W, H = H, rss = sum((V - W %*% H)^2), iterations = i)
}

run_nmf <- function(data, vars, rank = 2, curve_max_rank = 10, progress = NULL) {
  progress <- progress_or_noop(progress)
  vars <- require_vars(data, intersect(vars, numeric_vars(data)), 2, "numeric variables")
  require_complete(data, vars)
  V <- as.matrix(data[vars])
  if (any(V < 0)) stop("NMF requires non-negative data. Apply min-max scaling in Preprocessing > Transformation.", call. = FALSE)
  rank <- max(1, min(as.integer(rank), ncol(V)))
  tss <- sum((V - mean(V))^2)
  ranks <- sort(unique(c(seq_len(max(1, min(curve_max_rank, ncol(V)))), rank)))
  ev <- stats::setNames(numeric(length(ranks)), ranks)
  best <- NULL
  for (i in seq_along(ranks)) {
    r <- ranks[i]
    fit <- nmf_fit(V, r)
    ev[i] <- 1 - fit$rss / tss
    if (r == rank) best <- fit
    progress(i / length(ranks), sprintf("Rank %d", r))
  }
  comp <- paste0("NMF", seq_len(rank))
  W <- best$W; H <- best$H
  colnames(W) <- comp
  rownames(H) <- comp
  colnames(H) <- vars
  structure(list(
    type = "nmf", vars = vars, rank = rank, W = W, H = H, V = V,
    tss = tss, rss = best$rss, ev = unname(ev[as.character(rank)]),
    ev_curve = data.frame(rank = ranks, ev = unname(ev)), iterations = best$iterations
  ), class = "cm_result")
}

nmf_tables <- function(res) {
  list(
    summary = data.frame(
      Metric = c("Variables", "Observations", "Rank", "Total sum of squares", "Residual sum of squares",
                 "Explained variance %", "Iterations"),
      Value = fmt_values(length(res$vars), nrow(res$W), res$rank, signif(res$tss, 5), signif(res$rss, 5),
                round(100 * res$ev, 2), res$iterations)
    ),
    coefficients = matrix_table(t(res$H)),
    basis = signif_df(as.data.frame(res$W))
  )
}

# K-means clustering with model selection over K.
#
# The distance matrix needed by the silhouette is computed once (on a sample
# for large data) instead of once per run, and the best run for every K is
# kept so the user can inspect any K without recomputing.

run_kmeans <- function(data, vars, k_max = 10, rounds = 10, seed = 123, progress = NULL) {
  progress <- progress_or_noop(progress)
  vars <- require_vars(data, intersect(vars, numeric_vars(data)), 2, "numeric variables")
  require_complete(data, vars)
  raw <- as.matrix(data[vars])
  n <- nrow(raw)
  k_max <- max(2, min(as.integer(k_max), n - 1))

  std <- if (is_standardized(raw)) list(x = raw, center = rep(0, ncol(raw)), scale = rep(1, ncol(raw))) else standardize(raw)
  X <- std$x
  sample_idx <- if (n > CM_LIMITS$silhouette_sample) with_seed(seed, sort(sample.int(n, CM_LIMITS$silhouette_sample))) else seq_len(n)
  d <- stats::dist(X[sample_idx, , drop = FALSE])

  ks <- 2:k_max
  ev <- wss <- sil <- matrix(NA_real_, rounds, length(ks), dimnames = list(NULL, ks))
  models <- vector("list", length(ks))
  names(models) <- ks
  total <- length(ks) * rounds
  step <- 0
  with_seed(seed, {
    for (j in seq_along(ks)) {
      k <- ks[j]
      best <- NULL
      for (r in seq_len(rounds)) {
        km <- tryCatch(suppressWarnings(stats::kmeans(X, centers = k, iter.max = 50)), error = function(e) NULL)
        step <- step + 1
        if (is.null(km)) next
        ev[r, j] <- km$betweenss / km$totss
        wss[r, j] <- km$tot.withinss
        sil[r, j] <- mean(cluster::silhouette(km$cluster[sample_idx], d)[, 3])
        if (is.null(best) || km$tot.withinss < best$tot.withinss) best <- km
      }
      if (!is.null(best)) {
        models[[j]] <- list(cluster = best$cluster, centers = best$centers, size = best$size,
                            withinss = best$withinss, totss = best$totss, betweenss = best$betweenss)
      }
      progress(step / total, sprintf("K = %d", k))
    }
  })
  sil_mean <- colMeans(sil, na.rm = TRUE)
  if (all(is.na(sil_mean))) stop("K-means failed for every K.", call. = FALSE)
  best_k <- ks[which.max(sil_mean)]
  structure(list(
    type = "kmeans", vars = vars, X = X, center = std$center, scale = std$scale,
    sample_idx = sample_idx, ks = ks, ev = ev, wss = wss, sil = sil,
    models = models, best_k = best_k
  ), class = "cm_result")
}

kmeans_silhouette <- function(res, k) {
  m <- res$models[[as.character(k)]]
  idx <- res$sample_idx
  cluster::silhouette(m$cluster[idx], stats::dist(res$X[idx, , drop = FALSE]))
}

kmeans_tables <- function(res, k = res$best_k) {
  m <- res$models[[as.character(k)]]
  centers_raw <- sweep(sweep(m$centers, 2, res$scale, "*"), 2, res$center, "+")
  rownames(centers_raw) <- paste("Cluster", seq_len(nrow(centers_raw)))
  sil_k <- mean(res$sil[, as.character(k)], na.rm = TRUE)
  list(
    summary = data.frame(
      Metric = c("Observations", "Variables", "Clusters (K)", "Best K (silhouette)", "Mean silhouette",
                 "Explained variance %", "Total SS", "Between SS", "Within SS"),
      Value = fmt_values(nrow(res$X), length(res$vars), k, res$best_k, signif(sil_k, 4),
                round(100 * m$betweenss / m$totss, 2), signif(m$totss, 5), signif(m$betweenss, 5),
                signif(sum(m$withinss), 5))
    ),
    clusters = data.frame(Cluster = seq_along(m$size), Size = m$size, `Within SS` = signif(m$withinss, 5),
                          check.names = FALSE),
    centers = matrix_table(centers_raw, "Cluster")
  )
}

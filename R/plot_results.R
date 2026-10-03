# Charts for computed results (statistical tests, PCA, NMF, models).
# All functions take the result object returned by run_*() and are cheap,
# so they can be re-rendered whenever the user changes a display option.

# ---- Statistical tests --------------------------------------------------------

stats_treemap <- function(res, family) {
  df <- res$tests[res$tests$family == family, , drop = FALSE]
  if (!nrow(df)) return(empty_plot("No tests for this family"))
  ok_col <- CM_COLORS$green; ns_col <- CM_COLORS$red; na_col <- CM_COLORS$line
  cats <- unique(df$cat_var)
  nodes <- list(data.frame(id = cats, label = paste("<b>", cats, "</b>"), parent = "",
                           color = CM_COLORS$blue, hover = "", stringsAsFactors = FALSE))
  num_nodes <- unique(df[!is.na(df$num_var), c("cat_var", "num_var")])
  if (nrow(num_nodes)) {
    nodes[[2]] <- data.frame(id = paste(num_nodes$cat_var, num_nodes$num_var, sep = "/"),
                             label = num_nodes$num_var, parent = num_nodes$cat_var,
                             color = CM_COLORS$grid, hover = "", stringsAsFactors = FALSE)
  }
  leaf_parent <- ifelse(is.na(df$num_var), df$cat_var, paste(df$cat_var, df$num_var, sep = "/"))
  leaf_color <- ifelse(df$status != "success", na_col, ifelse(df$significant, ok_col, ns_col))
  hover <- ifelse(
    df$status == "success",
    sprintf("%s<br>statistic: %.4g<br>p: %s<br>p (BH): %s", df$test, df$statistic, format_p(df$p_value), format_p(df$p_adj)),
    paste("Not available:", df$message)
  )
  nodes[[length(nodes) + 1]] <- data.frame(
    id = paste(leaf_parent, df$groups, seq_len(nrow(df)), sep = "/"),
    label = ifelse(is.na(df$groups), "n/a", df$groups), parent = leaf_parent,
    color = leaf_color, hover = hover, stringsAsFactors = FALSE
  )
  tree <- do.call(rbind, nodes)
  p <- plotly::plot_ly(
    type = "treemap", ids = tree$id, labels = tree$label, parents = tree$parent,
    marker = list(colors = tree$color, line = list(color = "#ffffff", width = 2)),
    text = tree$hover, hovertemplate = "<b>%{label}</b><br>%{text}<extra></extra>",
    textinfo = "label", tiling = list(pad = 4), pathbar = list(visible = TRUE)
  )
  title <- sprintf("%s<br><sup>green: significant (BH-adjusted p < %.2f) · rose: not significant · grey: not testable</sup>",
                   names(STAT_FAMILIES)[STAT_FAMILIES == family], res$alpha)
  cm_layout(p, title, margin = list(t = 70, l = 5, r = 5, b = 5))
}

# ---- PCA ------------------------------------------------------------------------

pca_scatter <- function(res, x = "PC1", y = "PC2", group = NULL, group_name = NULL) {
  ve <- stats::setNames(round(100 * res$var_explained, 1), colnames(res$scores))
  plot_scatter(res$scores[, x], res$scores[, y], group, "PCA scores",
               sprintf("%s (%.1f%%)", x, ve[[x]]), sprintf("%s (%.1f%%)", y, ve[[y]]), group_name)
}

pca_scatter3d <- function(res, axes = c("PC1", "PC2", "PC3"), group = NULL, group_name = NULL) {
  if (ncol(res$scores) < 3) return(empty_plot("At least three components are needed"))
  plot_scatter3d(res$scores[, axes[1]], res$scores[, axes[2]], res$scores[, axes[3]], group,
                 "PCA scores (3D)", axes, group_name)
}

pca_biplot <- function(res, x = "PC1", y = "PC2", group = NULL, group_name = NULL) {
  p <- pca_scatter(res, x, y, group, group_name)
  load <- res$rotation[, c(x, y), drop = FALSE]
  k <- 0.8 * max(abs(res$scores[, c(x, y)])) / max(abs(load))
  for (i in seq_len(nrow(load))) {
    p <- plotly::add_annotations(p, x = load[i, 1] * k, y = load[i, 2] * k, ax = 0, ay = 0,
                                 axref = "x", ayref = "y", text = rownames(load)[i], showarrow = TRUE,
                                 arrowhead = 2, arrowwidth = 1.5, arrowcolor = CM_COLORS$highlight,
                                 font = list(color = CM_COLORS$highlight, size = 11))
  }
  plotly::layout(p, title = list(text = "PCA biplot"))
}

pca_scree <- function(res) {
  pcs <- factor(colnames(res$scores), levels = colnames(res$scores))
  plot_curve(pcs, 100 * res$var_explained, "Scree plot", "Component", "Explained variance %",
             mark = knee_point(seq_along(pcs), res$var_explained), mark_label = "Elbow", bars = TRUE)
}

pca_kaiser <- function(res) {
  pcs <- factor(colnames(res$scores), levels = colnames(res$scores))
  plot_curve(pcs, res$eigen, "Eigenvalues (Kaiser criterion: keep > 1)", "Component", "Eigenvalue",
             threshold = if (res$scale) 1 else mean(res$eigen), bars = TRUE)
}

pca_cumulative <- function(res, threshold = 80) {
  cum <- 100 * cumsum(res$var_explained)
  plot_curve(seq_along(cum), cum, "Cumulative explained variance", "Number of components",
             "Cumulative variance %", threshold = threshold, mark = which(cum >= threshold)[1],
             mark_label = sprintf("%d%% reached", threshold))
}

pca_loadings <- function(res) {
  plot_heatmap(res$rotation, title = "Loadings", xlab = "Component", ylab = "Variable",
               zmin = -1, zmax = 1)
}

# ---- NMF ------------------------------------------------------------------------

nmf_scatter <- function(res, x = "NMF1", y = "NMF2", group = NULL, group_name = NULL) {
  if (res$rank < 2) return(empty_plot("Rank must be at least 2"))
  plot_scatter(res$W[, x], res$W[, y], group, "NMF basis (W)", x, y, group_name)
}

nmf_scatter3d <- function(res, axes = c("NMF1", "NMF2", "NMF3"), group = NULL, group_name = NULL) {
  if (res$rank < 3) return(empty_plot("Rank must be at least 3"))
  plot_scatter3d(res$W[, axes[1]], res$W[, axes[2]], res$W[, axes[3]], group, "NMF basis (3D)", axes, group_name)
}

nmf_fitted <- function(res, var = res$vars[1], group = NULL, group_name = NULL) {
  j <- match(var, res$vars)
  fitted <- drop(res$W %*% res$H[, j])
  p <- plot_scatter(res$V[, j], fitted, group, paste("Original vs reconstructed:", var), "Original", "Reconstructed", group_name)
  r <- range(c(res$V[, j], fitted))
  plotly::add_trace(p, x = r, y = r, type = "scatter", mode = "lines", name = "Identity",
                    line = list(color = CM_COLORS$highlight, dash = "dash"), hoverinfo = "skip")
}

nmf_ev_curve <- function(res) {
  plot_curve(res$ev_curve$rank, 100 * res$ev_curve$ev, "Explained variance by rank", "Rank",
             "Explained variance %", threshold = 80, mark = match(res$rank, res$ev_curve$rank),
             mark_label = "Selected rank")
}

nmf_heatmap_W <- function(res) {
  plot_heatmap(res$W, y = as.character(seq_len(nrow(res$W))), title = "Basis matrix W",
               xlab = "Component", ylab = "Observation", colorscale = CM_SEQUENTIAL, show_values = FALSE)
}

nmf_heatmap_H <- function(res) {
  plot_heatmap(res$H, title = "Coefficient matrix H", xlab = "Variable", ylab = "Component",
               colorscale = CM_SEQUENTIAL)
}

# ---- Linear regression ----------------------------------------------------------

lm_fit_plot <- function(res, group = NULL, group_name = NULL) {
  m <- res$model
  y <- stats::model.response(stats::model.frame(m))
  if (length(res$predictors) == 1) {
    x <- stats::model.frame(m)[[2]]
    p <- plot_scatter(x, y, group, "Regression line", res$predictors, res$response, group_name)
    grid <- data.frame(seq(min(x), max(x), length.out = 100))
    names(grid) <- res$predictors
    ci <- stats::predict(m, newdata = grid, interval = "confidence")
    p <- plotly::add_ribbons(p, x = grid[[1]], ymin = ci[, "lwr"], ymax = ci[, "upr"], name = "95% CI",
                             fillcolor = cm_alpha(CM_COLORS$highlight, 0.18), line = list(color = "transparent"),
                             hoverinfo = "skip")
    plotly::add_lines(p, x = grid[[1]], y = ci[, "fit"], name = "Fit",
                      line = list(color = CM_COLORS$highlight, width = 3), hoverinfo = "skip")
  } else {
    fit <- stats::fitted(m)
    p <- plot_scatter(fit, y, group, "Observed vs predicted", "Predicted", res$response, group_name)
    r <- range(c(fit, y))
    plotly::add_trace(p, x = r, y = r, type = "scatter", mode = "lines", name = "Identity",
                      line = list(color = CM_COLORS$highlight, dash = "dash", width = 2), hoverinfo = "skip")
  }
}

with_trend <- function(p, x, y) {
  lw <- stats::lowess(x, y)
  plotly::add_lines(p, x = lw$x, y = lw$y, name = "Trend", line = list(color = CM_COLORS$highlight, width = 2.5),
                    hoverinfo = "skip")
}

lm_residuals <- function(res, group = NULL, group_name = NULL) {
  f <- stats::fitted(res$model); r <- stats::residuals(res$model)
  with_trend(plot_scatter(f, r, group, "Residuals vs fitted", "Fitted", "Residual", group_name), f, r)
}

lm_qq <- function(res, group = NULL, group_name = NULL) {
  r <- stats::rstandard(res$model)
  q <- stats::qqnorm(r, plot.it = FALSE)
  p <- plot_scatter(q$x, q$y, group, "Normal Q-Q", "Theoretical quantiles", "Standardised residuals", group_name)
  lim <- range(q$x)
  plotly::add_trace(p, x = lim, y = lim, type = "scatter", mode = "lines", name = "Reference",
                    line = list(color = CM_COLORS$highlight, dash = "dash"), hoverinfo = "skip")
}

lm_scale_location <- function(res, group = NULL, group_name = NULL) {
  f <- stats::fitted(res$model); s <- sqrt(abs(stats::rstandard(res$model)))
  with_trend(plot_scatter(f, s, group, "Scale-location", "Fitted", "sqrt(|standardised residual|)", group_name), f, s)
}

lm_leverage <- function(res, group = NULL, group_name = NULL) {
  h <- stats::hatvalues(res$model); r <- stats::rstandard(res$model)
  cook <- stats::cooks.distance(res$model)
  p <- plot_scatter(h, r, group, "Residuals vs leverage", "Leverage", "Standardised residuals", group_name,
                    text = sprintf("Cook's D: %.3g", cook))
  with_trend(p, h, r)
}

lm_selection <- function(res, top = 25) {
  means <- colMeans(res$scores)
  keep <- utils::head(order(-means), top)
  mat <- res$scores[, keep, drop = FALSE]
  colnames(mat) <- vapply(colnames(mat), function(s) if (nchar(s) > 40) paste0(substr(s, 1, 37), "...") else s, "")
  plot_metric_boxes(mat, best = 1, title = sprintf("Cross-validated adjusted R² (top %d subsets)", length(keep)),
                    ylab = "Adjusted R²")
}

# ---- K-means --------------------------------------------------------------------

kmeans_scatter <- function(res, k, x, y) {
  m <- res$models[[as.character(k)]]
  i <- match(c(x, y), res$vars)
  p <- plot_scatter(res$X[, i[1]], res$X[, i[2]], paste("Cluster", m$cluster), sprintf("K-means (K = %d)", k),
                    paste(x, "(standardised)"), paste(y, "(standardised)"), "Cluster")
  plotly::add_trace(p, x = m$centers[, i[1]], y = m$centers[, i[2]], type = "scatter", mode = "markers",
                    name = "Centroids", marker = list(symbol = "x", size = 14, color = CM_COLORS$ink,
                                                      line = list(width = 2)))
}

kmeans_scatter3d <- function(res, k, axes) {
  if (length(res$vars) < 3) return(empty_plot("At least three variables are needed"))
  m <- res$models[[as.character(k)]]
  i <- match(axes, res$vars)
  plot_scatter3d(res$X[, i[1]], res$X[, i[2]], res$X[, i[3]], paste("Cluster", m$cluster),
                 sprintf("K-means (K = %d)", k), axes, "Cluster")
}

kmeans_silhouette_plot <- function(res, k) {
  s <- kmeans_silhouette(res, k)
  df <- data.frame(cluster = s[, 1], width = s[, 3])
  df <- df[order(df$cluster, -df$width), ]
  df$pos <- seq_len(nrow(df))
  pal <- cm_palette(sort(unique(df$cluster)))
  p <- plotly::plot_ly()
  for (cl in names(pal)) {
    sel <- df$cluster == as.integer(cl)
    p <- plotly::add_bars(p, x = df$pos[sel], y = df$width[sel], name = paste("Cluster", cl),
                          marker = list(color = pal[[cl]]), hovertemplate = "width: %{y:.3f}<extra>%{fullData.name}</extra>")
  }
  avg <- mean(df$width)
  p <- plotly::add_lines(p, x = range(df$pos), y = rep(avg, 2), name = sprintf("Mean %.3f", avg),
                         line = list(color = CM_COLORS$highlight, dash = "dash"))
  sub <- if (length(res$sample_idx) < nrow(res$X)) sprintf(" (sample of %d)", length(res$sample_idx)) else ""
  cm_layout(p, paste0("Silhouette (K = ", k, ")", sub), "Observation", "Silhouette width", bargap = 0)
}

kmeans_silhouette_by_k <- function(res) {
  plot_metric_boxes(res$sil, best = match(res$best_k, res$ks), "Mean silhouette by K", "K", "Mean silhouette")
}

kmeans_wss <- function(res) {
  w <- colMeans(res$wss, na.rm = TRUE)
  plot_curve(res$ks, w, "Elbow method", "K", "Within-cluster SS", mark = knee_point(res$ks, w),
             mark_label = "Elbow", bars = TRUE)
}

kmeans_ev <- function(res) {
  e <- 100 * colMeans(res$ev, na.rm = TRUE)
  plot_curve(res$ks, e, "Explained variance (between SS / total SS)", "K", "Explained variance %",
             mark = knee_point(res$ks, e), mark_label = "Knee")
}

# ---- k-NN -----------------------------------------------------------------------

knn_boundary <- function(res, x, y) {
  i <- match(c(x, y), res$vars)
  n <- CM_LIMITS$boundary_grid
  pad <- 0.5
  gx <- seq(min(res$X[, i[1]]) - pad, max(res$X[, i[1]]) + pad, length.out = n)
  gy <- seq(min(res$X[, i[2]]) - pad, max(res$X[, i[2]]) + pad, length.out = n)
  grid <- matrix(0, n * n, ncol(res$X))        # other predictors fixed at their mean (0 once standardised)
  grid[, i[1]] <- rep(gx, times = n)
  grid[, i[2]] <- rep(gy, each = n)
  pred <- knn_predict(res, grid)
  codes <- matrix(as.integer(pred), n, n, byrow = TRUE)
  lv <- res$levels
  pal <- cm_palette(lv)
  k <- max(length(lv) - 1, 1)
  scale <- if (length(lv) > 1) lapply(seq_along(lv), function(j) list((j - 1) / k, cm_alpha(pal[[j]], 0.35)))
           else list(list(0, cm_alpha(pal[[1]], 0.35)), list(1, cm_alpha(pal[[1]], 0.35)))
  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, x = gx, y = gy, z = codes, type = "heatmap", colorscale = scale, zmin = 1,
                         zmax = length(lv), showscale = FALSE, hoverinfo = "skip")
  g <- as.character(res$y)
  for (lv_i in lv) {
    sel <- g == lv_i
    p <- plotly::add_trace(p, x = res$X[sel, i[1]], y = res$X[sel, i[2]], type = "scatter", mode = "markers",
                           name = lv_i, marker = marker_style(pal[[lv_i]], size = 6, alpha = 0.9),
                           hovertemplate = "%{x:.3g}, %{y:.3g}<extra>%{fullData.name}</extra>")
  }
  others <- if (ncol(res$X) > 2) " — other predictors at their mean" else ""
  cm_layout(p, sprintf("Decision regions (k = %d)%s", res$k, others), paste(x, "(standardised)"),
            paste(y, "(standardised)"), legend_title = res$target, showgrid = FALSE)
}

knn_accuracy_plot <- function(res) {
  plot_metric_boxes(res$accuracy, best = res$k, "Validation accuracy by k", "k", "Accuracy")
}

knn_kappa_plot <- function(res) {
  plot_metric_boxes(res$kappa, best = res$k, "Validation Cohen's kappa by k", "k", "Kappa")
}

# ---- Random forest --------------------------------------------------------------

rf_importance <- function(res) {
  imp <- res$model$variable.importance
  plot_hbar(100 * imp / sum(imp), names(imp), "Variable importance (impurity)", "Relative importance %")
}

rf_learning_curve <- function(res, mtry = res$mtry) {
  g <- res$grid[res$grid$mtry == mtry, ]
  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, x = g$trees, y = g$train_loss, type = "scatter", mode = "lines+markers", name = "Training",
                         line = list(color = CM_COLORS$blue_dark, width = 2.5), marker = list(color = CM_COLORS$blue_dark))
  p <- plotly::add_trace(p, x = g$trees, y = g$valid_loss, type = "scatter", mode = "lines+markers", name = "Validation",
                         line = list(color = CM_COLORS$red, width = 2.5), marker = list(color = CM_COLORS$red))
  cm_layout(p, sprintf("Learning curve (mtry = %d)", mtry), "Number of trees", "Log loss")
}

rf_accuracy_heatmap <- function(res) {
  z <- matrix(res$grid$accuracy, nrow = length(res$mtry_grid), dimnames = list(res$mtry_grid, res$tree_grid))
  p <- plot_heatmap(z, x = as.character(res$tree_grid), y = as.character(res$mtry_grid),
                    title = sprintf("Mean validation accuracy (selected: mtry = %d, trees = %d)", res$mtry, res$trees),
                    xlab = "Trees", ylab = "mtry", colorscale = CM_SEQUENTIAL, value_format = ".3f")
  plotly::layout(p, yaxis = list(autorange = TRUE))
}

#' Sunburst view of one tree of the final forest (depth-limited for readability).
rf_tree_plot <- function(res, tree = 1, depth = 5) {
  tree <- max(1, min(as.integer(tree), res$model$num.trees))
  info <- ranger::treeInfo(res$model, tree)
  parent <- rep(NA_integer_, nrow(info))
  is_split <- !info$terminal
  parent[info$leftChild[is_split] + 1] <- info$nodeID[is_split]
  parent[info$rightChild[is_split] + 1] <- info$nodeID[is_split]
  side <- rep("", nrow(info))
  side[info$leftChild[is_split] + 1] <- "≤"
  side[info$rightChild[is_split] + 1] <- ">"
  pidx <- parent + 1
  edge <- ifelse(is.na(parent), "Root",
                 sprintf("%s %s %s", info$splitvarName[pidx], side, signif(info$splitval[pidx], 3)))
  pred_cols <- grep("^pred\\.", names(info), value = TRUE)
  leaf_txt <- if (length(pred_cols)) {
    probs <- as.matrix(info[pred_cols])
    paste0("→ ", sub("^pred\\.", "", pred_cols)[max.col(probs, ties.method = "first")])
  } else paste("→", info$prediction)
  label <- ifelse(info$terminal, paste(edge, leaf_txt, sep = "<br>"), edge)
  colors <- ifelse(info$terminal, CM_COLORS$green, cm_palette(res$vars)[info$splitvarName])
  colors[is.na(colors)] <- CM_COLORS$blue
  p <- plotly::plot_ly(
    type = "sunburst", ids = info$nodeID, parents = ifelse(is.na(parent), "", parent), labels = label,
    marker = list(colors = colors, line = list(color = "#ffffff", width = 1)), maxdepth = depth,
    insidetextorientation = "radial", hovertemplate = "%{label}<extra></extra>"
  )
  cm_layout(p, sprintf("Tree %d of %d (click a node to zoom)", tree, res$model$num.trees))
}

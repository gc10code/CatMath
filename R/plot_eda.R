# Exploratory charts. They are built on demand (only the chart being viewed
# is computed) instead of pre-computing every variable combination.

EDA_CHARTS <- c(
  "Correlation matrix" = "cor",
  "Box plot" = "box",
  "Violin plot" = "violin",
  "Density plot" = "density",
  "Histogram" = "hist",
  "Scatter-plot matrix" = "splom",
  "Parallel coordinates" = "parcoords",
  "Category frequencies" = "bar",
  "Missing values" = "missing"
)

eda_correlation <- function(df, vars, method = "pearson") {
  if (length(vars) < 2) return(empty_plot("Select at least two numeric variables"))
  cm <- stats::cor(df[vars], use = "pairwise.complete.obs", method = method)
  plot_heatmap(cm, title = sprintf("Correlation matrix (%s)", method), zmin = -1, zmax = 1)
}

# Box / violin for several numeric variables, optionally split by group.
eda_distribution <- function(df, vars, group = NULL, type = c("box", "violin")) {
  type <- match.arg(type)
  if (!length(vars)) return(empty_plot("Select at least one numeric variable"))
  n <- nrow(df)
  var_x <- factor(rep(vars, each = n), levels = vars)
  values <- unlist(df[vars], use.names = FALSE)
  p <- plotly::plot_ly()
  add <- function(p, sel, name, color) {
    args <- list(p, x = var_x[sel], y = values[sel], type = type, name = name,
                 line = list(color = color), fillcolor = cm_alpha(color, 0.35))
    if (type == "box") args$marker <- list(color = color, size = 3, opacity = 0.5)
    else args$meanline <- list(visible = TRUE)
    do.call(plotly::add_trace, args)
  }
  if (is.null(group)) {
    p <- add(p, rep(TRUE, length(values)), "All", CM_COLORS$blue_dark)
  } else {
    g <- rep(group_labels(df[[group]]), times = length(vars))
    pal <- cm_palette(sort(unique(g)))
    for (lv in names(pal)) p <- add(p, g == lv, lv, pal[[lv]])
  }
  p <- cm_layout(p, if (type == "box") "Box plot" else "Violin plot", "Variable", "Value", legend_title = group)
  if (type == "box") return(plotly::layout(p, boxmode = "group"))
  # violinmode is valid plotly.js but missing from the R schema: set it client-side.
  htmlwidgets::onRender(p, "function(el) { Plotly.relayout(el, {violinmode: 'group'}); }")
}

eda_density <- function(df, var, group = NULL) {
  if (is.null(var)) return(empty_plot("Select a numeric variable"))
  x <- df[[var]]
  p <- plotly::plot_ly()
  add <- function(p, v, name, color) {
    v <- v[!is.na(v)]
    if (length(v) < 2) return(p)
    d <- stats::density(v)
    plotly::add_trace(p, x = d$x, y = d$y, type = "scatter", mode = "lines", name = name,
                      fill = "tozeroy", fillcolor = cm_alpha(color, 0.3), line = list(color = color, width = 2),
                      hovertemplate = paste0(name, "<br>value: %{x:.4g}<br>density: %{y:.4g}<extra></extra>"))
  }
  if (is.null(group)) {
    p <- add(p, x, var, CM_COLORS$blue_dark)
  } else {
    g <- group_labels(df[[group]])
    pal <- cm_palette(sort(unique(g)))
    for (lv in names(pal)) p <- add(p, x[g == lv], lv, pal[[lv]])
  }
  cm_layout(p, paste("Density of", var), var, "Density", legend_title = group)
}

eda_histogram <- function(df, var, group = NULL) {
  if (is.null(var)) return(empty_plot("Select a numeric variable"))
  x <- df[[var]]
  p <- plotly::plot_ly()
  if (is.null(group)) {
    p <- plotly::add_histogram(p, x = x, name = var, marker = list(color = cm_alpha(CM_COLORS$blue_dark, 0.7),
                                                                    line = list(color = CM_COLORS$blue_dark, width = 1)))
  } else {
    g <- group_labels(df[[group]])
    pal <- cm_palette(sort(unique(g)))
    for (lv in names(pal)) {
      p <- plotly::add_histogram(p, x = x[g == lv], name = lv, marker = list(color = cm_alpha(pal[[lv]], 0.6)))
    }
  }
  cm_layout(p, paste("Histogram of", var), var, "Count", legend_title = group, barmode = "overlay")
}

eda_splom <- function(df, vars, group = NULL) {
  if (length(vars) < 2) return(empty_plot("Select at least two numeric variables"))
  vars <- utils::head(vars, CM_LIMITS$splom_max_vars)
  dims <- lapply(vars, function(v) list(label = v, values = df[[v]]))
  p <- plotly::plot_ly()
  if (is.null(group)) {
    p <- plotly::add_trace(p, type = "splom", dimensions = dims, name = "Observations",
                           marker = marker_style(CM_COLORS$blue_dark, size = 4, alpha = 0.5), diagonal = list(visible = FALSE))
  } else {
    g <- group_labels(df[[group]])
    pal <- cm_palette(sort(unique(g)))
    for (lv in names(pal)) {
      sel <- g == lv
      d <- lapply(vars, function(v) list(label = v, values = df[[v]][sel]))
      p <- plotly::add_trace(p, type = "splom", dimensions = d, name = lv,
                             marker = marker_style(pal[[lv]], size = 4, alpha = 0.55), diagonal = list(visible = FALSE))
    }
  }
  title <- if (length(vars) == CM_LIMITS$splom_max_vars) sprintf("Scatter-plot matrix (first %d variables)", length(vars)) else "Scatter-plot matrix"
  cm_layout(p, title, legend_title = group, dragmode = "select")
}

eda_parcoords <- function(df, vars, group = NULL) {
  if (length(vars) < 2) return(empty_plot("Select at least two numeric variables"))
  dims <- lapply(vars, function(v) list(label = v, values = df[[v]], range = range(df[[v]], na.rm = TRUE)))
  if (is.null(group)) {
    line <- list(color = CM_COLORS$blue_dark)
    p <- plotly::plot_ly(type = "parcoords", line = line, dimensions = dims)
  } else {
    g <- group_labels(df[[group]])
    lv <- sort(unique(g))
    pal <- cm_palette(lv)
    codes <- match(g, lv) - 1
    k <- max(length(lv) - 1, 1)
    scale <- lapply(seq_along(lv), function(i) list((i - 1) / k, pal[[i]]))
    if (length(lv) == 1) scale <- list(list(0, pal[[1]]), list(1, pal[[1]]))
    p <- plotly::plot_ly(type = "parcoords", dimensions = dims,
                         line = list(color = codes, colorscale = scale, cmin = 0, cmax = k, showscale = FALSE))
    for (i in seq_along(lv)) {   # invisible traces only to draw a legend
      p <- plotly::add_trace(p, type = "scatter", mode = "markers", x = list(NULL), y = list(NULL),
                             name = lv[i], marker = list(color = pal[[i]], size = 10), inherit = FALSE)
    }
  }
  cm_layout(p, "Parallel coordinates", legend_title = group,
            xaxis = list(visible = FALSE), yaxis = list(visible = FALSE), margin = list(t = 80))
}

eda_category_bars <- function(df, vars) {
  if (!length(vars)) return(empty_plot("The data has no categorical variables"))
  p <- plotly::plot_ly()
  for (v in vars) {
    counts <- sort(table(group_labels(df[[v]])), decreasing = TRUE)
    pal <- cm_palette(names(counts))
    p <- plotly::add_trace(p, x = rep(v, length(counts)), y = as.vector(counts), type = "bar",
                           name = names(counts), text = names(counts), showlegend = FALSE,
                           marker = list(color = cm_alpha(pal, 0.75), line = list(color = pal, width = 1)),
                           hovertemplate = paste0(v, "<br>%{text}: %{y}<extra></extra>"))
  }
  cm_layout(p, "Category frequencies", "Variable", "Count", barmode = "stack")
}

eda_missing <- function(df) {
  na <- colSums(is.na(df))
  if (sum(na) == 0) return(empty_plot("No missing values in the dataset"))
  na <- na[na > 0]
  plot_hbar(100 * na / nrow(df), names(na), "Missing values by variable", "Missing %")
}

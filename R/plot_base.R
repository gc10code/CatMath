# Plotly building blocks shared by every chart (theme + generic chart types).

cm_alpha <- function(hex, alpha = 0.6) {
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1, ], rgb[2, ], rgb[3, ], alpha)
}

cm_palette <- function(levels) {
  levels <- as.character(levels)
  stats::setNames(rep_len(CM_QUALITATIVE, length(levels)), levels)
}

group_labels <- function(g) {
  g <- as.character(g)
  g[is.na(g)] <- "(missing)"
  g
}

cm_axis <- function(title = NULL, showgrid = TRUE, ...) {
  utils::modifyList(list(
    title = list(text = title, standoff = 10, font = list(color = CM_COLORS$ink)), showgrid = showgrid, gridcolor = CM_COLORS$grid,
    zerolinecolor = CM_COLORS$line, linecolor = CM_COLORS$line, ticks = "", automargin = TRUE
  ), list(...))
}

cm_layout <- function(p, title = NULL, xlab = NULL, ylab = NULL, legend_title = NULL,
                      showgrid = TRUE, xaxis = list(), yaxis = list(), ...) {
  p <- plotly::layout(
    p,
    title = list(text = title, x = 0.01, xanchor = "left",
                 font = list(family = CM_FONT_TITLE, size = 16, color = CM_COLORS$ink)),
    font = list(family = CM_FONT, size = 12, color = CM_COLORS$ink_soft),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    colorway = CM_QUALITATIVE,
    xaxis = utils::modifyList(cm_axis(xlab, showgrid), xaxis),
    yaxis = utils::modifyList(cm_axis(ylab, showgrid), yaxis),
    legend = list(title = list(text = legend_title, font = list(color = CM_COLORS$ink)),
                  bgcolor = "rgba(255,255,255,0)", borderwidth = 0, itemsizing = "constant"),
    margin = list(l = 60, r = 20, t = 50, b = 50),
    modebar = list(bgcolor = "rgba(255,255,255,0)", color = "#b6bccb", activecolor = CM_COLORS$blue_dark),
    hoverlabel = list(bgcolor = "rgba(255,255,255,0.96)", bordercolor = CM_COLORS$line,
                      font = list(family = CM_FONT, color = CM_COLORS$ink)),
    ...
  )
  plotly::config(
    p, displaylogo = FALSE,
    modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
    toImageButtonOptions = list(format = "png", scale = 2)
  )
}

# Pastel fill with a thin white outline: the signature marker of the app.
marker_style <- function(color, size = 8, alpha = 0.8) {
  list(color = cm_alpha(color, alpha), size = size, line = list(color = "#ffffff", width = 1))
}

#' Scatter plot, optionally coloured by a grouping vector (one trace per level
#' so legends and click-to-hide work).
plot_scatter <- function(x, y, group = NULL, title = NULL, xlab = NULL, ylab = NULL,
                         legend_title = NULL, text = NULL) {
  type <- if (length(x) > CM_LIMITS$scattergl_from) "scattergl" else "scatter"
  hover <- paste0(xlab %||% "x", ": %{x:.4g}<br>", ylab %||% "y", ": %{y:.4g}",
                  if (!is.null(text)) "<br>%{text}" else "", "<extra>%{fullData.name}</extra>")
  p <- plotly::plot_ly()
  if (is.null(group)) {
    p <- plotly::add_trace(p, x = x, y = y, type = type, mode = "markers", name = "Observations",
                           marker = marker_style(CM_COLORS$blue_dark), text = text, hovertemplate = hover)
  } else {
    g <- group_labels(group)
    pal <- cm_palette(sort(unique(g)))
    for (lv in names(pal)) {
      sel <- g == lv
      p <- plotly::add_trace(p, x = x[sel], y = y[sel], type = type, mode = "markers", name = lv,
                             marker = marker_style(pal[[lv]]), text = text[sel], hovertemplate = hover)
    }
  }
  cm_layout(p, title, xlab, ylab, legend_title)
}

plot_scatter3d <- function(x, y, z, group = NULL, title = NULL, labels = c("x", "y", "z"), legend_title = NULL) {
  p <- plotly::plot_ly()
  add <- function(p, sel, name, color) {
    plotly::add_trace(p, x = x[sel], y = y[sel], z = z[sel], type = "scatter3d", mode = "markers",
                      name = name, marker = marker_style(color, size = 4, alpha = 0.75),
                      hovertemplate = paste0(labels[1], ": %{x:.4g}<br>", labels[2], ": %{y:.4g}<br>",
                                             labels[3], ": %{z:.4g}<extra>%{fullData.name}</extra>"))
  }
  if (is.null(group)) {
    p <- add(p, rep(TRUE, length(x)), "Observations", CM_COLORS$blue_dark)
  } else {
    g <- group_labels(group)
    pal <- cm_palette(sort(unique(g)))
    for (lv in names(pal)) p <- add(p, g == lv, lv, pal[[lv]])
  }
  p <- cm_layout(p, title, legend_title = legend_title)
  plotly::layout(p, scene = list(
    xaxis = list(title = labels[1]), yaxis = list(title = labels[2]), zaxis = list(title = labels[3])
  ))
}

plot_heatmap <- function(z, x = colnames(z), y = rownames(z), title = NULL, xlab = NULL, ylab = NULL,
                         colorscale = CM_DIVERGING, zmin = NULL, zmax = NULL, show_values = NULL,
                         value_format = ".2f") {
  show_values <- show_values %||% (length(z) <= 400)
  p <- plotly::plot_ly(
    x = x, y = y, z = z, type = "heatmap", colorscale = colorscale, zmin = zmin, zmax = zmax,
    texttemplate = if (show_values) paste0("%{z:", value_format, "}") else NULL,
    hovertemplate = paste0("%{x}<br>%{y}<br>value: %{z:.4g}<extra></extra>")
  )
  cm_layout(p, title, xlab, ylab, showgrid = FALSE,
            yaxis = list(autorange = "reversed", type = "category"), xaxis = list(type = "category"))
}

#' One box per column of `mat`; the column `best` is highlighted.
plot_metric_boxes <- function(mat, best = NULL, title = NULL, xlab = NULL, ylab = NULL) {
  labels <- colnames(mat) %||% as.character(seq_len(ncol(mat)))
  x <- factor(rep(labels, each = nrow(mat)), levels = labels)
  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, x = x, y = as.vector(mat), type = "box", name = "All",
                         fillcolor = cm_alpha(CM_COLORS$base, 0.35), line = list(color = CM_COLORS$base),
                         marker = list(color = CM_COLORS$base, size = 4), boxpoints = FALSE)
  if (!is.null(best)) {
    p <- plotly::add_trace(p, x = factor(rep(labels[best], nrow(mat)), levels = labels), y = mat[, best],
                           type = "box", name = "Selected", fillcolor = cm_alpha(CM_COLORS$highlight, 0.45),
                           line = list(color = CM_COLORS$highlight), marker = list(color = CM_COLORS$highlight),
                           boxpoints = FALSE)
  }
  cm_layout(p, title, xlab, ylab, showlegend = FALSE, boxmode = "overlay",
            xaxis = list(type = "category", categoryorder = "array", categoryarray = labels))
}

#' Line chart with optional filled area, horizontal threshold and highlighted point.
plot_curve <- function(x, y, title = NULL, xlab = NULL, ylab = NULL, threshold = NULL,
                       mark = NULL, mark_label = "Selected", fill = TRUE, bars = FALSE) {
  p <- plotly::plot_ly()
  if (bars) {
    p <- plotly::add_trace(p, x = x, y = y, type = "bar", name = ylab,
                           marker = list(color = cm_alpha(CM_COLORS$blue, 0.55), line = list(color = CM_COLORS$blue_dark, width = 1)),
                           hovertemplate = "%{x}: %{y:.4g}<extra></extra>")
  }
  p <- plotly::add_trace(p, x = x, y = y, type = "scatter", mode = "lines+markers", name = ylab,
                         fill = if (fill && !bars) "tozeroy" else "none", fillcolor = cm_alpha(CM_COLORS$blue, 0.25),
                         line = list(color = CM_COLORS$blue_dark, width = 2.5), marker = list(color = CM_COLORS$blue_dark, size = 7),
                         hovertemplate = "%{x}: %{y:.4g}<extra></extra>")
  if (!is.null(threshold)) {
    p <- plotly::add_trace(p, x = x[c(1, length(x))], y = rep(threshold, 2), type = "scatter", mode = "lines",
                           name = "Threshold", line = list(color = CM_COLORS$highlight, dash = "dash", width = 2),
                           hoverinfo = "skip")
  }
  if (!is.null(mark) && !is.na(mark)) {
    p <- plotly::add_trace(p, x = x[mark], y = y[mark], type = "scatter", mode = "markers", name = mark_label,
                           marker = list(color = CM_COLORS$highlight, size = 14, symbol = "circle-open", line = list(width = 3)),
                           hovertemplate = paste0(mark_label, ": %{x}<extra></extra>"))
  }
  cm_layout(p, title, xlab, ylab, showlegend = FALSE)
}

#' Confusion matrix: cells coloured by share of the actual class
#' (green = correct, red = misclassified), labelled with counts.
plot_confusion <- function(conf, title = "Confusion matrix") {
  conf <- as.matrix(conf)
  share <- conf / pmax(rowSums(conf), 1)
  z <- 0.5 - share / 2
  diag(z) <- 0.5 + diag(share) / 2
  scale <- list(list(0, CM_COLORS$red), list(0.5, "#f7f8fa"), list(1, CM_COLORS$green))
  p <- plotly::plot_ly(
    x = colnames(conf), y = rownames(conf), z = z, type = "heatmap", colorscale = scale,
    zmin = 0, zmax = 1, showscale = FALSE, text = conf, customdata = round(100 * share, 1),
    texttemplate = "%{text}",
    hovertemplate = "Actual: %{y}<br>Predicted: %{x}<br>Count: %{text} (%{customdata}%)<extra></extra>"
  )
  cm_layout(p, title, "Predicted", "Actual", showgrid = FALSE,
            xaxis = list(type = "category"), yaxis = list(type = "category", autorange = "reversed"))
}

plot_hbar <- function(values, labels, title = NULL, xlab = NULL) {
  ord <- order(values)
  p <- plotly::plot_ly(
    x = values[ord], y = factor(labels[ord], levels = labels[ord]), type = "bar", orientation = "h",
    marker = list(color = cm_alpha(CM_COLORS$blue_dark, 0.7), line = list(color = CM_COLORS$blue_dark, width = 1)),
    hovertemplate = "%{y}: %{x:.4g}<extra></extra>"
  )
  cm_layout(p, title, xlab, NULL)
}

empty_plot <- function(message) {
  p <- plotly::plot_ly(type = "scatter", mode = "markers")
  plotly::layout(
    p, xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
    annotations = list(list(text = message, showarrow = FALSE, font = list(size = 15, color = CM_COLORS$ink_soft))),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
  )
}

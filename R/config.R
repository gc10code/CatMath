# Global configuration: palette, plot defaults and computational limits.
# Only constants live here; every other file defines functions.

CM_COLORS <- list(
  ink = "#3d4559", ink_soft = "#6b7389", line = "#e6e9f0", grid = "#eef0f5",
  paper = "#fbfaf8", panel = "#ffffff",
  blue = "#7aa2c8", blue_dark = "#5b84ad", rose = "#e3a0a7", sage = "#94c4a6",
  peach = "#f0b98a", lavender = "#b9a5d9", butter = "#ecd391",
  red = "#d98a93", green = "#8fc3a0", base = "#a3abbd", highlight = "#d9798a"
)

# Soft pastel palette shared by the whole app: charts, tables and CSS use the
# same tones so every view feels consistent.

# Qualitative palette for categorical levels (recycled when there are more levels).
CM_QUALITATIVE <- c(
  "#7aa2c8", "#e3a0a7", "#94c4a6", "#f0b98a", "#b9a5d9", "#ecd391",
  "#86c5c7", "#d9a3cc", "#a9bd8f", "#9fb3e6", "#cfae92", "#8fa9b8"
)

# Diverging colour scale (negative -> neutral -> positive) for heatmaps.
CM_DIVERGING <- list(list(0, "#7aa2c8"), list(0.5, "#fbf7f2"), list(1, "#d9798a"))
CM_SEQUENTIAL <- list(list(0, "#fbf7f2"), list(0.5, "#c9d9ea"), list(1, "#6f93bd"))

CM_FONT <- "Nunito, 'Segoe UI', Roboto, Arial, sans-serif"
CM_FONT_TITLE <- "Quicksand, Nunito, 'Segoe UI', sans-serif"

CM_PLOT_HEIGHT <- 520

CM_LIMITS <- list(
  max_upload_mb = 500,
  preview_rows = 5000,         # rows sent to the browser in data previews
  lm_exhaustive_max = 12,      # above this, best-subset search becomes forward selection
  splom_max_vars = 10,
  silhouette_sample = 3000,    # silhouette is O(n^2): computed on a sample above this size
  normality_sample = 5000,     # Shapiro-Wilk upper bound
  knn_chunk = 1000,            # test rows per distance block in k-NN
  boundary_grid = 120,         # k-NN decision boundary resolution
  scattergl_from = 5000        # switch to WebGL scatter above this number of points
)

CM_RESULT_KEYS <- c("stats", "pca", "nmf", "lm", "kmeans", "knn", "rf")

CM_EXAMPLES <- c(
  "Iris (flowers)" = "iris.csv",
  "Titanic (passengers)" = "titanic.csv",
  "Mice protein expression" = "mice_protein.csv"
)

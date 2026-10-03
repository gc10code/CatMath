# Install the packages CatMath needs (run once): source("install.R")
pkgs <- c(
  "shiny", "shinydashboard", "shinyWidgets", "waiter", "plotly", "htmlwidgets",
  "reactable", "data.table", "ranger", "cluster", "mice", "testthat"
)
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing)

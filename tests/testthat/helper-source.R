# Source the application code (R/ is a flat folder of function definitions).
library(shiny)
app_dir <- normalizePath(testthat::test_path("..", ".."))
for (f in sort(list.files(file.path(app_dir, "R"), pattern = "\\.R$", full.names = TRUE))) source(f, local = FALSE)
iris_df <- read_dataset(file.path(app_dir, "data", "iris.csv"))
build_ok <- function(fig) { plotly::plotly_build(fig); TRUE }

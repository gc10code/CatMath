# CatMath — interactive statistics and machine-learning dashboard.
#
# Run from this folder with:   shiny::runApp()
# Files in R/ are sourced automatically by Shiny before this script.

library(shiny)

options(
  shiny.maxRequestSize = CM_LIMITS$max_upload_mb * 1024^2,
  shiny.autoreload = FALSE
)

shinyApp(ui = app_ui(), server = app_server)

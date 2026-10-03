# Top-level server: creates the shared state and wires every module.

app_server <- function(input, output, session) {
  app <- create_app_state()
  navigate <- function(tab) shinydashboard::updateTabItems(session, "tabs", tab)

  output$has_data <- reactive(!is.null(app$state$data))
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)

  output$dataset_badge <- renderUI({
    df <- app$state$data
    if (is.null(df)) return(div(class = "cm-badge cm-badge-empty", icon("circle-info"), "No dataset loaded"))
    div(
      class = "cm-badge",
      div(class = "cm-badge-name", icon("table"), app$state$name),
      div(class = "cm-badge-meta", sprintf("%s rows · %d cols", format(nrow(df), big.mark = ","), ncol(df))),
      div(class = "cm-badge-meta", sprintf("%d preprocessing steps", max(0, length(app$state$steps) - 1)))
    )
  })

  home_server("home", app, navigate)
  upload_server("upload", app)
  project_server("project", app)
  prep_select_server("prep_select", app)
  prep_custom_server("prep_custom", app)
  prep_missing_server("prep_missing", app)
  prep_transform_server("prep_transform", app)
  eda_server("eda", app)
  stats_server("stats", app)
  pca_server("pca", app)
  nmf_server("nmf", app)
  lm_server("lm", app)
  kmeans_server("kmeans", app)
  knn_server("knn", app)
  rf_server("rf", app)

  # Keep the loading screen visible long enough for the walking cat to show.
  later::later(function() shiny::withReactiveDomain(session, waiter::waiter_hide()), 2)
}

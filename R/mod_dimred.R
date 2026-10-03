# Dimensionality-reduction modules: PCA and NMF.

# Axis selectors + colour picker placed in a results toolbar.
axis_controls <- function(ns, three_d_condition = NULL, color = TRUE) {
  tagList(
    div(class = "cm-toolbar-item", selectInput(ns("ax_x"), "x", NULL)),
    div(class = "cm-toolbar-item", selectInput(ns("ax_y"), "y", NULL)),
    if (!is.null(three_d_condition)) conditionalPanel(three_d_condition, ns = ns, class = "cm-toolbar-item", selectInput(ns("ax_z"), "z", NULL)),
    if (color) div(class = "cm-toolbar-item", selectInput(ns("color"), "Colour", c("None" = "None")))
  )
}

update_axes <- function(session, choices) {
  updateSelectInput(session, "ax_x", choices = choices, selected = choices[1])
  updateSelectInput(session, "ax_y", choices = choices, selected = choices[min(2, length(choices))])
  updateSelectInput(session, "ax_z", choices = choices, selected = choices[min(3, length(choices))])
}

observe_color_choices <- function(app, session, input) {
  observe({
    cats <- app$cat_vars()
    sel <- isolate(input$color)
    updateSelectInput(session, "color", choices = c("None" = "None", cats),
                      selected = if (!is.null(sel) && sel %in% cats) sel else if (length(cats)) cats[1] else "None")
  })
}

color_groups <- function(app, input) {
  v <- group_choice(app, input$color)
  if (is.null(v)) NULL else app$state$data[[v]]
}

# ---- PCA ------------------------------------------------------------------------

pca_ui <- function(id) {
  ns <- NS(id)
  analysis_page_ui(
    id, "Principal component analysis", "Reduce dimensionality while keeping as much variance as possible.",
    options = tagList(
      var_picker(ns("vars"), "Numeric variables"),
      shinyWidgets::radioGroupButtons(ns("matrix"), "Decompose", c("Correlation" = "cor", "Covariance" = "cov"),
                                      justified = TRUE, size = "sm"),
      sliderInput(ns("threshold"), "Variance threshold (%)", 50, 99, 80, 1)
    ),
    results = result_tabs(
      plot_tab(ns, "viz", "Scores", c("2D scores" = "2d", "Biplot" = "biplot", "3D scores" = "3d"),
               axis_controls(ns, "input.viz_view == '3d'"), icon = "braille"),
      plot_tab(ns, "ana", "Components", c("Scree plot" = "scree", "Eigenvalues (Kaiser)" = "kaiser",
                                          "Cumulative variance" = "cum", "Loadings heatmap" = "load"), icon = "chart-column"),
      table_tab(ns, "tab", "Tables", c("Variance explained" = "importance", "Loadings" = "loadings", "Scores" = "scores"))
    ),
    empty = empty_state("No PCA yet", "Select the variables and press Run.", "img/pca.png", "compress",
                        c("2D, 3D scores and biplot", "Scree, Kaiser and cumulative-variance criteria", "Loadings heatmap and tables"))
  )
}

pca_server <- function(id, app) {
  moduleServer(id, function(input, output, session) {
    observe(update_var_picker(session, "vars", app$num_vars()))
    observe_color_choices(app, session, input)
    output$has_result <- reactive(!is.null(app$results$pca))
    outputOptions(output, "has_result", suspendWhenHidden = FALSE)

    observeEvent(input$run, {
      res <- run_with_progress("Computing PCA", function(progress) {
        run_pca(app$state$data, input$vars, scale = input$matrix == "cor", progress = progress)
      })
      if (!is.null(res)) app$results$pca <- res
    })
    res <- reactive(req(app$results$pca))
    observeEvent(app$results$pca, update_axes(session, colnames(app$results$pca$scores)))
    axis <- function(name, default) {
      v <- input[[name]]
      if (is.null(v) || !v %in% colnames(res()$scores)) default else v
    }
    register_plot_tab("viz", list(
      "2d" = function() pca_scatter(res(), axis("ax_x", "PC1"), axis("ax_y", "PC2"), color_groups(app, input), none_to_null(input$color)),
      "biplot" = function() pca_biplot(res(), axis("ax_x", "PC1"), axis("ax_y", "PC2"), color_groups(app, input), none_to_null(input$color)),
      "3d" = function() pca_scatter3d(res(), c(axis("ax_x", "PC1"), axis("ax_y", "PC2"), axis("ax_z", "PC3")), color_groups(app, input), none_to_null(input$color))
    ), input, output, session, "PCA")
    register_plot_tab("ana", list(
      scree = function() pca_scree(res()),
      kaiser = function() pca_kaiser(res()),
      cum = function() pca_cumulative(res(), input$threshold),
      load = function() pca_loadings(res())
    ), input, output, session, "PCA")
    tables <- reactive(pca_tables(res()))
    register_table_tab("tab", list(
      importance = function() tables()$importance,
      loadings = function() tables()$loadings,
      scores = function() tables()$scores
    ), input, output)
  })
}

# ---- NMF ------------------------------------------------------------------------

nmf_ui <- function(id) {
  ns <- NS(id)
  analysis_page_ui(
    id, "Non-negative matrix factorisation", "Decompose non-negative data into additive parts (V ≈ W·H).",
    options = tagList(
      var_picker(ns("vars"), "Numeric variables (non-negative)"),
      sliderInput(ns("rank"), "Rank (components)", 1, 10, 2, 1),
      sliderInput(ns("curve"), "Explained-variance curve up to rank", 1, 15, 8, 1)
    ),
    results = result_tabs(
      plot_tab(ns, "viz", "Basis", c("2D basis" = "2d", "3D basis" = "3d"), axis_controls(ns, "input.viz_view == '3d'"), icon = "braille"),
      plot_tab(ns, "ana", "Fit", c("Original vs reconstructed" = "fit", "Explained variance by rank" = "ev",
                                   "W heatmap" = "W", "H heatmap" = "H"),
               conditionalPanel("input.ana_view == 'fit'", ns = ns, class = "cm-toolbar-item", selectInput(ns("fit_var"), "Variable", NULL)),
               icon = "chart-column"),
      table_tab(ns, "tab", "Tables", c("Summary" = "summary", "Coefficients (H)" = "coefficients", "Basis (W)" = "basis"))
    ),
    empty = empty_state("No NMF yet", "Select non-negative variables and press Run.", "img/nmf.png", "layer-group",
                        c("Deterministic NNDSVD initialisation", "Explained variance by rank", "Basis and coefficient heatmaps"))
  )
}

nmf_server <- function(id, app) {
  moduleServer(id, function(input, output, session) {
    observe(update_var_picker(session, "vars", app$num_vars()))
    observe({
      p <- length(input$vars %||% app$num_vars())
      req(p > 0)
      updateSliderInput(session, "rank", max = p, value = min(max(isolate(input$rank), 2), p))
      updateSliderInput(session, "curve", max = p, value = min(max(isolate(input$curve), 8), p))
    })
    observe_color_choices(app, session, input)
    output$has_result <- reactive(!is.null(app$results$nmf))
    outputOptions(output, "has_result", suspendWhenHidden = FALSE)

    observeEvent(input$run, {
      res <- run_with_progress("Computing NMF", function(progress) {
        run_nmf(app$state$data, input$vars, input$rank, input$curve, progress)
      })
      if (!is.null(res)) app$results$nmf <- res
    })
    res <- reactive(req(app$results$nmf))
    observeEvent(app$results$nmf, {
      update_axes(session, colnames(app$results$nmf$W))
      updateSelectInput(session, "fit_var", choices = app$results$nmf$vars)
    })
    axis <- function(name, default) {
      v <- input[[name]]
      if (is.null(v) || !v %in% colnames(res()$W)) default else v
    }
    register_plot_tab("viz", list(
      "2d" = function() nmf_scatter(res(), axis("ax_x", "NMF1"), axis("ax_y", "NMF2"), color_groups(app, input), none_to_null(input$color)),
      "3d" = function() nmf_scatter3d(res(), c(axis("ax_x", "NMF1"), axis("ax_y", "NMF2"), axis("ax_z", "NMF3")), color_groups(app, input), none_to_null(input$color))
    ), input, output, session, "NMF")
    register_plot_tab("ana", list(
      fit = function() {
        v <- input$fit_var
        nmf_fitted(res(), if (!is.null(v) && v %in% res()$vars) v else res()$vars[1], color_groups(app, input), none_to_null(input$color))
      },
      ev = function() nmf_ev_curve(res()),
      W = function() nmf_heatmap_W(res()),
      H = function() nmf_heatmap_H(res())
    ), input, output, session, "NMF")
    tables <- reactive(nmf_tables(res()))
    register_table_tab("tab", list(
      summary = function() tables()$summary,
      coefficients = function() tables()$coefficients,
      basis = function() tables()$basis
    ), input, output)
  })
}

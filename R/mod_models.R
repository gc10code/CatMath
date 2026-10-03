# Machine-learning modules: linear regression, k-means, k-NN, random forest.
# Each can use the current data or the scores of a previous PCA / NMF.

feature_options <- function(ns, target_label = NULL, target_multiple = FALSE) {
  tagList(
    source_picker_ui(ns),
    if (!is.null(target_label)) var_picker(ns("target"), target_label, multiple = target_multiple),
    var_picker(ns("vars"), "Predictors / features")
  )
}

# Keeps source / feature pickers in sync; `targets` returns target choices.
observe_features <- function(app, session, input, targets = NULL) {
  observe_sources(app, session, input)
  observe(update_var_picker(session, "vars", feature_vars(app, input$source)))
  if (!is.null(targets)) {
    observe({
      t <- targets()
      sel <- isolate(input$target)
      update_var_picker(session, "target", t, if (!is.null(sel) && sel %in% t) sel else t[1])
    })
  }
}

result_flag <- function(output, app, key) {
  output$has_result <- reactive(!is.null(app$results[[key]]))
  outputOptions(output, "has_result", suspendWhenHidden = FALSE)
}

validation_options <- function(ns, rounds, rounds_max = 100) {
  option_group(
    "Validation",
    sliderInput(ns("rounds"), "Random train/validation splits", 1, rounds_max, rounds, 1),
    sliderInput(ns("train"), "Training share", 0.5, 0.95, 0.8, 0.05)
  )
}

# ---- Linear regression ------------------------------------------------------------

lm_ui <- function(id) {
  ns <- NS(id)
  analysis_page_ui(
    id, "Linear regression", "Pick the best predictor subset by cross-validated adjusted R².",
    options = tagList(feature_options(ns, "Response (numeric)"), validation_options(ns, 30)),
    results = result_tabs(
      plot_tab(ns, "fit", "Model", c("Fit" = "fit"),
               div(class = "cm-toolbar-item", selectInput(ns("color"), "Colour", c("None" = "None"))), icon = "chart-line"),
      plot_tab(ns, "diag", "Diagnostics", c("Residuals vs fitted" = "res", "Normal Q-Q" = "qq",
                                            "Scale-location" = "sl", "Residuals vs leverage" = "lev"), icon = "stethoscope"),
      plot_tab(ns, "sel", "Selection", c("Subset comparison" = "sel"), icon = "ranking-star"),
      table_tab(ns, "tab", "Tables", c("Summary" = "summary", "Coefficients" = "coefficients", "Subset ranking" = "ranking"))
    ),
    empty = empty_state("No model yet", "Choose response and predictors, then press Run.", "img/lr.png", "chart-line",
                        c("Exhaustive best subset (up to 12 predictors), forward selection above",
                          "Regression line or observed-vs-predicted", "Four diagnostic plots", "Coefficient table with p-values"))
  )
}

lm_server <- function(id, app) {
  moduleServer(id, function(input, output, session) {
    observe_features(app, session, input, targets = reactive(app$num_vars()))
    observe_color_choices(app, session, input)
    result_flag(output, app, "lm")
    observeEvent(input$run, {
      res <- run_with_progress("Fitting regression", function(progress) {
        run_lm(feature_frame(app, input$source), input$target, input$vars, input$rounds, input$train, progress = progress)
      })
      if (!is.null(res)) app$results$lm <- res
    })
    res <- reactive(req(app$results$lm))
    grp <- function() color_groups(app, input)
    gname <- function() none_to_null(input$color)
    register_plot_tab("fit", list(fit = function() lm_fit_plot(res(), grp(), gname())), input, output, session, "Linear regression")
    register_plot_tab("diag", list(
      res = function() lm_residuals(res(), grp(), gname()),
      qq = function() lm_qq(res(), grp(), gname()),
      sl = function() lm_scale_location(res(), grp(), gname()),
      lev = function() lm_leverage(res(), grp(), gname())
    ), input, output, session, "Diagnostics")
    register_plot_tab("sel", list(sel = function() lm_selection(res())), input, output, session, "Subset selection")
    tables <- reactive(lm_tables(res()))
    register_table_tab("tab", list(
      summary = function() tables()$summary,
      coefficients = function() tables()$coefficients,
      ranking = function() tables()$ranking
    ), input, output)
  })
}

# ---- K-means ------------------------------------------------------------------------

kmeans_ui <- function(id) {
  ns <- NS(id)
  analysis_page_ui(
    id, "K-means clustering", "Group observations into K clusters; K is chosen by mean silhouette.",
    options = tagList(
      feature_options(ns),
      sliderInput(ns("k_max"), "Maximum K", 2, 30, 10, 1),
      sliderInput(ns("rounds"), "Random starts per K", 1, 50, 10, 1)
    ),
    results = result_tabs(
      plot_tab(ns, "clu", "Clusters", c("2D" = "2d", "3D" = "3d"), tagList(
        div(class = "cm-toolbar-item", selectInput(ns("k"), "K", NULL)),
        axis_controls(ns, "input.clu_view == '3d'", color = FALSE)
      ), icon = "circle-nodes"),
      plot_tab(ns, "qual", "Quality", c("Silhouette (selected K)" = "sil", "Silhouette by K" = "silk"), icon = "gauge"),
      plot_tab(ns, "sel", "Selection", c("Elbow (within SS)" = "wss", "Explained variance" = "ev"), icon = "ranking-star"),
      table_tab(ns, "tab", "Tables", c("Summary" = "summary", "Clusters" = "clusters", "Centres" = "centers"))
    ),
    empty = empty_state("No clustering yet", "Choose the variables and press Run.", "img/km.png", "circle-nodes",
                        c("Every K kept: explore any K after the run", "Silhouette, elbow and explained variance",
                          "Centroids in original units"))
  )
}

kmeans_server <- function(id, app) {
  moduleServer(id, function(input, output, session) {
    observe_features(app, session, input)
    result_flag(output, app, "kmeans")
    observeEvent(input$run, {
      res <- run_with_progress("Clustering", function(progress) {
        run_kmeans(feature_frame(app, input$source), input$vars, input$k_max, input$rounds, progress = progress)
      })
      if (!is.null(res)) app$results$kmeans <- res
    })
    res <- reactive(req(app$results$kmeans))
    observeEvent(app$results$kmeans, {
      r <- app$results$kmeans
      ok <- r$ks[!vapply(r$models, is.null, logical(1))]
      updateSelectInput(session, "k", choices = ok, selected = r$best_k)
      update_axes(session, r$vars)
    })
    k_sel <- reactive({
      k <- suppressWarnings(as.integer(input$k))
      if (is.na(k) || is.null(res()$models[[as.character(k)]])) res()$best_k else k
    })
    axis <- function(name, i) {
      v <- input[[name]]
      if (is.null(v) || !v %in% res()$vars) res()$vars[min(i, length(res()$vars))] else v
    }
    register_plot_tab("clu", list(
      "2d" = function() kmeans_scatter(res(), k_sel(), axis("ax_x", 1), axis("ax_y", 2)),
      "3d" = function() kmeans_scatter3d(res(), k_sel(), c(axis("ax_x", 1), axis("ax_y", 2), axis("ax_z", 3)))
    ), input, output, session, "K-means")
    register_plot_tab("qual", list(
      sil = function() kmeans_silhouette_plot(res(), k_sel()),
      silk = function() kmeans_silhouette_by_k(res())
    ), input, output, session, "Cluster quality")
    register_plot_tab("sel", list(wss = function() kmeans_wss(res()), ev = function() kmeans_ev(res())),
                      input, output, session, "Choosing K")
    tables <- reactive(kmeans_tables(res(), k_sel()))
    register_table_tab("tab", list(
      summary = function() tables()$summary,
      clusters = function() tables()$clusters,
      centers = function() tables()$centers
    ), input, output)
  })
}

# ---- k-NN --------------------------------------------------------------------------------

knn_ui <- function(id) {
  ns <- NS(id)
  analysis_page_ui(
    id, "k-nearest neighbours", "Classify observations by majority vote of their nearest neighbours.",
    options = tagList(
      feature_options(ns, "Target (categorical)"),
      sliderInput(ns("k_max"), "Maximum k", 1, 150, 30, 1),
      validation_options(ns, 20)
    ),
    results = result_tabs(
      plot_tab(ns, "model", "Model", c("Decision regions" = "db"), axis_controls(ns, color = FALSE), icon = "map"),
      plot_tab(ns, "qual", "Quality", c("Confusion matrix" = "conf"), icon = "table-cells"),
      plot_tab(ns, "sel", "Selection", c("Accuracy by k" = "acc", "Cohen's kappa by k" = "kappa"), icon = "ranking-star"),
      table_tab(ns, "tab", "Tables", c("Summary" = "summary", "Per-class metrics" = "classes"))
    ),
    empty = empty_state("No classifier yet", "Choose target and features, then press Run.", "img/knn.png", "location-crosshairs",
                        c("All k evaluated in a single vectorised pass", "Accuracy and Cohen's kappa by k",
                          "Decision regions on any two predictors", "Precision, recall and F1 per class"))
  )
}

knn_server <- function(id, app) {
  moduleServer(id, function(input, output, session) {
    observe_features(app, session, input, targets = reactive(app$cat_vars()))
    result_flag(output, app, "knn")
    observeEvent(input$run, {
      res <- run_with_progress("Training k-NN", function(progress) {
        run_knn(feature_frame(app, input$source), input$vars, input$target, input$k_max, input$rounds, input$train, progress = progress)
      })
      if (!is.null(res)) app$results$knn <- res
    })
    res <- reactive(req(app$results$knn))
    observeEvent(app$results$knn, update_axes(session, app$results$knn$vars))
    axis <- function(name, i) {
      v <- input[[name]]
      if (is.null(v) || !v %in% res()$vars) res()$vars[min(i, length(res()$vars))] else v
    }
    register_plot_tab("model", list(db = function() {
      if (length(res()$vars) < 2) return(empty_plot("Decision regions need at least two predictors"))
      knn_boundary(res(), axis("ax_x", 1), axis("ax_y", 2))
    }), input, output, session, "k-NN")
    register_plot_tab("qual", list(conf = function() plot_confusion(res()$confusion, "Confusion matrix (validation, all splits)")),
                      input, output, session, "k-NN")
    register_plot_tab("sel", list(acc = function() knn_accuracy_plot(res()), kappa = function() knn_kappa_plot(res())),
                      input, output, session, "Choosing k")
    tables <- reactive(knn_tables(res()))
    register_table_tab("tab", list(summary = function() tables()$summary, classes = function() tables()$classes), input, output)
  })
}

# ---- Random forest ---------------------------------------------------------------------

rf_ui <- function(id) {
  ns <- NS(id)
  analysis_page_ui(
    id, "Random forest", "Ensemble of decision trees, tuned over mtry and number of trees.",
    options = tagList(
      feature_options(ns, "Target (categorical)"),
      sliderInput(ns("trees"), "Maximum number of trees", 50, 2000, 500, 50),
      validation_options(ns, 5, 30)
    ),
    results = result_tabs(
      plot_tab(ns, "model", "Trees", c("Tree explorer" = "tree"), tagList(
        div(class = "cm-toolbar-item", numericInput(ns("tree"), "Tree", 1, min = 1, step = 1)),
        div(class = "cm-toolbar-item", sliderInput(ns("depth"), "Depth", 2, 10, 5, 1))
      ), icon = "tree"),
      plot_tab(ns, "qual", "Quality", c("Confusion matrix" = "conf", "Variable importance" = "imp"), icon = "table-cells"),
      plot_tab(ns, "sel", "Tuning", c("Learning curve" = "loss", "Accuracy grid" = "grid"),
               conditionalPanel("input.sel_view == 'loss'", ns = ns, class = "cm-toolbar-item", selectInput(ns("mtry"), "mtry", NULL)),
               icon = "ranking-star"),
      table_tab(ns, "tab", "Tables", c("Summary" = "summary", "Importance" = "importance", "Tuning grid" = "grid", "Per-class metrics" = "classes"))
    ),
    empty = empty_state("No forest yet", "Choose target and features, then press Run.", "img/rf.png", "tree",
                        c("Multi-threaded ranger engine", "Tree-count grid evaluated without re-training",
                          "Learning curves and accuracy grid", "Importance and per-class metrics"))
  )
}

rf_server <- function(id, app) {
  moduleServer(id, function(input, output, session) {
    observe_features(app, session, input, targets = reactive(app$cat_vars()))
    result_flag(output, app, "rf")
    observeEvent(input$run, {
      res <- run_with_progress("Growing forests", function(progress) {
        run_rf(feature_frame(app, input$source), input$vars, input$target, input$trees,
               rounds = input$rounds, train_frac = input$train, progress = progress)
      })
      if (!is.null(res)) app$results$rf <- res
    })
    res <- reactive(req(app$results$rf))
    observeEvent(app$results$rf, {
      r <- app$results$rf
      updateSelectInput(session, "mtry", choices = r$mtry_grid, selected = r$mtry)
      updateNumericInput(session, "tree", max = r$model$num.trees, value = 1)
    })
    register_plot_tab("model", list(tree = function() rf_tree_plot(res(), input$tree %||% 1, input$depth)),
                      input, output, session, "Random forest")
    register_plot_tab("qual", list(
      conf = function() plot_confusion(res()$confusion, "Confusion matrix (validation, all splits)"),
      imp = function() rf_importance(res())
    ), input, output, session, "Random forest")
    register_plot_tab("sel", list(
      loss = function() {
        m <- suppressWarnings(as.integer(input$mtry))
        rf_learning_curve(res(), if (is.na(m) || !m %in% res()$mtry_grid) res()$mtry else m)
      },
      grid = function() rf_accuracy_heatmap(res())
    ), input, output, session, "Tuning")
    tables <- reactive(rf_tables(res()))
    register_table_tab("tab", list(
      summary = function() tables()$summary, importance = function() tables()$importance,
      grid = function() tables()$grid, classes = function() tables()$classes
    ), input, output)
  })
}

# Modules: home page, dataset upload, project save/load.

TOOLS <- list(
  list(tab = "eda", title = "Data visualisation", img = "img/eda.png", icon = "chart-simple",
       text = "Correlations, distributions, scatter-plot matrices and parallel coordinates."),
  list(tab = "stats", title = "Statistical tests", img = "img/st.png", icon = "scale-balanced",
       text = "t, Welch, Wilcoxon, ANOVA, Kruskal-Wallis with automatic assumption checks."),
  list(tab = "pca", title = "PCA", img = "img/pca.png", icon = "compress",
       text = "Principal components, scree and Kaiser criteria, loadings and biplots."),
  list(tab = "nmf", title = "NMF", img = "img/nmf.png", icon = "layer-group",
       text = "Non-negative matrix factorisation for latent pattern discovery."),
  list(tab = "lm", title = "Linear regression", img = "img/lr.png", icon = "chart-line",
       text = "Cross-validated best-subset regression with full diagnostics."),
  list(tab = "kmeans", title = "K-means", img = "img/km.png", icon = "circle-nodes",
       text = "Clustering with silhouette, elbow and explained-variance model selection."),
  list(tab = "knn", title = "k-nearest neighbours", img = "img/knn.png", icon = "location-crosshairs",
       text = "Vectorised k-NN with cross-validated k and decision regions."),
  list(tab = "rf", title = "Random forest", img = "img/rf.png", icon = "tree",
       text = "Tuned forests, learning curves, importance and tree explorer.")
)

# ---- Home -------------------------------------------------------------------

home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "cm-hero",
      div(
        class = "cm-hero-text",
        div(class = "cm-hero-kicker", "Statistics & machine learning, made gentle"),
        h1("Welcome to ", span(class = "cm-brand", "CatMath")),
        p("A fast, point-and-click toolkit for data analysis: load a dataset, clean it, explore it ",
          "and model it — every result comes with interactive charts and tables."),
        div(
          class = "cm-hero-actions",
          actionButton(ns("go_upload"), "Upload data", icon = icon("upload"), class = "cm-btn cm-btn-primary"),
          actionButton(ns("go_project"), "Open project", icon = icon("folder-open"), class = "cm-btn cm-btn-ghost")
        ),
        div(
          class = "cm-hero-example",
          selectInput(ns("example"), NULL, choices = CM_EXAMPLES, width = "260px"),
          actionButton(ns("load_example"), "Try an example", icon = icon("wand-magic-sparkles"), class = "cm-btn cm-btn-soft")
        )
      ),
      div(class = "cm-hero-art", icon("cat"))
    ),
    h3(class = "cm-section-title", "Tools"),
    div(
      class = "cm-tool-grid",
      lapply(TOOLS, function(t) {
        tags$a(
          class = "cm-tool", href = "#", onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("open_tool"), t$tab),
          div(class = "cm-tool-img", tags$img(src = t$img, alt = "")),
          div(class = "cm-tool-body", h4(icon(t$icon), t$title), p(t$text))
        )
      })
    )
  )
}

home_server <- function(id, app, navigate) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$go_upload, navigate("upload"))
    observeEvent(input$go_project, navigate("project"))
    observeEvent(input$open_tool, navigate(input$open_tool))
    observeEvent(input$load_example, {
      file <- input$example
      df <- tryCatch(read_example(file), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
      if (is.null(df)) return()
      set_dataset(app, df, names(CM_EXAMPLES)[CM_EXAMPLES == file])
      navigate("upload")
    })
  })
}

# ---- Upload -------------------------------------------------------------------

upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_header("Data", "Upload a delimited text file or open an example dataset."),
    div(
      class = "cm-analysis",
      div(
        class = "cm-options",
        cm_card(
          "Upload file", icon = "file-arrow-up",
          fileInput(ns("file"), NULL, accept = c(".csv", ".tsv", ".txt"), buttonLabel = "Browse...",
                    placeholder = ".csv, .tsv or .txt"),
          option_group(
            "Format",
            switch_input(ns("header"), "First row is a header", TRUE),
            selectInput(ns("sep"), "Separator", c("Auto-detect" = "auto", "Comma" = ",", "Semicolon" = ";", "Tab" = "\t", "Space" = " ")),
            selectInput(ns("quote"), "Quote", c("Double quote" = "\"", "Single quote" = "'", "None" = "")),
            selectInput(ns("dec"), "Decimal mark", c("Point (.)" = ".", "Comma (,)" = ","))
          ),
          footer = run_button(ns("load"), "Load file", "file-import")
        ),
        cm_card(
          "Examples", icon = "flask",
          selectInput(ns("example"), NULL, CM_EXAMPLES),
          secondary_button(ns("load_example"), "Open example", "wand-magic-sparkles")
        )
      ),
      div(class = "cm-output", when_no_data(), when_data(uiOutput(ns("overview")), data_preview_ui(ns("preview"))))
    )
  )
}

upload_server <- function(id, app) {
  moduleServer(id, function(input, output, session) {
    load_file <- function() {
      req(input$file)
      df <- tryCatch(
        read_dataset(input$file$datapath, input$file$name, header = input$header, sep = input$sep,
                     quote = input$quote, dec = input$dec),
        error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 8); NULL }
      )
      if (!is.null(df)) {
        set_dataset(app, df, input$file$name)
        notify_done(sprintf("Loaded %s rows and %d columns.", format(nrow(df), big.mark = ","), ncol(df)))
      }
    }
    observeEvent(input$load, load_file())
    observeEvent(input$load_example, {
      df <- read_example(input$example)
      set_dataset(app, df, names(CM_EXAMPLES)[CM_EXAMPLES == input$example])
    })

    output$overview <- renderUI({
      req(app$state$raw)
      o <- dataset_overview(app$state$raw)
      div(
        class = "cm-tiles",
        stat_tile("Rows", format(o$rows, big.mark = ","), "table-list", "blue"),
        stat_tile("Columns", o$cols, "table-columns", "lavender"),
        stat_tile("Numeric", o$numeric, "hashtag", "sage"),
        stat_tile("Categorical", o$categorical, "tags", "peach"),
        stat_tile("Missing cells", format(o$missing, big.mark = ","), "circle-question", "rose")
      )
    })
    data_preview_server("preview", reactive(app$state$raw))
  })
}

# ---- Data preview (shared by data and preprocessing pages) -----------------------

data_preview_ui <- function(id) {
  ns <- NS(id)
  cm_card(
    NULL, class = "cm-card-flush",
    tabsetPanel(
      type = "pills",
      tabPanel("Data", icon = icon("table"), div(class = "cm-table-wrap", reactable::reactableOutput(ns("data")), uiOutput(ns("note")))),
      tabPanel("Numeric summary", icon = icon("hashtag"), div(class = "cm-table-wrap", reactable::reactableOutput(ns("num")))),
      tabPanel("Categorical summary", icon = icon("tags"), div(class = "cm-table-wrap", reactable::reactableOutput(ns("cat"))))
    )
  )
}

data_preview_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$data <- reactable::renderReactable({
      df <- req(data())
      cm_table(signif_df(utils::head(df, CM_LIMITS$preview_rows), 5))
    })
    output$note <- renderUI({
      df <- req(data())
      if (nrow(df) > CM_LIMITS$preview_rows) {
        p(class = "cm-note", sprintf("Showing the first %s of %s rows.", format(CM_LIMITS$preview_rows, big.mark = ","),
                                     format(nrow(df), big.mark = ",")))
      }
    })
    output$num <- reactable::renderReactable(cm_table(summarize_numeric(req(data()))))
    output$cat <- reactable::renderReactable(cm_table(summarize_categorical(req(data()))))
  })
}

# ---- Project save / load ----------------------------------------------------------

PROJECT_FORMAT <- "catmath-project"
PROJECT_VERSION <- 2L

project_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_header("Project", "Save the current session (data, preprocessing history and results) or reopen one."),
    div(
      class = "cm-analysis",
      div(
        class = "cm-options",
        cm_card(
          "Open project", icon = "folder-open",
          fileInput(ns("file"), NULL, accept = ".rds", buttonLabel = "Browse...", placeholder = ".rds project file"),
          footer = run_button(ns("open"), "Open", "box-open")
        ),
        when_data(cm_card(
          "Save project", icon = "floppy-disk",
          checkboxGroupInput(ns("include"), "Results to include", choices = NULL),
          switch_input(ns("include_raw"), "Keep the original data (allows reset)", TRUE),
          footer = downloadButton(ns("save"), "Download project", class = "cm-btn cm-btn-primary cm-btn-block")
        ))
      ),
      div(class = "cm-output", when_no_data(), when_data(uiOutput(ns("summary"))))
    )
  )
}

result_headline <- function(key, res) {
  switch(key,
    stats = list("Statistical tests", sprintf("%d tests, %d significant", nrow(res$tests), sum(res$tests$significant, na.rm = TRUE)), "scale-balanced"),
    pca = list("PCA", sprintf("PC1 explains %.1f%%", 100 * res$var_explained[1]), "compress"),
    nmf = list("NMF", sprintf("Rank %d, %.1f%% explained", res$rank, 100 * res$ev), "layer-group"),
    lm = list("Linear regression", sprintf("Adj. R² %.3f", summary(res$model)$adj.r.squared), "chart-line"),
    kmeans = list("K-means", sprintf("Best K = %d", res$best_k), "circle-nodes"),
    knn = list("k-NN", sprintf("Accuracy %.1f%% (k = %d)", 100 * res$cv_accuracy, res$k), "location-crosshairs"),
    rf = list("Random forest", sprintf("Accuracy %.1f%%", 100 * res$cv_accuracy), "tree")
  )
}

project_server <- function(id, app) {
  moduleServer(id, function(input, output, session) {
    available <- reactive({
      keys <- CM_RESULT_KEYS[vapply(CM_RESULT_KEYS, function(k) !is.null(app$results[[k]]), logical(1))]
      stats::setNames(keys, vapply(keys, function(k) result_headline(k, app$results[[k]])[[1]], ""))
    })
    observe({
      a <- available()
      updateCheckboxGroupInput(session, "include", choices = a, selected = a)
    })

    output$summary <- renderUI({
      keys <- available()
      tiles <- lapply(unname(keys), function(k) {
        h <- result_headline(k, app$results[[k]])
        stat_tile(h[[1]], h[[2]], h[[3]], "blue")
      })
      tagList(
        cm_card("Dataset", icon = "database",
                p(tags$b(app$state$name), sprintf(" — %s rows x %d columns", format(nrow(app$state$data), big.mark = ","), ncol(app$state$data))),
                tags$ol(class = "cm-history", lapply(app$state$steps, tags$li))),
        cm_card("Results", icon = "square-poll-vertical",
                if (length(tiles)) div(class = "cm-tiles", tiles) else p(class = "cm-note", "No analysis has been run yet."))
      )
    })

    output$save <- downloadHandler(
      filename = function() paste0("catmath-project-", format(Sys.time(), "%Y%m%d-%H%M"), ".rds"),
      content = function(file) {
        res <- stats::setNames(lapply(CM_RESULT_KEYS, function(k) if (k %in% input$include) app$results[[k]]), CM_RESULT_KEYS)
        saveRDS(list(
          format = PROJECT_FORMAT, version = PROJECT_VERSION, created = Sys.time(),
          name = app$state$name, raw = if (isTRUE(input$include_raw)) app$state$raw,
          data = app$state$data, steps = app$state$steps, results = res
        ), file, compress = "xz")
      }
    )

    observeEvent(input$open, {
      req(input$file)
      proj <- tryCatch(readRDS(input$file$datapath), error = function(e) NULL)
      if (!is.list(proj) || !identical(proj$format, PROJECT_FORMAT) || !is.data.frame(proj$data)) {
        showNotification("This file is not a CatMath project.", type = "error")
        return()
      }
      app$state$raw <- proj$raw %||% proj$data
      app$state$data <- proj$data
      app$state$name <- proj$name %||% input$file$name
      app$state$steps <- proj$steps %||% character(0)
      for (k in CM_RESULT_KEYS) app$results[[k]] <- proj$results[[k]]
      notify_done("Project opened.")
    })
  })
}

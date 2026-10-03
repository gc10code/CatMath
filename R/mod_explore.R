# Exploration modules: data visualisation (live) and statistical tests.

# ---- Data visualisation ---------------------------------------------------------

eda_ui <- function(id) {
  ns <- NS(id)
  analysis_page_ui(
    id, "Data visualisation", "Charts update as you change the options — nothing to run.",
    options = tagList(
      var_picker(ns("num_vars"), "Numeric variables"),
      var_picker(ns("group"), "Split / colour by", multiple = FALSE),
      selectInput(ns("cor_method"), "Correlation method", c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall"))
    ),
    results = result_tabs(
      tabPanel(
        "Charts", icon = icon("chart-simple"),
        div(
          class = "cm-toolbar",
          div(class = "cm-toolbar-main", selectInput(ns("chart"), NULL, EDA_CHARTS)),
          conditionalPanel("input.chart == 'density' || input.chart == 'hist'", ns = ns,
                           selectInput(ns("single_var"), NULL, choices = NULL)),
          conditionalPanel("input.chart == 'bar'", ns = ns, var_picker(ns("bar_vars"), NULL))
        ),
        plot_card_ui(ns("chart_plot"))
      )
    ),
    empty = NULL, run_label = NULL
  )
}

eda_server <- function(id, app) {
  moduleServer(id, function(input, output, session) {
    observe(update_var_picker(session, "num_vars", app$num_vars()))
    observe({
      cats <- app$cat_vars()
      update_var_picker(session, "group", cats, "None", none = TRUE)
      update_var_picker(session, "bar_vars", cats)
    })
    selected <- debounce(reactive(intersect(input$num_vars %||% character(0), app$num_vars())), 400)
    observe({
      v <- selected()
      updateSelectInput(session, "single_var", choices = v, selected = if (isolate(input$single_var) %in% v) isolate(input$single_var) else v[1])
    })
    figure <- reactive({
      df <- req(app$state$data)
      vars <- selected()
      group <- group_choice(app, input$group)
      switch(input$chart,
        cor = eda_correlation(df, vars, input$cor_method),
        box = eda_distribution(df, vars, group, "box"),
        violin = eda_distribution(df, vars, group, "violin"),
        density = eda_density(df, none_to_null(input$single_var), group),
        hist = eda_histogram(df, none_to_null(input$single_var), group),
        splom = eda_splom(df, vars, group),
        parcoords = eda_parcoords(df, vars, group),
        bar = eda_category_bars(df, intersect(input$bar_vars, names(df))),
        missing = eda_missing(df)
      )
    })
    register_plot("chart_plot", figure, input, output, session, "Data visualisation")
  })
}

# ---- Statistical tests -------------------------------------------------------------

stats_ui <- function(id) {
  ns <- NS(id)
  analysis_page_ui(
    id, "Statistical tests", "Compare groups; the right test is chosen from normality and variance checks.",
    options = tagList(
      var_picker(ns("cat_vars"), "Grouping (categorical) variables"),
      var_picker(ns("num_vars"), "Dependent (numeric) variables"),
      checkboxGroupInput(ns("families"), "Tests", STAT_FAMILIES, selected = STAT_FAMILIES),
      sliderInput(ns("alpha"), "Significance level", 0.01, 0.10, 0.05, 0.01)
    ),
    results = result_tabs(
      plot_tab(ns, "map", "Overview", STAT_FAMILIES, icon = "border-all"),
      table_tab(ns, "tab", "Tables", STAT_FAMILIES)
    ),
    empty = empty_state("No tests yet", "Choose the variables and press Run.", "img/st.png", "scale-balanced",
                        c("Student / Welch t-test or Wilcoxon", "Fisher F or Brown-Forsythe variance tests",
                          "ANOVA, Welch ANOVA or Kruskal-Wallis", "Benjamini-Hochberg adjusted p-values"))
  )
}

stats_server <- function(id, app) {
  moduleServer(id, function(input, output, session) {
    observe(update_var_picker(session, "num_vars", app$num_vars()))
    observe(update_var_picker(session, "cat_vars", app$cat_vars()))
    output$has_result <- reactive(!is.null(app$results$stats))
    outputOptions(output, "has_result", suspendWhenHidden = FALSE)

    observeEvent(input$run, {
      res <- run_with_progress("Running tests", function(progress) {
        run_stat_tests(app$state$data, input$num_vars, input$cat_vars, input$families, input$alpha, progress)
      })
      if (!is.null(res)) app$results$stats <- res
    })
    res <- reactive(req(app$results$stats))
    family_renderers <- function(f) stats::setNames(lapply(STAT_FAMILIES, function(fam) function() f(res(), fam)), STAT_FAMILIES)
    register_plot_tab("map", family_renderers(stats_treemap), input, output, session, "Statistical tests")
    register_table_tab("tab", family_renderers(stats_table), input, output)
  })
}

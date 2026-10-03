# Preprocessing modules: feature selection, custom handling, missing data,
# transformation. They share one layout (options + history | data preview).

prep_page_ui <- function(id, title, subtitle, options, apply_label = "Apply") {
  ns <- NS(id)
  tagList(
    page_header(title, subtitle),
    when_no_data(),
    when_data(div(
      class = "cm-analysis",
      div(
        class = "cm-options",
        cm_card("Options", icon = "sliders", div(class = "cm-options-body", options),
                footer = run_button(ns("apply"), apply_label, "check")),
        cm_card("History", icon = "clock-rotate-left", uiOutput(ns("history")),
                footer = secondary_button(ns("reset"), "Reset to original data", "rotate-left"))
      ),
      div(class = "cm-output", data_preview_ui(ns("preview")))
    ))
  )
}

# Server parts common to every preprocessing page.
prep_common_server <- function(input, output, app) {
  output$history <- renderUI(tags$ol(class = "cm-history", lapply(app$state$steps, tags$li)))
  observeEvent(input$reset, reset_data(app))
  data_preview_server("preview", reactive(app$state$data))
}

try_step <- function(expr) {
  tryCatch(expr, error = function(e) {
    if (!inherits(e, "shiny.silent.error")) showNotification(conditionMessage(e), type = "error", duration = 8)
    NULL
  })
}

# ---- Feature selection -------------------------------------------------------------

prep_select_ui <- function(id) {
  ns <- NS(id)
  prep_page_ui(id, "Feature selection", "Keep the variables relevant for the analysis and drop redundant ones.", tagList(
    var_picker(ns("cat_vars"), "Categorical variables"),
    var_picker(ns("num_vars"), "Numeric variables"),
    option_group(
      "Redundancy",
      switch_input(ns("drop_cor"), "Remove highly correlated variables", FALSE),
      conditionalPanel("input.drop_cor", ns = ns,
                       sliderInput(ns("cutoff"), "Absolute correlation above", 0.5, 0.99, 0.9, 0.01))
    )
  ), "Keep selected")
}

prep_select_server <- function(id, app) {
  moduleServer(id, function(input, output, session) {
    prep_common_server(input, output, app)
    observe(update_var_picker(session, "num_vars", app$num_vars()))
    observe(update_var_picker(session, "cat_vars", app$cat_vars()))
    observeEvent(input$apply, {
      apply_step(app, try_step(select_features(
        app$state$data, input$num_vars %||% character(0), input$cat_vars %||% character(0),
        if (isTRUE(input$drop_cor)) input$cutoff
      )))
    })
  })
}

# ---- Custom handling -------------------------------------------------------------------

prep_custom_ui <- function(id) {
  ns <- NS(id)
  prep_page_ui(id, "Custom handling", "Rewrite labels, aggregate repeated measures, drop sparse variables, change types.", tagList(
    selectInput(ns("op"), "Operation", c(
      "Find & replace (regex)" = "regex",
      "Aggregate rows by group" = "aggregate",
      "Drop variables with many missing values" = "sparse",
      "Convert numeric to categorical" = "categorical"
    )),
    conditionalPanel(
      "input.op == 'regex'", ns = ns,
      var_picker(ns("regex_var"), "Categorical variable", multiple = FALSE),
      textInput(ns("pattern"), "Regular expression", placeholder = "e.g. _\\d+$"),
      textInput(ns("replacement"), "Replace with", placeholder = "(empty to delete)")
    ),
    conditionalPanel("input.op == 'aggregate'", ns = ns,
                     var_picker(ns("group_var"), "Group by", multiple = FALSE),
                     p(class = "cm-note", "Numeric variables are averaged, categorical ones take the most frequent value.")),
    conditionalPanel(
      "input.op == 'sparse'", ns = ns,
      sliderInput(ns("threshold"), "Maximum share of missing values", 0, 0.9, 0.1, 0.05),
      var_picker(ns("sparse_group"), "Check inside each group of (optional)", multiple = FALSE)
    ),
    conditionalPanel("input.op == 'categorical'", ns = ns, var_picker(ns("to_cat"), "Numeric variables"))
  ))
}

prep_custom_server <- function(id, app) {
  moduleServer(id, function(input, output, session) {
    prep_common_server(input, output, app)
    observe({
      cats <- app$cat_vars()
      update_var_picker(session, "regex_var", cats, cats[1])
      update_var_picker(session, "group_var", cats, cats[1])
      update_var_picker(session, "sparse_group", cats, "None", none = TRUE)
    })
    observe(update_var_picker(session, "to_cat", app$num_vars(), character(0)))
    observeEvent(input$apply, {
      df <- app$state$data
      step <- try_step(switch(input$op,
        regex = replace_pattern(df, req(input$regex_var), input$pattern, input$replacement %||% ""),
        aggregate = aggregate_rows(df, req(input$group_var)),
        sparse = drop_sparse_vars(df, input$threshold, none_to_null(input$sparse_group)),
        categorical = as_categorical(df, req(input$to_cat))
      ))
      apply_step(app, step)
    })
  })
}

# ---- Missing data ------------------------------------------------------------------------

prep_missing_ui <- function(id) {
  ns <- NS(id)
  prep_page_ui(id, "Missing data", "Remove incomplete rows or impute missing values.", tagList(
    uiOutput(ns("na_info")),
    shinyWidgets::radioGroupButtons(ns("kind"), NULL, c("Numeric" = "numeric", "Categorical" = "categorical"),
                                    justified = TRUE, size = "sm"),
    var_picker(ns("vars"), "Variables"),
    textInput(ns("indicator"), "Value that means 'missing'", value = "NA", placeholder = "NA, -999, ?"),
    conditionalPanel("input.kind == 'numeric'", ns = ns, selectInput(ns("num_method"), "Method", c(
      "Remove rows" = "remove", "Mean" = "mean", "Median" = "median",
      "Constant" = "constant", "MICE (predictive mean matching)" = "mice"
    ))),
    conditionalPanel("input.kind == 'categorical'", ns = ns, selectInput(ns("cat_method"), "Method", c(
      "Remove rows" = "remove", "Most frequent value" = "mode", "Constant" = "constant"
    ))),
    conditionalPanel("(input.kind == 'numeric' && input.num_method == 'constant') || (input.kind == 'categorical' && input.cat_method == 'constant')",
                     ns = ns, textInput(ns("constant"), "Constant", value = "0"))
  ))
}

prep_missing_server <- function(id, app) {
  moduleServer(id, function(input, output, session) {
    prep_common_server(input, output, app)
    observe({
      df <- app$state$data
      vars <- if (identical(input$kind, "categorical")) app$cat_vars() else app$num_vars()
      with_na <- vars[vapply(df[vars], anyNA, logical(1))]
      update_var_picker(session, "vars", vars, if (length(with_na)) with_na else vars)
    })
    output$na_info <- renderUI({
      df <- req(app$state$data)
      na <- colSums(is.na(df))
      if (!sum(na)) return(div(class = "cm-pill cm-pill-ok", icon("circle-check"), "No missing values"))
      div(class = "cm-pill cm-pill-warn", icon("circle-exclamation"),
          sprintf("%s missing cells in %d variables", format(sum(na), big.mark = ","), sum(na > 0)))
    })
    observeEvent(input$apply, {
      df <- app$state$data
      vars <- req(input$vars)
      step <- try_step(if (input$kind == "numeric") {
        constant <- suppressWarnings(as.numeric(input$constant))
        if (input$num_method == "constant" && is.na(constant)) stop("The constant must be a number.", call. = FALSE)
        impute_numeric(df, vars, input$num_method, parse_indicator(input$indicator, TRUE), constant)
      } else {
        impute_categorical(df, vars, input$cat_method, parse_indicator(input$indicator, FALSE), input$constant)
      })
      apply_step(app, step)
    })
  })
}

# ---- Transformation ------------------------------------------------------------------------

prep_transform_ui <- function(id) {
  ns <- NS(id)
  prep_page_ui(id, "Transformation", "Rescale or variance-stabilise numeric variables.", tagList(
    var_picker(ns("vars"), "Numeric variables"),
    shinyWidgets::prettyRadioButtons(ns("method"), "Method", c(
      "Z-score standardisation" = "zscore", "Min-max scaling [0, 1]" = "minmax",
      "Square root" = "sqrt", "Logarithm log(1 + x)" = "log"
    ), status = "primary", shape = "round", animation = "smooth")
  ))
}

prep_transform_server <- function(id, app) {
  moduleServer(id, function(input, output, session) {
    prep_common_server(input, output, app)
    observe(update_var_picker(session, "vars", app$num_vars()))
    observeEvent(input$apply, {
      apply_step(app, try_step(transform_numeric(app$state$data, req(input$vars), input$method)))
    })
  })
}

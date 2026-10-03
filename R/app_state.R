# Central application state shared by all modules.
#
# state$raw   : dataset as loaded
# state$data  : current (preprocessed) dataset used by every analysis
# results$*   : one entry per analysis (see CM_RESULT_KEYS)
# Any change to state$data clears the results so they never refer to rows
# that no longer exist.

create_app_state <- function() {
  state <- reactiveValues(raw = NULL, data = NULL, name = NULL, steps = character(0))
  results <- do.call(reactiveValues, stats::setNames(vector("list", length(CM_RESULT_KEYS)), CM_RESULT_KEYS))
  app <- list(state = state, results = results)
  app$num_vars <- reactive(numeric_vars(state$data))
  app$cat_vars <- reactive(categorical_vars(state$data))
  app
}

clear_results <- function(app) {
  for (k in CM_RESULT_KEYS) app$results[[k]] <- NULL
}

set_dataset <- function(app, df, name) {
  app$state$raw <- df
  app$state$data <- df
  app$state$name <- name
  app$state$steps <- sprintf("Loaded %s (%s rows x %d columns)", name, format(nrow(df), big.mark = ","), ncol(df))
  clear_results(app)
}

# Apply the output of a preprocessing function (list(data, message)).
apply_step <- function(app, step) {
  if (is.null(step)) return(invisible(FALSE))
  if (!nrow(step$data)) {
    showNotification("This operation would remove every row; not applied.", type = "warning")
    return(invisible(FALSE))
  }
  had_results <- any(vapply(CM_RESULT_KEYS, function(k) !is.null(isolate(app$results[[k]])), logical(1)))
  app$state$data <- step$data
  app$state$steps <- c(isolate(app$state$steps), step$message)
  clear_results(app)
  showNotification(
    paste0(step$message, if (had_results) " — previous results were cleared." else ""),
    type = "message", duration = 4
  )
  invisible(TRUE)
}

reset_data <- function(app) {
  raw <- isolate(app$state$raw)
  if (is.null(raw)) return()
  app$state$data <- raw
  app$state$steps <- utils::head(isolate(app$state$steps), 1)
  clear_results(app)
  showNotification("Data restored to the original upload.", type = "message")
}

# ---- Feature sources for machine-learning modules ---------------------------

feature_sources <- function(app) {
  src <- c("Current data" = "data")
  if (!is.null(app$results$pca)) src <- c(src, "PCA scores" = "pca")
  if (!is.null(app$results$nmf)) src <- c(src, "NMF basis (W)" = "nmf")
  src
}

feature_vars <- function(app, source) {
  switch(source %||% "data",
    pca = colnames(app$results$pca$scores),
    nmf = colnames(app$results$nmf$W),
    numeric_vars(app$state$data)
  )
}

# Current data with the chosen derived features appended as extra columns.
feature_frame <- function(app, source) {
  data <- app$state$data
  extra <- switch(source %||% "data",
    pca = app$results$pca$scores,
    nmf = app$results$nmf$W,
    NULL
  )
  if (is.null(extra)) return(data)
  if (nrow(extra) != nrow(data)) stop("Derived features are out of date: re-run the decomposition.", call. = FALSE)
  cbind(data[setdiff(names(data), colnames(extra))], as.data.frame(extra))
}

source_picker_ui <- function(ns) {
  selectInput(ns("source"), "Features from", choices = c("Current data" = "data"))
}

# Keep the "Features from" select in sync with available decompositions.
observe_sources <- function(app, session, input) {
  observe({
    src <- feature_sources(app)
    sel <- isolate(input$source)
    updateSelectInput(session, "source", choices = src, selected = if (!is.null(sel) && sel %in% src) sel else "data")
  })
}

group_choice <- function(app, input_value) {
  v <- none_to_null(input_value)
  if (is.null(v) || !v %in% names(app$state$data)) return(NULL)
  v
}

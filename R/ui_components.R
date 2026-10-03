# Reusable UI building blocks (layout, inputs, result views).

page_header <- function(title, subtitle = NULL, icon = NULL) {
  div(
    class = "cm-page-header",
    h2(class = "cm-page-title", if (!is.null(icon)) shiny::icon(icon), title),
    if (!is.null(subtitle)) p(class = "cm-page-subtitle", subtitle)
  )
}

cm_card <- function(title = NULL, ..., icon = NULL, class = NULL, footer = NULL) {
  div(
    class = paste("cm-card", class),
    if (!is.null(title)) div(class = "cm-card-header", if (!is.null(icon)) shiny::icon(icon), span(title)),
    div(class = "cm-card-body", ...),
    if (!is.null(footer)) div(class = "cm-card-footer", footer)
  )
}

empty_state <- function(title, message, image = NULL, icon = "cat", features = NULL) {
  div(
    class = "cm-empty",
    div(
      class = "cm-empty-text",
      div(class = "cm-empty-icon", shiny::icon(icon)),
      h3(title), p(message),
      if (!is.null(features)) tags$ul(class = "cm-feature-list", lapply(features, tags$li))
    ),
    if (!is.null(image)) div(class = "cm-empty-img", tags$img(src = image, alt = ""))
  )
}

run_button <- function(id, label = "Run analysis", icon = "play") {
  actionButton(id, label, icon = shiny::icon(icon), class = "cm-btn cm-btn-primary cm-btn-block")
}

secondary_button <- function(id, label, icon = NULL) {
  actionButton(id, label, icon = if (!is.null(icon)) shiny::icon(icon), class = "cm-btn cm-btn-ghost cm-btn-block")
}

picker_options <- function(multiple = TRUE) {
  shinyWidgets::pickerOptions(
    actionsBox = multiple, liveSearch = TRUE, size = 10,
    selectedTextFormat = if (multiple) "count > 3" else "values",
    countSelectedText = "{0} selected", noneSelectedText = "Nothing selected",
    selectAllText = "All", deselectAllText = "None", iconBase = "fa", tickIcon = "fa-check"
  )
}

var_picker <- function(id, label, multiple = TRUE) {
  shinyWidgets::pickerInput(id, label, choices = NULL, multiple = multiple, options = picker_options(multiple))
}

update_var_picker <- function(session, id, choices, selected = choices, none = FALSE) {
  if (none) choices <- c("None" = "None", choices)
  shinyWidgets::updatePickerInput(session, id, choices = choices, selected = selected)
}

option_group <- function(title, ...) {
  div(class = "cm-option-group", div(class = "cm-option-title", title), ...)
}

switch_input <- function(id, label, value = FALSE) {
  shinyWidgets::prettySwitch(id, label, value = value, status = "primary", fill = TRUE)
}

# Panels shown depending on whether a dataset is loaded (global output flag).
when_data <- function(...) conditionalPanel("output.has_data", ...)
when_no_data <- function(...) {
  conditionalPanel(
    "!output.has_data",
    cm_card(NULL, empty_state(
      "No dataset loaded", "Upload a file or open one of the example datasets to get started.",
      image = "img/lf.png", icon = "folder-open"
    ))
  )
}

#' Standard analysis page (JASP-like): options on the left, output on the right.
analysis_page_ui <- function(id, title, subtitle, options, results, empty, run_label = "Run analysis") {
  ns <- NS(id)
  tagList(
    page_header(title, subtitle),
    when_no_data(),
    when_data(
      div(
        class = "cm-analysis",
        div(
          class = "cm-options",
          cm_card("Options", icon = "sliders", div(class = "cm-options-body", options),
                  footer = if (!is.null(run_label)) run_button(ns("run"), run_label))
        ),
        div(
          class = "cm-output",
          if (!is.null(run_label)) conditionalPanel("!output.has_result", ns = ns, cm_card(NULL, empty)),
          if (!is.null(run_label)) conditionalPanel("output.has_result", ns = ns, results) else results
        )
      )
    )
  )
}

result_tabs <- function(...) {
  div(class = "cm-results", do.call(tabsetPanel, c(list(type = "pills"), list(...))))
}

#' A results tab holding one chart: a view selector, optional extra controls
#' (inputs of the calling module) and a zoomable plot.
plot_tab <- function(ns, id, title, choices, controls = NULL, icon = "chart-simple") {
  tabPanel(
    title, icon = shiny::icon(icon),
    div(
      class = "cm-toolbar",
      if (length(choices) > 1) div(class = "cm-toolbar-main", selectInput(ns(paste0(id, "_view")), NULL, choices)),
      controls
    ),
    plot_card_ui(ns(paste0(id, "_plot")))
  )
}

table_tab <- function(ns, id, title, choices, icon = "table") {
  tabPanel(
    title, icon = shiny::icon(icon),
    if (length(choices) > 1) div(class = "cm-toolbar", div(class = "cm-toolbar-main", selectInput(ns(paste0(id, "_view")), NULL, choices))),
    div(class = "cm-table-wrap", reactable::reactableOutput(ns(paste0(id, "_table"))))
  )
}

plot_card_ui <- function(id, height = CM_PLOT_HEIGHT) {
  div(
    class = "cm-plot",
    actionButton(paste0(id, "_zoom"), NULL, icon = shiny::icon("expand"), class = "cm-zoom", title = "Enlarge"),
    plotly::plotlyOutput(id, height = paste0(height, "px"))
  )
}

#' Server side of plot_tab(): `renderers` is a named list (view value ->
#' function returning a plotly figure). Only the selected view is computed.
register_plot_tab <- function(id, renderers, input, output, session, title = NULL) {
  current <- reactive({
    view <- input[[paste0(id, "_view")]] %||% names(renderers)[1]
    if (!view %in% names(renderers)) view <- names(renderers)[1]
    renderers[[view]]()
  })
  register_plot(paste0(id, "_plot"), current, input, output, session, title)
}

register_plot <- function(id, figure, input, output, session, title = NULL) {
  output[[id]] <- plotly::renderPlotly(figure())
  output[[paste0(id, "_big")]] <- plotly::renderPlotly(figure())
  observeEvent(input[[paste0(id, "_zoom")]], {
    showModal(modalDialog(
      plotly::plotlyOutput(session$ns(paste0(id, "_big")), height = "78vh"),
      title = title, size = "l", easyClose = TRUE, footer = NULL
    ))
  })
}

register_table_tab <- function(id, renderers, input, output) {
  output[[paste0(id, "_table")]] <- reactable::renderReactable({
    view <- input[[paste0(id, "_view")]] %||% names(renderers)[1]
    if (!view %in% names(renderers)) view <- names(renderers)[1]
    cm_table(renderers[[view]]())
  })
}

cm_table <- function(df, page_size = 10, searchable = TRUE) {
  if (is.null(df) || !nrow(df)) {
    return(reactable::reactable(data.frame(Info = "Nothing to show"), sortable = FALSE))
  }
  reactable::reactable(
    df, searchable = searchable && nrow(df) > page_size, pagination = nrow(df) > page_size,
    defaultPageSize = page_size, showPageSizeOptions = nrow(df) > page_size,
    pageSizeOptions = c(10, 25, 50, 100), resizable = TRUE, highlight = TRUE, compact = TRUE,
    borderless = TRUE, wrap = FALSE, theme = cm_table_theme(),
    defaultColDef = reactable::colDef(minWidth = 90, headerClass = "cm-th"),
    language = reactable::reactableLang(searchPlaceholder = "Search...")
  )
}

cm_table_theme <- function() {
  reactable::reactableTheme(
    color = CM_COLORS$ink, backgroundColor = "transparent", borderColor = CM_COLORS$line,
    stripedColor = "#f7f8fb", highlightColor = "#eef3f9",
    cellPadding = "7px 10px", style = list(fontFamily = CM_FONT, fontSize = "13px"),
    headerStyle = list(color = CM_COLORS$ink_soft, fontWeight = 700, fontSize = "12px",
                       textTransform = "uppercase", letterSpacing = "0.04em"),
    searchInputStyle = list(borderRadius = "10px", border = paste("1px solid", CM_COLORS$line), width = "220px")
  )
}

#' Run a compute function with a progress bar and user-friendly errors.
run_with_progress <- function(message, fun) {
  withProgress(message = message, value = 0, {
    progress <- function(value, detail = NULL) setProgress(value = value, detail = detail)
    tryCatch(
      fun(progress),
      error = function(e) {
        if (!inherits(e, "shiny.silent.error")) showNotification(conditionMessage(e), type = "error", duration = 8)
        NULL
      }
    )
  })
}

notify_done <- function(message) showNotification(message, type = "message", duration = 3)

stat_tile <- function(label, value, icon = "circle", color = "blue") {
  div(class = paste("cm-tile", paste0("cm-tile-", color)),
      div(class = "cm-tile-icon", shiny::icon(icon)),
      div(class = "cm-tile-body", div(class = "cm-tile-value", value), div(class = "cm-tile-label", label)))
}

# Top-level layout: header, sidebar navigation and one tab per module.

loading_screen <- function() {
  div(
    class = "cm-loading",
    div(class = "cm-loading-title", "CatMath"),
    div(class = "cm-loading-track", div(class = "cm-loading-cat")),
    div(class = "cm-loading-text", "Warming up the paws...")
  )
}

app_sidebar <- function() {
  shinydashboard::dashboardSidebar(
    width = 240,
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Home", tabName = "home", icon = icon("house")),
      shinydashboard::menuItem("Data", tabName = "upload", icon = icon("database")),
      shinydashboard::menuItem("Project", tabName = "project", icon = icon("folder-open")),
      shinydashboard::menuItem(
        "Preprocessing", icon = icon("broom"), startExpanded = FALSE,
        shinydashboard::menuSubItem("Feature selection", tabName = "prep_select"),
        shinydashboard::menuSubItem("Custom handling", tabName = "prep_custom"),
        shinydashboard::menuSubItem("Missing data", tabName = "prep_missing"),
        shinydashboard::menuSubItem("Transformation", tabName = "prep_transform")
      ),
      shinydashboard::menuItem(
        "Explore", icon = icon("magnifying-glass-chart"),
        shinydashboard::menuSubItem("Data visualisation", tabName = "eda"),
        shinydashboard::menuSubItem("Statistical tests", tabName = "stats")
      ),
      shinydashboard::menuItem(
        "Dimensionality reduction", icon = icon("compress"),
        shinydashboard::menuSubItem("PCA", tabName = "pca"),
        shinydashboard::menuSubItem("NMF", tabName = "nmf")
      ),
      shinydashboard::menuItem(
        "Machine learning", icon = icon("brain"),
        shinydashboard::menuSubItem("Linear regression", tabName = "lm"),
        shinydashboard::menuSubItem("K-means", tabName = "kmeans"),
        shinydashboard::menuSubItem("k-nearest neighbours", tabName = "knn"),
        shinydashboard::menuSubItem("Random forest", tabName = "rf")
      )
    ),
    uiOutput("dataset_badge")
  )
}

app_ui <- function() {
  tab <- shinydashboard::tabItem
  shinydashboard::dashboardPage(
    title = "CatMath",
    skin = "blue",
    shinydashboard::dashboardHeader(
      title = tags$span(class = "cm-logo", icon("cat"), "CatMath"),
      titleWidth = 240
    ),
    app_sidebar(),
    shinydashboard::dashboardBody(
      tags$head(
        tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
        tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Nunito:wght@400;600;700;800&family=Quicksand:wght@500;600;700&display=swap"),
        tags$link(rel = "stylesheet", type = "text/css", href = "catmath.css"),
        tags$link(rel = "icon", type = "image/svg+xml", href = "assets/cat.svg")
      ),
      waiter::useWaiter(),
      waiter::waiterShowOnLoad(html = loading_screen(), color = CM_COLORS$paper),
      shinydashboard::tabItems(
        tab("home", home_ui("home")),
        tab("upload", upload_ui("upload")),
        tab("project", project_ui("project")),
        tab("prep_select", prep_select_ui("prep_select")),
        tab("prep_custom", prep_custom_ui("prep_custom")),
        tab("prep_missing", prep_missing_ui("prep_missing")),
        tab("prep_transform", prep_transform_ui("prep_transform")),
        tab("eda", eda_ui("eda")),
        tab("stats", stats_ui("stats")),
        tab("pca", pca_ui("pca")),
        tab("nmf", nmf_ui("nmf")),
        tab("lm", lm_ui("lm")),
        tab("kmeans", kmeans_ui("kmeans")),
        tab("knn", knn_ui("knn")),
        tab("rf", rf_ui("rf"))
      )
    )
  )
}

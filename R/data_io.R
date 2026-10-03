# Dataset loading and descriptive summaries.

SUPPORTED_EXTENSIONS <- c("csv", "tsv", "txt")

#' Read a delimited text file into a data.frame.
#'
#' Uses data.table::fread (multi-threaded C parser). Column names are made
#' syntactic so they can be used safely in model formulas.
read_dataset <- function(path, name = basename(path), header = TRUE, sep = "auto",
                         quote = "\"", dec = ".", na_strings = c("", "NA", "NaN")) {
  ext <- tolower(tools::file_ext(name))
  if (!ext %in% SUPPORTED_EXTENSIONS) {
    stop(sprintf("Unsupported file type '.%s'. Use .csv, .tsv or .txt.", ext), call. = FALSE)
  }
  df <- data.table::fread(
    path, header = header, sep = sep, quote = quote, dec = dec,
    na.strings = na_strings, data.table = FALSE, check.names = TRUE,
    stringsAsFactors = FALSE, showProgress = FALSE
  )
  prepare_dataset(df)
}

# Drop empty columns and normalise column types (logical/factor -> character).
prepare_dataset <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (ncol(df) == 0 || nrow(df) == 0) stop("The file contains no data.", call. = FALSE)
  empty <- vapply(df, function(x) all(is.na(x)), logical(1))
  df <- df[!empty]
  for (v in names(df)) {
    if (is.logical(df[[v]]) || is.factor(df[[v]])) df[[v]] <- as.character(df[[v]])
  }
  rownames(df) <- NULL
  df
}

example_path <- function(file) file.path("data", file)

read_example <- function(file) read_dataset(example_path(file))

summarize_numeric <- function(df, vars = numeric_vars(df)) {
  if (length(vars) == 0) return(data.frame())
  stats <- vapply(df[vars], function(x) {
    q <- stats::quantile(x, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE, names = FALSE)
    c(q[1], q[2], q[3], mean(x, na.rm = TRUE), q[4], q[5], stats::sd(x, na.rm = TRUE), sum(is.na(x)))
  }, numeric(8))
  out <- data.frame(
    Variable = vars,
    Min = stats[1, ], Q1 = stats[2, ], Median = stats[3, ], Mean = stats[4, ],
    Q3 = stats[5, ], Max = stats[6, ], SD = stats[7, ],
    Missing = as.integer(stats[8, ]),
    `Missing %` = round(100 * stats[8, ] / nrow(df), 2),
    check.names = FALSE, row.names = NULL
  )
  signif_df(out)
}

summarize_categorical <- function(df, vars = categorical_vars(df), top = 6) {
  if (length(vars) == 0) return(data.frame())
  rows <- lapply(vars, function(v) {
    x <- df[[v]]
    counts <- sort(table(x, useNA = "no"), decreasing = TRUE)
    shown <- utils::head(counts, top)
    label <- paste0(names(shown), " (", as.integer(shown), ")", collapse = ", ")
    if (length(counts) > top) label <- paste0(label, ", ...")
    na <- sum(is.na(x))
    data.frame(
      Variable = v, Levels = length(counts), `Most frequent` = label,
      Missing = na, `Missing %` = round(100 * na / length(x), 2),
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

dataset_overview <- function(df) {
  list(
    rows = nrow(df), cols = ncol(df),
    numeric = length(numeric_vars(df)), categorical = length(categorical_vars(df)),
    missing = sum(is.na(df))
  )
}

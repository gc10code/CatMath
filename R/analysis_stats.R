# Group comparison tests with automatic choice of the appropriate test.
#
# For every (categorical, numeric) pair three families are available:
#   pairwise   - two-level comparisons: Student t / Welch t / Wilcoxon rank-sum
#   variance   - two-level variance tests: Fisher F / Brown-Forsythe (Levene)
#   multigroup - all levels at once: ANOVA / Welch ANOVA / Kruskal-Wallis
# Results come back as one tidy data.frame with BH-adjusted p-values.

STAT_FAMILIES <- c(
  "Pairwise mean/median comparison" = "pairwise",
  "Pairwise variance comparison" = "variance",
  "Multi-group comparison" = "multigroup"
)

is_normal <- function(x, alpha = 0.05, seed = 1) {
  n <- length(x)
  if (n < 3 || length(unique(x)) < 3) return(FALSE)
  if (n > CM_LIMITS$normality_sample) x <- with_seed(seed, sample(x, CM_LIMITS$normality_sample))
  stats::shapiro.test(x)$p.value > alpha
}

# Brown-Forsythe test (median-centred Levene test, as car::leveneTest default).
brown_forsythe <- function(x, g) {
  g <- droplevels(factor(g))
  z <- abs(x - stats::ave(x, g, FUN = stats::median))
  res <- stats::oneway.test(z ~ g, var.equal = TRUE)
  list(statistic = unname(res$statistic), p.value = res$p.value)
}

stat_row <- function(family, cat_var, num_var = NA_character_, groups = NA_character_,
                     test = NA_character_, normal = NA, equal_var = NA,
                     statistic = NA_real_, p_value = NA_real_, message = "") {
  list(
    family = family, cat_var = cat_var, num_var = num_var, groups = groups, test = test,
    normal = normal, equal_var = equal_var, statistic = as.numeric(statistic),
    p_value = as.numeric(p_value), status = if (is.na(p_value)) "error" else "success",
    message = message
  )
}

pairwise_tests <- function(xs, normal, cat_var, num_var, alpha, families) {
  rows <- list()
  levels <- names(xs)
  pairs <- utils::combn(levels, 2, simplify = FALSE)
  for (pr in pairs) {
    a <- xs[[pr[1]]]; b <- xs[[pr[2]]]
    label <- paste(pr, collapse = " vs ")
    problem <- if (length(a) < 3 || length(b) < 3) "fewer than 3 observations in a group"
               else if (stats::var(a) == 0 || stats::var(b) == 0) "zero variance in a group"
               else NULL
    both_normal <- normal[[pr[1]]] && normal[[pr[2]]]
    equal_var <- NA
    if (is.null(problem)) {
      bf <- brown_forsythe(c(a, b), rep(1:2, c(length(a), length(b))))
      equal_var <- bf$p.value > alpha
    }
    if ("pairwise" %in% families) {
      if (!is.null(problem)) {
        rows[[length(rows) + 1]] <- stat_row("pairwise", cat_var, num_var, label, message = problem)
      } else {
        res <- if (both_normal) stats::t.test(a, b, var.equal = equal_var)
               else stats::wilcox.test(a, b, exact = FALSE)
        test <- if (!both_normal) "Wilcoxon rank-sum" else if (equal_var) "Student t-test" else "Welch t-test"
        rows[[length(rows) + 1]] <- stat_row("pairwise", cat_var, num_var, label, test,
                                             both_normal, equal_var, res$statistic, res$p.value)
      }
    }
    if ("variance" %in% families) {
      if (!is.null(problem)) {
        rows[[length(rows) + 1]] <- stat_row("variance", cat_var, num_var, label, message = problem)
      } else if (both_normal) {
        res <- stats::var.test(a, b)
        rows[[length(rows) + 1]] <- stat_row("variance", cat_var, num_var, label, "Fisher F-test",
                                             TRUE, NA, res$statistic, res$p.value)
      } else {
        rows[[length(rows) + 1]] <- stat_row("variance", cat_var, num_var, label, "Brown-Forsythe (Levene)",
                                             FALSE, NA, bf$statistic, bf$p.value)
      }
    }
  }
  rows
}

multigroup_test <- function(x, g, normal, cat_var, num_var, alpha) {
  sizes <- table(g)
  label <- sprintf("%d groups", length(sizes))
  if (any(sizes < 2)) return(stat_row("multigroup", cat_var, num_var, label, message = "a group has fewer than 2 observations"))
  bf <- tryCatch(brown_forsythe(x, g), error = function(e) NULL)
  if (is.null(bf) || is.na(bf$p.value)) {
    return(stat_row("multigroup", cat_var, num_var, label, message = "variance homogeneity test failed"))
  }
  equal_var <- bf$p.value > alpha
  all_normal <- all(normal)
  if (all_normal) {
    res <- stats::oneway.test(x ~ g, var.equal = equal_var)
    test <- if (equal_var) "One-way ANOVA" else "Welch ANOVA"
  } else {
    res <- stats::kruskal.test(x, g)
    test <- "Kruskal-Wallis"
  }
  stat_row("multigroup", cat_var, num_var, label, test, all_normal, equal_var, res$statistic, res$p.value)
}

run_stat_tests <- function(data, num_vars, cat_vars, families = unname(STAT_FAMILIES),
                           alpha = 0.05, progress = NULL) {
  progress <- progress_or_noop(progress)
  num_vars <- require_vars(data, intersect(num_vars, numeric_vars(data)), 1, "numeric variable")
  cat_vars <- require_vars(data, cat_vars, 1, "categorical variable")
  if (!length(families)) stop("Select at least one test family.", call. = FALSE)

  rows <- list()
  total <- length(cat_vars) * length(num_vars)
  step <- 0
  for (cv in cat_vars) {
    g_all <- data[[cv]]
    if (length(unique(stats::na.omit(g_all))) < 2) {
      for (f in families) rows[[length(rows) + 1]] <- stat_row(f, cv, message = "fewer than two levels")
      step <- step + length(num_vars)
      next
    }
    for (nv in num_vars) {
      ok <- !is.na(g_all) & !is.na(data[[nv]])
      x <- data[[nv]][ok]
      g <- droplevels(factor(g_all[ok]))
      xs <- split(x, g)
      normal <- vapply(xs, is_normal, logical(1), alpha = alpha)   # computed once per pair of variables
      if (any(c("pairwise", "variance") %in% families)) {
        rows <- c(rows, pairwise_tests(xs, normal, cv, nv, alpha, families))
      }
      if ("multigroup" %in% families) {
        rows[[length(rows) + 1]] <- multigroup_test(x, g, normal, cv, nv, alpha)
      }
      step <- step + 1
      progress(step / total, sprintf("%s ~ %s", nv, cv))
    }
  }
  out <- as.data.frame(data.table::rbindlist(rows))
  out$p_adj <- NA_real_
  for (f in unique(out$family)) {
    sel <- out$family == f
    out$p_adj[sel] <- stats::p.adjust(out$p_value[sel], method = "BH")
  }
  out$significant <- out$p_adj < alpha
  structure(list(type = "stats", tests = out, alpha = alpha, families = families), class = "cm_result")
}

stats_table <- function(res, family) {
  df <- res$tests[res$tests$family == family, , drop = FALSE]
  data.frame(
    Grouping = df$cat_var, Variable = df$num_var, Groups = df$groups, Test = df$test,
    Normal = df$normal, `Equal var.` = df$equal_var,
    Statistic = signif(df$statistic, 4), p = format_p(df$p_value), `p (BH)` = format_p(df$p_adj),
    Significant = df$significant, Note = df$message,
    check.names = FALSE
  )
}

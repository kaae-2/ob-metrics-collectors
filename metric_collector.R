#!/usr/bin/env Rscript

required_packages <- c(
  "jsonlite",
  "dplyr",
  "tidyr",
  "readr",
  "ggplot2",
  "stringr",
  "rmarkdown",
  "knitr",
  "scales"
)

user_lib <- Sys.getenv("R_LIBS_USER")
if (user_lib == "") {
  user_lib <- file.path(Sys.getenv("HOME"), "R", "library")
}
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(unique(c(user_lib, .libPaths())))

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org", lib = user_lib)
}

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(stringr)
  library(rmarkdown)
  library(knitr)
  library(scales)
})

parse_cli_args <- function() {
  has_argparse <- requireNamespace("argparse", quietly = TRUE)
  if (has_argparse) {
    parser <- argparse::ArgumentParser(
      description = "Collect and summarize Omnibenchmark metrics"
    )
    parser$add_argument(
      "--metrics.scores",
      dest = "metrics_scores",
      type = "character",
      nargs = "+",
      required = TRUE,
      help = "Metric score file(s) or directories to search"
    )
    parser$add_argument(
      "--data.order",
      dest = "data_order",
      type = "character",
      nargs = "+",
      required = TRUE,
      help = "Order JSON file(s) from data_import"
    )
    parser$add_argument(
      "--output_dir",
      type = "character",
      required = TRUE,
      help = "Output directory"
    )
    parser$add_argument("--name", type = "character", required = TRUE)
    return(parser$parse_args())
  }

  args <- commandArgs(trailingOnly = TRUE)
  parsed <- list(
    metrics_scores = character(),
    data_order = character(),
    output_dir = NULL,
    name = NULL
  )
  i <- 1
  while (i <= length(args)) {
    key <- args[[i]]
    if (startsWith(key, "--")) {
      key <- sub("^--", "", key)
      value <- NULL
      if (grepl("=", key)) {
        parts <- strsplit(key, "=", fixed = TRUE)[[1]]
        key <- parts[[1]]
        value <- parts[[2]]
      } else if (i < length(args)) {
        value <- args[[i + 1]]
        i <- i + 1
      }
      if (key == "metrics.scores") {
        parsed$metrics_scores <- c(parsed$metrics_scores, value)
      } else if (key == "data.order") {
        parsed$data_order <- c(parsed$data_order, value)
      } else if (key == "output_dir") {
        parsed$output_dir <- value
      } else if (key == "name") {
        parsed$name <- value
      }
    }
    i <- i + 1
  }

  if (length(parsed$metrics_scores) == 0) {
    stop("--metrics.scores is required")
  }
  if (length(parsed$data_order) == 0) {
    stop("--data.order is required")
  }
  if (is.null(parsed$output_dir) || parsed$output_dir == "") {
    stop("--output_dir is required")
  }
  if (is.null(parsed$name) || parsed$name == "") {
    stop("--name is required")
  }
  parsed
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  x
}

normalize_paths <- function(values) {
  unique(values[!is.na(values) & values != ""])
}

sanitize_label <- function(value) {
  cleaned <- str_replace_all(as.character(value), "[^A-Za-z0-9._-]+", "-")
  cleaned <- str_replace_all(cleaned, "-+", "-")
  str_replace_all(cleaned, "^-|-$", "")
}

dataset_label_cache <- new.env(parent = emptyenv())

dataset_label_from_path <- function(path) {
  if (length(path) == 0) {
    return(NULL)
  }
  labels <- vapply(path, function(item) {
    normalized <- str_replace_all(item, "\\\\", "/")
    match <- str_match(normalized, "(.*/data/[^/]+/[^/]+)")
    if (is.na(match[, 2])) {
      return(NA_character_)
    }
    dataset_root <- match[, 2]
    if (exists(dataset_root, envir = dataset_label_cache, inherits = FALSE)) {
      return(get(dataset_root, envir = dataset_label_cache, inherits = FALSE))
    }
    parameters_path <- file.path(dataset_root, "parameters.json")
    if (!file.exists(parameters_path)) {
      assign(dataset_root, basename(dataset_root), envir = dataset_label_cache)
      return(basename(dataset_root))
    }
    parameters <- jsonlite::fromJSON(parameters_path)
    if (length(parameters) == 0) {
      assign(dataset_root, basename(dataset_root), envir = dataset_label_cache)
      return(basename(dataset_root))
    }
    keys <- sort(names(parameters))
    parts <- vapply(keys, function(key) {
      value <- parameters[[key]]
      sprintf("%s-%s", sanitize_label(key), sanitize_label(value))
    }, character(1))
    label <- paste(parts, collapse = "_")
    assign(dataset_root, label, envir = dataset_label_cache)
    label
  }, character(1))
  labels
}

dataset_root_from_path <- function(path) {
  normalized <- str_replace_all(path, "\\\\", "/")
  match <- str_match(normalized, "(.*/data/[^/]+/[^/]+)")
  ifelse(is.na(match[, 2]), NA_character_, match[, 2])
}

read_order_sample_count <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Order file not found: %s", path))
  }
  con <- gzfile(path, open = "rt")
  on.exit(close(con), add = TRUE)
  payload <- paste(readLines(con, warn = FALSE), collapse = "")
  data <- jsonlite::fromJSON(payload)
  if (!is.list(data) || is.null(data$order)) {
    stop(sprintf("Order JSON missing 'order' key: %s", path))
  }
  order <- data$order
  if (!is.vector(order) || length(order) == 0) {
    stop(sprintf("Order JSON must contain a non-empty list: %s", path))
  }
  length(order)
}

build_order_map <- function(paths) {
  labels <- dataset_label_from_path(paths)
  mapping <- list()
  for (idx in seq_along(paths)) {
    path <- paths[[idx]]
    dataset <- labels[[idx]]
    if (is.na(dataset) || dataset == "") {
      dataset <- basename(dirname(path))
    }
    sample_count <- read_order_sample_count(path)
    existing <- mapping[[dataset]] %||% NA_integer_
    if (!is.na(existing) && existing != sample_count) {
      stop(
        sprintf(
          "Conflicting sample counts for dataset '%s' (got %s and %s).",
          dataset,
          existing,
          sample_count
        )
      )
    }
    mapping[[dataset]] <- sample_count
  }
  mapping
}

read_preprocessing_num <- function(dataset_root, crossvalidation) {
  if (is.na(dataset_root) || dataset_root == "") {
    return(NA_integer_)
  }
  params_path <- file.path(
    dataset_root,
    "preprocessing",
    "data_preprocessing",
    crossvalidation,
    "parameters.json"
  )
  if (!file.exists(params_path)) {
    return(NA_integer_)
  }
  params <- jsonlite::fromJSON(params_path)
  if (!is.list(params) || is.null(params$num)) {
    return(NA_integer_)
  }
  as.integer(params$num)
}

expand_metric_inputs <- function(inputs) {
  paths <- c()
  for (entry in inputs) {
    if (dir.exists(entry)) {
      candidates <- list.files(
        entry,
        pattern = "\\.flow_metrics\\.json\\.gz$",
        recursive = TRUE,
        full.names = TRUE
      )
      paths <- c(paths, candidates)
      next
    }
    if (length(Sys.glob(entry)) > 0) {
      paths <- c(paths, Sys.glob(entry))
      next
    }
    if (file.exists(entry)) {
      paths <- c(paths, entry)
    }
  }
  normalize_paths(paths)
}

read_metrics_json <- function(path) {
  con <- gzfile(path, open = "rt")
  on.exit(close(con), add = TRUE)
  payload <- paste(readLines(con, warn = FALSE), collapse = "")
  payload <- gsub("\\bNaN\\b", "null", payload)
  payload <- gsub("\\bInfinity\\b", "null", payload)
  payload <- gsub("\\b-Infinity\\b", "null", payload)
  jsonlite::fromJSON(payload, simplifyVector = FALSE)
}

extract_match <- function(path, pattern, default_value) {
  match <- str_match(path, pattern)
  ifelse(is.na(match[, 2]), default_value, match[, 2])
}

parse_lineage <- function(path, payload) {
  normalized <- str_replace_all(path, "\\\\", "/")
  dataset_label <- dataset_label_from_path(normalized)
  dataset <- dataset_label %||% str_replace(
    basename(normalized),
    "\\.flow_metrics\\.json\\.gz$",
    ""
  )
  dataset <- ifelse(dataset == "", payload$name %||% "unknown_dataset", dataset)
  model <- extract_match(normalized, "/analysis/([^/]+)/", "unknown_model")
  crossvalidation <- extract_match(
    normalized,
    "/preprocessing/[^/]+/([^/]+)/",
    "unknown_crossvalidation"
  )
  list(dataset = dataset, model = model, crossvalidation = crossvalidation)
}

compute_weighted_f1 <- function(per_population) {
  if (is.null(per_population) || length(per_population) == 0) {
    return(list(weighted_f1 = NA_real_, total_n = NA_real_))
  }
  pop_entries <- lapply(per_population, function(entry) {
    f1 <- as.numeric(entry$f1 %||% NA_real_)
    n_val <- entry$n %||% entry$support %||% NA_real_
    list(f1 = f1, n = as.numeric(n_val))
  })
  f1_vals <- sapply(pop_entries, function(entry) entry$f1)
  n_vals <- sapply(pop_entries, function(entry) entry$n)
  total_n <- sum(n_vals, na.rm = TRUE)
  if (is.na(total_n) || total_n == 0) {
    return(list(weighted_f1 = NA_real_, total_n = total_n))
  }
  weighted <- sum(f1_vals * n_vals, na.rm = TRUE) / total_n
  list(weighted_f1 = weighted, total_n = total_n)
}

extract_population_label <- function(entry, population_id = NA_character_) {
  candidates <- c(
    entry$population_name,
    entry$population,
    entry$label,
    entry$name,
    entry$id,
    entry$class,
    population_id
  )
  candidates <- candidates[!is.na(candidates) & candidates != ""]
  if (length(candidates) == 0) {
    return("unknown_population")
  }
  as.character(candidates[[1]])
}

collect_metrics <- function(path) {
  payload <- read_metrics_json(path)
  results <- payload$results
  if (is.null(results) || length(results) == 0) {
    return(tibble())
  }
  lineage <- parse_lineage(path, payload)
  rows <- lapply(names(results), function(run_id) {
    run <- results[[run_id]]
    weighted <- compute_weighted_f1(run$per_population)
    n_samples <- run$n %||% weighted$total_n
    tibble(
      dataset = lineage$dataset,
      model = lineage$model,
      crossvalidation = lineage$crossvalidation,
      run_id = run_id,
      f1_macro = as.numeric(run$f1_macro %||% NA_real_),
      precision_macro = as.numeric(run$precision_macro %||% NA_real_),
      recall_macro = as.numeric(run$recall_macro %||% NA_real_),
      accuracy = as.numeric(run$accuracy %||% NA_real_),
      mcc = as.numeric(run$mcc %||% NA_real_),
      pop_freq_corr = as.numeric(run$pop_freq_corr %||% NA_real_),
      overlap = as.numeric(run$overlap %||% NA_real_),
      runtime_seconds = as.numeric(run$runtime_seconds %||% NA_real_),
      scalability_seconds_per_item = as.numeric(
        run$scalability_seconds_per_item %||% NA_real_
      ),
      f1_weighted = as.numeric(weighted$weighted_f1),
      n_samples = as.numeric(n_samples),
      source_path = path
    )
  })
  bind_rows(rows)
}

collect_per_population <- function(path) {
  payload <- read_metrics_json(path)
  results <- payload$results
  if (is.null(results) || length(results) == 0) {
    return(tibble())
  }
  lineage <- parse_lineage(path, payload)
  rows <- lapply(names(results), function(run_id) {
    run <- results[[run_id]]
    per_population <- run$per_population
    if (is.null(per_population) || length(per_population) == 0) {
      return(NULL)
    }
    pop_rows <- lapply(names(per_population), function(pop_id) {
      entry <- per_population[[pop_id]]
      f1 <- as.numeric(entry$f1 %||% NA_real_)
      n_val <- entry$n %||% entry$support %||% NA_real_
      tibble(
        dataset = lineage$dataset,
        model = lineage$model,
        crossvalidation = lineage$crossvalidation,
        run_id = run_id,
        population_id = as.character(pop_id),
        population_name = as.character(entry$population_name %||% NA_character_),
        population = extract_population_label(entry, pop_id),
        f1 = f1,
        precision = as.numeric(entry$precision %||% NA_real_),
        recall = as.numeric(entry$recall %||% NA_real_),
        accuracy = as.numeric(entry$accuracy %||% NA_real_),
        tp = as.numeric(entry$tp %||% NA_real_),
        fp = as.numeric(entry$fp %||% NA_real_),
        fn = as.numeric(entry$fn %||% NA_real_),
        tn = as.numeric(entry$tn %||% NA_real_),
        scaling_rate = as.numeric(entry$scaling_rate %||% NA_real_),
        support = as.numeric(n_val),
        source_path = path
      )
    })
    bind_rows(pop_rows)
  })
  bind_rows(rows)
}

write_table <- function(df, path) {
  if (nrow(df) == 0) {
    writeLines("", path)
    return(invisible())
  }
  readr::write_tsv(df, path)
}

plot_boxplot <- function(
  df,
  value_col,
  output_path,
  title,
  x_col = "model",
  facet_col = "dataset"
) {
  if (nrow(df) == 0) {
    return(invisible())
  }
  plot <- ggplot(df, aes(x = .data[[x_col]], y = .data[[value_col]])) +
    geom_boxplot(outlier.alpha = 0.4) +
    geom_jitter(width = 0.15, alpha = 0.35, size = 1.5) +
    labs(title = title, x = x_col, y = value_col) +
    theme_minimal(base_size = 12)
  if (!is.null(facet_col) && facet_col != "") {
    plot <- plot + facet_wrap(as.formula(paste("~", facet_col)), scales = "free_x")
  }
  ggsave(output_path, plot, width = 10, height = 6, dpi = 150)
}

compute_support_entropy <- function(supports) {
  values <- supports[!is.na(supports) & supports > 0]
  total <- sum(values)
  if (length(values) == 0 || total == 0) {
    return(NA_real_)
  }
  probs <- values / total
  -sum(probs * log2(probs))
}

bucket_support_fraction <- function(fraction) {
  if (is.na(fraction)) {
    return(NA_character_)
  }
  if (fraction < 0.01) {
    return("<1%")
  }
  if (fraction < 0.05) {
    return("1-5%")
  }
  if (fraction < 0.2) {
    return("5-20%")
  }
  ">20%"
}

parse_performance <- function(path) {
  empty_performance <- function() {
    tibble::tibble(
      dataset = character(),
      model = character(),
      crossvalidation = character(),
      runtime_seconds = numeric()
    )
  }
  if (!file.exists(path)) {
    return(empty_performance())
  }
  perf <- readr::read_tsv(path, show_col_types = FALSE, progress = FALSE)
  if (!"module" %in% names(perf) || !"path" %in% names(perf)) {
    return(empty_performance())
  }
  perf <- perf %>%
    filter(module == "flow_metrics") %>%
    mutate(
      normalized_path = str_replace_all(path, "\\\\", "/"),
      model = extract_match(normalized_path, "/analysis/([^/]+)/", "unknown_model"),
      crossvalidation = extract_match(
        normalized_path,
        "/preprocessing/[^/]+/([^/]+)/",
        "unknown_crossvalidation"
      ),
      dataset = dataset_label_from_path(normalized_path),
      runtime_seconds = as.numeric(.data[["s"]] %||% NA_real_)
    ) %>%
    mutate(
      dataset = ifelse(
        is.na(dataset),
        str_match(
          params %||% "",
          "dataset_name\\\"\\s*:\\s*\\\"([^\\\"]+)\\\""
        )[, 2],
        dataset
      ),
      dataset = ifelse(is.na(dataset), "unknown_dataset", dataset)
    )
  perf
}

plot_runtime_scatter <- function(df, value_col, output_path, title) {
  if (nrow(df) == 0) {
    return(invisible())
  }
  plot <- ggplot(df, aes(x = n_samples, y = .data[[value_col]])) +
    geom_point(aes(size = runtime_seconds), alpha = 0.6) +
    scale_size_continuous(labels = label_number()) +
    facet_wrap(~dataset, scales = "free") +
    labs(
      title = title,
      x = "Number of cells",
      y = value_col,
      size = "Runtime (s)"
    ) +
    theme_minimal(base_size = 12)
  ggsave(output_path, plot, width = 10, height = 6, dpi = 150)
}

plot_simple_scatter <- function(
  df,
  x_col,
  y_col,
  output_path,
  title,
  facet_col = NULL
) {
  if (nrow(df) == 0) {
    return(invisible())
  }
  plot <- ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_point(alpha = 0.6) +
    labs(title = title, x = x_col, y = y_col) +
    theme_minimal(base_size = 12)
  if (!is.null(facet_col) && facet_col != "") {
    plot <- plot + facet_wrap(as.formula(paste("~", facet_col)), scales = "free")
  }
  ggsave(output_path, plot, width = 10, height = 6, dpi = 150)
}

render_report <- function(output_dir, plot_paths, tables, name, performance_note = NULL) {
  report_path <- file.path(output_dir, "metrics_report.Rmd")
  output_html <- file.path(output_dir, "metrics_report.html")
  if (!rmarkdown::pandoc_available()) {
    macro_table <- readr::read_tsv(
      file.path(output_dir, tables$macro_by_cv),
      show_col_types = FALSE
    )
    population_table <- readr::read_tsv(
      file.path(output_dir, tables$weighted_by_cv),
      show_col_types = FALSE
    )
    run_metrics_table <- readr::read_tsv(
      file.path(output_dir, tables$run_metrics),
      show_col_types = FALSE
    )
    per_population_summary_table <- readr::read_tsv(
      file.path(output_dir, tables$per_population_summary),
      show_col_types = FALSE
    )
    per_population_stability_table <- readr::read_tsv(
      file.path(output_dir, tables$per_population_stability),
      show_col_types = FALSE
    )
    per_population_confusion_table <- readr::read_tsv(
      file.path(output_dir, tables$per_population_confusion),
      show_col_types = FALSE
    )
    rare_population_table <- readr::read_tsv(
      file.path(output_dir, tables$rare_population),
      show_col_types = FALSE
    )
    dataset_context_table <- readr::read_tsv(
      file.path(output_dir, tables$dataset_context),
      show_col_types = FALSE
    )
    dominant_fnr_table <- readr::read_tsv(
      file.path(output_dir, tables$dominant_fnr),
      show_col_types = FALSE
    )
    dominant_fpr_table <- readr::read_tsv(
      file.path(output_dir, tables$dominant_fpr),
      show_col_types = FALSE
    )
    summary_counts <- macro_table %>% summarize(
      datasets = n_distinct(dataset),
      models = n_distinct(model),
      crossvalidations = n_distinct(crossvalidation)
    )
    outputs <- data.frame(
      file = c(
        tables$macro_by_cv,
        tables$weighted_by_cv,
        tables$macro_summary,
        tables$weighted_summary,
        tables$run_metrics,
        tables$per_population_summary,
        tables$per_population_stability,
        tables$per_population_confusion,
        tables$rare_population,
        tables$dataset_context,
        tables$dominant_fnr,
        tables$dominant_fpr,
        tables$macro_scatter_table,
        tables$weighted_scatter_table
      )
    )
    html_lines <- c(
      "<html>",
      "<head><meta charset=\"utf-8\"></head>",
      "<body>",
      sprintf("<h1>Metrics Report - %s</h1>", name),
      "<h2>Overview</h2>",
      knitr::kable(summary_counts, format = "html"),
      "<h2>Macro F1 By Crossvalidation</h2>",
      knitr::kable(macro_table, format = "html"),
      "<h2>Per-population Metrics By Crossvalidation</h2>",
      knitr::kable(population_table, format = "html"),
      "<h2>Run-level Metrics</h2>",
      knitr::kable(run_metrics_table, format = "html"),
      "<h2>Per-population Summary</h2>",
      knitr::kable(per_population_summary_table, format = "html"),
      "<h2>Per-population Stability</h2>",
      knitr::kable(per_population_stability_table, format = "html"),
      "<h2>Per-population Confusion Stats</h2>",
      knitr::kable(per_population_confusion_table, format = "html"),
      "<h2>Rare Population Buckets</h2>",
      knitr::kable(rare_population_table, format = "html"),
      "<h2>Dataset Context</h2>",
      knitr::kable(dataset_context_table, format = "html"),
      "<h2>Dominant Errors (FNR)</h2>",
      knitr::kable(dominant_fnr_table, format = "html"),
      "<h2>Dominant Errors (FPR)</h2>",
      knitr::kable(dominant_fpr_table, format = "html"),
      "<h2>Plots</h2>",
      if (!is.null(performance_note) && performance_note != "") {
        sprintf("<p><em>Note: %s</em></p>", performance_note)
      } else {
        ""
      },
      if (file.exists(file.path(output_dir, plot_paths$macro_boxplot))) {
        sprintf("<img src=\"%s\" />", plot_paths$macro_boxplot)
      } else {
        ""
      },
      if (file.exists(file.path(output_dir, plot_paths$weighted_boxplot))) {
        sprintf("<img src=\"%s\" />", plot_paths$weighted_boxplot)
      } else {
        ""
      },
      if (file.exists(file.path(output_dir, plot_paths$macro_dataset_boxplot))) {
        sprintf("<img src=\"%s\" />", plot_paths$macro_dataset_boxplot)
      } else {
        ""
      },
      if (file.exists(file.path(output_dir, plot_paths$macro_scatter))) {
        sprintf("<img src=\"%s\" />", plot_paths$macro_scatter)
      } else {
        ""
      },
      if (file.exists(file.path(output_dir, plot_paths$weighted_scatter))) {
        sprintf("<img src=\"%s\" />", plot_paths$weighted_scatter)
      } else {
        ""
      },
      if (file.exists(file.path(output_dir, plot_paths$runtime_scatter))) {
        sprintf("<img src=\"%s\" />", plot_paths$runtime_scatter)
      } else {
        ""
      },
      if (file.exists(file.path(output_dir, plot_paths$pop_freq_corr_scatter))) {
        sprintf("<img src=\"%s\" />", plot_paths$pop_freq_corr_scatter)
      } else {
        ""
      },
      "<h2>Outputs</h2>",
      knitr::kable(outputs, format = "html"),
      "</body>",
      "</html>"
    )
    writeLines(html_lines, output_html)
    return(invisible())
  }
  note_lines <- if (!is.null(performance_note) && performance_note != "") {
    c(sprintf("Note: %s", performance_note), "")
  } else {
    character(0)
  }
  report_content <- c(
    "---",
    sprintf("title: \"Metrics Report - %s\"", name),
    "output:",
    "  html_document:",
    "    toc: true",
    "    toc_depth: 2",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE)",
    "library(readr)",
    "library(dplyr)",
    "library(knitr)",
    "```",
    "",
    "## Overview",
    "",
    "```{r}",
    sprintf("macro_table <- read_tsv('%s')", tables$macro_by_cv),
    sprintf("population_table <- read_tsv('%s')", tables$weighted_by_cv),
    sprintf("run_metrics_table <- read_tsv('%s')", tables$run_metrics),
    sprintf(
      "per_population_summary_table <- read_tsv('%s')",
      tables$per_population_summary
    ),
    sprintf(
      "per_population_stability_table <- read_tsv('%s')",
      tables$per_population_stability
    ),
    sprintf(
      "per_population_confusion_table <- read_tsv('%s')",
      tables$per_population_confusion
    ),
    sprintf(
      "rare_population_table <- read_tsv('%s')",
      tables$rare_population
    ),
    sprintf(
      "dataset_context_table <- read_tsv('%s')",
      tables$dataset_context
    ),
    sprintf("dominant_fnr_table <- read_tsv('%s')", tables$dominant_fnr),
    sprintf("dominant_fpr_table <- read_tsv('%s')", tables$dominant_fpr),
    "summary_counts <- macro_table %>% summarize(",
    "  datasets = n_distinct(dataset),",
    "  models = n_distinct(model),",
    "  crossvalidations = n_distinct(crossvalidation)",
    ")",
    "kable(summary_counts)",
    "```",
    "",
    "## Macro F1 By Crossvalidation",
    "",
    "```{r}",
    "kable(macro_table)",
    "```",
    "",
    "## Per-population Metrics By Crossvalidation",
    "",
    "```{r}",
    "kable(population_table)",
    "```",
    "",
    "## Run-level Metrics",
    "",
    "```{r}",
    "kable(run_metrics_table)",
    "```",
    "",
    "## Per-population Summary",
    "",
    "```{r}",
    "kable(per_population_summary_table)",
    "```",
    "",
    "## Per-population Stability",
    "",
    "```{r}",
    "kable(per_population_stability_table)",
    "```",
    "",
    "## Per-population Confusion Stats",
    "",
    "```{r}",
    "kable(per_population_confusion_table)",
    "```",
    "",
    "## Rare Population Buckets",
    "",
    "```{r}",
    "kable(rare_population_table)",
    "```",
    "",
    "## Dataset Context",
    "",
    "```{r}",
    "kable(dataset_context_table)",
    "```",
    "",
    "## Dominant Errors (FNR)",
    "",
    "```{r}",
    "kable(dominant_fnr_table)",
    "```",
    "",
    "## Dominant Errors (FPR)",
    "",
    "```{r}",
    "kable(dominant_fpr_table)",
    "```",
    "",
    "## Plots",
    "",
    note_lines,
    "```{r}",
    sprintf("knitr::include_graphics('%s')", plot_paths$macro_boxplot),
    "```",
    "",
    "```{r}",
    sprintf("knitr::include_graphics('%s')", plot_paths$weighted_boxplot),
    "```",
    "",
    "```{r}",
    sprintf(
      "if (file.exists('%s')) knitr::include_graphics('%s')",
      plot_paths$macro_dataset_boxplot,
      plot_paths$macro_dataset_boxplot
    ),
    "```",
    "",
    "```{r}",
    sprintf(
      "if (file.exists('%s')) knitr::include_graphics('%s')",
      plot_paths$macro_scatter,
      plot_paths$macro_scatter
    ),
    "```",
    "",
    "```{r}",
    sprintf(
      "if (file.exists('%s')) knitr::include_graphics('%s')",
      plot_paths$weighted_scatter,
      plot_paths$weighted_scatter
    ),
    "```",
    "",
    "```{r}",
    sprintf(
      "if (file.exists('%s')) knitr::include_graphics('%s')",
      plot_paths$runtime_scatter,
      plot_paths$runtime_scatter
    ),
    "```",
    "",
    "```{r}",
    sprintf(
      "if (file.exists('%s')) knitr::include_graphics('%s')",
      plot_paths$pop_freq_corr_scatter,
      plot_paths$pop_freq_corr_scatter
    ),
    "```",
    "",
    "## Outputs",
    "",
    "```{r}",
    "outputs <- data.frame(",
    "  file = c(",
    sprintf("    '%s',", tables$macro_by_cv),
    sprintf("    '%s',", tables$weighted_by_cv),
    sprintf("    '%s',", tables$macro_summary),
    sprintf("    '%s',", tables$weighted_summary),
    sprintf("    '%s',", tables$run_metrics),
    sprintf("    '%s',", tables$per_population_summary),
    sprintf("    '%s',", tables$per_population_stability),
    sprintf("    '%s',", tables$per_population_confusion),
    sprintf("    '%s',", tables$rare_population),
    sprintf("    '%s',", tables$dataset_context),
    sprintf("    '%s',", tables$dominant_fnr),
    sprintf("    '%s',", tables$dominant_fpr),
    sprintf("    '%s',", tables$macro_scatter_table),
    sprintf("    '%s'", tables$weighted_scatter_table),
    "  )",
    ")",
    "kable(outputs)",
    "```",
    ""
  )
  writeLines(report_content, report_path)
  rmarkdown::render(
    input = report_path,
    output_file = output_html,
    quiet = TRUE
  )
}

args <- parse_cli_args()

args$output_dir <- normalizePath(args$output_dir, winslash = "/", mustWork = FALSE)
if (!dir.exists(args$output_dir)) {
  dir.create(args$output_dir, recursive = TRUE, showWarnings = FALSE)
}
if (!dir.exists(args$output_dir)) {
  stop(sprintf("Output directory could not be created: %s", args$output_dir))
}

input_paths <- expand_metric_inputs(unlist(args$metrics_scores))
if (length(input_paths) == 0) {
  stop("No metrics files found for --metrics.scores")
}

order_paths <- normalize_paths(unlist(args$data_order))
if (length(order_paths) == 0) {
  stop("No order files found for --data.order")
}
missing_order_paths <- order_paths[!file.exists(order_paths)]
if (length(missing_order_paths) > 0) {
  stop(
    sprintf(
      "Order files missing: %s",
      paste(missing_order_paths, collapse = ", ")
    )
  )
}
order_map <- build_order_map(order_paths)

metrics_rows <- lapply(input_paths, collect_metrics)
per_population_rows <- lapply(input_paths, collect_per_population)
metrics_df <- bind_rows(metrics_rows)
per_population_df <- bind_rows(per_population_rows)
if (nrow(metrics_df) == 0) {
  stop("No metrics rows parsed from inputs")
}

missing_datasets <- setdiff(unique(metrics_df$dataset), names(order_map))
if (length(missing_datasets) > 0) {
  stop(
    sprintf(
      "Missing order data for datasets: %s",
      paste(missing_datasets, collapse = ", ")
    )
  )
}

metrics_df <- metrics_df %>%
  mutate(
    dataset_root = dataset_root_from_path(source_path),
    preprocessing_num = vapply(
      seq_len(n()),
      function(idx) {
        read_preprocessing_num(dataset_root[[idx]], crossvalidation[[idx]])
      },
      integer(1)
    ),
    sample_count = vapply(
      dataset,
      function(item) {
        if (is.na(item) || item == "") {
          return(NA_integer_)
        }
        count <- order_map[[item]]
        if (is.null(count)) {
          return(NA_integer_)
        }
        count
      },
      integer(1)
    ),
    effective_crossvalidation = ifelse(
      !is.na(preprocessing_num) & !is.na(sample_count),
      sprintf("num-%d", ((preprocessing_num - 1) %% sample_count) + 1),
      crossvalidation
    )
  )

per_population_df <- per_population_df %>%
  mutate(
    dataset_root = dataset_root_from_path(source_path),
    preprocessing_num = vapply(
      seq_len(n()),
      function(idx) {
        read_preprocessing_num(dataset_root[[idx]], crossvalidation[[idx]])
      },
      integer(1)
    ),
    sample_count = vapply(
      dataset,
      function(item) {
        if (is.na(item) || item == "") {
          return(NA_integer_)
        }
        count <- order_map[[item]]
        if (is.null(count)) {
          return(NA_integer_)
        }
        count
      },
      integer(1)
    ),
    effective_crossvalidation = ifelse(
      !is.na(preprocessing_num) & !is.na(sample_count),
      sprintf("num-%d", ((preprocessing_num - 1) %% sample_count) + 1),
      crossvalidation
    )
  )

deduped_metrics <- metrics_df %>%
  distinct(dataset, model, run_id, effective_crossvalidation, .keep_all = TRUE)
if (nrow(deduped_metrics) < nrow(metrics_df)) {
  warning(
    sprintf(
      "Filtered %d duplicate metric rows after crossvalidation wrap.",
      nrow(metrics_df) - nrow(deduped_metrics)
    ),
    call. = FALSE
  )
}

metrics_df <- deduped_metrics %>%
  mutate(crossvalidation = effective_crossvalidation) %>%
  select(-effective_crossvalidation, -dataset_root, -preprocessing_num, -sample_count)

per_population_df <- per_population_df %>%
  distinct(
    dataset,
    model,
    run_id,
    effective_crossvalidation,
    population_id,
    .keep_all = TRUE
  ) %>%
  mutate(crossvalidation = effective_crossvalidation) %>%
  select(-effective_crossvalidation, -dataset_root, -preprocessing_num, -sample_count)

per_population_df <- per_population_df %>%
  left_join(
    metrics_df %>%
      select(
        dataset,
        model,
        crossvalidation,
        run_id,
        n_samples,
        f1_macro,
        precision_macro,
        recall_macro,
        accuracy,
        mcc,
        pop_freq_corr,
        overlap,
        runtime_seconds,
        scalability_seconds_per_item
      ),
    by = c("dataset", "model", "crossvalidation", "run_id")
  )

macro_table <- metrics_df %>%
  select(dataset, model, crossvalidation, run_id, f1_macro, n_samples) %>%
  arrange(dataset, model, crossvalidation, run_id)

per_population_table <- per_population_df %>%
  mutate(support_fraction = ifelse(n_samples > 0, support / n_samples, NA_real_)) %>%
  select(
    dataset,
    model,
    crossvalidation,
    run_id,
    population_id,
    population_name,
    population,
    f1,
    precision,
    recall,
    accuracy,
    support,
    n_samples,
    support_fraction,
    tp,
    fp,
    fn,
    tn
  ) %>%
  arrange(dataset, model, crossvalidation, run_id, population)

run_metrics_table <- metrics_df %>%
  mutate(
    throughput_events_per_sec = ifelse(
      runtime_seconds > 0,
      n_samples / runtime_seconds,
      NA_real_
    )
  ) %>%
  select(
    dataset,
    model,
    crossvalidation,
    run_id,
    n_samples,
    f1_macro,
    precision_macro,
    recall_macro,
    accuracy,
    mcc,
    pop_freq_corr,
    overlap,
    runtime_seconds,
    scalability_seconds_per_item,
    throughput_events_per_sec
  ) %>%
  arrange(dataset, model, crossvalidation, run_id)

per_population_summary <- per_population_df %>%
  group_by(dataset, model, population_id, population_name, population) %>%
  summarize(
    median_f1 = median(f1, na.rm = TRUE),
    mean_f1 = mean(f1, na.rm = TRUE),
    median_precision = median(precision, na.rm = TRUE),
    median_recall = median(recall, na.rm = TRUE),
    median_support = median(support, na.rm = TRUE),
    n_runs = n(),
    .groups = "drop"
  )

per_population_stability <- per_population_df %>%
  group_by(dataset, model, population_id, population_name, population) %>%
  summarize(
    f1_mean = mean(f1, na.rm = TRUE),
    f1_sd = sd(f1, na.rm = TRUE),
    precision_sd = sd(precision, na.rm = TRUE),
    recall_sd = sd(recall, na.rm = TRUE),
    n_runs = n(),
    .groups = "drop"
  )

per_population_confusion <- per_population_df %>%
  mutate(
    tpr = ifelse((tp + fn) > 0, tp / (tp + fn), NA_real_),
    fpr = ifelse((fp + tn) > 0, fp / (fp + tn), NA_real_),
    fnr = ifelse((fn + tp) > 0, fn / (fn + tp), NA_real_),
    tnr = ifelse((tn + fp) > 0, tn / (tn + fp), NA_real_)
  ) %>%
  select(
    dataset,
    model,
    crossvalidation,
    run_id,
    population_id,
    population_name,
    population,
    tp,
    fp,
    fn,
    tn,
    tpr,
    fpr,
    fnr,
    tnr
  ) %>%
  arrange(dataset, model, crossvalidation, run_id, population)

rare_population_table <- per_population_df %>%
  mutate(support_fraction = ifelse(n_samples > 0, support / n_samples, NA_real_)) %>%
  mutate(rare_bucket = vapply(support_fraction, bucket_support_fraction, character(1))) %>%
  group_by(dataset, model, crossvalidation, run_id, rare_bucket) %>%
  summarize(
    n_populations = n(),
    median_f1 = median(f1, na.rm = TRUE),
    median_precision = median(precision, na.rm = TRUE),
    median_recall = median(recall, na.rm = TRUE),
    total_support = sum(support, na.rm = TRUE),
    n_samples = ifelse(
      all(is.na(n_samples)),
      NA_real_,
      max(n_samples, na.rm = TRUE)
    ),
    support_share = ifelse(
      !all(is.na(n_samples)) && max(n_samples, na.rm = TRUE) > 0,
      sum(support, na.rm = TRUE) / max(n_samples, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  )

dataset_context_table <- per_population_df %>%
  group_by(dataset, model, crossvalidation, run_id) %>%
  summarize(
    n_samples = ifelse(
      all(is.na(n_samples)),
      NA_real_,
      max(n_samples, na.rm = TRUE)
    ),
    n_populations = n_distinct(population),
    min_support = ifelse(all(is.na(support)), NA_real_, min(support, na.rm = TRUE)),
    median_support = ifelse(
      all(is.na(support)),
      NA_real_,
      median(support, na.rm = TRUE)
    ),
    max_support = ifelse(all(is.na(support)), NA_real_, max(support, na.rm = TRUE)),
    imbalance_ratio = ifelse(
      !all(is.na(support)) && min(support, na.rm = TRUE) > 0,
      max(support, na.rm = TRUE) / min(support, na.rm = TRUE),
      NA_real_
    ),
    support_entropy = compute_support_entropy(support),
    .groups = "drop"
  )

dominant_fnr_table <- per_population_confusion %>%
  group_by(dataset, model, crossvalidation, run_id) %>%
  slice_max(order_by = fnr, n = 5, with_ties = FALSE) %>%
  ungroup()

dominant_fpr_table <- per_population_confusion %>%
  group_by(dataset, model, crossvalidation, run_id) %>%
  slice_max(order_by = fpr, n = 5, with_ties = FALSE) %>%
  ungroup()

macro_summary <- metrics_df %>%
  group_by(model) %>%
  summarize(
    median_f1_macro = median(f1_macro, na.rm = TRUE),
    mean_f1_macro = mean(f1_macro, na.rm = TRUE),
    n_runs = n(),
    .groups = "drop"
  )

weighted_summary <- metrics_df %>%
  group_by(model) %>%
  summarize(
    median_f1_weighted = median(f1_weighted, na.rm = TRUE),
    mean_f1_weighted = mean(f1_weighted, na.rm = TRUE),
    n_runs = n(),
    .groups = "drop"
  )

macro_table_path <- file.path(args$output_dir, "f1_macro_by_crossvalidation.tsv")
weighted_table_path <- file.path(args$output_dir, "f1_weighted_by_crossvalidation.tsv")
macro_summary_path <- file.path(args$output_dir, "f1_macro_summary_by_model.tsv")
weighted_summary_path <- file.path(args$output_dir, "f1_weighted_summary_by_model.tsv")
run_metrics_path <- file.path(args$output_dir, "run_metrics.tsv")
per_population_summary_path <- file.path(
  args$output_dir,
  "per_population_summary.tsv"
)
per_population_stability_path <- file.path(
  args$output_dir,
  "per_population_stability.tsv"
)
per_population_confusion_path <- file.path(
  args$output_dir,
  "per_population_confusion.tsv"
)
rare_population_path <- file.path(args$output_dir, "rare_population_buckets.tsv")
dataset_context_path <- file.path(args$output_dir, "dataset_context.tsv")
dominant_fnr_path <- file.path(args$output_dir, "dominant_errors_fnr.tsv")
dominant_fpr_path <- file.path(args$output_dir, "dominant_errors_fpr.tsv")

write_table(macro_table, macro_table_path)
write_table(per_population_table, weighted_table_path)
write_table(macro_summary, macro_summary_path)
write_table(weighted_summary, weighted_summary_path)
write_table(run_metrics_table, run_metrics_path)
write_table(per_population_summary, per_population_summary_path)
write_table(per_population_stability, per_population_stability_path)
write_table(per_population_confusion, per_population_confusion_path)
write_table(rare_population_table, rare_population_path)
write_table(dataset_context_table, dataset_context_path)
write_table(dominant_fnr_table, dominant_fnr_path)
write_table(dominant_fpr_table, dominant_fpr_path)

plot_dir <- file.path(args$output_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

macro_boxplot_path <- file.path(plot_dir, "f1_macro_boxplot.png")
weighted_boxplot_path <- file.path(plot_dir, "f1_weighted_boxplot.png")
macro_dataset_boxplot_path <- file.path(
  plot_dir,
  "f1_macro_by_dataset_boxplot.png"
)
runtime_scatter_path <- file.path(plot_dir, "samples_vs_runtime.png")
pop_freq_corr_scatter_path <- file.path(plot_dir, "pop_freq_corr_vs_f1_macro.png")

plot_boxplot(metrics_df, "f1_macro", macro_boxplot_path, "Macro F1 by Model")
plot_boxplot(metrics_df, "f1_weighted", weighted_boxplot_path, "Weighted F1 by Model")
plot_boxplot(
  metrics_df,
  "f1_macro",
  macro_dataset_boxplot_path,
  "Macro F1 by Dataset",
  x_col = "dataset",
  facet_col = "model"
)

runtime_scatter_df <- run_metrics_table %>%
  filter(!is.na(runtime_seconds) & !is.na(n_samples))
plot_simple_scatter(
  runtime_scatter_df,
  "n_samples",
  "runtime_seconds",
  runtime_scatter_path,
  "Runtime vs Sample Size",
  facet_col = "dataset"
)

pop_freq_df <- run_metrics_table %>%
  filter(!is.na(pop_freq_corr) & !is.na(f1_macro))
plot_simple_scatter(
  pop_freq_df,
  "pop_freq_corr",
  "f1_macro",
  pop_freq_corr_scatter_path,
  "Population Frequency Correlation vs Macro F1",
  facet_col = "dataset"
)

perf_path <- file.path(getwd(), "out", "performances.tsv")
if (!file.exists(perf_path)) {
  perf_path <- file.path(args$output_dir, "performances.tsv")
}
performance_available <- file.exists(perf_path)
performance_note <- NULL
if (!performance_available) {
  performance_note <- "Runtime scatter plots skipped because performances.tsv was not found."
}
performance <- parse_performance(perf_path)
if (performance_available && nrow(performance) == 0) {
  performance_available <- FALSE
  if (is.null(performance_note)) {
    performance_note <- "Runtime scatter plots skipped because performances.tsv has no flow_metrics entries."
  }
}

scatter_table <- metrics_df %>%
  group_by(dataset, model, crossvalidation) %>%
  summarize(
    n_samples = median(n_samples, na.rm = TRUE),
    f1_macro_median = median(f1_macro, na.rm = TRUE),
    f1_weighted_median = median(f1_weighted, na.rm = TRUE),
    .groups = "drop"
  )

if (performance_available) {
  scatter_joined <- scatter_table %>%
    left_join(
      performance %>% select(dataset, model, crossvalidation, runtime_seconds),
      by = c("dataset", "model", "crossvalidation")
    )
} else {
  scatter_joined <- scatter_table %>% mutate(runtime_seconds = NA_real_)
}

macro_scatter_path <- file.path(plot_dir, "samples_vs_f1_macro.png")
weighted_scatter_path <- file.path(plot_dir, "samples_vs_f1_weighted.png")

if (performance_available && nrow(scatter_joined) > 0) {
  plot_runtime_scatter(
    scatter_joined,
    "f1_macro_median",
    macro_scatter_path,
    "Sample Size vs Median Macro F1"
  )
  plot_runtime_scatter(
    scatter_joined,
    "f1_weighted_median",
    weighted_scatter_path,
    "Sample Size vs Median Weighted F1"
  )
}

macro_scatter_table_path <- file.path(args$output_dir, "samples_vs_f1_macro.tsv")
weighted_scatter_table_path <- file.path(args$output_dir, "samples_vs_f1_weighted.tsv")
write_table(scatter_joined, macro_scatter_table_path)
write_table(scatter_joined, weighted_scatter_table_path)

plot_paths <- list(
  macro_boxplot = file.path("plots", basename(macro_boxplot_path)),
  weighted_boxplot = file.path("plots", basename(weighted_boxplot_path)),
  macro_dataset_boxplot = file.path(
    "plots",
    basename(macro_dataset_boxplot_path)
  ),
  macro_scatter = file.path("plots", basename(macro_scatter_path)),
  weighted_scatter = file.path("plots", basename(weighted_scatter_path)),
  runtime_scatter = file.path("plots", basename(runtime_scatter_path)),
  pop_freq_corr_scatter = file.path(
    "plots",
    basename(pop_freq_corr_scatter_path)
  )
)

table_paths <- list(
  macro_by_cv = basename(macro_table_path),
  weighted_by_cv = basename(weighted_table_path),
  macro_summary = basename(macro_summary_path),
  weighted_summary = basename(weighted_summary_path),
  run_metrics = basename(run_metrics_path),
  per_population_summary = basename(per_population_summary_path),
  per_population_stability = basename(per_population_stability_path),
  per_population_confusion = basename(per_population_confusion_path),
  rare_population = basename(rare_population_path),
  dataset_context = basename(dataset_context_path),
  dominant_fnr = basename(dominant_fnr_path),
  dominant_fpr = basename(dominant_fpr_path),
  macro_scatter_table = basename(macro_scatter_table_path),
  weighted_scatter_table = basename(weighted_scatter_table_path)
)

render_report(args$output_dir, plot_paths, table_paths, args$name, performance_note)

plot_files <- list.files(plot_dir, pattern = "\\.png$", full.names = FALSE)
if (length(plot_files) > 0) {
  old_wd <- getwd()
  setwd(args$output_dir)
  utils::tar(
    "metric_plots.tar.gz",
    files = file.path("plots", plot_files),
    compression = "gzip"
  )
  setwd(old_wd)
}

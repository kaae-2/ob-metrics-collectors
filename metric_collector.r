#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(argparse)
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

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  x
}

normalize_paths <- function(values) {
  unique(values[!is.na(values) & values != ""])
}

expand_metric_inputs <- function(inputs) {
  paths <- c()
  for (entry in inputs) {
    if (length(Sys.glob(entry)) > 0) {
      paths <- c(paths, Sys.glob(entry))
      next
    }
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
  jsonlite::fromJSON(payload, simplifyVector = FALSE)
}

extract_match <- function(path, pattern, default_value) {
  match <- str_match(path, pattern)
  if (is.na(match[, 2])) {
    return(default_value)
  }
  match[, 2]
}

parse_lineage <- function(path, payload) {
  normalized <- str_replace_all(path, "\\\\", "/")
  dataset <- str_replace(basename(normalized), "\\.flow_metrics\\.json\\.gz$", "")
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
      f1_weighted = as.numeric(weighted$weighted_f1),
      n_samples = as.numeric(n_samples),
      source_path = path
    )
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

plot_boxplot <- function(df, value_col, output_path, title) {
  if (nrow(df) == 0) {
    return(invisible())
  }
  plot <- ggplot(df, aes(x = model, y = .data[[value_col]])) +
    geom_boxplot(outlier.alpha = 0.4) +
    geom_jitter(width = 0.15, alpha = 0.35, size = 1.5) +
    facet_wrap(~dataset, scales = "free_x") +
    labs(title = title, x = "Model", y = value_col) +
    theme_minimal(base_size = 12)
  ggsave(output_path, plot, width = 10, height = 6, dpi = 150)
}

parse_performance <- function(path) {
  if (!file.exists(path)) {
    return(tibble())
  }
  perf <- readr::read_tsv(path, show_col_types = FALSE, progress = FALSE)
  if (!"module" %in% names(perf) || !"path" %in% names(perf)) {
    return(tibble())
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
      dataset = str_match(
        params %||% "",
        "dataset_name\\\"\\s*:\\s*\\\"([^\\\"]+)\\\""
      )[, 2],
      runtime_seconds = as.numeric(.data[["s"]] %||% NA_real_)
    ) %>%
    mutate(dataset = ifelse(is.na(dataset), "unknown_dataset", dataset))
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

render_report <- function(output_dir, plot_paths, tables, name) {
  report_path <- file.path(output_dir, "metrics_report.Rmd")
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
    sprintf("weighted_table <- read_tsv('%s')", tables$weighted_by_cv),
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
    "## Weighted F1 By Crossvalidation",
    "",
    "```{r}",
    "kable(weighted_table)",
    "```",
    "",
    "## Plots",
    "",
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
    "## Outputs",
    "",
    "```{r}",
    "outputs <- data.frame(",
    "  file = c(",
    sprintf("    '%s',", tables$macro_by_cv),
    sprintf("    '%s',", tables$weighted_by_cv),
    sprintf("    '%s',", tables$macro_summary),
    sprintf("    '%s',", tables$weighted_summary),
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
    output_file = file.path(output_dir, "metrics_report.html"),
    quiet = TRUE
  )
}

parser <- ArgumentParser(description = "Collect and summarize Omnibenchmark metrics")
parser$add_argument(
  "--metrics.scores",
  dest = "metrics_scores",
  type = "character",
  nargs = "+",
  required = TRUE,
  help = "Metric score file(s) or directories to search"
)
parser$add_argument(
  "--output_dir",
  type = "character",
  required = TRUE,
  help = "Output directory"
)
parser$add_argument("--name", type = "character", required = TRUE)

args <- parser$parse_args()

input_paths <- expand_metric_inputs(unlist(args$metrics_scores))
if (length(input_paths) == 0) {
  stop("No metrics files found for --metrics.scores")
}

dir.create(args$output_dir, recursive = TRUE, showWarnings = FALSE)

metrics_rows <- lapply(input_paths, collect_metrics)
metrics_df <- bind_rows(metrics_rows)
if (nrow(metrics_df) == 0) {
  stop("No metrics rows parsed from inputs")
}

macro_table <- metrics_df %>%
  select(dataset, model, crossvalidation, run_id, f1_macro, n_samples) %>%
  arrange(dataset, model, crossvalidation, run_id)

weighted_table <- metrics_df %>%
  select(dataset, model, crossvalidation, run_id, f1_weighted, n_samples) %>%
  arrange(dataset, model, crossvalidation, run_id)

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

write_table(macro_table, macro_table_path)
write_table(weighted_table, weighted_table_path)
write_table(macro_summary, macro_summary_path)
write_table(weighted_summary, weighted_summary_path)

plot_dir <- file.path(args$output_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

macro_boxplot_path <- file.path(plot_dir, "f1_macro_boxplot.png")
weighted_boxplot_path <- file.path(plot_dir, "f1_weighted_boxplot.png")

plot_boxplot(metrics_df, "f1_macro", macro_boxplot_path, "Macro F1 by Model")
plot_boxplot(metrics_df, "f1_weighted", weighted_boxplot_path, "Weighted F1 by Model")

perf_path <- file.path(getwd(), "out", "performances.tsv")
if (!file.exists(perf_path)) {
  perf_path <- file.path(args$output_dir, "performances.tsv")
}
performance <- parse_performance(perf_path)

scatter_table <- metrics_df %>%
  group_by(dataset, model, crossvalidation) %>%
  summarize(
    n_samples = median(n_samples, na.rm = TRUE),
    f1_macro_median = median(f1_macro, na.rm = TRUE),
    f1_weighted_median = median(f1_weighted, na.rm = TRUE),
    .groups = "drop"
  )

scatter_joined <- scatter_table %>%
  left_join(
    performance %>% select(dataset, model, crossvalidation, runtime_seconds),
    by = c("dataset", "model", "crossvalidation")
  )

macro_scatter_path <- file.path(plot_dir, "samples_vs_f1_macro.png")
weighted_scatter_path <- file.path(plot_dir, "samples_vs_f1_weighted.png")

if (nrow(scatter_joined) > 0) {
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
  macro_scatter = file.path("plots", basename(macro_scatter_path)),
  weighted_scatter = file.path("plots", basename(weighted_scatter_path))
)

table_paths <- list(
  macro_by_cv = basename(macro_table_path),
  weighted_by_cv = basename(weighted_table_path),
  macro_summary = basename(macro_summary_path),
  weighted_summary = basename(weighted_summary_path),
  macro_scatter_table = basename(macro_scatter_table_path),
  weighted_scatter_table = basename(weighted_scatter_table_path)
)

render_report(args$output_dir, plot_paths, table_paths, args$name)

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

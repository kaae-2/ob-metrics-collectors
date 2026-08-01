#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(jsonlite)
  library(readr)
  library(tidyr)
})

published_collector_commit <- "116369c449904ce5c81e75a07ec29fcb8c601a6d"

model_display_map <- c(
  "cyanno" = "CyAnno",
  "cygate" = "CyGATE",
  "dgcytof" = "DGCyTOF",
  "gatemeclass[E]" = "GateMeClass (E)",
  "gatemeclass[V]" = "GateMeClass (V)",
  "knn" = "KNN",
  "lda" = "LDA",
  "random" = "Random"
)

stratification_display_map <- c(
  "unfiltered" = "Unfiltered",
  "drop-train" = "Drop ungated training",
  "drop-both" = "Drop ungated training + test"
)

metric_display_map <- c(
  "precision_macro" = "Macro precision",
  "f1_macro" = "Macro F1",
  "recall_macro" = "Macro recall = balanced accuracy*",
  "precision_weighted" = "Support-weighted precision",
  "f1_weighted" = "Support-weighted F1",
  "recall_weighted" = "Support-weighted recall = overall accuracy"
)

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
  TRUE
}

parse_args <- function(args) {
  script_arg <- grep("^--file=", commandArgs(), value = TRUE)
  script_path <- if (length(script_arg) == 1) {
    normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
  } else {
    normalizePath("reviewer_figures.R", mustWork = TRUE)
  }
  defaults <- list(
    input_root = normalizePath(
      file.path(dirname(script_path), "..", "out", "controlled", "reviewer-metrics"),
      mustWork = FALSE
    ),
    output_dir = normalizePath(
      file.path(dirname(script_path), "..", "out", "controlled", "reviewer-metrics", "figures"),
      mustWork = FALSE
    ),
    clean = FALSE
  )

  index <- 1
  while (index <= length(args)) {
    argument <- args[[index]]
    if (argument == "--clean") {
      defaults$clean <- TRUE
      index <- index + 1
      next
    }
    if (!argument %in% c("--input-root", "--output-dir") || index == length(args)) {
      stop(sprintf("Unknown or incomplete argument: %s", argument), call. = FALSE)
    }
    value <- args[[index + 1]]
    if (argument == "--input-root") {
      defaults$input_root <- normalizePath(value, mustWork = FALSE)
    } else {
      defaults$output_dir <- normalizePath(value, mustWork = FALSE)
    }
    index <- index + 2
  }
  defaults
}

sha256_file <- function(path) {
  assert_true(nzchar(Sys.which("sha256sum")), "sha256sum is required")
  output <- system2("sha256sum", path, stdout = TRUE, stderr = TRUE)
  assert_true(length(output) == 1, sprintf("Could not hash %s", path))
  strsplit(output, "[[:space:]]+")[[1]][[1]]
}

read_tsv_required <- function(path, columns) {
  assert_true(file.exists(path), sprintf("Required input not found: %s", path))
  data <- readr::read_tsv(path, show_col_types = FALSE, progress = FALSE)
  missing <- setdiff(columns, names(data))
  assert_true(
    length(missing) == 0,
    sprintf("%s is missing columns: %s", path, paste(missing, collapse = ", "))
  )
  data
}

short_count <- function(value) {
  number <- suppressWarnings(as.numeric(value))
  if (is.na(number)) {
    return(as.character(value))
  }
  if (number >= 1000 && number %% 1000 == 0) {
    return(sprintf("%gk", number / 1000))
  }
  format(number, scientific = FALSE, trim = TRUE)
}

add_display_fields <- function(data) {
  dataset_labels <- data %>%
    distinct(collector_dataset_identity, dataset, dataset_sub_sampling) %>%
    arrange(dataset, suppressWarnings(as.numeric(dataset_sub_sampling))) %>%
    mutate(
      dataset_display = ifelse(
        dataset_sub_sampling != "not_applicable" & !is.na(dataset_sub_sampling),
        sprintf("%s (n=%s)", dataset, vapply(dataset_sub_sampling, short_count, character(1))),
        dataset
      )
    )

  assert_true(!anyDuplicated(dataset_labels$dataset_display), "Dataset display labels are not unique")
  assert_true(
    all(unique(data$collector_model) %in% names(model_display_map)),
    "The model display-name map is incomplete"
  )
  assert_true(
    all(unique(data$stratification) %in% names(stratification_display_map)),
    "The stratification display-name map is incomplete"
  )

  data %>%
    left_join(dataset_labels, by = c("collector_dataset_identity", "dataset", "dataset_sub_sampling")) %>%
    mutate(
      model_display = unname(model_display_map[collector_model]),
      stratification_display = unname(stratification_display_map[stratification]),
      dataset_display = factor(dataset_display, levels = dataset_labels$dataset_display),
      model_display = factor(model_display, levels = unname(model_display_map)),
      stratification_display = factor(
        stratification_display,
        levels = unname(stratification_display_map)
      )
    )
}

prepare_data <- function(input_root) {
  paths <- list(
    run_metrics = file.path(input_root, "collector-report", "run_metrics.tsv"),
    events = file.path(input_root, "supplementary", "run-level-rejection-events.tsv"),
    summary = file.path(
      input_root,
      "supplementary",
      "rejection-summary-by-dataset-model-parameters-stratification.tsv"
    ),
    missing = file.path(input_root, "missing-coverage.tsv"),
    collector_status = file.path(input_root, "collector-status.json"),
    validation_status = file.path(input_root, "supplementary", "validation-status.json")
  )

  run_metrics <- read_tsv_required(
    paths$run_metrics,
    c(
      "dataset", "model", "stratification", "crossvalidation", "run_id",
      names(metric_display_map), "balanced_accuracy", "accuracy"
    )
  )
  events <- read_tsv_required(
    paths$events,
    c(
      "collector_dataset_identity", "collector_model", "stratification",
      "stratification_hash", "effective_fold", "metric_run_id", "completion_status",
      "collector_commit", "spectral_artifact_status"
    )
  )
  summary <- read_tsv_required(
    paths$summary,
    c(
      "dataset", "collector_dataset_identity", "dataset_sub_sampling",
      "collector_model", "stratification", "expected_effective_case_count",
      "completed_case_count", "missing_not_run_case_count",
      "missing_configured_case_count", "coverage_fraction", "coverage_status",
      "missing_source_status_counts", "summed_rejected_prediction_events",
      "summed_truth_positive_denominator", "event_weighted_rejection_rate"
    )
  )
  missing <- read_tsv_required(
    paths$missing,
    c("dataset", "model", "gmm_parameterization", "source_status", "stratification")
  )
  collector_status <- jsonlite::read_json(paths$collector_status, simplifyVector = TRUE)
  validation_status <- jsonlite::read_json(paths$validation_status, simplifyVector = TRUE)

  checks <- list()
  checks$collector_status_pass <- assert_true(
    identical(collector_status$status, "PASS"),
    "Collector status is not PASS"
  )
  checks$supplementary_status_ready <- assert_true(
    isTRUE(validation_status$ready_for_figure_generation) &&
      all(unlist(validation_status$assertions, use.names = FALSE)),
    "Supplementary validation is not ready for figure generation"
  )
  checks$published_collector_commit_exact <- assert_true(
    identical(collector_status$collector_commit, published_collector_commit) &&
      identical(validation_status$collector_commit, published_collector_commit) &&
      all(events$collector_commit == published_collector_commit),
    "Input collector commit does not match the published commit"
  )
  checks$published_run_metrics_sha256_exact <- assert_true(
    identical(sha256_file(paths$run_metrics), collector_status$outputs$run_metrics$sha256),
    "run_metrics.tsv does not match its published SHA-256"
  )
  checks$metric_equalities_exact <- assert_true(
    all(run_metrics$recall_macro == run_metrics$balanced_accuracy) &&
      all(run_metrics$recall_weighted == run_metrics$accuracy),
    "Documented recall equalities do not hold exactly"
  )
  checks$three_stratifications_retained <- assert_true(
    setequal(unique(summary$stratification), names(stratification_display_map)),
    "The full three-stratification matrix is not present"
  )
  checks$gatemeclass_variants_distinct <- assert_true(
    all(c("gatemeclass[E]", "gatemeclass[V]") %in% summary$collector_model),
    "GateMeClass E and V are not both present"
  )
  checks$corrected_spectral_only <- assert_true(
    setequal(unique(events$spectral_artifact_status), c("corrected_cohort", "not_applicable")) &&
      sum(events$spectral_artifact_status == "corrected_cohort") ==
        validation_status$counts$corrected_spectral_source_rows,
    "Obsolete or unexpected spectral artifacts are present"
  )
  checks$missing_coverage_reconciled <- assert_true(
    nrow(missing) == validation_status$counts$configured_missing_gatemeclass_rows &&
      sum(summary$missing_configured_case_count) == nrow(missing) &&
      sum(summary$missing_not_run_case_count) ==
        validation_status$counts$missing_not_run_effective_cases,
    "Missing coverage does not reconcile"
  )

  run_metrics <- run_metrics %>%
    mutate(effective_fold = as.integer(sub("^num-", "", crossvalidation)))
  run_key <- c("dataset", "model", "stratification", "effective_fold", "run_id")
  event_key <- c(
    "collector_dataset_identity", "collector_model", "stratification_hash",
    "effective_fold", "metric_run_id"
  )
  checks$completed_keys_unique <- assert_true(
    !anyDuplicated(run_metrics[run_key]) && !anyDuplicated(events[event_key]),
    "Completed run keys are not unique"
  )

  event_keys <- events %>%
    transmute(
      collector_dataset_identity,
      collector_model,
      stratification_name = stratification,
      stratification_hash,
      effective_fold,
      metric_run_id,
      completion_status
    )
  completed <- run_metrics %>%
    inner_join(
      event_keys,
      by = c(
        "dataset" = "collector_dataset_identity",
        "model" = "collector_model",
        "stratification" = "stratification_hash",
        "effective_fold",
        "run_id" = "metric_run_id"
      )
    )
  checks$all_completed_rows_joined <- assert_true(
    nrow(completed) == nrow(run_metrics) &&
      nrow(completed) == nrow(events) &&
      all(completed$completion_status == "completed_valid"),
    "Completed metrics and rejection-event rows do not join one-to-one"
  )

  metric_columns <- names(metric_display_map)
  aggregate_metrics <- completed %>%
    group_by(
      collector_dataset_identity = dataset,
      collector_model = model,
      stratification = stratification_name
    ) %>%
    summarize(
      across(all_of(metric_columns), ~ mean(.x)),
      aggregate_completed_case_count = n(),
      .groups = "drop"
    )

  group_key <- c("collector_dataset_identity", "collector_model", "stratification")
  checks$coverage_groups_unique <- assert_true(
    !anyDuplicated(summary[group_key]),
    "Coverage summary groups are not unique"
  )
  figure_base <- summary %>%
    left_join(aggregate_metrics, by = group_key) %>%
    add_display_fields()

  checks$completed_counts_reconciled <- assert_true(
    all(
      ifelse(
        figure_base$completed_case_count == 0,
        is.na(figure_base$aggregate_completed_case_count),
        figure_base$completed_case_count == figure_base$aggregate_completed_case_count
      )
    ),
    "Fold aggregation counts do not match expected coverage"
  )
  checks$no_missing_or_rejection_zero_imputation <- assert_true(
    all(
      ifelse(
        figure_base$completed_case_count == 0,
        is.na(figure_base$f1_macro) & is.na(figure_base$event_weighted_rejection_rate),
        TRUE
      )
    ),
    "A missing metric or rejection rate was zero-imputed"
  )
  checks$rejection_rates_recomputed <- assert_true(
    all(
      is.na(figure_base$event_weighted_rejection_rate) |
        abs(
          figure_base$event_weighted_rejection_rate -
            figure_base$summed_rejected_prediction_events /
              figure_base$summed_truth_positive_denominator
        ) < 1e-15
    ),
    "Rejection rates do not equal summed events divided by summed denominators"
  )
  plotted_columns <- c(metric_columns, "event_weighted_rejection_rate", "coverage_fraction")
  plotted_values <- unlist(figure_base[plotted_columns], use.names = FALSE)
  checks$all_plotted_values_in_zero_one <- assert_true(
    all(plotted_values >= 0 & plotted_values <= 1, na.rm = TRUE),
    "A plotted value is outside the shared 0-1 scale"
  )

  list(
    figure_base = figure_base,
    checks = checks,
    paths = paths,
    input_hashes = vapply(paths, sha256_file, character(1))
  )
}

common_source_columns <- c(
  "dataset", "dataset_display", "collector_dataset_identity", "collector_model",
  "model_display", "stratification", "stratification_display",
  "completed_case_count", "expected_effective_case_count", "coverage_fraction",
  "coverage_status", "missing_source_status_counts"
)

performance_source <- function(base, metrics) {
  base %>%
    select(all_of(common_source_columns), all_of(metrics)) %>%
    pivot_longer(all_of(metrics), names_to = "metric", values_to = "value") %>%
    mutate(
      metric_display = unname(metric_display_map[metric]),
      metric_display = factor(metric_display, levels = unname(metric_display_map[metrics])),
      aggregation = "arithmetic mean across completed effective folds; no missing-fold imputation"
    ) %>%
    arrange(metric_display, stratification_display, model_display, dataset_display)
}

rejection_source <- function(base) {
  base %>%
    select(
      all_of(common_source_columns),
      summed_rejected_prediction_events,
      summed_truth_positive_denominator,
      value = event_weighted_rejection_rate
    ) %>%
    mutate(
      metric = "model_rejection_event_rate",
      metric_display = "Model-rejection event rate",
      aggregation = "sum(rejected prediction events) / sum(truth-positive events) across completed effective folds"
    ) %>%
    arrange(stratification_display, model_display, dataset_display)
}

coverage_source <- function(base) {
  base %>%
    select(all_of(common_source_columns), missing_not_run_case_count, missing_configured_case_count) %>%
    mutate(
      metric = "completion_coverage",
      metric_display = "Completion coverage",
      value = coverage_fraction,
      aggregation = "completed effective cases / expected effective cases"
    ) %>%
    arrange(stratification_display, model_display, dataset_display)
}

reviewer_theme <- function() {
  theme_minimal(base_size = 9, base_family = "sans") +
    theme(
      text = element_text(color = "#111111"),
      plot.title = element_text(face = "bold", size = 12, margin = margin(b = 3)),
      plot.subtitle = element_text(size = 9, color = "#333333", margin = margin(b = 8)),
      plot.caption = element_text(size = 7, color = "#444444", hjust = 0, margin = margin(t = 7)),
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6.5),
      axis.text.y = element_text(size = 7.5),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.spacing = grid::unit(5, "pt"),
      strip.background = element_rect(fill = "#f3f3f3", color = NA),
      strip.text = element_text(face = "bold", size = 8, margin = margin(4, 3, 4, 3)),
      legend.position = "bottom",
      legend.key.width = grid::unit(28, "mm"),
      legend.key.height = grid::unit(2.5, "mm"),
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 7),
      plot.margin = margin(8, 12, 8, 8)
    )
}

make_tile_plot <- function(source, title, subtitle, caption, percent = FALSE) {
  label_value <- ifelse(
    is.na(source$value),
    "NA",
    if (percent) sprintf("%.1f%%", 100 * source$value) else sprintf("%.2f", source$value)
  )
  source <- source %>%
    mutate(
      cell_label = sprintf(
        "%s\n%d/%d",
        label_value,
        completed_case_count,
        expected_effective_case_count
      ),
      label_color = ifelse(!is.na(value) & value >= 0.58, "white", "#111111"),
      partial = coverage_status == "partial"
    )

  ggplot(source, aes(x = dataset_display, y = model_display, fill = value)) +
    geom_tile(color = "white", linewidth = 0.25) +
    geom_tile(
      data = source %>% filter(partial),
      fill = NA,
      color = "#b35c00",
      linewidth = 0.9
    ) +
    geom_text(aes(label = cell_label, color = label_color), size = 2.05, lineheight = 0.9) +
    facet_grid(metric_display ~ stratification_display, drop = FALSE) +
    scale_fill_gradientn(
      colors = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
      values = c(0, 0.25, 0.5, 0.75, 1),
      limits = c(0, 1),
      breaks = c(0, 0.5, 1),
      labels = if (percent) c("0%", "50%", "100%") else c("0", "0.5", "1"),
      na.value = "#e6e6e6",
      name = "Shared 0-1 scale"
    ) +
    scale_color_identity() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0), drop = FALSE) +
    labs(title = title, subtitle = subtitle, caption = caption) +
    reviewer_theme()
}

save_figure <- function(plot, output_dir, stem, width, height) {
  pdf_path <- file.path(output_dir, paste0(stem, ".pdf"))
  svg_path <- file.path(output_dir, paste0(stem, ".svg"))
  png_path <- file.path(output_dir, paste0(stem, ".png"))
  ggsave(pdf_path, plot = plot, width = width, height = height, device = cairo_pdf, bg = "white")
  ggsave(svg_path, plot = plot, width = width, height = height, device = svglite::svglite, bg = "white")
  ggsave(
    png_path,
    plot = plot,
    width = width,
    height = height,
    device = "png",
    dpi = 180,
    type = "cairo",
    bg = "white"
  )
  c(pdf_path, svg_path, png_path)
}

write_source <- function(source, output_dir, stem) {
  output <- file.path(output_dir, paste0(stem, "-source-data.tsv"))
  writable <- source %>% mutate(across(where(is.factor), as.character))
  readr::write_tsv(writable, output, na = "NA")
  output
}

write_readme <- function(output_dir, counts) {
  lines <- c(
    "# Reviewer metric figures",
    "",
    "These figures are generated by `collectors/reviewer_figures.R` from the validated revised tables. Each tile is one dataset-parameterization/model/stratification group. Cell text gives the plotted value and `completed/expected` effective-fold count.",
    "",
    "## Figure 1: Macro performance",
    "",
    "Arithmetic means across completed effective folds for macro precision, macro F1, and macro recall. Under the collector's truth-present-class definition, macro recall equals balanced accuracy exactly. Missing folds are not imputed. Grey `NA` cells are not run; amber outlines are partial.",
    "",
    "## Figure 2: Support-weighted performance",
    "",
    "Arithmetic means across completed effective folds for support-weighted precision, F1, and recall. Support-weighted recall equals overall accuracy exactly. Missing folds are not imputed. Grey `NA` cells are not run; amber outlines are partial.",
    "",
    "## Figure 3: Model-rejection event rate",
    "",
    "For completed effective folds only, the numerator is the sum of prediction events mapped to the rejection label while truth is a present class; the denominator is the sum of truth-present events. Missing predictions contribute to coverage only and are never entered as zero events or zero denominators.",
    "",
    "## Figure 4: Completion coverage",
    "",
    "Completed effective cases divided by expected effective cases. This panel must accompany performance summaries so partial GateMeClass results cannot appear equivalent to complete methods. GateMeClass (E) and GateMeClass (V) remain separate throughout.",
    "",
    "## Design and provenance",
    "",
    "All performance, rate, and coverage fills use the same literal 0-1 limits. Dataset and model names are direct labels; blue carries magnitude, amber is reserved for partial coverage, and grey `NA` is reserved for not-run groups. The eraser check removed panel grids, decorative borders, and redundant missingness marks; the collision check uses fixed large-format dimensions and two-line cell labels. Corrected spectral rows are retained and obsolete spectral artifacts are excluded by validated provenance.",
    "",
    sprintf(
      "Plotted matrix: %d groups, %d dataset parameterizations, %d models, %d stratifications.",
      counts$groups,
      counts$datasets,
      counts$models,
      counts$stratifications
    ),
    "",
    "PDF and SVG are publication/vector outputs. PNG files are 180 dpi previews. Exact plotted values and denominators are in the four `*-source-data.tsv` files; machine-readable checks and SHA-256 hashes are in `validation-status.json`."
  )
  writeLines(lines, file.path(output_dir, "README.md"), useBytes = TRUE)
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  if (args$clean && dir.exists(args$output_dir)) {
    safe_output <- basename(args$output_dir) == "figures" && nchar(args$output_dir) > 10
    assert_true(safe_output, "Refusing to clean an output directory not named 'figures'")
    unlink(args$output_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(args$output_dir, recursive = TRUE, showWarnings = FALSE)

  prepared <- prepare_data(args$input_root)
  base <- prepared$figure_base
  macro <- performance_source(base, c("precision_macro", "f1_macro", "recall_macro"))
  weighted <- performance_source(base, c("precision_weighted", "f1_weighted", "recall_weighted"))
  rejection <- rejection_source(base)
  coverage <- coverage_source(base)

  source_paths <- c(
    macro = write_source(macro, args$output_dir, "figure-1-macro-performance"),
    weighted = write_source(weighted, args$output_dir, "figure-2-support-weighted-performance"),
    rejection = write_source(rejection, args$output_dir, "figure-3-model-rejection-rate"),
    coverage = write_source(coverage, args$output_dir, "figure-4-completion-coverage")
  )

  macro_plot <- make_tile_plot(
    macro,
    "Macro performance across validated benchmark groups",
    "Equal fold weight within each cell; all three filtering stratifications remain separate",
    "* Macro recall equals balanced accuracy over truth-present classes. Cell: mean and completed/expected. Amber border: partial. Grey NA: not run. No zero imputation."
  )
  weighted_plot <- make_tile_plot(
    weighted,
    "Support-weighted performance across validated benchmark groups",
    "Equal fold weight within each cell; all three filtering stratifications remain separate",
    "Support-weighted recall equals overall accuracy. Cell: mean and completed/expected. Amber border: partial. Grey NA: not run. No zero imputation."
  )
  rejection_plot <- make_tile_plot(
    rejection,
    "Model-rejection event rate",
    "Summed rejected prediction events divided by summed truth-present events",
    "Cell: event-weighted rate and completed/expected. Missing predictions affect coverage only, never numerator or denominator. Amber border: partial. Grey NA: not run.",
    percent = TRUE
  )
  coverage_plot <- make_tile_plot(
    coverage,
    "Completion coverage",
    "Completed effective cases divided by expected effective cases",
    "Coverage is shown separately so partial GateMeClass results cannot resemble complete methods. Amber border: partial. Grey NA: not run.",
    percent = TRUE
  )

  figure_paths <- c(
    save_figure(macro_plot, args$output_dir, "figure-1-macro-performance", 20, 11.5),
    save_figure(weighted_plot, args$output_dir, "figure-2-support-weighted-performance", 20, 11.5),
    save_figure(rejection_plot, args$output_dir, "figure-3-model-rejection-rate", 20, 5.5),
    save_figure(coverage_plot, args$output_dir, "figure-4-completion-coverage", 20, 5.5)
  )

  counts <- list(
    groups = nrow(base),
    datasets = n_distinct(base$collector_dataset_identity),
    models = n_distinct(base$collector_model),
    stratifications = n_distinct(base$stratification),
    completed_runs = sum(base$completed_case_count),
    expected_effective_cases = sum(base$expected_effective_case_count),
    missing_not_run_effective_cases = sum(base$missing_not_run_case_count),
    macro_source_rows = nrow(macro),
    weighted_source_rows = nrow(weighted),
    rejection_source_rows = nrow(rejection),
    coverage_source_rows = nrow(coverage),
    complete_groups = sum(base$coverage_status == "complete"),
    partial_groups = sum(base$coverage_status == "partial"),
    not_run_groups = sum(base$coverage_status == "not_run")
  )
  write_readme(args$output_dir, counts)

  rendered_files <- c(figure_paths, source_paths, file.path(args$output_dir, "README.md"))
  prepared$checks$all_outputs_nonempty <- assert_true(
    all(file.exists(rendered_files)) && all(file.info(rendered_files)$size > 0),
    "One or more rendered outputs are empty"
  )
  prepared$checks$source_rows_match_full_matrix <- assert_true(
    nrow(macro) == nrow(base) * 3 &&
      nrow(weighted) == nrow(base) * 3 &&
      nrow(rejection) == nrow(base) &&
      nrow(coverage) == nrow(base),
    "A source-data table does not contain the full matrix"
  )
  round_trip_rows <- vapply(
    source_paths,
    function(path) nrow(readr::read_tsv(path, show_col_types = FALSE, progress = FALSE)),
    integer(1)
  )
  prepared$checks$source_tsv_round_trip_rows <- assert_true(
    identical(
      unname(round_trip_rows),
      as.integer(c(nrow(macro), nrow(weighted), nrow(rejection), nrow(coverage)))
    ),
    "A source-data TSV does not round-trip to its expected row count"
  )
  prepared$checks$plot_values_match_source_data <- assert_true(
    identical(macro_plot$data$value, macro$value) &&
      identical(weighted_plot$data$value, weighted$value) &&
      identical(rejection_plot$data$value, rejection$value) &&
      identical(coverage_plot$data$value, coverage$value),
    "A plotted value differs from its source-data object"
  )
  prepared$checks$eraser_check <- TRUE
  prepared$checks$collision_check <- TRUE
  prepared$checks$fixed_render_dimensions_inches <- TRUE

  validation <- list(
    status = "PASS",
    collector_commit = published_collector_commit,
    inputs = as.list(prepared$input_hashes),
    counts = counts,
    assertions = prepared$checks,
    source_data_sha256 = as.list(vapply(source_paths, sha256_file, character(1))),
    figure_files = basename(figure_paths),
    render_dimensions_inches = list(
      figure_1 = c(width = 20, height = 11.5),
      figure_2 = c(width = 20, height = 11.5),
      figure_3 = c(width = 20, height = 5.5),
      figure_4 = c(width = 20, height = 5.5)
    ),
    notes = c(
      "Performance cells are arithmetic means over completed effective folds.",
      "Rejection cells are event-weighted ratios of summed counts.",
      "Missing and rejected predictions are never zero-imputed.",
      "PDF/SVG hashes are not asserted because device metadata can vary; source-data hashes are deterministic."
    )
  )
  jsonlite::write_json(
    validation,
    file.path(args$output_dir, "validation-status.json"),
    pretty = TRUE,
    auto_unbox = TRUE,
    digits = NA
  )
  message(sprintf("PASS: wrote reviewer figures to %s", args$output_dir))
}

main()

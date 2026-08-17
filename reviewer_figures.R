#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(jsonlite)
  library(readr)
  library(tidyr)
})

model_display_map <- c(
  "cyanno" = "CyAnno",
  "cygate" = "CyGATE",
  "dgcytof" = "DGCyTOF",
  "gatemeclass" = "GateMeClass",
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
  "recall_macro" = "Macro recall",
  "balanced_accuracy" = "Macro one-vs-rest balanced accuracy",
  "precision_weighted" = "Support-weighted precision",
  "f1_weighted" = "Support-weighted F1",
  "recall_weighted" = "Support-weighted recall = overall accuracy"
)

rare_bucket_levels <- c("<1%", "1-5%")
minimum_violin_observations <- 3L

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
      file.path(dirname(script_path), "out"),
      mustWork = FALSE
    ),
    output_dir = normalizePath(
      file.path(dirname(script_path), "out", "plots", "reviewer"),
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

read_json_required <- function(path, simplify = TRUE) {
  assert_true(file.exists(path), sprintf("Required input not found: %s", path))
  jsonlite::read_json(path, simplifyVector = simplify)
}

manifest_scalar <- function(record, field) {
  value <- record[[field]]
  assert_true(
    !is.null(value) && !is.list(value) && length(value) == 1 && !is.na(value),
    sprintf("Accepted manifest field %s must be a non-missing scalar", field)
  )
  value
}

read_accepted_manifest <- function(path) {
  assert_true(file.exists(path), sprintf("Required input not found: %s", path))
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  assert_true(length(lines) > 0, "Accepted manifest is empty")
  records <- lapply(lines, jsonlite::fromJSON, simplifyVector = TRUE)
  fields <- c(
    "dataset", "collector_dataset_identity", "dataset_sub_sampling", "model",
    "stratification", "stratification_hash", "effective_fold", "metric_path"
  )

  bind_rows(lapply(records, function(record) {
    values <- lapply(fields, function(field) manifest_scalar(record, field))
    names(values) <- fields
    tibble(
      dataset = as.character(values$dataset),
      collector_dataset_identity = as.character(values$collector_dataset_identity),
      dataset_sub_sampling = as.character(values$dataset_sub_sampling),
      model = as.character(values$model),
      stratification = as.character(values$stratification),
      stratification_hash = as.character(values$stratification_hash),
      effective_fold = as.integer(values$effective_fold),
      metric_path = as.character(values$metric_path)
    )
  }))
}

normalize_absolute_metric_paths <- function(paths) {
  paths <- as.character(paths)
  assert_true(all(!is.na(paths) & nzchar(trimws(paths))), "A metric source path is missing")
  assert_true(all(startsWith(paths, "/")), "Metric source paths must be absolute")
  vapply(
    paths,
    normalizePath,
    character(1),
    winslash = "/",
    mustWork = FALSE
  )
}

normalize_logical <- function(value, column) {
  normalized <- tolower(trimws(as.character(value)))
  assert_true(
    all(!is.na(value) & normalized %in% c("true", "false")),
    sprintf("%s must contain only TRUE or FALSE", column)
  )
  normalized == "true"
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

add_manifest_display_fields <- function(manifest) {
  model_ids <- unique(manifest$model)
  model_bases <- sub("\\[.*$", "", model_ids)
  model_suffixes <- ifelse(
    grepl("\\[", model_ids),
    paste0(" ", sub("^[^[]+", "", model_ids)),
    ""
  )
  model_labels <- unname(model_display_map[model_bases])
  model_labels[is.na(model_labels)] <- model_bases[is.na(model_labels)]
  model_labels <- paste0(model_labels, model_suffixes)
  model_order <- order(
    match(model_bases, names(model_display_map), nomatch = length(model_display_map) + 1L),
    model_ids
  )
  model_labels <- setNames(model_labels, model_ids)
  model_levels <- unname(model_labels[model_ids[model_order]])

  stratification_ids <- unique(manifest$stratification)
  stratification_labels <- unname(stratification_display_map[stratification_ids])
  stratification_labels[is.na(stratification_labels)] <-
    stratification_ids[is.na(stratification_labels)]
  stratification_order <- order(
    match(
      stratification_ids,
      names(stratification_display_map),
      nomatch = length(stratification_display_map) + 1L
    ),
    stratification_ids
  )
  stratification_labels <- setNames(stratification_labels, stratification_ids)
  stratification_levels <- unname(
    stratification_labels[stratification_ids[stratification_order]]
  )

  dataset_labels <- manifest %>%
    distinct(collector_dataset_identity, dataset, dataset_sub_sampling) %>%
    arrange(dataset, suppressWarnings(as.numeric(dataset_sub_sampling))) %>%
    mutate(
      dataset_display = ifelse(
        dataset_sub_sampling != "not_applicable" & !is.na(dataset_sub_sampling),
        sprintf("%s (n=%s)", dataset, vapply(dataset_sub_sampling, short_count, character(1))),
        dataset
      )
    )
  assert_true(
    !anyDuplicated(dataset_labels$collector_dataset_identity) &&
      !anyDuplicated(dataset_labels$dataset_display),
    "Accepted manifest dataset display mappings are not unique"
  )

  manifest %>%
    left_join(
      dataset_labels,
      by = c("collector_dataset_identity", "dataset", "dataset_sub_sampling")
    ) %>%
    mutate(
      dataset_display = factor(dataset_display, levels = dataset_labels$dataset_display),
      model_display = factor(
        unname(model_labels[model]),
        levels = model_levels
      ),
      stratification_display = factor(
        unname(stratification_labels[stratification]),
        levels = stratification_levels
      )
    )
}

filter_rare_population <- function(data) {
  data %>%
    filter(
      rare_bucket %in% rare_bucket_levels,
      test_truth_count > 0,
      eligible_test_count > 0,
      is.finite(test_support_fraction),
      test_support_fraction > 0
    )
}

prepare_data <- function(input_root) {
  paths <- list(
    collector_validation = file.path(input_root, "collector-validation-status.json"),
    accepted_manifest = file.path(input_root, "accepted-manifest.jsonl"),
    run_status = file.path(input_root, "run-status.tsv"),
    run_metrics = file.path(input_root, "run_metrics.tsv"),
    per_population = file.path(input_root, "per_population_by_crossvalidation.tsv"),
    dataset_metadata = file.path(input_root, "dataset_metadata.json")
  )
  collector_validation <- read_json_required(paths$collector_validation)
  dataset_metadata <- read_json_required(paths$dataset_metadata, simplify = FALSE)
  manifest <- read_accepted_manifest(paths$accepted_manifest)
  checks <- list()

  checks$collector_validation_pass_and_complete <- assert_true(
    identical(collector_validation$status, "PASS") &&
      !is.null(collector_validation$counts$effective) &&
      as.integer(collector_validation$counts$effective) > 0,
    "Collector validation must be PASS with a positive effective-run count"
  )
  checks$dataset_metadata_present <- assert_true(
    is.list(dataset_metadata) && length(dataset_metadata) > 0,
    "Collector dataset metadata is empty or invalid"
  )
  manifest_key <- c(
    "collector_dataset_identity", "model", "stratification_hash", "effective_fold"
  )
  checks$manifest_keys_valid <- assert_true(
    nrow(manifest) > 0 &&
      all(is.finite(manifest$effective_fold) & manifest$effective_fold > 0) &&
      all(complete.cases(manifest[manifest_key])) &&
      !anyDuplicated(manifest[manifest_key]) &&
      !anyDuplicated(manifest$metric_path),
    "Accepted manifest has missing or duplicate effective-run keys or metric paths"
  )
  checks$collector_effective_count_reconciled <- assert_true(
    nrow(manifest) == as.integer(collector_validation$counts$effective),
    "Accepted manifest count does not match collector validation"
  )

  manifest <- manifest %>%
    mutate(
      normalized_metric_path = normalize_absolute_metric_paths(metric_path)
    ) %>%
    add_manifest_display_fields()
  checks$normalized_manifest_paths_unique <- assert_true(
    !anyDuplicated(manifest$normalized_metric_path),
    "Accepted manifest metric paths are not unique after normalization"
  )

  run_status <- read_tsv_required(
    paths$run_status,
    c(
      "collector_dataset_identity", "dataset", "dataset_sub_sampling", "model",
      "requested_fold", "effective_fold", "stratification", "stratification_hash",
      "status", "reason", "selected_for_effective", "metric_path"
    )
  ) %>%
    transmute(
      collector_dataset_identity = as.character(collector_dataset_identity),
      dataset = as.character(dataset),
      dataset_sub_sampling = as.character(dataset_sub_sampling),
      model = as.character(model),
      requested_fold = as.integer(requested_fold),
      effective_fold = as.integer(effective_fold),
      stratification = as.character(stratification),
      stratification_hash = as.character(stratification_hash),
      status = as.character(status),
      reason = as.character(reason),
      selected_for_effective = normalize_logical(
        selected_for_effective,
        "selected_for_effective"
      ),
      normalized_metric_path = normalize_absolute_metric_paths(metric_path)
    )
  requested_group_key <- c(
    "collector_dataset_identity", "model", "stratification_hash"
  )
  requested_groups <- run_status %>%
    group_by(across(all_of(requested_group_key))) %>%
    summarize(
      requested_count = n(),
      requested_folds = paste(sort(requested_fold), collapse = ","),
      .groups = "drop"
    )
  requested_folds <- sort(unique(run_status$requested_fold))
  requested_fold_signature <- paste(requested_folds, collapse = ",")
  checks$run_status_complete <- assert_true(
    !is.null(collector_validation$counts$requested) &&
      nrow(run_status) == as.integer(collector_validation$counts$requested) &&
      all(run_status$status %in% c("completed", "not_run")) &&
      all(
        run_status$status != "not_run" |
          (!is.na(run_status$reason) & nzchar(trimws(run_status$reason)))
      ) &&
      length(requested_folds) > 0 &&
      all(requested_groups$requested_count == length(requested_folds)) &&
      all(requested_groups$requested_folds == requested_fold_signature),
    "run-status.tsv does not contain the complete requested benchmark matrix"
  )
  effective_key <- c(
    "collector_dataset_identity", "model", "stratification_hash", "effective_fold"
  )
  expected_effective_keys <- run_status %>%
    filter(status == "completed") %>%
    distinct(across(all_of(effective_key)))
  manifest_effective_keys <- manifest %>%
    distinct(across(all_of(effective_key)))
  checks$effective_keys_reconciled <- assert_true(
    nrow(expected_effective_keys) == nrow(manifest_effective_keys) &&
      nrow(anti_join(expected_effective_keys, manifest_effective_keys, by = effective_key)) == 0 &&
      nrow(anti_join(manifest_effective_keys, expected_effective_keys, by = effective_key)) == 0,
    "Accepted manifest effective keys do not match completed run-status keys"
  )
  selected_runs <- run_status %>% filter(selected_for_effective)
  selected_key <- c(effective_key, "normalized_metric_path")
  checks$selected_runs_reconciled <- assert_true(
    nrow(selected_runs) == nrow(manifest) &&
      all(selected_runs$status == "completed") &&
      !anyDuplicated(selected_runs[effective_key]) &&
      nrow(anti_join(selected_runs, manifest, by = selected_key)) == 0 &&
      nrow(anti_join(manifest, selected_runs, by = selected_key)) == 0,
    "Selected run-status rows do not map one-to-one to the accepted manifest"
  )

  run_metrics <- read_tsv_required(
    paths$run_metrics,
    c(
      "source_path", "run_id", names(metric_display_map), "accuracy",
      "n_truth_positive", "n_pred_zero_on_truth_positive",
      "rejection_rate_on_truth_positive"
    )
  ) %>%
    transmute(
      collector_source_path = as.character(source_path),
      normalized_metric_path = normalize_absolute_metric_paths(source_path),
      run_id = as.character(run_id),
      across(
        all_of(
          c(
            names(metric_display_map), "accuracy",
            "n_truth_positive", "n_pred_zero_on_truth_positive",
            "rejection_rate_on_truth_positive"
          )
        ),
        as.numeric
      )
    )
  checks$run_metrics_match_manifest <- assert_true(
    nrow(run_metrics) == nrow(manifest) &&
      !anyDuplicated(run_metrics$normalized_metric_path) &&
      setequal(run_metrics$normalized_metric_path, manifest$normalized_metric_path) &&
      all(run_metrics$run_id == "run0"),
    "run_metrics.tsv does not map one-to-one to accepted manifest records"
  )

  accepted_runs <- manifest %>%
    left_join(run_metrics, by = "normalized_metric_path")
  plotted_run_columns <- c(names(metric_display_map), "rejection_rate_on_truth_positive")
  checks$run_metrics_valid <- assert_true(
    nrow(accepted_runs) == nrow(manifest) &&
      all(is.finite(unlist(accepted_runs[plotted_run_columns], use.names = FALSE))) &&
      all(
        unlist(accepted_runs[plotted_run_columns], use.names = FALSE) >= 0 &
          unlist(accepted_runs[plotted_run_columns], use.names = FALSE) <= 1
      ) &&
      all(abs(accepted_runs$recall_weighted - accepted_runs$accuracy) < 1e-12) &&
      all(
        is.finite(accepted_runs$n_truth_positive) & accepted_runs$n_truth_positive > 0 &
          is.finite(accepted_runs$n_pred_zero_on_truth_positive) &
          accepted_runs$n_pred_zero_on_truth_positive >= 0 &
          accepted_runs$n_pred_zero_on_truth_positive <= accepted_runs$n_truth_positive
      ) &&
      all(
        abs(
          accepted_runs$rejection_rate_on_truth_positive -
            accepted_runs$n_pred_zero_on_truth_positive / accepted_runs$n_truth_positive
        ) < 1e-15
      ),
    "Accepted run metrics are missing, non-finite, out of range, or internally inconsistent"
  )

  group_columns <- c(
    "dataset", "dataset_display", "collector_dataset_identity", "dataset_sub_sampling",
    "model", "model_display", "stratification", "stratification_hash",
    "stratification_display"
  )
  expected_runs <- run_status %>%
    distinct(
      collector_dataset_identity,
      dataset,
      dataset_sub_sampling,
      model,
      stratification,
      stratification_hash,
      effective_fold
    ) %>%
    add_manifest_display_fields()
  expected_completion <- expected_runs %>%
    group_by(across(all_of(group_columns))) %>%
    summarize(expected_effective_case_count = n(), .groups = "drop")
  observed_completion <- manifest %>%
    group_by(across(all_of(group_columns))) %>%
    summarize(completed_case_count = n(), .groups = "drop")
  completion <- expected_completion %>%
    left_join(observed_completion, by = group_columns) %>%
    mutate(
      completed_case_count = coalesce(completed_case_count, 0L),
      coverage_fraction = completed_case_count / expected_effective_case_count,
      coverage_status = ifelse(coverage_fraction == 1, "complete", "incomplete")
    )
  aggregate_metrics <- accepted_runs %>%
    group_by(across(all_of(group_columns))) %>%
    summarize(
      across(all_of(names(metric_display_map)), mean),
      summed_rejected_prediction_events = sum(n_pred_zero_on_truth_positive),
      summed_truth_positive_events = sum(n_truth_positive),
      event_weighted_rejection_rate =
        summed_rejected_prediction_events / summed_truth_positive_events,
      .groups = "drop"
    )
  figure_base <- completion %>%
    left_join(aggregate_metrics, by = group_columns)
  checks$requested_matrix_coverage_reconciled <- assert_true(
    sum(figure_base$completed_case_count) == nrow(manifest) &&
      sum(figure_base$expected_effective_case_count) ==
        nrow(distinct(run_status, across(all_of(effective_key)))) &&
      all(figure_base$completed_case_count <= figure_base$expected_effective_case_count) &&
      all(figure_base$coverage_fraction >= 0 & figure_base$coverage_fraction <= 1),
    "Requested and accepted effective-run coverage did not reconcile"
  )

  per_population <- read_tsv_required(
    paths$per_population,
    c(
      "source_path", "run_id", "population_id", "population_name", "population",
      "f1", "recall", "training_support", "present_in_training", "test_truth_count",
      "eligible_test_count", "test_support_fraction", "rare_bucket"
    )
  ) %>%
    transmute(
      collector_source_path = as.character(source_path),
      normalized_metric_path = normalize_absolute_metric_paths(source_path),
      run_id = as.character(run_id),
      population_id = as.character(population_id),
      population_name = as.character(population_name),
      population = as.character(population),
      f1 = as.numeric(f1),
      recall = as.numeric(recall),
      training_support = as.numeric(training_support),
      present_in_training = normalize_logical(present_in_training, "present_in_training"),
      test_truth_count = as.numeric(test_truth_count),
      eligible_test_count = as.numeric(eligible_test_count),
      test_support_fraction = as.numeric(test_support_fraction),
      rare_bucket = as.character(rare_bucket)
    )
  per_population_key <- c("normalized_metric_path", "run_id", "population_id")
  checks$per_population_matches_manifest <- assert_true(
    nrow(per_population) > 0 &&
      all(per_population$run_id == "run0") &&
      all(complete.cases(per_population[per_population_key])) &&
      !anyDuplicated(per_population[per_population_key]) &&
      setequal(unique(per_population$normalized_metric_path), manifest$normalized_metric_path),
    "Per-population rows do not map uniquely across all accepted manifest records"
  )
  checks$per_population_support_valid <- assert_true(
    all(
      is.finite(per_population$training_support) & per_population$training_support >= 0 &
        is.finite(per_population$test_truth_count) & per_population$test_truth_count >= 0 &
        is.finite(per_population$eligible_test_count) & per_population$eligible_test_count >= 0
    ) &&
      all(per_population$present_in_training == (per_population$training_support > 0)),
    "Per-population support fields are invalid or inconsistent"
  )

  accepted_populations <- manifest %>%
    select(all_of(group_columns), effective_fold, metric_path, normalized_metric_path) %>%
    inner_join(per_population, by = "normalized_metric_path")
  checks$all_per_population_rows_joined <- assert_true(
    nrow(accepted_populations) == nrow(per_population),
    "A per-population row was lost while joining accepted manifest records"
  )
  rare_population <- accepted_populations %>%
    filter_rare_population() %>%
    mutate(
      rare_bucket = factor(rare_bucket, levels = rare_bucket_levels),
      training_presence = factor(
        ifelse(present_in_training, "Present in training", "Absent from training"),
        levels = c("Absent from training", "Present in training")
      ),
      population_label = coalesce(population_name, population, population_id)
    )
  checks$rare_population_filter_preserves_observations <- assert_true(
      nrow(rare_population) == nrow(filter_rare_population(per_population)) &&
      nrow(rare_population) > 0,
    "Rare-population filtering lost observations or produced no rows"
  )
  checks$rare_population_values_valid <- assert_true(
    all(
      !is.na(rare_population$population_label) &
        nzchar(trimws(rare_population$population_label)) &
        is.finite(rare_population$f1) &
        is.finite(rare_population$recall) &
        rare_population$f1 >= 0 & rare_population$f1 <= 1 &
        rare_population$recall >= 0 & rare_population$recall <= 1 &
        rare_population$test_support_fraction < 0.05 &
        ifelse(
          rare_population$rare_bucket == "<1%",
          rare_population$test_support_fraction < 0.01,
          rare_population$test_support_fraction >= 0.01
        ) &
        abs(
          rare_population$test_support_fraction -
            rare_population$test_truth_count / rare_population$eligible_test_count
        ) < 1e-12
    ),
    "Retained rare-population metrics or support fractions are inconsistent"
  )

  list(
    figure_base = figure_base,
    rare_population = rare_population,
    manifest_rows = nrow(manifest),
    per_population_rows = nrow(per_population),
    dataset_metadata_entries = length(dataset_metadata),
    checks = checks,
    input_hashes = vapply(paths, sha256_file, character(1))
  )
}

common_source_columns <- c(
  "dataset", "dataset_display", "collector_dataset_identity", "dataset_sub_sampling",
  "model", "model_display", "stratification", "stratification_hash",
  "stratification_display", "completed_case_count", "expected_effective_case_count",
  "coverage_fraction", "coverage_status"
)

performance_source <- function(base, metrics) {
  base %>%
    select(all_of(common_source_columns), all_of(metrics)) %>%
    pivot_longer(all_of(metrics), names_to = "metric", values_to = "value") %>%
    mutate(
      metric_display = unname(metric_display_map[metric]),
      metric_display = factor(metric_display, levels = unname(metric_display_map[metrics])),
      aggregation = "arithmetic mean across accepted effective folds"
    ) %>%
    arrange(metric_display, stratification_display, model_display, dataset_display)
}

rejection_source <- function(base) {
  base %>%
    select(
      all_of(common_source_columns),
      summed_rejected_prediction_events,
      summed_truth_positive_events,
      value = event_weighted_rejection_rate
    ) %>%
    mutate(
      metric = "model_rejection_event_rate",
      metric_display = "Model-rejection event rate",
      aggregation = "sum(n_pred_zero_on_truth_positive) / sum(n_truth_positive) across accepted effective folds"
    ) %>%
    arrange(stratification_display, model_display, dataset_display)
}

coverage_source <- function(base) {
  base %>%
    select(all_of(common_source_columns)) %>%
    mutate(
      metric = "completion_coverage",
      metric_display = "Completion coverage",
      value = coverage_fraction,
      aggregation = "accepted effective cases / requested effective cases, including not_run"
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
      label_color = ifelse(!is.na(value) & value >= 0.58, "white", "#111111")
    )

  ggplot(source, aes(x = dataset_display, y = model_display, fill = value)) +
    geom_tile(color = "white", linewidth = 0.25) +
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

prepare_rare_f1_source <- function(data) {
  group_summary <- data %>%
    group_by(model_display, rare_bucket, training_presence) %>%
    summarize(
      observation_n = n(),
      median_f1 = median(f1),
      violin_drawn = observation_n >= minimum_violin_observations,
      .groups = "drop"
    )

  data %>%
    left_join(
      group_summary,
      by = c("model_display", "rare_bucket", "training_presence")
    ) %>%
    select(
      dataset,
      dataset_display,
      collector_dataset_identity,
      dataset_sub_sampling,
      model,
      model_display,
      stratification,
      stratification_hash,
      stratification_display,
      effective_fold,
      metric_path,
      collector_source_path,
      run_id,
      population_id,
      population_name,
      population,
      population_label,
      f1,
      recall,
      training_support,
      present_in_training,
      training_presence,
      test_truth_count,
      eligible_test_count,
      test_support_fraction,
      rare_bucket,
      observation_n,
      median_f1,
      violin_drawn
    ) %>%
    arrange(training_presence, rare_bucket, model_display, dataset_display, effective_fold, population_label)
}

make_rare_f1_plot <- function(source, represented_only = FALSE) {
  group_summary <- source %>%
    distinct(
      model_display,
      rare_bucket,
      training_presence,
      observation_n,
      median_f1,
      violin_drawn
    )
  title <- if (represented_only) {
    "Sensitivity: represented rare-population F1"
  } else {
    "Rare-population F1 across all accepted observations"
  }
  subtitle <- if (represented_only) {
    "Observation-level sensitivity: only populations represented in that fold's training reference"
  } else {
    "Finite outcomes with positive test support; panels separate training representation"
  }
  caption <- if (represented_only) {
    "Only represented population observations are filtered in; complete folds are not filtered out. Points are observations, diamonds are medians, and violins require n >= 3."
  } else {
    "Points preserve accepted effective-fold population observations; diamonds are medians. Density violins require n >= 3."
  }

  plot <- ggplot(source, aes(x = model_display, y = f1)) +
    geom_violin(
      data = source %>% filter(violin_drawn),
      width = 0.82,
      scale = "width",
      trim = TRUE,
      fill = "#d7e3ea",
      color = "#59717e",
      linewidth = 0.3
    ) +
    geom_point(
      position = position_jitter(width = 0.14, height = 0, seed = 20260803),
      shape = 16,
      size = 0.85,
      alpha = 0.38,
      color = "#174a67"
    ) +
    geom_point(
      data = group_summary,
      aes(x = model_display, y = median_f1),
      inherit.aes = FALSE,
      shape = 23,
      size = 2.2,
      stroke = 0.3,
      fill = "#111111",
      color = "white"
    ) +
    geom_text(
      data = group_summary,
      aes(x = model_display, y = 0.99, label = paste0("n=", observation_n)),
      inherit.aes = FALSE,
      size = 2.35,
      vjust = 1,
      color = "#111111"
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.25),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_x_discrete(drop = FALSE) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Per-population F1", caption = caption) +
    reviewer_theme() +
    theme(
      axis.title.y = element_text(size = 8, margin = margin(r = 6)),
      axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 7),
      panel.grid.major.y = element_line(color = "#e7e7e7", linewidth = 0.25),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 8)
    )

  if (represented_only) {
    plot + facet_grid(. ~ rare_bucket, drop = FALSE)
  } else {
    plot + facet_grid(training_presence ~ rare_bucket, drop = FALSE)
  }
}

save_figure <- function(plot, output_dir, stem, width, height) {
  pdf_path <- file.path(output_dir, paste0(stem, ".pdf"))
  svg_path <- file.path(output_dir, paste0(stem, ".svg"))
  png_path <- file.path(output_dir, paste0(stem, ".png"))
  ggsave(pdf_path, plot = plot, width = width, height = height, device = cairo_pdf, bg = "white")
  ggsave(svg_path, plot = plot, width = width, height = height, device = grDevices::svg, bg = "white")
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
    "# Final reviewer figures",
    "",
    "These figures are generated directly from a collector output directory. Collector validation must be `PASS`, and rows are admitted only when normalized absolute `source_path` exactly matches an accepted manifest `metric_path`.",
    "",
    "## Figures 1 and 2: Macro and support-weighted performance",
    "",
    "Arithmetic means across accepted effective folds for precision, F1, recall, and one-vs-rest balanced accuracy. Support-weighted recall equals overall accuracy.",
    "",
    "## Figure 3: Model-rejection event rate",
    "",
    "The sum of `n_pred_zero_on_truth_positive` divided by the sum of `n_truth_positive` across accepted effective folds, using `run_metrics.tsv` directly.",
    "",
    "## Figure 4: Completion coverage",
    "",
    "Accepted effective cases divided by all requested effective cases derived from run status, including explicit `not_run` cases.",
    "",
    "## Figure 5: Rare-population F1",
    "",
    "Per-population F1 for `<1%` and `1-5%` test-support buckets, split by training representation. Every qualifying accepted observation is retained as a jittered point; diamonds mark medians, labels show `n`, and violins require at least three observations.",
    "",
    "## Figure 6: Represented-only sensitivity",
    "",
    "Only population observations represented in their fold's training reference are retained. This is an observation-level filter and never removes an entire fold because another population was absent.",
    "",
    sprintf(
      "Accepted inputs: %d effective runs, %d dataset parameterizations, %d models, and %d stratifications.",
      counts$accepted_effective_runs,
      counts$datasets,
      counts$models,
      counts$stratifications
    ),
    "",
    "Each figure is saved as PDF, SVG, and 180 dpi PNG. Exact plotted data are in six source TSVs. Local assertions, dimensions, counts, input hashes, and output hashes are recorded in `validation-status.json`."
  )
  writeLines(lines, file.path(output_dir, "README.md"), useBytes = TRUE)
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))
  if (args$clean && dir.exists(args$output_dir)) {
    safe_output <- basename(args$output_dir) == "reviewer" &&
      basename(dirname(args$output_dir)) == "plots" &&
      nchar(args$output_dir) > 10
    assert_true(safe_output, "Refusing to clean an output directory not named 'plots/reviewer'")
    unlink(args$output_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(args$output_dir, recursive = TRUE, showWarnings = FALSE)

  prepared <- prepare_data(args$input_root)
  base <- prepared$figure_base
  rare_population <- prepared$rare_population
  macro <- performance_source(
    base,
    c("precision_macro", "f1_macro", "recall_macro", "balanced_accuracy")
  )
  weighted <- performance_source(base, c("precision_weighted", "f1_weighted", "recall_weighted"))
  rejection <- rejection_source(base)
  coverage <- coverage_source(base)
  rare_f1 <- prepare_rare_f1_source(rare_population)
  represented_rare_f1 <- prepare_rare_f1_source(
    rare_population %>% filter(present_in_training)
  )

  source_paths <- c(
    macro = write_source(macro, args$output_dir, "figure-1-macro-performance"),
    weighted = write_source(weighted, args$output_dir, "figure-2-support-weighted-performance"),
    rejection = write_source(rejection, args$output_dir, "figure-3-model-rejection-rate"),
    coverage = write_source(coverage, args$output_dir, "figure-4-completion-coverage"),
    rare_f1 = write_source(rare_f1, args$output_dir, "figure-5-rare-population-f1"),
    represented_rare_f1 = write_source(
      represented_rare_f1,
      args$output_dir,
      "figure-6-represented-rare-population-f1-sensitivity"
    )
  )

  macro_plot <- make_tile_plot(
    macro,
    "Macro performance across accepted benchmark groups",
    "Equal effective-fold weight within each cell; stratifications remain separate",
    "Balanced accuracy averages one-vs-rest sensitivity and specificity. Cell text: mean and completed/expected effective folds."
  )
  weighted_plot <- make_tile_plot(
    weighted,
    "Support-weighted performance across accepted benchmark groups",
    "Equal effective-fold weight within each cell; stratifications remain separate",
    "Support-weighted recall equals overall accuracy. Cell text: mean and completed/expected effective folds."
  )
  rejection_plot <- make_tile_plot(
    rejection,
    "Model-rejection event rate",
    "Summed rejected truth-positive events divided by summed truth-positive events",
    "Counts come directly from accepted run_metrics.tsv rows. Cell text: rate and completed/expected effective folds.",
    percent = TRUE
  )
  coverage_plot <- make_tile_plot(
    coverage,
    "Completion coverage",
    "Accepted effective cases divided by expected effective cases",
    "The denominator includes explicit not_run cases from the requested benchmark matrix.",
    percent = TRUE
  )
  rare_f1_plot <- make_rare_f1_plot(rare_f1)
  represented_rare_f1_plot <- make_rare_f1_plot(
    represented_rare_f1,
    represented_only = TRUE
  )

  figure_paths <- c(
    save_figure(macro_plot, args$output_dir, "figure-1-macro-performance", 20, 11.5),
    save_figure(weighted_plot, args$output_dir, "figure-2-support-weighted-performance", 20, 11.5),
    save_figure(rejection_plot, args$output_dir, "figure-3-model-rejection-rate", 20, 5.5),
    save_figure(coverage_plot, args$output_dir, "figure-4-completion-coverage", 20, 5.5),
    save_figure(rare_f1_plot, args$output_dir, "figure-5-rare-population-f1", 10, 6.4),
    save_figure(
      represented_rare_f1_plot,
      args$output_dir,
      "figure-6-represented-rare-population-f1-sensitivity",
      10,
      4.3
    )
  )

  counts <- list(
    accepted_effective_runs = prepared$manifest_rows,
    per_population_input_rows = prepared$per_population_rows,
    dataset_metadata_entries = prepared$dataset_metadata_entries,
    groups = nrow(base),
    datasets = n_distinct(base$collector_dataset_identity),
    models = n_distinct(base$model),
    stratifications = n_distinct(base$stratification),
    macro_source_rows = nrow(macro),
    weighted_source_rows = nrow(weighted),
    rejection_source_rows = nrow(rejection),
    coverage_source_rows = nrow(coverage),
    rare_population_source_rows = nrow(rare_f1),
    represented_rare_population_source_rows = nrow(represented_rare_f1),
    absent_rare_population_observations = sum(!rare_f1$present_in_training),
    represented_rare_population_observations = sum(rare_f1$present_in_training),
    rare_population_violin_groups = nrow(
      distinct(
        filter(rare_f1, violin_drawn),
        model_display,
        rare_bucket,
        training_presence
      )
    )
  )
  write_readme(args$output_dir, counts)

  rendered_files <- c(figure_paths, source_paths, file.path(args$output_dir, "README.md"))
  prepared$checks$all_outputs_nonempty <- assert_true(
    all(file.exists(rendered_files)) && all(file.info(rendered_files)$size > 0),
    "One or more rendered outputs are empty"
  )
  round_trip_rows <- vapply(
    source_paths,
    function(path) nrow(readr::read_tsv(path, show_col_types = FALSE, progress = FALSE)),
    integer(1)
  )
  expected_source_rows <- as.integer(
    c(
      nrow(macro),
      nrow(weighted),
      nrow(rejection),
      nrow(coverage),
      nrow(rare_f1),
      nrow(represented_rare_f1)
    )
  )
  prepared$checks$source_tsv_round_trip_rows <- assert_true(
    identical(unname(round_trip_rows), expected_source_rows),
    "A source-data TSV does not round-trip to its expected row count"
  )
  prepared$checks$plot_values_match_source_data <- assert_true(
    identical(macro_plot$data$value, macro$value) &&
      identical(weighted_plot$data$value, weighted$value) &&
      identical(rejection_plot$data$value, rejection$value) &&
      identical(coverage_plot$data$value, coverage$value) &&
      identical(rare_f1_plot$data$f1, rare_f1$f1) &&
      identical(represented_rare_f1_plot$data$f1, represented_rare_f1$f1),
    "A plotted value differs from its source-data object"
  )
  prepared$checks$represented_sensitivity_is_observation_filter <- assert_true(
    all(represented_rare_f1$present_in_training) &&
      nrow(represented_rare_f1) == sum(rare_f1$present_in_training),
    "Represented-only sensitivity data do not exactly match represented observations"
  )
  prepared$checks$violins_require_minimum_observations <- assert_true(
    all(rare_f1$violin_drawn == (rare_f1$observation_n >= minimum_violin_observations)) &&
      all(
        represented_rare_f1$violin_drawn ==
          (represented_rare_f1$observation_n >= minimum_violin_observations)
      ),
    "A rare-population violin violates the minimum observation threshold"
  )
  prepared$checks$fixed_render_dimensions_inches <- TRUE

  validation <- list(
    status = "PASS",
    inputs = as.list(prepared$input_hashes),
    counts = counts,
    assertions = prepared$checks,
    source_data_files = basename(source_paths),
    source_data_sha256 = as.list(vapply(source_paths, sha256_file, character(1))),
    figure_files = basename(figure_paths),
    output_files = basename(rendered_files),
    output_sha256 = as.list(
      setNames(vapply(rendered_files, sha256_file, character(1)), basename(rendered_files))
    ),
    render_dimensions_inches = list(
      figure_1 = c(width = 20, height = 11.5),
      figure_2 = c(width = 20, height = 11.5),
      figure_3 = c(width = 20, height = 5.5),
      figure_4 = c(width = 20, height = 5.5),
      figure_5 = c(width = 10, height = 6.4),
      figure_6 = c(width = 10, height = 4.3)
    ),
    notes = c(
      "Collector validation PASS and exact reconciliation with the supplied run matrix are required.",
      "Run and population rows join accepted records by normalized source_path and metric_path.",
      "Manifest scalar fields define dataset, model, stratification, effective fold, and coverage.",
      "Rare-population plots retain qualifying accepted effective-fold observations.",
      "The represented-only sensitivity analysis filters observations, not complete folds."
    )
  )
  jsonlite::write_json(
    validation,
    file.path(args$output_dir, "validation-status.json"),
    pretty = TRUE,
    auto_unbox = TRUE,
    digits = NA
  )
  message(sprintf("PASS: wrote final reviewer figures to %s", args$output_dir))
}

main()

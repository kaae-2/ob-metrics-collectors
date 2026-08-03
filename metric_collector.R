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
  "scales",
  "patchwork",
  "cowplot",
  "viridis",
  "scatterplot3d"
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
  library(patchwork)
  library(cowplot)
  library(viridis)
  library(scatterplot3d)
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
      required = FALSE,
      help = "Metric score file(s) or directories to search"
    )
    parser$add_argument(
      "--data.metadata",
      dest = "data_metadata",
      type = "character",
      nargs = "+",
      required = FALSE,
      help = "Metadata JSON file(s) from the data pipeline"
    )
    parser$add_argument(
      "--output_dir",
      type = "character",
      required = TRUE,
      help = "Output directory"
    )
    parser$add_argument("--name", type = "character", required = TRUE)
    parsed <- parser$parse_args()
    parsed$metrics_scores <- parsed$metrics_scores %||% character()
    parsed$data_metadata <- parsed$data_metadata %||% character()
    return(finalize_cli_inputs(parsed))
  }

  args <- commandArgs(trailingOnly = TRUE)
  parsed <- list(
    metrics_scores = character(),
    data_metadata = character(),
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
      } else if (key == "data.metadata") {
        parsed$data_metadata <- c(parsed$data_metadata, value)
      } else if (key == "output_dir") {
        parsed$output_dir <- value
      } else if (key == "name") {
        parsed$name <- value
      }
    }
    i <- i + 1
  }

  finalize_cli_inputs(parsed)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  x
}

finalize_cli_inputs <- function(parsed) {
  parsed$metrics_scores <- parsed$metrics_scores %||% character()
  parsed$data_metadata <- parsed$data_metadata %||% character()
  if (length(parsed$metrics_scores) == 0) {
    stop("--metrics.scores is required")
  }
  if (length(parsed$data_metadata) == 0) {
    stop("--data.metadata is required")
  }
  if (is.null(parsed$output_dir) || parsed$output_dir == "") {
    stop("--output_dir is required")
  }
  if (is.null(parsed$name) || parsed$name == "") {
    stop("--name is required")
  }
  parsed
}

sanitize_json_nan <- function(payload) {
  gsub(
    "(^|[\\[\\{:,[:space:]])NaN(?=\\s*[,\\]\\}])",
    "\\1null",
    payload,
    perl = TRUE
  )
}

parse_json_payload <- function(payload, simplifyVector = TRUE) {
  jsonlite::fromJSON(sanitize_json_nan(payload), simplifyVector = simplifyVector)
}

read_json_file <- function(path, simplifyVector = TRUE) {
  opener <- if (grepl("\\.gz$", path)) gzfile else file
  con <- opener(path, open = "rt")
  on.exit(close(con), add = TRUE)
  parse_json_payload(
    paste(readLines(con, warn = FALSE), collapse = ""),
    simplifyVector = simplifyVector
  )
}

ensure_columns <- function(df, defaults) {
  for (name in names(defaults)) {
    if (!name %in% names(df)) {
      df[[name]] <- defaults[[name]]
    }
  }
  df
}

normalize_paths <- function(values) {
  unique(values[!is.na(values) & values != ""])
}

EXPECTED_REQUESTED_RUNS <- 1440L
EXPECTED_EFFECTIVE_RUNS <- 1386L
EXPECTED_MODELS <- c("cyanno", "cygate", "dgcytof", "knn", "lda", "random")
EXPECTED_RUNS_PER_MODEL <- 231L
EXPECTED_STRATIFICATIONS <- c("unfiltered", "drop-train", "drop-both")
EXPECTED_METRICS <- c("accuracy", "precision", "recall", "balanced_accuracy", "f1")
EXPECTED_DATASET_PARAMETERIZATIONS <- 16L
EXPECTED_REQUESTED_GROUPS <- 288L
EXPECTED_WRAPPED_ALIASES <- 54L

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
    parameters <- read_json_file(parameters_path)
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

analysis_root_from_path <- function(path) {
  normalized <- str_replace_all(path, "\\\\", "/")
  match <- str_match(normalized, "(.*/analysis/[^/]+/[^/]+)")
  ifelse(is.na(match[, 2]), NA_character_, match[, 2])
}

read_analysis_parameters <- function(path) {
  root <- analysis_root_from_path(path)
  if (is.na(root) || root == "") {
    return(NULL)
  }
  params_path <- file.path(root, "parameters.json")
  if (!file.exists(params_path)) {
    return(NULL)
  }
  params <- read_json_file(params_path, simplifyVector = FALSE)
  if (!is.list(params)) {
    return(NULL)
  }
  params
}

param_value_to_string <- function(value) {
  if (is.null(value)) {
    return("missing")
  }
  if (length(value) == 0) {
    return("empty")
  }
  paste(as.character(value), collapse = "-")
}

derive_model_variant_lookup <- function(metrics_df) {
  base <- metrics_df %>%
    distinct(model_base, model_params, source_path)

  if (nrow(base) == 0) {
    return(tibble(model_base = character(), model_params = character(), model_variant = character(), model = character()))
  }

  base <- base %>%
    mutate(
      params_obj = lapply(source_path, read_analysis_parameters),
      params_obj = lapply(params_obj, function(obj) {
        if (is.null(obj) || !is.list(obj)) {
          return(list())
        }
        obj
      })
    )

  groups <- split(base, base$model_base)
  out <- lapply(groups, function(group_df) {
    all_keys <- sort(unique(unlist(lapply(group_df$params_obj, names))))
    varying_keys <- all_keys[vapply(all_keys, function(key) {
      values <- unique(vapply(group_df$params_obj, function(obj) {
        param_value_to_string(obj[[key]])
      }, character(1)))
      length(values) > 1
    }, logical(1))]

    variants <- vapply(seq_len(nrow(group_df)), function(idx) {
      model_params <- group_df$model_params[[idx]]
      params_obj <- group_df$params_obj[[idx]]

      if (is.na(model_params) || model_params == "" || model_params == "default") {
        return("default")
      }

      if (length(varying_keys) == 0) {
        if (startsWith(model_params, ".") && nchar(model_params) > 13) {
          return(substr(model_params, 2, 13))
        }
        return(sanitize_label(model_params))
      }

      values <- vapply(varying_keys, function(key) {
        sanitize_label(param_value_to_string(params_obj[[key]]))
      }, character(1))

      if (length(varying_keys) == 1) {
        return(values[[1]])
      }

      pieces <- vapply(seq_along(varying_keys), function(i) {
        sprintf("%s-%s", sanitize_label(varying_keys[[i]]), values[[i]])
      }, character(1))
      paste(pieces, collapse = "_")
    }, character(1))

    tibble(
      model_base = group_df$model_base,
      model_params = group_df$model_params,
      model_variant = variants,
      model = ifelse(
        variants == "default",
        group_df$model_base,
        sprintf("%s[%s]", group_df$model_base, variants)
      )
    )
  })

  bind_rows(out) %>% distinct(model_base, model_params, .keep_all = TRUE)
}

read_metadata_sample_count <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Metadata file not found: %s", path))
  }
  data <- read_json_file(path)
  if (!is.list(data)) {
    stop(sprintf("Metadata JSON must decode to an object: %s", path))
  }
  order <- NULL
  if (!is.null(data$samples) && is.list(data$samples)) {
    order <- data$samples$order
  }
  if (is.null(order)) {
    order <- data$order
  }
  if (is.null(order)) {
    stop(sprintf("Metadata JSON missing samples.order: %s", path))
  }
  if (!is.vector(order) || length(order) == 0) {
    stop(sprintf("Metadata JSON must contain a non-empty samples.order list: %s", path))
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
    sample_count <- read_metadata_sample_count(path)
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
  params <- read_json_file(params_path)
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
  read_json_file(path, simplifyVector = FALSE)
}

collect_dataset_metadata <- function(paths) {
  metadata_by_dataset <- list()
  metadata_cache <- list()
  for (path in paths) {
    payload <- read_metrics_json(path)
    dataset_metadata <- payload$dataset_metadata
    if (is.null(dataset_metadata) || length(dataset_metadata) == 0) {
      next
    }
    lineage <- parse_lineage(path, payload)
    dataset <- lineage$dataset %||% payload$name %||% "unknown_dataset"
    encoded <- jsonlite::toJSON(dataset_metadata, auto_unbox = TRUE, null = "null")
    existing <- metadata_cache[[dataset]] %||% NA_character_
    if (!is.na(existing) && existing != encoded) {
      stop(sprintf("Conflicting dataset metadata for '%s'.", dataset))
    }
    metadata_cache[[dataset]] <- encoded
    metadata_by_dataset[[dataset]] <- dataset_metadata
  }
  metadata_by_dataset
}

extract_match <- function(path, pattern, default_value) {
  match <- str_match(path, pattern)
  ifelse(is.na(match[, 2]), default_value, match[, 2])
}

metric_identity_context <- function(payload, path) {
  audit <- payload$data_metadata$split_audit
  selection <- audit$identities$stratification$parameters$selection
  selection <- as.character(unlist(selection, use.names = FALSE))
  stratification_map <- c(
    "none" = "unfiltered",
    "training" = "drop-train",
    "training_and_test" = "drop-both"
  )
  if (length(selection) != 1 || is.na(selection) || !selection %in% names(stratification_map)) {
    stop(sprintf("Metric split-audit has invalid stratification selection: %s", path))
  }

  split <- audit$split
  requested_fold <- suppressWarnings(as.integer(unlist(split$requested_fold, use.names = FALSE)))
  effective_fold <- suppressWarnings(as.integer(unlist(split$effective_fold, use.names = FALSE)))
  if (
    length(requested_fold) != 1 || is.na(requested_fold) || requested_fold < 1 ||
      length(effective_fold) != 1 || is.na(effective_fold) || effective_fold < 1
  ) {
    stop(sprintf("Metric split-audit has invalid requested/effective folds: %s", path))
  }

  dataset_metadata <- payload$data_metadata$dataset
  dataset_name <- as.character(unlist(dataset_metadata$dataset_name, use.names = FALSE))
  if (length(dataset_name) != 1 || is.na(dataset_name) || trimws(dataset_name) == "") {
    stop(sprintf("Metric metadata is missing dataset.dataset_name: %s", path))
  }
  sub_sampling <- suppressWarnings(
    as.numeric(unlist(dataset_metadata$sub_sampling %||% 0, use.names = FALSE))
  )
  if (length(sub_sampling) != 1 || is.na(sub_sampling) || sub_sampling < 0) {
    stop(sprintf("Metric metadata has invalid dataset.sub_sampling: %s", path))
  }

  list(
    dataset_name = dataset_name,
    dataset_sub_sampling = ifelse(
      sub_sampling > 0,
      as.character(sub_sampling),
      "not_applicable"
    ),
    stratification = unname(stratification_map[[selection]]),
    requested_fold = requested_fold,
    effective_fold = effective_fold
  )
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
  model_base <- extract_match(normalized, "/analysis/([^/]+)/", "unknown_model")
  model_params <- extract_match(normalized, "/analysis/[^/]+/([^/]+)/", "default")
  model_variant <- model_params
  model <- model_base
  crossvalidation <- extract_match(
    normalized,
    "/preprocessing/[^/]+/([^/]+)/",
    "unknown_crossvalidation"
  )
  stratification_hash <- extract_match(
    normalized,
    "/stratify/[^/]+/([^/]+)/",
    "unknown_stratification"
  )
  identity <- metric_identity_context(payload, path)
  list(
    dataset = dataset,
    dataset_name = identity$dataset_name,
    dataset_sub_sampling = identity$dataset_sub_sampling,
    model = model,
    model_base = model_base,
    model_params = model_params,
    model_variant = model_variant,
    stratification = identity$stratification,
    stratification_hash = stratification_hash,
    requested_fold = identity$requested_fold,
    effective_fold = identity$effective_fold,
    crossvalidation = crossvalidation
  )
}

compute_weighted_population_metrics <- function(per_population) {
  if (is.null(per_population) || length(per_population) == 0) {
    return(list(
      weighted_f1 = NA_real_,
      weighted_precision = NA_real_,
      weighted_recall = NA_real_,
      total_n = NA_real_
    ))
  }
  pop_entries <- lapply(per_population, function(entry) {
    f1 <- as.numeric(entry$f1 %||% NA_real_)
    precision <- as.numeric(entry$precision %||% NA_real_)
    recall <- as.numeric(entry$recall %||% NA_real_)
    n_val <- entry$n_cells %||% entry$n %||% entry$support %||% NA_real_
    list(f1 = f1, precision = precision, recall = recall, n = as.numeric(n_val))
  })
  f1_vals <- sapply(pop_entries, function(entry) entry$f1)
  precision_vals <- sapply(pop_entries, function(entry) entry$precision)
  recall_vals <- sapply(pop_entries, function(entry) entry$recall)
  n_vals <- sapply(pop_entries, function(entry) entry$n)
  total_n <- sum(n_vals, na.rm = TRUE)
  if (is.na(total_n) || total_n == 0) {
    return(list(
      weighted_f1 = NA_real_,
      weighted_precision = NA_real_,
      weighted_recall = NA_real_,
      total_n = total_n
    ))
  }
  list(
    weighted_f1 = sum(f1_vals * n_vals, na.rm = TRUE) / total_n,
    weighted_precision = sum(precision_vals * n_vals, na.rm = TRUE) / total_n,
    weighted_recall = sum(recall_vals * n_vals, na.rm = TRUE) / total_n,
    total_n = total_n
  )
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

normalize_population_id <- function(value) {
  raw <- as.character(unlist(value, use.names = FALSE))
  if (length(raw) != 1 || is.na(raw) || trimws(raw) == "") {
    return(NA_real_)
  }
  normalized <- suppressWarnings(as.numeric(raw))
  if (length(normalized) != 1 || is.na(normalized) || !is.finite(normalized)) {
    return(NA_real_)
  }
  normalized
}

extract_split_audit_populations <- function(payload, path) {
  populations <- payload$data_metadata$split_audit$populations
  if (is.null(populations)) {
    stop(
      sprintf(
        "Metric payload is missing data_metadata.split_audit.populations: %s",
        path
      )
    )
  }
  if (!is.list(populations)) {
    stop(sprintf("Metric split-audit populations must be a list: %s", path))
  }
  if (length(populations) == 0) {
    return(tibble(
      population_id_normalized = numeric(),
      audit_population_id = character(),
      audit_population_name = character(),
      nominal_train_count = numeric(),
      training_support = numeric(),
      present_in_training = logical(),
      test_truth_count = numeric()
    ))
  }

  rows <- lapply(seq_along(populations), function(idx) {
    entry <- populations[[idx]]
    required <- c(
      "id",
      "nominal_train_count",
      "training_support",
      "present_in_training",
      "test_truth_count"
    )
    if (!is.list(entry) || any(!required %in% names(entry))) {
      stop(
        sprintf(
          "Metric split-audit population %d is missing required fields: %s",
          idx,
          path
        )
      )
    }

    population_id <- normalize_population_id(entry$id)
    counts <- vapply(
      c("nominal_train_count", "training_support", "test_truth_count"),
      function(name) {
        value <- suppressWarnings(as.numeric(unlist(entry[[name]], use.names = FALSE)))
        if (length(value) != 1 || is.na(value) || !is.finite(value) || value < 0) {
          stop(
            sprintf(
              "Metric split-audit population %d has invalid %s: %s",
              idx,
              name,
              path
            )
          )
        }
        value
      },
      numeric(1)
    )
    present_in_training <- unlist(entry$present_in_training, use.names = FALSE)
    if (
      is.na(population_id) ||
        length(present_in_training) != 1 ||
        !is.logical(present_in_training) ||
        is.na(present_in_training)
    ) {
      stop(sprintf("Metric split-audit population %d is invalid: %s", idx, path))
    }
    if (present_in_training != (counts[["training_support"]] > 0)) {
      stop(
        sprintf(
          paste0(
            "Metric split-audit population %s has present_in_training=%s ",
            "but training_support=%s: %s"
          ),
          as.character(entry$id),
          present_in_training,
          counts[["training_support"]],
          path
        )
      )
    }

    tibble(
      population_id_normalized = population_id,
      audit_population_id = as.character(population_id),
      audit_population_name = as.character(entry$name %||% NA_character_),
      nominal_train_count = counts[["nominal_train_count"]],
      training_support = counts[["training_support"]],
      present_in_training = present_in_training,
      test_truth_count = counts[["test_truth_count"]]
    )
  })
  audit <- bind_rows(rows)
  duplicate_ids <- audit$population_id_normalized[duplicated(audit$population_id_normalized)]
  if (length(duplicate_ids) > 0) {
    stop(
      sprintf(
        "Metric split-audit contains duplicate normalized population IDs (%s): %s",
        paste(unique(duplicate_ids), collapse = ", "),
        path
      )
    )
  }
  audit
}

collect_metrics <- function(path) {
  payload <- read_metrics_json(path)
  metrics_requested <- as.character(unlist(payload$metrics_requested, use.names = FALSE))
  if (
    length(metrics_requested) != length(EXPECTED_METRICS) ||
      anyDuplicated(metrics_requested) ||
      !setequal(metrics_requested, EXPECTED_METRICS)
  ) {
    stop(
      sprintf(
        "Metric payload does not contain the canonical requested metrics: %s",
        path
      )
    )
  }
  results <- payload$results
  if (is.null(results) || length(results) == 0) {
    return(tibble())
  }
  lineage <- parse_lineage(path, payload)
  rows <- lapply(names(results), function(run_id) {
    run <- results[[run_id]]
    weighted <- compute_weighted_population_metrics(run$per_population)
    n_cells <- run$n_cells %||% run$n %||% weighted$total_n
    n_cells_total <- run$n_cells_total %||% n_cells
    tibble(
      dataset = lineage$dataset,
      dataset_name = lineage$dataset_name,
      dataset_sub_sampling = lineage$dataset_sub_sampling,
      model = lineage$model,
      model_base = lineage$model_base,
      model_variant = lineage$model_variant,
      model_params = lineage$model_params,
      stratification = lineage$stratification,
      stratification_hash = lineage$stratification_hash,
      crossvalidation = lineage$crossvalidation,
      requested_fold = lineage$requested_fold,
      effective_fold = lineage$effective_fold,
      run_id = run_id,
      f1_macro = as.numeric(run$f1_macro %||% NA_real_),
      precision_macro = as.numeric(run$precision_macro %||% NA_real_),
      recall_macro = as.numeric(run$recall_macro %||% NA_real_),
      balanced_accuracy = as.numeric(
        run$balanced_accuracy %||% run$recall_macro %||% NA_real_
      ),
      accuracy = as.numeric(run$accuracy %||% NA_real_),
      mcc = as.numeric(run$mcc %||% NA_real_),
      pop_freq_corr = as.numeric(run$pop_freq_corr %||% NA_real_),
      overlap = as.numeric(run$overlap %||% NA_real_),
      runtime_seconds = as.numeric(run$runtime_seconds %||% NA_real_),
      scalability_seconds_per_item = as.numeric(
        run$scalability_seconds_per_item %||% NA_real_
      ),
      f1_weighted = as.numeric(weighted$weighted_f1),
      precision_weighted = as.numeric(weighted$weighted_precision),
      recall_weighted = as.numeric(weighted$weighted_recall),
      n_cells = as.numeric(n_cells),
      n_cells_total = as.numeric(n_cells_total),
      n_truth_positive = as.numeric(run$n_truth_positive %||% NA_real_),
      n_truth_zero = as.numeric(run$n_truth_zero %||% NA_real_),
      n_pred_zero_on_truth_positive = as.numeric(
        run$n_pred_zero_on_truth_positive %||% NA_real_
      ),
      rejection_rate_on_truth_positive = as.numeric(
        run$rejection_rate_on_truth_positive %||% NA_real_
      ),
      n_pred_zero_on_truth_zero = as.numeric(
        run$n_pred_zero_on_truth_zero %||% NA_real_
      ),
      n_pred_missing_mapped_to_zero = as.numeric(
        run$n_pred_missing_mapped_to_zero %||% NA_real_
      ),
      source_path = path
    )
  })
  bind_rows(rows)
}

collect_per_population <- function(path) {
  empty_per_population <- function() {
    tibble(
      dataset = character(),
      dataset_name = character(),
      dataset_sub_sampling = character(),
      model = character(),
      model_base = character(),
      model_variant = character(),
      model_params = character(),
      stratification = character(),
      stratification_hash = character(),
      crossvalidation = character(),
      requested_fold = integer(),
      effective_fold = integer(),
      run_id = character(),
      population_id = character(),
      population_name = character(),
      population = character(),
      f1 = numeric(),
      precision = numeric(),
      recall = numeric(),
      accuracy = numeric(),
      tp = numeric(),
      fp = numeric(),
      fn = numeric(),
      tn = numeric(),
      scaling_rate = numeric(),
      support = numeric(),
      nominal_train_count = numeric(),
      training_support = numeric(),
      present_in_training = logical(),
      test_truth_count = numeric(),
      source_path = character()
    )
  }
  payload <- read_metrics_json(path)
  audit <- extract_split_audit_populations(payload, path)
  results <- payload$results
  if (is.null(results) || length(results) == 0) {
    return(empty_per_population())
  }
  lineage <- parse_lineage(path, payload)
  rows <- lapply(names(results), function(run_id) {
    run <- results[[run_id]]
    per_population <- run$per_population
    if (is.null(per_population) || length(per_population) == 0) {
      return(empty_per_population())
    }
    pop_rows <- lapply(names(per_population), function(pop_id) {
      entry <- per_population[[pop_id]]
      f1 <- as.numeric(entry$f1 %||% NA_real_)
      n_val <- entry$support %||% entry$n_cells %||% entry$n %||% NA_real_
      tibble(
        dataset = lineage$dataset,
        dataset_name = lineage$dataset_name,
        dataset_sub_sampling = lineage$dataset_sub_sampling,
        model = lineage$model,
        model_base = lineage$model_base,
        model_variant = lineage$model_variant,
        model_params = lineage$model_params,
        stratification = lineage$stratification,
        stratification_hash = lineage$stratification_hash,
        crossvalidation = lineage$crossvalidation,
        requested_fold = lineage$requested_fold,
        effective_fold = lineage$effective_fold,
        run_id = run_id,
        population_id = as.character(pop_id),
        population_id_normalized = normalize_population_id(pop_id),
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
    pop_rows <- bind_rows(pop_rows)
    if (run_id != "run0") {
      return(
        pop_rows %>%
          mutate(
            nominal_train_count = NA_real_,
            training_support = NA_real_,
            present_in_training = NA,
            test_truth_count = NA_real_
          ) %>%
          select(-population_id_normalized)
      )
    }

    pop_rows <- pop_rows %>%
      left_join(audit, by = "population_id_normalized")
    unmatched_truth <- pop_rows %>%
      filter(
        is.na(test_truth_count),
        (!is.na(support) & support > 0) |
          (!is.na(tp) & !is.na(fn) & (tp + fn) > 0)
      )
    if (nrow(unmatched_truth) > 0) {
      stop(
        sprintf(
          "Truth-present metric populations lack split-audit matches (%s): %s",
          paste(unique(unmatched_truth$population_id), collapse = ", "),
          path
        )
      )
    }
    support_mismatch <- pop_rows %>%
      filter(
        !is.na(test_truth_count),
        is.na(support) | support != test_truth_count
      )
    if (nrow(support_mismatch) > 0) {
      mismatch <- support_mismatch[1, ]
      stop(
        sprintf(
          paste0(
            "Metric support differs from split-audit test_truth_count for ",
            "population %s (%s != %s): %s"
          ),
          mismatch$population_id,
          mismatch$support,
          mismatch$test_truth_count,
          path
        )
      )
    }

    pop_rows %>%
      select(-population_id_normalized, -audit_population_id, -audit_population_name)
  })
  bind_rows(rows)
}

collect_population_availability <- function(path) {
  payload <- read_metrics_json(path)
  audit <- extract_split_audit_populations(payload, path)
  lineage <- parse_lineage(path, payload)
  audit %>%
    transmute(
      dataset = lineage$dataset,
      dataset_name = lineage$dataset_name,
      dataset_sub_sampling = lineage$dataset_sub_sampling,
      model = lineage$model,
      model_base = lineage$model_base,
      model_variant = lineage$model_variant,
      model_params = lineage$model_params,
      stratification = lineage$stratification,
      stratification_hash = lineage$stratification_hash,
      crossvalidation = lineage$crossvalidation,
      requested_fold = lineage$requested_fold,
      effective_fold = lineage$effective_fold,
      run_id = "run0",
      population_id = audit_population_id,
      population_name = audit_population_name,
      nominal_train_count,
      training_support,
      present_in_training,
      test_truth_count,
      source_path = path
    )
}

read_model_wall_seconds <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Model performance file not found: %s", path))
  }
  performance <- tryCatch(
    readr::read_tsv(path, show_col_types = FALSE, progress = FALSE),
    error = function(error) {
      stop(sprintf("Failed to read model performance file %s: %s", path, error$message))
    }
  )
  if (!"s" %in% names(performance) || nrow(performance) != 1) {
    stop(sprintf("Model performance file must contain one row and column s: %s", path))
  }
  seconds <- suppressWarnings(as.numeric(performance$s[[1]]))
  if (is.na(seconds) || !is.finite(seconds) || seconds < 0) {
    stop(sprintf("Model performance file has invalid wall time: %s", path))
  }
  seconds
}

build_metric_artifact_context <- function(metric_paths, metadata_paths) {
  metadata_roots <- dirname(metadata_paths)
  rows <- lapply(metric_paths, function(metric_path) {
    stratification_root <- sub("/analysis/.*$", "", metric_path)
    matches <- metadata_paths[metadata_roots == stratification_root]
    if (length(matches) != 1) {
      stop(
        sprintf(
          "Expected one data.metadata input beside metric path, found %d: %s",
          length(matches),
          metric_path
        )
      )
    }

    metric_payload <- read_metrics_json(metric_path)
    metadata_payload <- read_json_file(matches[[1]], simplifyVector = FALSE)
    embedded_metadata <- metric_payload$data_metadata
    if (is.null(embedded_metadata)) {
      stop(sprintf("Metric payload has no embedded data metadata: %s", metric_path))
    }
    metadata_json <- jsonlite::toJSON(
      metadata_payload,
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    )
    embedded_json <- jsonlite::toJSON(
      embedded_metadata,
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    )
    if (!identical(metadata_json, embedded_json)) {
      stop(
        sprintf(
          "Metric embedded metadata does not match its data.metadata input: %s",
          metric_path
        )
      )
    }

    analysis_root <- analysis_root_from_path(metric_path)
    if (is.na(analysis_root) || analysis_root == "") {
      stop(sprintf("Could not derive analysis root from metric path: %s", metric_path))
    }
    metric_name <- sub("\\.flow_metrics\\.json\\.gz$", "", basename(metric_path))
    prediction_path <- normalizePath(
      file.path(analysis_root, paste0(metric_name, "_predicted_labels.tar.gz")),
      winslash = "/",
      mustWork = FALSE
    )
    performance_path <- normalizePath(
      file.path(analysis_root, paste0(metric_name, "_performance.txt")),
      winslash = "/",
      mustWork = FALSE
    )
    if (!file.exists(prediction_path)) {
      stop(sprintf("Prediction archive not found: %s", prediction_path))
    }
    wall_seconds <- read_model_wall_seconds(performance_path)

    tibble(
      source_path = metric_path,
      metadata_path = matches[[1]],
      prediction_path = prediction_path,
      performance_path = performance_path,
      model_wall_seconds = wall_seconds
    )
  })
  bind_rows(rows)
}

assert_alias_values_equal <- function(data, key_columns, value_columns, label) {
  duplicate_rows <- data %>%
    add_count(across(all_of(key_columns)), name = ".alias_count") %>%
    filter(.alias_count > 1)
  if (nrow(duplicate_rows) == 0) {
    return(invisible(TRUE))
  }

  group_id <- do.call(
    interaction,
    c(
      lapply(duplicate_rows[key_columns], as.character),
      list(drop = TRUE, lex.order = TRUE)
    )
  )
  groups <- split(seq_len(nrow(duplicate_rows)), group_id)
  for (indices in groups) {
    group <- duplicate_rows[indices, , drop = FALSE]
    for (column in value_columns) {
      values <- group[[column]]
      reference <- values[[1]]
      equal <- if (is.numeric(values)) {
        same_missing <- all(is.na(values) == is.na(reference))
        finite_values <- values[!is.na(values)]
        same_missing && (
          length(finite_values) == 0 ||
            all(
              is.finite(finite_values) &
                abs(finite_values - reference) <=
                  1e-12 * max(1, abs(reference), na.rm = TRUE)
            )
        )
      } else {
        identical(as.character(values), rep(as.character(reference), length(values)))
      }
      if (!isTRUE(equal)) {
        stop(
          sprintf(
            "Wrapped aliases disagree in %s column %s for effective key %s",
            label,
            column,
            paste(as.character(group[1, key_columns, drop = TRUE]), collapse = "|")
          )
        )
      }
    }
  }
  invisible(TRUE)
}

write_jsonl <- function(df, path) {
  records <- vapply(seq_len(nrow(df)), function(idx) {
    jsonlite::toJSON(
      as.list(df[idx, , drop = FALSE]),
      auto_unbox = TRUE,
      na = "null",
      null = "null",
      digits = NA
    )
  }, character(1))
  writeLines(records, path, useBytes = TRUE)
}

write_collector_validation <- function(validation, path) {
  jsonlite::write_json(
    validation,
    path,
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = NA
  )
}

build_finalization_outputs <- function(requested_metrics, effective_metrics, output_dir) {
  accepted_manifest <- effective_metrics %>%
    transmute(
      collector_dataset_identity = dataset,
      dataset = dataset_name,
      dataset_sub_sampling,
      model = model_base,
      stratification,
      stratification_hash,
      effective_fold,
      metric_path = source_path,
      metadata_path,
      prediction_path,
      model_wall_seconds
    ) %>%
    arrange(
      dataset,
      collector_dataset_identity,
      effective_fold,
      stratification,
      model
    )

  run_status <- requested_metrics %>%
    transmute(
      collector_dataset_identity = dataset,
      dataset = dataset_name,
      dataset_sub_sampling,
      model = model_base,
      requested_fold,
      effective_fold,
      wrapped_fold = requested_fold != effective_fold,
      stratification,
      stratification_hash,
      status = "completed",
      prediction_path,
      metric_path = source_path,
      metadata_path,
      performance_path,
      model_wall_seconds
    ) %>%
    arrange(
      dataset,
      collector_dataset_identity,
      requested_fold,
      stratification,
      model
    )

  model_wall_times <- effective_metrics %>%
    transmute(
      collector_dataset_identity = dataset,
      dataset = dataset_name,
      dataset_sub_sampling,
      model = model_base,
      stratification,
      stratification_hash,
      effective_fold,
      model_wall_seconds,
      performance_path,
      metric_path = source_path
    )

  paths <- list(
    accepted_manifest = file.path(output_dir, "accepted-manifest.jsonl"),
    run_status = file.path(output_dir, "run-status.tsv"),
    model_wall_times = file.path(output_dir, "model-wall-times.tsv"),
    collector_validation = file.path(output_dir, "collector-validation-status.json")
  )
  write_jsonl(accepted_manifest, paths$accepted_manifest)
  readr::write_tsv(run_status, paths$run_status)
  readr::write_tsv(model_wall_times, paths$model_wall_times)

  model_counts <- table(accepted_manifest$model)
  accepted_key <- c(
    "collector_dataset_identity",
    "model",
    "stratification_hash",
    "effective_fold"
  )
  validation <- list(
    status = "PASS",
    counts = list(
      requested = nrow(requested_metrics),
      effective = nrow(accepted_manifest),
      models = length(unique(accepted_manifest$model)),
      effective_per_model = as.list(model_counts),
      stratifications = length(unique(accepted_manifest$stratification))
    ),
    assertions = list(
      requested_count = nrow(requested_metrics) == EXPECTED_REQUESTED_RUNS,
      effective_count = nrow(accepted_manifest) == EXPECTED_EFFECTIVE_RUNS,
      canonical_models = setequal(unique(accepted_manifest$model), EXPECTED_MODELS),
      rows_per_model = isTRUE(
        all(model_counts[EXPECTED_MODELS] == EXPECTED_RUNS_PER_MODEL)
      ),
      canonical_stratifications = setequal(
        unique(accepted_manifest$stratification),
        EXPECTED_STRATIFICATIONS
      ),
      requested_rows_completed = all(run_status$status == "completed"),
      effective_keys_unique = !anyDuplicated(accepted_manifest[accepted_key]),
      metric_paths_unique = !anyDuplicated(accepted_manifest$metric_path)
    ),
    outputs = list(
      accepted_manifest = basename(paths$accepted_manifest),
      run_status = basename(paths$run_status),
      model_wall_times = basename(paths$model_wall_times),
      collector_validation = basename(paths$collector_validation),
      metrics_report = "metrics_report.html",
      metric_plots = "metric_plots.tar.gz"
    )
  )
  if (!all(unlist(validation$assertions, use.names = FALSE))) {
    stop("Collector finalization assertions failed before PASS status write")
  }
  write_collector_validation(validation, paths$collector_validation)

  list(paths = paths, validation = validation)
}

write_table <- function(df, path) {
  if (nrow(df) == 0) {
    writeLines("", path)
    return(invisible())
  }
  readr::write_tsv(df, path)
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

first_non_empty <- function(values, fallback = NA_character_) {
  values <- as.character(unlist(values, use.names = FALSE))
  values <- values[!is.na(values) & trimws(values) != ""]
  if (length(values) == 0) {
    return(fallback)
  }
  values[[1]]
}

meta_as_vector <- function(meta, key) {
  if (is.null(meta) || !is.list(meta) || is.null(meta[[key]])) {
    return(character())
  }
  as.character(unlist(meta[[key]], use.names = FALSE))
}

compact_dataset_label <- function(dataset_id) {
  cleaned <- gsub("^dataset_name-", "", dataset_id)
  cleaned <- gsub("_seed-[0-9]+$", "", cleaned)
  cleaned <- gsub("_", "-", cleaned)
  cleaned
}

pretty_model_label <- function(values) {
  model_map <- c(
    "cyanno" = "CyAnno",
    "cygate" = "CyGATE",
    "dgcytof" = "DGCyTOF",
    "knn" = "KNN",
    "lda" = "LDA",
    "random" = "Random"
  )

  raw <- as.character(values)
  key <- tolower(raw)
  mapped <- unname(model_map[key])
  out <- ifelse(!is.na(mapped), mapped, raw)
  out[is.na(values)] <- NA_character_
  out
}

pretty_platform_label <- function(values) {
  platform_map <- c(
    "fcm" = "Flow",
    "cytof" = "CyTOF"
  )

  raw <- as.character(values)
  key <- tolower(trimws(raw))
  mapped <- unname(platform_map[key])
  out <- ifelse(!is.na(mapped), mapped, raw)
  out[is.na(values) | trimws(raw) == ""] <- "Unknown"
  out
}

build_dataset_metadata_table <- function(metrics_df, dataset_metadata) {
  datasets <- sort(unique(metrics_df$dataset))
  rows <- lapply(datasets, function(dataset_id) {
    meta <- dataset_metadata[[dataset_id]]
    if (is.null(meta) || !is.list(meta)) {
      meta <- list()
    }
    short_name <- first_non_empty(
      c(
        meta_as_vector(meta, "expected_abbreviation"),
        meta_as_vector(meta, "shortnames"),
        meta_as_vector(meta, "dataset_name"),
        compact_dataset_label(dataset_id)
      ),
      fallback = dataset_id
    )
    n_markers <- suppressWarnings(as.numeric(first_non_empty(meta_as_vector(meta, "n_variables"), NA_character_)))
    n_populations <- suppressWarnings(as.numeric(first_non_empty(meta_as_vector(meta, "population_count"), NA_character_)))
    cells_per_sample <- suppressWarnings(as.numeric(meta_as_vector(meta, "cells_per_sample")))
    mean_cells <- if (length(cells_per_sample) > 0 && !all(is.na(cells_per_sample))) {
      mean(cells_per_sample, na.rm = TRUE)
    } else {
      NA_real_
    }
    platform <- first_non_empty(meta_as_vector(meta, "platform"), fallback = "Unknown")
    tibble(
      dataset = dataset_id,
      dataset_label = short_name,
      platform = platform,
      n_markers = n_markers,
      n_populations = n_populations,
      mean_cells = mean_cells,
      is_sub_sampling = str_detect(dataset_id, regex("sub-sampling", ignore_case = TRUE)),
      train_size = suppressWarnings(as.numeric(str_match(dataset_id, "sub-sampling-([0-9]+)")[, 2]))
    )
  })

  bind_rows(rows) %>%
    mutate(
      display_name = ifelse(
        !is.na(n_markers) & !is.na(n_populations),
        paste0(dataset_label, " (M:", n_markers, ", P:", n_populations, ")"),
        dataset_label
      )
    )
}

build_model_palette <- function(models) {
  model_levels <- sort(unique(models[!is.na(models) & models != ""]))
  if (length(model_levels) == 0) {
    return(c())
  }
  colors <- scales::hue_pal(h = c(15, 375), c = 100, l = 55)(length(model_levels))
  names(colors) <- model_levels
  colors
}

empty_panel <- function(message_text) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = message_text, size = 3.5) +
    theme_void()
}

filter_plot_metric_rows <- function(df, metric_column) {
  df %>%
    filter(!is.na(.data[[metric_column]]), is.finite(.data[[metric_column]])) %>%
    filter(.data[[metric_column]] >= 0, .data[[metric_column]] <= 1)
}

generate_figure1_heatmap_boxplot <- function(metrics_df, dataset_meta, plot_dir) {
  df <- metrics_df %>%
    filter(!is.na(f1_macro)) %>%
    left_join(dataset_meta, by = "dataset") %>%
    filter(!is_sub_sampling) %>%
    mutate(
      platform_plot = pretty_platform_label(platform),
      model_pretty = pretty_model_label(model)
    )

  if (nrow(df) == 0) {
    return(NULL)
  }

  df_no_random <- df %>%
    filter(!str_detect(model, regex("random", ignore_case = TRUE)))

  dataset_order <- df_no_random %>%
    group_by(platform_plot, display_name) %>%
    summarize(global_mean = mean(f1_macro, na.rm = TRUE), .groups = "drop") %>%
    arrange(platform_plot, global_mean) %>%
    pull(display_name)

  model_order <- df %>%
    group_by(model_pretty) %>%
    summarize(global_mean = mean(f1_macro, na.rm = TRUE), .groups = "drop") %>%
    arrange(global_mean) %>%
    pull(model_pretty)

  if (length(dataset_order) == 0 || length(model_order) == 0) {
    return(NULL)
  }

  df <- df %>%
    mutate(
      model_pretty = factor(model_pretty, levels = model_order),
      display_name = factor(display_name, levels = dataset_order)
    )
  df_no_random <- df_no_random %>%
    mutate(display_name = factor(display_name, levels = dataset_order))

  heatmap_data <- df %>%
    group_by(model_pretty, display_name, platform_plot) %>%
    summarize(mean_f1 = mean(f1_macro, na.rm = TRUE), .groups = "drop") %>%
    group_by(platform_plot, display_name) %>%
    tidyr::complete(model_pretty, fill = list(mean_f1 = NA_real_)) %>%
    ungroup() %>%
    mutate(
      label_text = case_when(
        is.na(mean_f1) | is.nan(mean_f1) ~ "",
        TRUE ~ sprintf("%.2f", mean_f1)
      )
    )

  if (nrow(heatmap_data) == 0) {
    return(NULL)
  }

  theme_base <- theme_minimal(base_size = 8, base_family = "sans") +
    theme(
      text = element_text(color = "black"),
      axis.text = element_text(size = 7, color = "black"),
      axis.title = element_text(size = 8, face = "bold", color = "black"),
      plot.margin = margin(2, 2, 2, 2, unit = "pt")
    )

  p_heatmap <- ggplot(heatmap_data, aes(x = display_name, y = model_pretty, fill = mean_f1)) +
    geom_tile(color = "white", linewidth = 0.2) +
    geom_text(aes(label = label_text), size = 1.8, color = "black") +
    facet_grid(. ~ platform_plot, scales = "free_x", space = "free_x") +
    scale_fill_viridis_c(option = "viridis", limits = c(0, 1), na.value = "grey80", guide = "none") +
    scale_x_discrete(position = "top", expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(x = "", y = NULL) +
    theme_base +
    theme(
      panel.background = element_rect(fill = "grey92", color = NA),
      axis.text.x = element_text(angle = 45, hjust = 0, vjust = 0),
      panel.spacing = unit(6, "pt"),
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text = element_text(face = "bold", size = 8),
      panel.grid = element_blank(),
      plot.margin = margin(b = 0, r = 3, unit = "pt")
    )

  p_right <- ggplot(df, aes(x = f1_macro, y = model_pretty)) +
    geom_boxplot(outlier.size = 0.1, linewidth = 0.25, fill = "white") +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    labs(x = "F1-score", y = NULL) +
    theme_base +
    theme(
      panel.background = element_rect(fill = "grey95", color = NA),
      axis.text.y = element_blank(),
      plot.margin = margin(l = 3, unit = "pt")
    )

  p_bottom <- ggplot(df_no_random, aes(x = display_name, y = f1_macro)) +
    geom_boxplot(outlier.size = 0.1, linewidth = 0.25, fill = "white") +
    facet_grid(. ~ platform_plot, scales = "free_x", space = "free_x") +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    labs(x = NULL, y = "F1-score") +
    theme_base +
    theme(
      panel.background = element_rect(fill = "grey95", color = NA),
      axis.text.x = element_blank(),
      strip.background = element_blank(),
      strip.text = element_blank(),
      panel.spacing = unit(6, "pt"),
      plot.margin = margin(t = 0, unit = "pt")
    )

  design <- "
AAAAABB
AAAAABB
AAAAABB
AAAAABB
CCCCC##
"

  final_plot <- wrap_plots(A = p_heatmap, B = p_right, C = p_bottom, design = design)

  n_tools <- length(unique(df$model_pretty))
  n_datasets <- length(unique(df$display_name))
  final_h <- min(max((n_tools * 8) + 80, 120), 225)
  final_w <- min(max((n_datasets * 15) + 80, 160), 220)

  output_file <- file.path(plot_dir, "fig1_heatmap-boxplot.png")
  ggsave(output_file, plot = final_plot, width = final_w, height = final_h, units = "mm", dpi = 600)
  basename(output_file)
}

prepare_confusion_plot_data <- function(per_population_confusion, dataset_meta) {
  per_population_confusion %>%
    mutate(
      population_label = ifelse(
        !is.na(population_name) & population_name != "",
        population_name,
        population
      ),
      tp = as.numeric(tp),
      fp = as.numeric(fp),
      fn = as.numeric(fn),
      f1_score = ifelse((2 * tp + fp + fn) > 0, (2 * tp) / (2 * tp + fp + fn), NA_real_),
      actual_count = tp + fn
    ) %>%
    left_join(dataset_meta, by = "dataset") %>%
    filter(!is_sub_sampling)
}

generate_figure2_plots <- function(per_population_confusion, dataset_meta, plot_dir) {
  df <- prepare_confusion_plot_data(per_population_confusion, dataset_meta)
  df <- filter_plot_metric_rows(df, "f1_score")
  if (nrow(df) == 0) {
    return(list())
  }

  tool_colors <- build_model_palette(df$model)
  if (length(tool_colors) == 0) {
    return(list())
  }

  pop_meta <- df %>%
    group_by(dataset, dataset_label, population_label) %>%
    summarize(total_count = max(actual_count, na.rm = TRUE), .groups = "drop") %>%
    mutate(total_count = ifelse(is.finite(total_count), total_count, NA_real_)) %>%
    group_by(dataset, dataset_label) %>%
    mutate(
      dataset_total = sum(total_count, na.rm = TRUE),
      pct = ifelse(dataset_total > 0, (total_count / dataset_total) * 100, NA_real_),
      abundance_class = case_when(
        min_rank(desc(total_count)) <= 3 ~ "Most prevalent",
        min_rank(total_count) <= 3 ~ "Least prevalent",
        TRUE ~ NA_character_
      ),
      pop_label = paste0(population_label, " (", round(pct, 2), "%)"),
      pop_label_newline = paste0(population_label, "\n", round(pct, 2), "%")
    ) %>%
    group_by(dataset, dataset_label, abundance_class) %>%
    mutate(rank_within_class = min_rank(desc(total_count))) %>%
    ungroup()

  df_final <- df %>%
    inner_join(pop_meta, by = c("dataset", "dataset_label", "population_label"))

  gb_theme <- theme_bw(base_size = 9) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      panel.grid.major = element_line(color = "grey92"),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
      strip.text = element_text(face = "bold", size = 9, color = "black"),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "mm"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 4, color = "black"),
      axis.text.y = element_text(size = 8, color = "black"),
      axis.title.y = element_text(face = "bold", size = 9),
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      legend.title = element_text(face = "bold", size = 9),
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5)
    )

  unique_datasets <- sort(unique(df_final$dataset))
  generated <- list()

  for (ds_id in unique_datasets) {
    ds_data <- df_final %>% filter(dataset == ds_id)
    if (nrow(ds_data) == 0) {
      next
    }
    ds_label <- first_non_empty(ds_data$dataset_label, fallback = ds_id)
    ds_token <- sanitize_label(ds_label)
    ds_n_cells <- ds_data %>% summarize(dataset_total = max(dataset_total, na.rm = TRUE)) %>% pull(dataset_total)

    p1_data <- ds_data %>% mutate(pop_label = reorder(pop_label, -actual_count))
    p1 <- ggplot(p1_data, aes(x = pop_label, y = f1_score, fill = model, color = model)) +
      geom_boxplot(outlier.size = 0.2, linewidth = 0.35, median.linewidth = 1, alpha = 0.7, width = 0.75) +
      scale_fill_manual(values = tool_colors, name = "Method") +
      scale_color_manual(values = tool_colors, name = "Method") +
      scale_y_continuous(breaks = seq(0, 1, 0.2)) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(
        title = paste(ds_label, "- Full Population Profile"),
        x = NULL,
        y = "F1-Score",
        subtitle = paste("N cells:", format(ds_n_cells, big.mark = ","))
      ) +
      gb_theme

    w1 <- max(150, length(unique(p1_data$pop_label)) * 12)
    full_name <- paste0("fig2_full_", ds_token, ".png")
    ggsave(file.path(plot_dir, full_name), p1, width = w1, height = 120, units = "mm", dpi = 600)
    generated[[paste0("fig2_full_", ds_token)]] <- file.path("plots", full_name)

    p2_data <- ds_data %>%
      filter(abundance_class == "Most prevalent" | (abundance_class == "Least prevalent" & rank_within_class <= 5)) %>%
      mutate(
        abundance_class = factor(abundance_class, levels = c("Most prevalent", "Least prevalent")),
        pop_label = reorder(pop_label, -actual_count)
      )

    if (nrow(p2_data) > 0) {
      n_pop <- length(unique(p2_data$population_label))
      p2 <- ggplot(p2_data, aes(x = pop_label, y = f1_score, fill = model, color = model)) +
        geom_boxplot(outlier.size = 0.2, linewidth = 0.35, alpha = 0.7, width = 0.75) +
        (if (n_pop >= 6) facet_grid(. ~ abundance_class, scales = "free_x", space = "free_x") else NULL) +
        scale_fill_manual(values = tool_colors, name = "Method") +
        scale_color_manual(values = tool_colors, name = "Method") +
        scale_y_continuous(breaks = seq(0, 1, 0.2)) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(
          title = paste(ds_label, "- Most and least prevalent cell populations"),
          x = NULL,
          y = "F1-Score",
          subtitle = paste("N cells:", format(ds_n_cells, big.mark = ","))
        ) +
        gb_theme

      extremes_name <- paste0("fig2_extremes_", ds_token, ".png")
      ggsave(file.path(plot_dir, extremes_name), p2, width = 180, height = 120, units = "mm", dpi = 600)
      generated[[paste0("fig2_extremes_", ds_token)]] <- file.path("plots", extremes_name)
    }
  }

  sub_theme <- theme_bw(base_size = 6) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.3),
      panel.grid.major = element_line(color = "grey95"),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.3),
      strip.text = element_text(face = "bold", size = 6, color = "black"),
      plot.margin = margin(2, 2, 2, 12, "mm"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 5, color = "black"),
      axis.text.y = element_text(size = 6, color = "black"),
      axis.title = element_blank(),
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 8, hjust = 0, margin = margin(b = 2))
    )

  plot_list <- list()
  for (ds_id in unique_datasets) {
    p_data <- df_final %>%
      filter(dataset == ds_id) %>%
      mutate(pop_label = reorder(pop_label, -actual_count))
    if (nrow(p_data) == 0) {
      next
    }
    ds_label <- first_non_empty(p_data$dataset_label, fallback = ds_id)
    ds_n_cells <- p_data %>% summarize(dataset_total = max(dataset_total, na.rm = TRUE)) %>% pull(dataset_total)
    p <- ggplot(p_data, aes(x = pop_label, y = f1_score, fill = model, color = model)) +
      geom_boxplot(outlier.size = 0.05, linewidth = 0.25, median.linewidth = 0.8, alpha = 0.7, width = 0.7) +
      scale_fill_manual(values = tool_colors) +
      scale_color_manual(values = tool_colors) +
      scale_y_continuous(breaks = c(0, 0.5, 1)) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(title = ds_label, subtitle = paste("N cells:", format(ds_n_cells, big.mark = ","))) +
      sub_theme
    plot_list[[ds_label]] <- p
  }

  if (length(plot_list) > 0) {
    plot_list <- plot_list[sort(names(plot_list))]
    legend_plot <- ggplot(df_final, aes(x = model, y = f1_score, fill = model, color = model)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = tool_colors, name = "Method") +
      scale_color_manual(values = tool_colors, name = "Method") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.text = element_text(size = 8, face = "bold"))

    common_legend <- cowplot::get_legend(legend_plot)
    final_grid <- wrap_plots(plot_list, ncol = 2)
    final_figure <- cowplot::plot_grid(final_grid, common_legend, ncol = 1, rel_heights = c(1, 0.05))
    multi_name <- "fig2_multipanel_a4.png"
    ggsave(file.path(plot_dir, multi_name), plot = final_figure, width = 220, height = 300, units = "mm", dpi = 1200)
    generated$figure2_multipanel <- file.path("plots", multi_name)
  }

  refined_theme <- theme_bw(base_size = 8) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.3),
      panel.grid.major = element_line(color = "grey95", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
      strip.text = element_text(face = "bold", size = 7, color = "black"),
      plot.margin = margin(2, 2, 2, 16, "mm"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 5.5, color = "black"),
      axis.text.y = element_text(size = 6, color = "black"),
      axis.title = element_blank(),
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 9, hjust = 0, margin = margin(b = 3))
    )

  plot_list_refined <- list()
  for (ds_id in unique_datasets) {
    p_data <- df_final %>%
      filter(dataset == ds_id) %>%
      filter(!is.na(abundance_class)) %>%
      filter(rank_within_class <= 5) %>%
      mutate(
        abundance_class = factor(abundance_class, levels = c("Most prevalent", "Least prevalent")),
        pop_label_newline = reorder(pop_label_newline, -pct)
      )
    if (nrow(p_data) == 0) {
      next
    }
    ds_label <- first_non_empty(p_data$dataset_label, fallback = ds_id)
    ds_n_cells <- p_data %>% summarize(dataset_total = max(dataset_total, na.rm = TRUE)) %>% pull(dataset_total)
    n_pop <- length(unique(p_data$population_label))
    p <- ggplot(p_data, aes(x = pop_label_newline, y = f1_score, fill = model, color = model)) +
      geom_boxplot(outlier.size = 0.1, linewidth = 0.25, median.linewidth = 1, alpha = 0.7, width = 0.65) +
      (if (n_pop >= 6) facet_grid(. ~ abundance_class, scales = "free_x", space = "free_x") else NULL) +
      scale_fill_manual(values = tool_colors) +
      scale_color_manual(values = tool_colors) +
      scale_y_continuous(breaks = c(0, 0.5, 1)) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(title = ds_label, subtitle = paste("N cells:", format(ds_n_cells, big.mark = ","))) +
      refined_theme
    plot_list_refined[[ds_label]] <- p
  }

  if (length(plot_list_refined) > 0) {
    legend_plot <- ggplot(df_final, aes(x = model, y = f1_score, fill = model, color = model)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = tool_colors, name = "Method") +
      scale_color_manual(values = tool_colors, name = "Method") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 8, face = "bold"),
        legend.key.size = unit(4, "mm")
      )
    common_legend <- cowplot::get_legend(legend_plot)
    p_grid <- cowplot::plot_grid(plotlist = plot_list_refined, ncol = 2, align = "hv", axis = "tb")
    final_plot <- cowplot::plot_grid(p_grid, common_legend, ncol = 1, rel_heights = c(1.5, 0.05))
    refined_name <- "fig2_a4_top5_refined.png"
    ggsave(file.path(plot_dir, refined_name), plot = final_plot, width = 210, height = 297, units = "mm", dpi = 1200)
    generated$figure2_refined <- file.path("plots", refined_name)
  }

  generated
}

generate_figure3_plots <- function(metrics_df, run_metrics_table, dataset_meta, plot_dir) {
  model_colors <- build_model_palette(metrics_df$model)
  if (length(model_colors) == 0) {
    return(list())
  }

  f1_non_sub <- metrics_df %>%
    filter(!str_detect(dataset, regex("sub-sampling", ignore_case = TRUE))) %>%
    filter(!str_detect(model, regex("random", ignore_case = TRUE))) %>%
    group_by(dataset, model) %>%
    summarize(mean_f1_macro = mean(f1_macro, na.rm = TRUE), .groups = "drop")

  runtime_non_sub <- run_metrics_table %>%
    filter(!str_detect(dataset, regex("sub-sampling", ignore_case = TRUE))) %>%
    filter(!str_detect(model, regex("random", ignore_case = TRUE))) %>%
    filter(!is.na(runtime_seconds)) %>%
    group_by(dataset, model) %>%
    summarize(mean_time_sec = mean(runtime_seconds, na.rm = TRUE), .groups = "drop")

  dataset_general <- dataset_meta %>%
    filter(!is_sub_sampling) %>%
    select(dataset, n_markers, n_populations, mean_cells)

  df_plot_f1 <- f1_non_sub %>% inner_join(dataset_general, by = "dataset")
  df_plot_time <- runtime_non_sub %>% inner_join(dataset_general, by = "dataset")

  df_subsampling_f1 <- metrics_df %>%
    filter(str_detect(dataset, regex("sub-sampling", ignore_case = TRUE))) %>%
    filter(!str_detect(model, regex("random", ignore_case = TRUE))) %>%
    mutate(train_size = suppressWarnings(as.numeric(str_match(dataset, "sub-sampling-([0-9]+)")[, 2]))) %>%
    filter(!is.na(train_size)) %>%
    group_by(model, train_size) %>%
    summarize(mean_f1 = mean(f1_macro, na.rm = TRUE), .groups = "drop")

  df_subsampling_time <- run_metrics_table %>%
    filter(str_detect(dataset, regex("sub-sampling", ignore_case = TRUE))) %>%
    filter(!str_detect(model, regex("random", ignore_case = TRUE))) %>%
    mutate(train_size = suppressWarnings(as.numeric(str_match(dataset, "sub-sampling-([0-9]+)")[, 2]))) %>%
    filter(!is.na(train_size), !is.na(runtime_seconds)) %>%
    group_by(model, train_size) %>%
    summarize(mean_time = mean(runtime_seconds, na.rm = TRUE), .groups = "drop")

  scatter_theme <- theme_bw(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom"
    )

  p_markers_f1 <- if (nrow(df_plot_f1) > 0) {
    ggplot(df_plot_f1, aes(n_markers, mean_f1_macro, color = model, group = model)) +
      geom_line(alpha = 0.3) +
      geom_point() +
      scale_color_manual(values = model_colors, name = "Method") +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
      labs(x = "Number of Markers", y = "Mean F1") +
      scatter_theme
  } else {
    empty_panel("No non sub-sampling marker data")
  }

  p_markers_time <- if (nrow(df_plot_time) > 0) {
    ggplot(df_plot_time, aes(n_markers, mean_time_sec, color = model, group = model)) +
      geom_line(alpha = 0.3) +
      geom_point() +
      scale_y_log10() +
      scale_color_manual(values = model_colors, name = "Method") +
      labs(x = "Number of Markers", y = "Mean Runtime (s)") +
      scatter_theme
  } else {
    empty_panel("No runtime data")
  }

  p_pops_f1 <- if (nrow(df_plot_f1) > 0) {
    ggplot(df_plot_f1, aes(n_populations, mean_f1_macro, color = model, group = model)) +
      geom_line(alpha = 0.3) +
      geom_point() +
      scale_color_manual(values = model_colors, name = "Method") +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
      labs(x = "Number of Populations", y = "Mean F1") +
      scatter_theme
  } else {
    empty_panel("No population data")
  }

  p_pops_time <- if (nrow(df_plot_time) > 0) {
    ggplot(df_plot_time, aes(n_populations, mean_time_sec, color = model, group = model)) +
      geom_line(alpha = 0.3) +
      geom_point() +
      scale_y_log10() +
      scale_color_manual(values = model_colors, name = "Method") +
      labs(x = "Number of Populations", y = "Mean Runtime (s)") +
      scatter_theme
  } else {
    empty_panel("No runtime data")
  }

  p_cells_f1 <- if (nrow(df_plot_f1) > 0) {
    ggplot(df_plot_f1, aes(mean_cells, mean_f1_macro, color = model, group = model)) +
      geom_line(alpha = 0.3) +
      geom_point() +
      scale_x_log10(labels = label_log()) +
      scale_color_manual(values = model_colors, name = "Method") +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
      labs(x = "Mean Cells per Sample", y = "Mean F1") +
      scatter_theme
  } else {
    empty_panel("No mean cell data")
  }

  p_cells_time <- if (nrow(df_plot_time) > 0) {
    ggplot(df_plot_time, aes(mean_cells, mean_time_sec, color = model, group = model)) +
      geom_line(alpha = 0.3) +
      geom_point() +
      scale_x_log10(labels = label_log()) +
      scale_y_log10() +
      scale_color_manual(values = model_colors, name = "Method") +
      labs(x = "Mean Cells per Sample", y = "Mean Runtime (s)") +
      scatter_theme
  } else {
    empty_panel("No runtime data")
  }

  p_train_f1 <- if (nrow(df_subsampling_f1) > 0) {
    ggplot(df_subsampling_f1, aes(train_size, mean_f1, color = model, group = model)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(labels = label_number(suffix = "K", scale = 1e-3)) +
      scale_color_manual(values = model_colors, name = "Method") +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
      labs(x = "Train Size", y = "Mean F1") +
      scatter_theme
  } else {
    empty_panel("No sub-sampling F1 data")
  }

  p_train_time <- if (nrow(df_subsampling_time) > 0) {
    ggplot(df_subsampling_time, aes(train_size, mean_time, color = model, group = model)) +
      geom_line() +
      geom_point() +
      scale_y_log10() +
      scale_x_continuous(labels = label_number(suffix = "K", scale = 1e-3)) +
      scale_color_manual(values = model_colors, name = "Method") +
      labs(x = "Train Size", y = "Mean Runtime (s)") +
      scatter_theme
  } else {
    empty_panel("No sub-sampling runtime data")
  }

  legend_source <- bind_rows(
    df_plot_f1 %>% select(model),
    df_subsampling_f1 %>% select(model)
  ) %>% distinct()

  common_legend <- if (nrow(legend_source) > 0) {
    legend_plot <- ggplot(legend_source, aes(x = model, y = 1, color = model)) +
      geom_point(size = 3) +
      scale_color_manual(values = model_colors, name = "Method") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.text = element_text(size = 8, face = "bold"))
    cowplot::get_legend(legend_plot)
  } else {
    cowplot::ggdraw()
  }

  strip_legend <- function(plot_obj) {
    plot_obj + theme(legend.position = "none")
  }

  grid1 <- cowplot::plot_grid(
    strip_legend(p_markers_f1), strip_legend(p_markers_time),
    strip_legend(p_pops_f1), strip_legend(p_pops_time),
    ncol = 2,
    labels = "auto",
    align = "hv"
  )
  final1 <- cowplot::plot_grid(grid1, common_legend, ncol = 1, rel_heights = c(1, 0.1))

  grid2 <- cowplot::plot_grid(
    strip_legend(p_cells_f1), strip_legend(p_cells_time),
    strip_legend(p_train_f1), strip_legend(p_train_time),
    ncol = 2,
    labels = "auto",
    align = "hv"
  )
  final2 <- cowplot::plot_grid(grid2, common_legend, ncol = 1, rel_heights = c(1, 0.1))

  fig3a_name <- "fig3a_markers_pops.png"
  fig3b_name <- "fig3b_cells_trainsize.png"
  ggsave(file.path(plot_dir, fig3a_name), final1, width = 180, height = 160, units = "mm", dpi = 600)
  ggsave(file.path(plot_dir, fig3b_name), final2, width = 180, height = 160, units = "mm", dpi = 600)

  df_plot_f1_3d <- df_plot_f1 %>%
    mutate(markers_per_pop = n_markers / n_populations) %>%
    filter(!is.na(mean_cells), mean_cells > 0, !is.na(markers_per_pop), !is.na(mean_f1_macro))
  df_plot_time_3d <- df_plot_time %>%
    mutate(markers_per_pop = n_markers / n_populations, log_time = log10(mean_time_sec)) %>%
    filter(!is.na(mean_cells), mean_cells > 0, !is.na(markers_per_pop), !is.na(log_time))

  generated <- list(
    figure3_markers_pops = file.path("plots", fig3a_name),
    figure3_cells_train = file.path("plots", fig3b_name)
  )

  if (nrow(df_plot_f1_3d) > 0 && nrow(df_plot_time_3d) > 0) {
    active_models <- sort(unique(c(df_plot_f1_3d$model, df_plot_time_3d$model)))
    active_colors <- model_colors[active_models]

    fig3d_name <- "fig3_3d_performance.png"
    png(filename = file.path(plot_dir, fig3d_name), width = 14, height = 6.5, units = "in", res = 300)
    layout(matrix(c(1, 2), nrow = 1), widths = c(1, 1))
    par(oma = c(2, 2, 3, 10))

    par(mar = c(4, 3, 2, 2))
    scatterplot3d::scatterplot3d(
      x = log10(df_plot_f1_3d$mean_cells),
      y = df_plot_f1_3d$markers_per_pop,
      z = df_plot_f1_3d$mean_f1_macro,
      color = model_colors[df_plot_f1_3d$model],
      pch = 16,
      type = "h",
      lty.hplot = 3,
      angle = 45,
      scale.y = 0.8,
      zlim = c(0, 1),
      main = "Model Accuracy Profile",
      xlab = "Log10(Mean Cells)",
      ylab = "Markers / Population",
      zlab = "Mean Macro F1"
    )

    par(mar = c(4, 3, 2, 2))
    scatterplot3d::scatterplot3d(
      x = log10(df_plot_time_3d$mean_cells),
      y = df_plot_time_3d$markers_per_pop,
      z = df_plot_time_3d$log_time,
      color = model_colors[df_plot_time_3d$model],
      pch = 16,
      type = "h",
      lty.hplot = 3,
      angle = 45,
      scale.y = 0.8,
      main = "Model Efficiency Profile",
      xlab = "Log10(Mean Cells)",
      ylab = "Markers / Population",
      zlab = "Log10(Runtime in seconds)"
    )

    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    legend(
      "right",
      legend = active_models,
      col = active_colors,
      pch = 16,
      bty = "n",
      cex = 1.1,
      title = "Method",
      inset = c(0.02, 0)
    )
    dev.off()
    generated$figure3_3d <- file.path("plots", fig3d_name)
  }

  generated
}

generate_plots2_suite <- function(
  plot_dir,
  metrics_df,
  per_population_confusion,
  run_metrics_table,
  dataset_metadata
) {
  dataset_meta <- build_dataset_metadata_table(metrics_df, dataset_metadata)
  generated <- list()

  fig1 <- generate_figure1_heatmap_boxplot(metrics_df, dataset_meta, plot_dir)
  if (!is.null(fig1)) {
    generated$figure1_heatmap <- file.path("plots", fig1)
  }

  fig2_outputs <- generate_figure2_plots(per_population_confusion, dataset_meta, plot_dir)
  if (length(fig2_outputs) > 0) {
    generated <- c(generated, fig2_outputs)
  }

  fig3_outputs <- generate_figure3_plots(metrics_df, run_metrics_table, dataset_meta, plot_dir)
  if (length(fig3_outputs) > 0) {
    generated <- c(generated, fig3_outputs)
  }

  generated
}

default_table_paths <- function() {
  list(
    macro_by_cv = "f1_macro_by_crossvalidation.tsv",
    per_population_by_cv = "per_population_by_crossvalidation.tsv",
    macro_summary = "f1_macro_summary_by_model.tsv",
    weighted_summary = "f1_weighted_summary_by_model.tsv",
    run_metrics = "run_metrics.tsv",
    population_availability = "population_availability_by_crossvalidation.tsv",
    per_population_summary = "per_population_summary.tsv",
    per_population_stability = "per_population_stability.tsv",
    per_population_confusion = "per_population_confusion.tsv",
    rare_population = "rare_population_buckets.tsv",
    dataset_context = "dataset_context.tsv",
    dominant_fnr = "dominant_errors_fnr.tsv",
    dominant_fpr = "dominant_errors_fpr.tsv"
  )
}

reset_plot_dir <- function(plot_dir) {
  if (dir.exists(plot_dir)) {
    unlink(plot_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
}

repository_script_path <- function(file_name) {
  script_arg <- grep("^--file=", commandArgs(), value = TRUE)
  if (length(script_arg) != 1) {
    stop("Could not determine metric collector script directory")
  }
  collector_path <- normalizePath(
    sub("^--file=", "", script_arg),
    winslash = "/",
    mustWork = TRUE
  )
  path <- file.path(dirname(collector_path), file_name)
  if (!file.exists(path)) {
    stop(sprintf("Repository-local script not found: %s", path))
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

invoke_reviewer_figures <- function(input_root, output_dir) {
  reviewer_script <- repository_script_path("reviewer_figures.R")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    args = c(
      shQuote(reviewer_script),
      "--input-root",
      shQuote(input_root),
      "--output-dir",
      shQuote(output_dir)
    )
  )
  if (status != 0) {
    stop(sprintf("reviewer_figures.R exited with status %s", status))
  }
}

render_report <- function(
  output_dir,
  plot_paths,
  tables,
  name,
  performance_note = NULL,
  artifact_paths = character()
) {
  report_path <- file.path(output_dir, "metrics_report.Rmd")
  output_html <- file.path(output_dir, "metrics_report.html")
  plot_files <- unique(unlist(plot_paths, use.names = FALSE))
  plot_files <- plot_files[!is.na(plot_files) & plot_files != ""]
  report_files <- unique(c(unlist(tables, use.names = FALSE), artifact_paths))
  if (!rmarkdown::pandoc_available()) {
    macro_table <- readr::read_tsv(
      file.path(output_dir, tables$macro_by_cv),
      show_col_types = FALSE
    )
    population_table <- readr::read_tsv(
      file.path(output_dir, tables$per_population_by_cv),
      show_col_types = FALSE
    )
    population_availability_table <- readr::read_tsv(
      file.path(output_dir, tables$population_availability),
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
    outputs <- data.frame(file = report_files)
    plot_html <- c()
    for (plot_file in plot_files) {
      if (file.exists(file.path(output_dir, plot_file))) {
        plot_html <- c(
          plot_html,
          sprintf("<h3>%s</h3>", basename(plot_file)),
          sprintf("<img src=\"%s\" />", plot_file)
        )
      }
    }

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
      "<h2>Population Availability By Crossvalidation</h2>",
      knitr::kable(population_availability_table, format = "html"),
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
      plot_html,
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

  plot_chunk_lines <- if (length(plot_files) == 0) {
    c(
      "```{r}",
      "plot_files <- character()",
      "```"
    )
  } else {
    plot_file_lines <- sprintf(
      "  '%s'%s",
      plot_files,
      ifelse(seq_along(plot_files) < length(plot_files), ",", "")
    )
    c(
      "```{r}",
      "plot_files <- c(",
      plot_file_lines,
      ")",
      "for (plot_file in plot_files) {",
      "  if (file.exists(plot_file)) {",
      "    knitr::include_graphics(plot_file)",
      "  }",
      "}",
      "```"
    )
  }

  output_file_lines <- sprintf(
    "    '%s'%s",
    report_files,
    ifelse(seq_along(report_files) < length(report_files), ",", "")
  )

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
    sprintf("population_table <- read_tsv('%s')", tables$per_population_by_cv),
    sprintf(
      "population_availability_table <- read_tsv('%s')",
      tables$population_availability
    ),
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
    "## Population Availability By Crossvalidation",
    "",
    "```{r}",
    "kable(population_availability_table)",
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
    plot_chunk_lines,
    "",
    "## Outputs",
    "",
    "```{r}",
    "outputs <- data.frame(",
    "  file = c(",
    output_file_lines,
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
missing_input_paths <- input_paths[!file.exists(input_paths)]
if (length(missing_input_paths) > 0) {
  stop(
    sprintf(
      "Metric files missing: %s",
      paste(missing_input_paths, collapse = ", ")
    )
  )
}
input_paths <- normalizePath(input_paths, winslash = "/", mustWork = TRUE)
if (length(input_paths) != EXPECTED_REQUESTED_RUNS || anyDuplicated(input_paths)) {
  stop(
    sprintf(
      "Expected exactly %d unique input metric paths, got %d (%d unique).",
      EXPECTED_REQUESTED_RUNS,
      length(input_paths),
      length(unique(input_paths))
    )
  )
}

order_paths <- normalize_paths(unlist(args$data_metadata))
if (length(order_paths) == 0) {
  stop("No metadata files found for --data.metadata")
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
order_paths <- normalizePath(order_paths, winslash = "/", mustWork = TRUE)
order_map <- build_order_map(order_paths)
metric_artifact_context <- build_metric_artifact_context(input_paths, order_paths)

metrics_rows <- lapply(input_paths, collect_metrics)
per_population_rows <- lapply(input_paths, collect_per_population)
population_availability_rows <- lapply(input_paths, collect_population_availability)
dataset_metadata <- collect_dataset_metadata(input_paths)
metrics_df <- bind_rows(metrics_rows)
metrics_df <- ensure_columns(
  metrics_df,
  list(
    n_cells_total = NA_real_,
    precision_macro = NA_real_,
    recall_macro = NA_real_,
    accuracy = NA_real_,
    mcc = NA_real_,
    pop_freq_corr = NA_real_,
    overlap = NA_real_,
    runtime_seconds = NA_real_,
    scalability_seconds_per_item = NA_real_
  )
)
per_population_df <- bind_rows(per_population_rows)
per_population_df <- ensure_columns(
  per_population_df,
  list(
    population_id = NA_character_,
    population_name = NA_character_,
    population = NA_character_,
    f1 = NA_real_,
    precision = NA_real_,
    recall = NA_real_,
    accuracy = NA_real_,
    tp = NA_real_,
    fp = NA_real_,
    fn = NA_real_,
    tn = NA_real_,
    scaling_rate = NA_real_,
    support = NA_real_
  )
)
population_availability_df <- bind_rows(population_availability_rows)
if (
  nrow(metrics_df) != EXPECTED_REQUESTED_RUNS ||
    any(metrics_df$run_id != "run0") ||
    anyDuplicated(metrics_df$source_path)
) {
  stop(
    paste0(
      "Every input metric path must produce exactly one aggregate run0 row; got ",
      nrow(metrics_df),
      " rows from ",
      length(input_paths),
      " paths."
    )
  )
}
metrics_df <- metrics_df %>%
  left_join(metric_artifact_context, by = "source_path")

variant_lookup <- derive_model_variant_lookup(metrics_df)
if (nrow(variant_lookup) > 0) {
  metrics_df <- metrics_df %>%
    left_join(
      variant_lookup,
      by = c("model_base", "model_params"),
      suffix = c("", ".resolved")
    ) %>%
    mutate(
      model_variant = coalesce(model_variant.resolved, model_variant),
      model = coalesce(model.resolved, model)
    ) %>%
    select(-model_variant.resolved, -model.resolved)

  per_population_df <- per_population_df %>%
    left_join(
      variant_lookup,
      by = c("model_base", "model_params"),
      suffix = c("", ".resolved")
    ) %>%
    mutate(
      model_variant = coalesce(model_variant.resolved, model_variant),
      model = coalesce(model.resolved, model)
    ) %>%
    select(-model_variant.resolved, -model.resolved)

  population_availability_df <- population_availability_df %>%
    left_join(
      variant_lookup,
      by = c("model_base", "model_params"),
      suffix = c("", ".resolved")
    ) %>%
    mutate(
      model_variant = coalesce(model_variant.resolved, model_variant),
      model = coalesce(model.resolved, model)
    ) %>%
    select(-model_variant.resolved, -model.resolved)
}

metrics_df <- metrics_df %>% mutate(model = model_base)
per_population_df <- per_population_df %>% mutate(model = model_base)
population_availability_df <- population_availability_df %>% mutate(model = model_base)

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
  mutate(effective_crossvalidation = sprintf("num-%d", effective_fold))

per_population_df <- per_population_df %>%
  mutate(effective_crossvalidation = sprintf("num-%d", effective_fold))

population_availability_df <- population_availability_df %>%
  mutate(effective_crossvalidation = sprintf("num-%d", effective_fold))

requested_metrics_df <- metrics_df

requested_group_key <- c(
  "dataset",
  "model_base",
  "model_params",
  "stratification",
  "stratification_hash",
  "run_id"
)
requested_key <- c(requested_group_key, "requested_fold")
requested_groups <- metrics_df %>%
  group_by(across(all_of(requested_group_key))) %>%
  summarize(
    requested_count = n(),
    requested_folds = paste(sort(requested_fold), collapse = ","),
    .groups = "drop"
  )
if (
  n_distinct(metrics_df$dataset) != EXPECTED_DATASET_PARAMETERIZATIONS ||
    nrow(requested_groups) != EXPECTED_REQUESTED_GROUPS ||
    any(requested_groups$requested_count != 5L) ||
    any(requested_groups$requested_folds != "1,2,3,4,5") ||
    anyDuplicated(metrics_df[requested_key])
) {
  stop(
    paste0(
      "Requested-run matrix is not the complete 16-dataset x 6-model x ",
      "3-stratification x 5-fold design."
    )
  )
}

effective_key <- c(
  "dataset",
  "model_base",
  "model_params",
  "stratification",
  "stratification_hash",
  "run_id",
  "effective_crossvalidation"
)
assert_alias_values_equal(
  metrics_df,
  effective_key,
  c(
    "dataset_name", "dataset_sub_sampling", "f1_macro", "precision_macro",
    "recall_macro", "balanced_accuracy", "accuracy", "mcc", "pop_freq_corr",
    "overlap", "f1_weighted", "precision_weighted", "recall_weighted", "n_cells",
    "n_cells_total", "n_truth_positive", "n_truth_zero",
    "n_pred_zero_on_truth_positive", "rejection_rate_on_truth_positive",
    "n_pred_zero_on_truth_zero", "n_pred_missing_mapped_to_zero"
  ),
  "aggregate metrics"
)
per_population_alias_sets <- per_population_df %>%
  group_by(across(all_of(c(effective_key, "requested_fold")))) %>%
  summarize(
    population_count = n(),
    population_ids = paste(sort(unique(population_id)), collapse = ","),
    .groups = "drop"
  )
assert_alias_values_equal(
  per_population_alias_sets,
  effective_key,
  c("population_count", "population_ids"),
  "per-population row sets"
)
assert_alias_values_equal(
  per_population_df,
  c(effective_key, "population_id"),
  c(
    "dataset_name", "dataset_sub_sampling", "population_name", "population",
    "f1", "precision", "recall", "accuracy", "tp", "fp", "fn", "tn",
    "support", "nominal_train_count", "training_support", "present_in_training",
    "test_truth_count"
  ),
  "per-population metrics"
)
availability_alias_sets <- population_availability_df %>%
  group_by(across(all_of(c(effective_key, "requested_fold")))) %>%
  summarize(
    population_count = n(),
    population_ids = paste(sort(unique(population_id)), collapse = ","),
    .groups = "drop"
  )
assert_alias_values_equal(
  availability_alias_sets,
  effective_key,
  c("population_count", "population_ids"),
  "population-availability row sets"
)
assert_alias_values_equal(
  population_availability_df,
  c(effective_key, "population_id"),
  c(
    "dataset_name", "dataset_sub_sampling", "population_name",
    "nominal_train_count", "training_support", "present_in_training",
    "test_truth_count"
  ),
  "population availability"
)

deduped_metrics <- metrics_df %>%
  distinct(
    dataset,
    model_base,
    model_params,
    stratification,
    stratification_hash,
    run_id,
    effective_crossvalidation,
    .keep_all = TRUE
  )
wrapped_alias_count <- nrow(metrics_df) - nrow(deduped_metrics)
if (wrapped_alias_count != EXPECTED_WRAPPED_ALIASES) {
  stop(
    sprintf(
      "Expected exactly %d wrapped aliases, found %d.",
      EXPECTED_WRAPPED_ALIASES,
      wrapped_alias_count
    )
  )
}
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
  select(-effective_crossvalidation)

effective_model_counts <- table(metrics_df$model_base)
effective_stratifications <- metrics_df %>%
  distinct(stratification, stratification_hash)
effective_groups <- metrics_df %>%
  group_by(
    dataset,
    dataset_name,
    model_base,
    model_params,
    stratification,
    stratification_hash,
    run_id
  ) %>%
  summarize(effective_count = n(), .groups = "drop")
if (
  nrow(metrics_df) != EXPECTED_EFFECTIVE_RUNS ||
    !setequal(unique(metrics_df$model_base), EXPECTED_MODELS) ||
    !isTRUE(all(effective_model_counts[EXPECTED_MODELS] == EXPECTED_RUNS_PER_MODEL)) ||
    !setequal(unique(metrics_df$stratification), EXPECTED_STRATIFICATIONS) ||
    nrow(effective_stratifications) != length(EXPECTED_STRATIFICATIONS) ||
    anyDuplicated(effective_stratifications$stratification) ||
    anyDuplicated(effective_stratifications$stratification_hash) ||
    nrow(effective_groups) != EXPECTED_REQUESTED_GROUPS ||
    any(
      effective_groups$effective_count !=
        ifelse(effective_groups$dataset_name == "Levine", 2L, 5L)
    )
) {
  stop(
    sprintf(
      paste0(
        "Effective-run validation failed: rows=%d, models={%s}, ",
        "per-model={%s}, stratifications={%s}."
      ),
      nrow(metrics_df),
      paste(sort(unique(metrics_df$model_base)), collapse = ","),
      paste(effective_model_counts, collapse = ","),
      paste(sort(unique(metrics_df$stratification)), collapse = ",")
    )
  )
}

per_population_df <- per_population_df %>%
  distinct(
    dataset,
    model_base,
    model_params,
    stratification,
    stratification_hash,
    run_id,
    effective_crossvalidation,
    population_id,
    .keep_all = TRUE
  ) %>%
  mutate(crossvalidation = effective_crossvalidation) %>%
  select(-effective_crossvalidation)

population_availability_df <- population_availability_df %>%
  distinct(
    dataset,
    model_base,
    model_params,
    stratification,
    stratification_hash,
    run_id,
    effective_crossvalidation,
    population_id,
    .keep_all = TRUE
  ) %>%
  mutate(crossvalidation = effective_crossvalidation) %>%
  select(-effective_crossvalidation) %>%
  group_by(
    dataset,
    model,
    stratification,
    stratification_hash,
    crossvalidation,
    run_id
  ) %>%
  mutate(
    eligible_test_count = sum(test_truth_count[test_truth_count > 0]),
    test_support_fraction = ifelse(
      eligible_test_count > 0,
      test_truth_count / eligible_test_count,
      NA_real_
    )
  ) %>%
  ungroup()

per_population_df <- per_population_df %>%
  left_join(
    population_availability_df %>%
      distinct(
        dataset,
        model,
        stratification,
        stratification_hash,
        crossvalidation,
        run_id,
        eligible_test_count
      ),
    by = c(
      "dataset",
      "model",
      "stratification",
      "stratification_hash",
      "crossvalidation",
      "run_id"
    )
  ) %>%
  mutate(
    test_support_fraction = ifelse(
      !is.na(test_truth_count) & eligible_test_count > 0,
      test_truth_count / eligible_test_count,
      NA_real_
    )
  )

per_population_df <- per_population_df %>%
  left_join(
    metrics_df %>%
      select(
        dataset,
        model,
        model_base,
        model_variant,
        model_params,
        stratification,
        stratification_hash,
        crossvalidation,
        run_id,
        n_cells_total,
        n_cells,
        f1_macro,
        precision_macro,
        recall_macro,
        balanced_accuracy,
        overall_accuracy = accuracy,
        mcc,
        pop_freq_corr,
        overlap,
        runtime_seconds,
        scalability_seconds_per_item
      ),
    by = c(
      "dataset",
      "model",
      "model_base",
      "model_variant",
      "model_params",
      "stratification",
      "stratification_hash",
      "crossvalidation",
      "run_id"
    )
  )

macro_table <- metrics_df %>%
  select(
    dataset,
    model,
    stratification,
    stratification_hash,
    crossvalidation,
    run_id,
    n_cells_total,
    f1_macro,
    precision_macro,
    recall_macro,
    balanced_accuracy,
    n_cells
  ) %>%
  arrange(dataset, model, crossvalidation, run_id)

per_population_table <- per_population_df %>%
  mutate(
    rare_bucket = vapply(test_support_fraction, bucket_support_fraction, character(1))
  ) %>%
  select(
    dataset,
    model,
    stratification,
    stratification_hash,
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
    nominal_train_count,
    training_support,
    present_in_training,
    test_truth_count,
    eligible_test_count,
    test_support_fraction,
    rare_bucket,
    n_cells,
    source_path,
    tp,
    fp,
    fn,
    tn
  ) %>%
  arrange(dataset, model, crossvalidation, run_id, population)

population_availability_table <- population_availability_df %>%
  select(
    dataset,
    model,
    stratification,
    stratification_hash,
    crossvalidation,
    run_id,
    population_id,
    population_name,
    nominal_train_count,
    training_support,
    present_in_training,
    test_truth_count,
    eligible_test_count,
    test_support_fraction
  ) %>%
  arrange(dataset, model, crossvalidation, run_id, population_id)

run_metrics_table <- metrics_df %>%
  mutate(
    throughput_events_per_sec = ifelse(
      runtime_seconds > 0,
      n_cells / runtime_seconds,
      NA_real_
    )
  ) %>%
  select(
    dataset,
    model,
    stratification,
    stratification_hash,
    crossvalidation,
    run_id,
    n_cells,
    n_cells_total,
    n_truth_positive,
    n_truth_zero,
    n_pred_zero_on_truth_positive,
    rejection_rate_on_truth_positive,
    n_pred_zero_on_truth_zero,
    n_pred_missing_mapped_to_zero,
    f1_macro,
    precision_macro,
    recall_macro,
    balanced_accuracy,
    f1_weighted,
    precision_weighted,
    recall_weighted,
    accuracy,
    mcc,
    pop_freq_corr,
    overlap,
    runtime_seconds,
    scalability_seconds_per_item,
    throughput_events_per_sec,
    source_path
  ) %>%
  arrange(dataset, model, crossvalidation, run_id)

per_population_summary <- per_population_df %>%
  group_by(
    dataset,
    model,
    stratification,
    population_id,
    population_name,
    population
  ) %>%
  summarize(
    median_f1 = median(f1, na.rm = TRUE),
    mean_f1 = mean(f1, na.rm = TRUE),
    median_precision = median(precision, na.rm = TRUE),
    mean_precision = mean(precision, na.rm = TRUE),
    median_recall = median(recall, na.rm = TRUE),
    mean_recall = mean(recall, na.rm = TRUE),
    median_support = median(support, na.rm = TRUE),
    n_runs = n(),
    .groups = "drop"
  )

per_population_stability <- per_population_df %>%
  group_by(
    dataset,
    model,
    stratification,
    population_id,
    population_name,
    population
  ) %>%
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
    stratification,
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
  filter(!is.na(test_truth_count)) %>%
  mutate(
    rare_bucket = vapply(test_support_fraction, bucket_support_fraction, character(1))
  ) %>%
  group_by(
    dataset,
    model,
    stratification,
    crossvalidation,
    run_id,
    present_in_training,
    rare_bucket
  ) %>%
  summarize(
    n_populations = n(),
    median_f1 = median(f1, na.rm = TRUE),
    median_precision = median(precision, na.rm = TRUE),
    median_recall = median(recall, na.rm = TRUE),
    nominal_train_count = sum(nominal_train_count),
    training_support = sum(training_support),
    test_truth_count = sum(test_truth_count),
    eligible_test_count = ifelse(
      all(is.na(eligible_test_count)),
      NA_real_,
      max(eligible_test_count, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  mutate(
    test_support_fraction = ifelse(
      eligible_test_count > 0,
      test_truth_count / eligible_test_count,
      NA_real_
    )
  )

dataset_context_table <- per_population_df %>%
  group_by(dataset, model, stratification, crossvalidation, run_id) %>%
  summarize(
    n_cells = ifelse(
      all(is.na(n_cells)),
      NA_real_,
      max(n_cells, na.rm = TRUE)
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
  group_by(dataset, model, stratification, crossvalidation, run_id) %>%
  slice_max(order_by = fnr, n = 5, with_ties = FALSE) %>%
  ungroup()

dominant_fpr_table <- per_population_confusion %>%
  group_by(dataset, model, stratification, crossvalidation, run_id) %>%
  slice_max(order_by = fpr, n = 5, with_ties = FALSE) %>%
  ungroup()

macro_summary <- metrics_df %>%
  group_by(model, stratification) %>%
  summarize(
    median_f1_macro = median(f1_macro, na.rm = TRUE),
    mean_f1_macro = mean(f1_macro, na.rm = TRUE),
    median_precision_macro = median(precision_macro, na.rm = TRUE),
    mean_precision_macro = mean(precision_macro, na.rm = TRUE),
    median_recall_macro = median(recall_macro, na.rm = TRUE),
    mean_recall_macro = mean(recall_macro, na.rm = TRUE),
    median_balanced_accuracy = median(balanced_accuracy, na.rm = TRUE),
    mean_balanced_accuracy = mean(balanced_accuracy, na.rm = TRUE),
    n_runs = n(),
    .groups = "drop"
  )

weighted_summary <- metrics_df %>%
  group_by(model, stratification) %>%
  summarize(
    median_f1_weighted = median(f1_weighted, na.rm = TRUE),
    mean_f1_weighted = mean(f1_weighted, na.rm = TRUE),
    median_precision_weighted = median(precision_weighted, na.rm = TRUE),
    mean_precision_weighted = mean(precision_weighted, na.rm = TRUE),
    median_recall_weighted = median(recall_weighted, na.rm = TRUE),
    mean_recall_weighted = mean(recall_weighted, na.rm = TRUE),
    n_runs = n(),
    .groups = "drop"
  )

macro_table_path <- file.path(args$output_dir, "f1_macro_by_crossvalidation.tsv")
per_population_table_path <- file.path(
  args$output_dir,
  "per_population_by_crossvalidation.tsv"
)
macro_summary_path <- file.path(args$output_dir, "f1_macro_summary_by_model.tsv")
weighted_summary_path <- file.path(args$output_dir, "f1_weighted_summary_by_model.tsv")
run_metrics_path <- file.path(args$output_dir, "run_metrics.tsv")
population_availability_path <- file.path(
  args$output_dir,
  "population_availability_by_crossvalidation.tsv"
)
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
dataset_metadata_path <- file.path(args$output_dir, "dataset_metadata.json")

write_table(macro_table, macro_table_path)
write_table(per_population_table, per_population_table_path)
write_table(macro_summary, macro_summary_path)
write_table(weighted_summary, weighted_summary_path)
write_table(run_metrics_table, run_metrics_path)
write_table(population_availability_table, population_availability_path)
write_table(per_population_summary, per_population_summary_path)
write_table(per_population_stability, per_population_stability_path)
write_table(per_population_confusion, per_population_confusion_path)
write_table(rare_population_table, rare_population_path)
write_table(dataset_context_table, dataset_context_path)
write_table(dominant_fnr_table, dominant_fnr_path)
write_table(dominant_fpr_table, dominant_fpr_path)
jsonlite::write_json(
  dataset_metadata,
  dataset_metadata_path,
  auto_unbox = TRUE,
  pretty = TRUE
)

finalization <- build_finalization_outputs(
  requested_metrics_df,
  metrics_df,
  args$output_dir
)

plot_dir <- file.path(args$output_dir, "plots")
reset_plot_dir(plot_dir)

reviewer_dir <- file.path(plot_dir, "reviewer")
reviewer_error <- NULL
tryCatch(
  invoke_reviewer_figures(args$output_dir, reviewer_dir),
  error = function(error) {
    reviewer_error <<- error
  }
)
unlink(finalization$paths$collector_validation)
if (!is.null(reviewer_error)) {
  stop(reviewer_error)
}
reviewer_files <- list.files(
  reviewer_dir,
  recursive = TRUE,
  full.names = FALSE,
  all.files = TRUE,
  no.. = TRUE
)
reviewer_artifacts <- file.path("plots", "reviewer", reviewer_files)
plot_paths <- as.list(
  reviewer_artifacts[grepl("\\.png$", reviewer_artifacts, ignore.case = TRUE)]
)

table_paths <- list(
  macro_by_cv = basename(macro_table_path),
  per_population_by_cv = basename(per_population_table_path),
  macro_summary = basename(macro_summary_path),
  weighted_summary = basename(weighted_summary_path),
  run_metrics = basename(run_metrics_path),
  population_availability = basename(population_availability_path),
  per_population_summary = basename(per_population_summary_path),
  per_population_stability = basename(per_population_stability_path),
  per_population_confusion = basename(per_population_confusion_path),
  rare_population = basename(rare_population_path),
  dataset_context = basename(dataset_context_path),
  dominant_fnr = basename(dominant_fnr_path),
  dominant_fpr = basename(dominant_fpr_path),
  accepted_manifest = basename(finalization$paths$accepted_manifest),
  run_status = basename(finalization$paths$run_status),
  model_wall_times = basename(finalization$paths$model_wall_times),
  collector_validation = basename(finalization$paths$collector_validation)
)

artifact_paths <- unique(c(
  basename(unlist(table_paths, use.names = FALSE)),
  basename(dataset_metadata_path),
  basename(unlist(finalization$paths, use.names = FALSE)),
  reviewer_artifacts
))
render_report(
  args$output_dir,
  plot_paths,
  table_paths,
  args$name,
  artifact_paths = artifact_paths
)

old_wd <- getwd()
write_collector_validation(
  finalization$validation,
  finalization$paths$collector_validation
)
tar_error <- NULL
tar_status <- tryCatch(
  {
    setwd(args$output_dir)
    utils::tar(
      "metric_plots.tar.gz",
      files = artifact_paths,
      compression = "gzip"
    )
  },
  error = function(error) {
    tar_error <<- error
    NA_integer_
  },
  finally = setwd(old_wd)
)
archive_path <- file.path(args$output_dir, "metric_plots.tar.gz")
if (!is.null(tar_error) || is.na(tar_status) || tar_status != 0L || !file.exists(archive_path)) {
  unlink(c(finalization$paths$collector_validation, archive_path))
  stop(
    sprintf(
      "Failed to create complete metric plot archive%s",
      if (is.null(tar_error)) "" else paste0(": ", tar_error$message)
    )
  )
}
archive_members <- utils::untar(archive_path, list = TRUE)
if (!setequal(archive_members, artifact_paths)) {
  unlink(c(finalization$paths$collector_validation, archive_path))
  stop("metric_plots.tar.gz members do not match the validated artifact set")
}

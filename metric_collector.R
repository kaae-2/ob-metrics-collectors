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
      "--data.order",
      dest = "data_order",
      type = "character",
      nargs = "+",
      required = FALSE,
      help = "Order JSON file(s) from data_import"
    )
    parser$add_argument(
      "--output_dir",
      type = "character",
      required = TRUE,
      help = "Output directory"
    )
    parser$add_argument("--name", type = "character", required = TRUE)
    parser$add_argument(
      "--existing_report_dir",
      type = "character",
      required = FALSE,
      help = "Reuse an existing metrics_report directory and regenerate plots/report"
    )

    parsed <- parser$parse_args()
    parsed$metrics_scores <- parsed$metrics_scores %||% character()
    parsed$data_order <- parsed$data_order %||% character()
    parsed$existing_report_dir <- parsed$existing_report_dir %||% ""

    if ((is.null(parsed$existing_report_dir) || parsed$existing_report_dir == "")) {
      if (length(parsed$metrics_scores) == 0) {
        stop("--metrics.scores is required unless --existing_report_dir is set")
      }
      if (length(parsed$data_order) == 0) {
        stop("--data.order is required unless --existing_report_dir is set")
      }
    }
    return(parsed)
  }

  args <- commandArgs(trailingOnly = TRUE)
  parsed <- list(
    metrics_scores = character(),
    data_order = character(),
    output_dir = NULL,
    name = NULL,
    existing_report_dir = ""
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
      } else if (key == "existing_report_dir") {
        parsed$existing_report_dir <- value
      }
    }
    i <- i + 1
  }

  if (is.null(parsed$existing_report_dir) || parsed$existing_report_dir == "") {
    if (length(parsed$metrics_scores) == 0) {
      stop("--metrics.scores is required unless --existing_report_dir is set")
    }
    if (length(parsed$data_order) == 0) {
      stop("--data.order is required unless --existing_report_dir is set")
    }
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
  params <- jsonlite::fromJSON(params_path, simplifyVector = FALSE)
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
  list(
    dataset = dataset,
    model = model,
    model_base = model_base,
    model_params = model_params,
    model_variant = model_variant,
    crossvalidation = crossvalidation
  )
}

compute_weighted_f1 <- function(per_population) {
  if (is.null(per_population) || length(per_population) == 0) {
    return(list(weighted_f1 = NA_real_, total_n = NA_real_))
  }
  pop_entries <- lapply(per_population, function(entry) {
    f1 <- as.numeric(entry$f1 %||% NA_real_)
    n_val <- entry$n_cells %||% entry$n %||% entry$support %||% NA_real_
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
    n_cells <- run$n_cells %||% run$n %||% weighted$total_n
    tibble(
      dataset = lineage$dataset,
      model = lineage$model,
      model_base = lineage$model_base,
      model_variant = lineage$model_variant,
      model_params = lineage$model_params,
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
      n_cells = as.numeric(n_cells),
      source_path = path
    )
  })
  bind_rows(rows)
}

collect_per_population <- function(path) {
  empty_per_population <- function() {
    tibble(
      dataset = character(),
      model = character(),
      model_base = character(),
      model_variant = character(),
      model_params = character(),
      crossvalidation = character(),
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
      source_path = character()
    )
  }
  payload <- read_metrics_json(path)
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
          model = lineage$model,
          model_base = lineage$model_base,
          model_variant = lineage$model_variant,
          model_params = lineage$model_params,
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
        paste0(dataset_label, " (", platform, ", M:", n_markers, ", P:", n_populations, ")"),
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

generate_figure1_heatmap_boxplot <- function(metrics_df, dataset_meta, plot_dir) {
  df <- metrics_df %>%
    filter(!is.na(f1_macro)) %>%
    left_join(dataset_meta, by = "dataset") %>%
    filter(!is_sub_sampling)

  if (nrow(df) == 0) {
    return(NULL)
  }

  df <- df %>%
    mutate(platform = coalesce(platform, "Unknown"))

  df_no_random <- df %>%
    filter(!str_detect(model, regex("random", ignore_case = TRUE)))

  dataset_order <- df_no_random %>%
    group_by(platform, display_name) %>%
    summarize(global_mean = mean(f1_macro, na.rm = TRUE), .groups = "drop") %>%
    arrange(platform, global_mean) %>%
    pull(display_name)

  model_order <- df %>%
    group_by(model) %>%
    summarize(global_mean = mean(f1_macro, na.rm = TRUE), .groups = "drop") %>%
    arrange(global_mean) %>%
    pull(model)

  if (length(dataset_order) == 0 || length(model_order) == 0) {
    return(NULL)
  }

  df <- df %>%
    mutate(
      model = factor(model, levels = model_order),
      display_name = factor(display_name, levels = dataset_order)
    )
  df_no_random <- df_no_random %>%
    mutate(display_name = factor(display_name, levels = dataset_order))

  heatmap_data <- df %>%
    group_by(model, display_name, platform) %>%
    summarize(mean_f1 = mean(f1_macro, na.rm = TRUE), .groups = "drop") %>%
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

  p_heatmap <- ggplot(heatmap_data, aes(x = display_name, y = model, fill = mean_f1)) +
    geom_tile(color = "white", linewidth = 0.2) +
    geom_text(aes(label = label_text), size = 1.8, color = "black") +
    facet_grid(. ~ platform, scales = "free_x", space = "free_x") +
    scale_fill_viridis_c(option = "viridis", limits = c(0, 1), guide = "none") +
    scale_x_discrete(position = "top", expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(x = "", y = NULL) +
    theme_base +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 0, vjust = 0),
      panel.spacing = unit(6, "pt"),
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text = element_text(face = "bold", size = 8),
      panel.grid = element_blank(),
      plot.margin = margin(b = 0, r = 3, unit = "pt")
    )

  p_right <- ggplot(df, aes(x = f1_macro, y = model)) +
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
    facet_grid(. ~ platform, scales = "free_x", space = "free_x") +
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
CCCCC##
"

  final_plot <- wrap_plots(A = p_heatmap, B = p_right, C = p_bottom, design = design)

  n_tools <- length(unique(df$model))
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
      geom_boxplot(outlier.size = 0.2, linewidth = 0.35, alpha = 0.7, width = 0.75, fatten = 1) +
      scale_fill_manual(values = tool_colors, name = "Method") +
      scale_color_manual(values = tool_colors, name = "Method") +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
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
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
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
      geom_boxplot(outlier.size = 0.05, linewidth = 0.25, alpha = 0.7, width = 0.7, fatten = 0.8) +
      scale_fill_manual(values = tool_colors) +
      scale_color_manual(values = tool_colors) +
      scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
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
      geom_boxplot(outlier.size = 0.1, linewidth = 0.25, alpha = 0.7, width = 0.65, fatten = 1) +
      (if (n_pop >= 6) facet_grid(. ~ abundance_class, scales = "free_x", space = "free_x") else NULL) +
      scale_fill_manual(values = tool_colors) +
      scale_color_manual(values = tool_colors) +
      scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
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

link_or_copy <- function(source, destination) {
  if (!file.exists(source) && !dir.exists(source)) {
    stop(sprintf("Source path does not exist: %s", source))
  }
  link_target <- suppressWarnings(Sys.readlink(destination))
  has_link <- !is.na(link_target) && link_target != ""
  if (file.exists(destination) || dir.exists(destination) || has_link) {
    unlink(destination, recursive = TRUE, force = TRUE)
  }
  ok <- suppressWarnings(file.symlink(source, destination))
  if (!isTRUE(ok)) {
    if (dir.exists(source)) {
      dir.create(destination, recursive = TRUE, showWarnings = FALSE)
      files <- list.files(source, full.names = TRUE, all.files = TRUE, no.. = TRUE)
      if (length(files) > 0) {
        copied <- file.copy(files, destination, recursive = TRUE, overwrite = TRUE)
        if (!all(copied)) {
          stop(sprintf("Failed to copy directory contents from %s", source))
        }
      }
    } else {
      copied <- file.copy(source, destination, overwrite = TRUE)
      if (!isTRUE(copied)) {
        stop(sprintf("Failed to copy file from %s", source))
      }
    }
  }
}

default_table_paths <- function() {
  list(
    macro_by_cv = "f1_macro_by_crossvalidation.tsv",
    weighted_by_cv = "f1_weighted_by_crossvalidation.tsv",
    macro_summary = "f1_macro_summary_by_model.tsv",
    weighted_summary = "f1_weighted_summary_by_model.tsv",
    run_metrics = "run_metrics.tsv",
    per_population_summary = "per_population_summary.tsv",
    per_population_stability = "per_population_stability.tsv",
    per_population_confusion = "per_population_confusion.tsv",
    rare_population = "rare_population_buckets.tsv",
    dataset_context = "dataset_context.tsv",
    dominant_fnr = "dominant_errors_fnr.tsv",
    dominant_fpr = "dominant_errors_fpr.tsv"
  )
}

build_plot_paths_from_dir <- function(plot_dir) {
  files <- list.files(plot_dir, pattern = "\\.png$", full.names = FALSE)
  files <- sort(files)
  as.list(file.path("plots", files))
}

reset_plot_dir <- function(plot_dir) {
  if (dir.exists(plot_dir)) {
    unlink(plot_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
}

run_existing_report_mode <- function(args) {
  source_dir <- normalizePath(args$existing_report_dir, winslash = "/", mustWork = TRUE)
  table_paths <- default_table_paths()
  required_files <- unique(c(unlist(table_paths, use.names = FALSE), "dataset_metadata.json"))
  missing <- required_files[!file.exists(file.path(source_dir, required_files))]
  if (length(missing) > 0) {
    stop(
      sprintf(
        "Existing report directory is missing required files: %s",
        paste(missing, collapse = ", ")
      )
    )
  }

  for (file_name in required_files) {
    source_path <- file.path(source_dir, file_name)
    destination_path <- file.path(args$output_dir, file_name)
    if (normalizePath(dirname(destination_path), winslash = "/", mustWork = TRUE) == source_dir && basename(destination_path) == basename(source_path)) {
      next
    }
    link_or_copy(source_path, destination_path)
  }

  plot_dir <- file.path(args$output_dir, "plots")
  reset_plot_dir(plot_dir)

  metrics_df <- readr::read_tsv(file.path(args$output_dir, table_paths$macro_by_cv), show_col_types = FALSE)
  per_population_confusion <- readr::read_tsv(file.path(args$output_dir, table_paths$per_population_confusion), show_col_types = FALSE)
  run_metrics_table <- readr::read_tsv(file.path(args$output_dir, table_paths$run_metrics), show_col_types = FALSE)
  dataset_metadata <- jsonlite::fromJSON(file.path(args$output_dir, "dataset_metadata.json"), simplifyVector = FALSE)

  generate_plots2_suite(
    plot_dir = plot_dir,
    metrics_df = metrics_df,
    per_population_confusion = per_population_confusion,
    run_metrics_table = run_metrics_table,
    dataset_metadata = dataset_metadata
  )

  plot_paths <- build_plot_paths_from_dir(plot_dir)
  performance_note <- sprintf("Reused existing collector tables from %s", source_dir)
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
}

render_report <- function(output_dir, plot_paths, tables, name, performance_note = NULL) {
  report_path <- file.path(output_dir, "metrics_report.Rmd")
  output_html <- file.path(output_dir, "metrics_report.html")
  plot_files <- unique(unlist(plot_paths, use.names = FALSE))
  plot_files <- plot_files[!is.na(plot_files) & plot_files != ""]
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
        tables$dominant_fpr
      )
    )
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
    plot_chunk_lines,
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
    sprintf("    '%s'", tables$dominant_fpr),
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

if (!is.null(args$existing_report_dir) && args$existing_report_dir != "") {
  run_existing_report_mode(args)
  quit(save = "no", status = 0)
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
dataset_metadata <- collect_dataset_metadata(input_paths)
metrics_df <- bind_rows(metrics_rows)
metrics_df <- ensure_columns(
  metrics_df,
  list(
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
if (nrow(metrics_df) == 0) {
  stop("No metrics rows parsed from inputs")
}

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
  distinct(dataset, model_base, model_params, run_id, effective_crossvalidation, .keep_all = TRUE)
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
    model_base,
    model_params,
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
        model_base,
        model_variant,
        model_params,
        crossvalidation,
        run_id,
        n_cells,
        f1_macro,
        precision_macro,
        recall_macro,
        overall_accuracy = accuracy,
        mcc,
        pop_freq_corr,
        overlap,
        runtime_seconds,
        scalability_seconds_per_item
      ),
    by = c("dataset", "model", "model_base", "model_variant", "model_params", "crossvalidation", "run_id")
  )

macro_table <- metrics_df %>%
  select(
    dataset,
    model,
    crossvalidation,
    run_id,
    f1_macro,
    n_cells
  ) %>%
  arrange(dataset, model, crossvalidation, run_id)

per_population_table <- per_population_df %>%
  mutate(support_fraction = ifelse(n_cells > 0, support / n_cells, NA_real_)) %>%
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
    n_cells,
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
      n_cells / runtime_seconds,
      NA_real_
    )
  ) %>%
  select(
    dataset,
    model,
    crossvalidation,
    run_id,
    n_cells,
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
  group_by(
    dataset,
    model,
    population_id,
    population_name,
    population
  ) %>%
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
  group_by(
    dataset,
    model,
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
  mutate(support_fraction = ifelse(n_cells > 0, support / n_cells, NA_real_)) %>%
  mutate(rare_bucket = vapply(support_fraction, bucket_support_fraction, character(1))) %>%
  group_by(dataset, model, crossvalidation, run_id, rare_bucket) %>%
  summarize(
    n_populations = n(),
    median_f1 = median(f1, na.rm = TRUE),
    median_precision = median(precision, na.rm = TRUE),
    median_recall = median(recall, na.rm = TRUE),
    total_support = sum(support, na.rm = TRUE),
    n_cells = ifelse(
      all(is.na(n_cells)),
      NA_real_,
      max(n_cells, na.rm = TRUE)
    ),
    support_share = ifelse(
      !all(is.na(n_cells)) && max(n_cells, na.rm = TRUE) > 0,
      sum(support, na.rm = TRUE) / max(n_cells, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  )

dataset_context_table <- per_population_df %>%
  group_by(dataset, model, crossvalidation, run_id) %>%
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
dataset_metadata_path <- file.path(args$output_dir, "dataset_metadata.json")

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
jsonlite::write_json(
  dataset_metadata,
  dataset_metadata_path,
  auto_unbox = TRUE,
  pretty = TRUE
)

plot_dir <- file.path(args$output_dir, "plots")
reset_plot_dir(plot_dir)

plot_paths <- generate_plots2_suite(
  plot_dir = plot_dir,
  metrics_df = metrics_df,
  per_population_confusion = per_population_confusion,
  run_metrics_table = run_metrics_table,
  dataset_metadata = dataset_metadata
)
if (length(plot_paths) == 0) {
  plot_paths <- list()
}

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
  dominant_fpr = basename(dominant_fpr_path)
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

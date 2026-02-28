# Collectors Repository

## What this module does

This module aggregates `*.flow_metrics.json.gz` outputs into benchmark reports.

- Main script: `metric_collector.R`
- Local runner: `run_metric_collector.sh`
- Outputs include:
  - `metrics_report.html`
  - `metric_plots.tar.gz`
  - intermediate TSV summary tables
  - extended figure suite (Figure1/Figure2/Figure3 variants)

Parameterized model runs (for example multiple GateMeClass settings) are split
by resolving parameter hashes from each run's `analysis/.../parameters.json`, so
report tables can separate variants without exposing raw parameter hashes.

The extended plot suite is generated from collector outputs only
(`f1_macro_by_crossvalidation.tsv`, `per_population_confusion.tsv`,
`run_metrics.tsv`, `dataset_metadata.json`). Dataset display names default to
the metadata abbreviation (`expected_abbreviation`) when available.

Generated plot files now use `fig*` prefixes and include only the rewritten
figure suite (legacy scatter/boxplot-only files are no longer produced).

## Run locally

```bash
bash collectors/run_metric_collector.sh
```

You can also pass explicit inputs:

```bash
bash collectors/run_metric_collector.sh --metrics.scores out --data.order out/data/data_import/.../data_import.order.json.gz --output_dir collectors/out/data/metric_collectors/default --name metrics_report
```

To rebuild plots/report from an already generated collector output (without
re-parsing raw `*.flow_metrics.json.gz`), run:

```bash
bash collectors/run_metric_collector.sh --existing_report_dir out/metric_collectors/metrics_report --output_dir out/metric_collectors/metrics_report_retest --name metrics_report_retest
```

This mode symlinks the source TSV/JSON tables into `--output_dir` and then
regenerates plots and `metric_plots.tar.gz`.

`run_metric_collector.sh` resolves relative `--output_dir` paths under the
collectors repo directory, so `--output_dir out/...` writes to
`collectors/out/...` by default. Use an absolute path if you intentionally
want output elsewhere.

## Run as part of benchmark

Configured under `metric_collectors` in `benchmark/Clustering_conda.yml`; run via:

```bash
just benchmark
```

## What `run_metric_collector.sh` needs

- `Rscript` available in `PATH`
- R dependencies used by `metric_collector.R` (for example `argparse`,
  `jsonlite`, `dplyr`, `tidyr`, `ggplot2`, `readr`, `rmarkdown`)
- Metrics JSON outputs and corresponding `data.order` files
- Writable output directory

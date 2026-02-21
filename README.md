# Collectors Repository

## What this module does

This module aggregates `*.flow_metrics.json.gz` outputs into benchmark reports.

- Main script: `metric_collector.R`
- Local runner: `run_metric_collector.sh`
- Outputs include:
  - `metrics_report.html`
  - `metric_plots.tar.gz`
  - intermediate TSV summary tables

Parameterized model runs (for example multiple GateMeClass settings) are split
by resolving parameter hashes from each run's `analysis/.../parameters.json`, so
report tables can separate variants without exposing raw parameter hashes.

## Run locally

```bash
bash collectors/run_metric_collector.sh
```

You can also pass explicit inputs:

```bash
bash collectors/run_metric_collector.sh --metrics.scores out --data.order out/data/data_import/.../data_import.order.json.gz --output_dir collectors/out/data/metric_collectors/default --name metrics_report
```

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

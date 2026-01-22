#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found in PATH" >&2
  exit 1
fi

metrics_input="${script_dir}/../out"
output_dir="${script_dir}/out/data/metric_collectors/default"
name="metrics_report"

Rscript "${script_dir}/metric_collector.R" \
  --metrics.scores "${metrics_input}" \
  --output_dir "${output_dir}" \
  --name "${name}"

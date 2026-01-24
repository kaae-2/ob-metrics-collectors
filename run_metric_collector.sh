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
metrics_scores=()
data_orders=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --metrics.scores)
      metrics_scores+=("$2")
      shift 2
      ;;
    --data.order)
      data_orders+=("$2")
      shift 2
      ;;
    --output_dir)
      output_dir="$2"
      shift 2
      ;;
    --name)
      name="$2"
      shift 2
      ;;
    *)
      echo "Unknown argument: $1" >&2
      exit 1
      ;;
  esac
done

if [ ${#metrics_scores[@]} -eq 0 ]; then
  metrics_scores=("${metrics_input}")
fi

if [ ${#data_orders[@]} -eq 0 ]; then
  shopt -s nullglob
  for path in "${metrics_input}/data/data_import"/*/data_import.order.json.gz; do
    data_orders+=("$path")
  done
  shopt -u nullglob
fi

if [ ${#data_orders[@]} -eq 0 ]; then
  echo "No --data.order inputs found or provided." >&2
  exit 1
fi

if [ ${#metrics_scores[@]} -eq 0 ]; then
  echo "No --metrics.scores inputs found or provided." >&2
  exit 1
fi

args=(
  --output_dir "${output_dir}"
  --name "${name}"
)

for score in "${metrics_scores[@]}"; do
  args+=(--metrics.scores "$score")
done

for order_path in "${data_orders[@]}"; do
  args+=(--data.order "$order_path")
done

Rscript "${script_dir}/metric_collector.R" "${args[@]}"

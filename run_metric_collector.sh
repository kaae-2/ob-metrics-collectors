#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "${script_dir}/.." && pwd)"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found in PATH" >&2
  exit 1
fi

metrics_input="${script_dir}/../out"
output_dir="${script_dir}/out/data/metric_collectors/default"
name="metrics_report"
metrics_scores=()
data_orders=()
existing_report_dir=""
reuse_existing="false"

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
    --existing_report_dir)
      existing_report_dir="$2"
      reuse_existing="true"
      shift 2
      ;;
    *)
      echo "Unknown argument: $1" >&2
      exit 1
      ;;
  esac
done

if [[ "${output_dir}" != /* ]]; then
  output_dir="${script_dir}/${output_dir#./}"
fi

if [[ "${existing_report_dir}" != "" && "${existing_report_dir}" != /* ]]; then
  if [ -d "${repo_root}/${existing_report_dir}" ]; then
    existing_report_dir="${repo_root}/${existing_report_dir}"
  else
    existing_report_dir="${script_dir}/${existing_report_dir#./}"
  fi
fi

if [[ "${reuse_existing}" != "true" ]] && [ ${#metrics_scores[@]} -eq 0 ] && [ ${#data_orders[@]} -eq 0 ]; then
  auto_report_dir="${metrics_input}/metric_collectors/metrics_report"
  if [ -d "${auto_report_dir}" ]; then
    existing_report_dir="${auto_report_dir}"
    reuse_existing="true"
    echo "Detected existing collector output; running in reuse mode:" >&2
    echo "  ${existing_report_dir}" >&2
  fi
fi

args=(
  --output_dir "${output_dir}"
  --name "${name}"
)

if [[ "${reuse_existing}" == "true" ]]; then
  if [ -z "${existing_report_dir}" ] || [ ! -d "${existing_report_dir}" ]; then
    echo "--existing_report_dir must point to an existing directory." >&2
    exit 1
  fi

  mkdir -p "${output_dir}"
  args+=(--existing_report_dir "${existing_report_dir}")
else
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

  for score in "${metrics_scores[@]}"; do
    args+=(--metrics.scores "$score")
  done

  for order_path in "${data_orders[@]}"; do
    args+=(--data.order "$order_path")
  done
fi

Rscript "${script_dir}/metric_collector.R" "${args[@]}"

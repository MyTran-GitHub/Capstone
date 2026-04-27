#!/usr/bin/env bash
# Minimal helper script to run the pipeline from a bash shell.
# Usage: ./scripts/run_pipeline.sh [--dry-run]

set -euo pipefail

DRY_RUN=0
if [[ "${1-}" == "--dry-run" ]]; then
  DRY_RUN=1
fi

echo "Ensure you have activated the conda environment:"
echo "  conda activate r-spatial"

if [[ $DRY_RUN -eq 1 ]]; then
  echo "Dry-run mode: printing commands without executing"
  echo "Rscript main.R --dry-run"
  exit 0
fi

echo "Running main pipeline"
Rscript main.R

echo "Pipeline finished"

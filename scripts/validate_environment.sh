#!/usr/bin/env bash
# Quick environment validation for Capstone project
set -euo pipefail

echo "Checking basic tools..."
command -v Rscript >/dev/null 2>&1 || { echo "Rscript not found"; exit 1; }
command -v python3 >/dev/null 2>&1 || { echo "python3 not found"; exit 1; }

echo "R version:"; Rscript -e 'cat(R.version.string, "\n")'
echo "Python version:"; python3 --version

echo "Checking a few R packages..."
Rscript - <<'RS'
pkgs <- c('yaml','optparse')
missing <- pkgs[!pkgs %in% installed.packages()[,'Package']]
if(length(missing)>0){
  message('Missing R packages: ', paste(missing, collapse=', '))
  quit(status=1)
} else message('R packages OK')
RS

echo "Checking a few Python packages (pip)..."
for pkg in numpy pandas rasterio; do
  if ! python3 -c "import importlib,sys; importlib.import_module('$pkg')" >/dev/null 2>&1; then
    echo "Missing Python package: $pkg"
    exit 1
  fi
done

echo "Environment check passed (basic). For full reproducibility, run Rscript main.R --dry-run after verifying data files." 

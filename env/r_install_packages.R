#!/usr/bin/env Rscript

cran_repo <- "https://cloud.r-project.org"

required_pkgs <- c(
  "EValue", "FNN", "Hmisc", "bootstrap", "pBrackets", "gridExtra",
  "forestplot", "pROC", "exactextractr", "uwot", "viridis", "xtable",
  "data.table", "dplyr", "tidyr", "ggplot2", "readr", "tibble",
  "tidyverse", "sf", "terra", "raster", "sp", "tigris", "fst",
  "jsonlite", "magrittr", "mltools", "ncdf4", "pbapply", "pbmcapply",
  "patchwork", "RANN", "sandwich", "scales", "dataverse"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) == 0) {
  cat("All required R packages are already installed.\n")
  quit(status = 0)
}

cat("Installing missing R packages:\n")
cat(paste0(" - ", missing_pkgs), sep = "\n")

install.packages(missing_pkgs, repos = cran_repo)

still_missing <- missing_pkgs[!vapply(missing_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing) > 0) {
  cat("\nThe following packages are still missing after install attempt:\n")
  cat(paste0(" - ", still_missing), sep = "\n")
  quit(status = 1)
}

cat("\nR package bootstrap complete.\n")

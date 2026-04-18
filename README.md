# Low-intensity fires mitigate the risk of catastrophic wildfires in California's forests

<img src="https://img.shields.io/badge/Study%20Status-Publication%20Available-green.svg" alt="Study Status: Publication Available"> 

This repository contains all code and configuration to reproduce the analyses in the paper:

> Wu, X., Sverdrup, E., Mastrandrea, M.D., Wara, M.W., and Wager, S., 2023. Low-intensity fires mitigate the risk of high-intensity wildfires in California's forests. Science Advances, 9(45), p.eadi4123. [DOI: 10.1126/sciadv.adi4123](https://www.science.org/doi/10.1126/sciadv.adi4123)

## Overview
We use a synthetic control approach to analyze twenty years of satellite-based fire activity data across 124,186 km² of forests in California, providing evidence that low-intensity fires substantially reduce the risk of future high-intensity fires.

## Repository Structure
- `data_processing/` — Scripts to process and harmonize raw geospatial/tabular data for analysis
- `balancing/` — Covariate balancing synthetic control (CBPS) and related utilities
- `analysis/` — Outcome analysis, regression, and figure/table generation
- `Embeddings/scripts/` — Embedding generation, K-selection, and integration with CBPS (Python & R)
- `diagnostics/` — Diagnostics scripts and results for pipeline validation
- `config/` — Central configuration files (edit `config/config.yaml` for all parameters)
- `env/` — Environment and dependency files for R and Python
- `infra/` — Infrastructure utilities (e.g., HPC scripts)
- `data/` — Raw, processed, and output data directories (with README files for structure)
- `figures/`, `tables/`, `docs/` — Project documentation, figures, and tables
- `tests/` — Unit and integration tests for pipeline validation

## Getting Started
1. **Clone the repository**
2. **Install dependencies**
   - R: `env/environment-capstone-r-spatial.yml`
   - Python: `env/requirements-extra.txt`
3. **Edit configuration**
   - Set all parameters in `config/config.yaml`
4. **Run the pipeline**
   - Use `main_orchestration.R` for end-to-end execution

## Pipeline Workflow
1. Data preparation (`data_processing/`)
2. Embedding extraction and K-selection (`Embeddings/scripts/`)
3. CBPS with selected controls (`balancing/`, `Embeddings/scripts/`)
4. Outcome analysis and figure/table generation (`analysis/`, `figures/`, `tables/`)
5. Diagnostics and reporting (`diagnostics/`)

## Data Sources
| Data    |  Source      |  Spatial Resolution  | Time Resolution | Time Periods |
| ------- | ------------ | ------------------- | --------------- | ------------ |
| Active Fires   | [MODIS FIRMS](https://firms.modaps.eosdis.nasa.gov/download/) | 1 km² | daily | 2000– |
| Meteorological | [Daymet](https://daymet.ornl.gov/) | 1 km² | daily | 2000– |
| Disturbance Agents | [Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/CVTNLY) | 30 m² | yearly | 2000– |
| Fractional Vegetation Cover | [Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/KMBYYM) | 30 m² | yearly | 2000– |
| Vegetation Class | [CAL FIRE](https://map.dfg.ca.gov/metadata/ds1327.html) | 30 m² | one time | 1990–2014 |
| Topography | [GMTED](https://www.earthenv.org/topography) | 1 km² | one time | 2010 |
| Fire Severity | [MTBS](https://www.mtbs.gov/project-overview) | 30 m² | yearly | 2000– |
| Fire Severity | [RAVG](https://burnseverity.cr.usgs.gov/products/ravg) | 30 m² | yearly | 2012– |
| Prescribed fires | [Federal FACTS](https://www.sciencedirect.com/science/article/pii/S0301479721021459) | unspecified | yearly | 2000– |
| Prescribed fires | [CAL FIRE](https://map.dfg.ca.gov/metadata/ds0397.html) | unspecified | yearly | 2000– |

All data needed to evaluate the conclusions in the paper are present in the paper and/or the Supplementary Materials and Online Repository. Those interested in the original data can contact the corresponding author.

## Reproducibility & Best Practices
- All scripts use logging/message for output (no print/cat)
- All scripts have module-level and function-level docstrings
- All configuration is centralized in `config/config.yaml`
- All data directories contain README files describing structure
- `.gitignore` excludes large/intermediate data and outputs

## Usage Example
```sh
Rscript Embeddings/scripts/04_run_cbps_with_selected_controls.R <year> <selected_units_csv> <output_prefix> <train_start> <train_end> <test_start> <test_end> [flags]
```
Or run the full pipeline with:
```sh
Rscript main_orchestration.R
```

## Citing This Work
If you use this code or data, please cite:
- Wu, X., Sverdrup, E., Mastrandrea, M.D., Wara, M.W., and Wager, S., 2023. Low-intensity fires mitigate the risk of high-intensity wildfires in California's forests. Science Advances, 9(45), p.eadi4123. DOI: [10.1126/sciadv.adi4123](https://www.science.org/doi/10.1126/sciadv.adi4123)

## Contact
- xw2892@cumc.columbia.edu
- erikcs@stanford.edu
- swager@stanford.edu

## License & Terms of Use
See LICENSE file. Authors/funders retain copyright.

---

For detailed documentation, see the `docs/` directory and in-script docstrings.

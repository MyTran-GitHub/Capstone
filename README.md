# Low-intensity fires mitigate the risk of catastrophic wildfires in California's forests

This repository contains the analysis code for:

Wu, X., Sverdrup, E., Mastrandrea, M.D., Wara, M.W., and Wager, S., 2023. Low-intensity fires mitigate the risk of high-intensity wildfires in California's forests. Science Advances, 9(45), p.eadi4123.

Purpose of this update: improve reproducibility and quick-start instructions so others can replicate the analysis locally or on HPC.

**Quick start (recommended)**
- Clone the repository:

   git clone https://github.com/<OWNER>/<REPO>.git
   cd Capstone

- Create the conda environment (R + Python packages):

   conda env create -f env/environment-capstone-r-spatial.yml
   conda activate r-spatial

- (Optional) Install additional Python-only packages:

   pip install -r env/requirements-extra.txt

- Install any R packages not available via conda using the helper, e.g. `env/r_install_packages.R`.

- Configure the run: edit `config/config.yaml` for paths and parameters.

- Dry-run to validate commands without executing:

   Rscript main.R --dry-run

- Run full pipeline (after validating config):

   Rscript main.R

**Repository layout (short)**
- `data_processing/` — data harmonization and processing scripts
- `Embeddings/` — embedding extraction and K-selection (Python + R glue scripts)
- `balancing/` — CBPS utilities and diagnostics
- `analysis/` — regression and outcome analysis scripts
- `config/` — central configuration files (edit `config/config.yaml`)
- `env/` — conda/environments and helper install scripts
- `diagnostics/`, `figures/`, `docs/`, `scripts/` — supporting materials

Files to inspect for replication:
- `main.R` — pipeline orchestration and entrypoint
- `config/config.yaml` — parameter and path configuration
- `env/environment-capstone-r-spatial.yml` — conda environment (R + Python)

Reproducibility checklist
- Use the provided conda environment to get matching R/Python binaries and major packages.
- Ensure access to raw data sources (external downloads may require credentials) — see `data/README.md` inside the `data` directory.
- Use `Rscript main.R --dry-run` to validate file paths and commands before running heavy processing.
- For HPC runs, adapt `scripts/run_pipeline.sh` or `infra/` utilities included in the repo.

Running a targeted step
- To run embedding extraction for one year (example):

   python3 Embeddings/scripts/02_extract_embeddings_single_year.py 2019

- To run CBPS for a given selection CSV:

   Rscript Embeddings/scripts/04_run_cbps_with_selected_controls.R 2019 Embeddings/data/k_selection/2019/selection_decision.json outprefix 2000 2010 2011 2015

Notes on configuration
- `config/config.yaml` centralizes parameters such as `year`, `train_start`, `train_end`, data paths, and `optimal_k`. Edit the file to point to local data paths if you keep data outside the repo.

Development & code-style recommendations
- Use `lintr` and `styler` for R code formatting and static checks.
- Use `black` and `ruff` or `flake8` for Python scripts in `Embeddings/`.
- Add docstrings to newly added helper functions; the repo already documents major scripts.

Support and contact
- For questions about data access or the original analysis, contact the authors listed in the paper.

License
- See the LICENSE file in this repository.

---

If you'd like, I can now: add a small `scripts/run_pipeline.sh` (included), make `main.R` accept CLI args and a `--dry-run` mode (I will), or run a quick lint pass on a small subset of files. Which would you prefer next?

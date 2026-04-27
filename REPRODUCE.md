REPRODUCE.md

Quick reproduction steps

1. Clone the repository and change directory:

   git clone https://github.com/MyTran-GitHub/Capstone.git
   cd Capstone

2. Create the conda environment (R + Python):

   conda env create -f env/environment-capstone-r-spatial.yml
   conda activate r-spatial

3. (Optional) install extra Python packages:

   pip install -r env/requirements-extra.txt

4. Validate environment (quick check):

   bash scripts/validate_environment.sh

5. Edit configuration if needed: update `config/config.yaml` to point to local data paths.

6. Dry-run to ensure commands and paths resolve:

   Rscript main.R --dry-run

7. Run the full pipeline (expect long runtime and large intermediate data):

   Rscript main.R

Notes
- Data downloads: some raw data must be downloaded manually or require credentials; see `data/README.md`.
- For HPC runs adapt `scripts/run_pipeline.sh` or the `infra/` folder scripts.
- For reproducible snapshots use `git tag` and record the conda environment export:

   conda env export -n r-spatial > env/environment-capstone-r-spatial.lock.yml

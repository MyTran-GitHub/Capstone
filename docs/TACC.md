# TACC Quickstart Guide

This guide helps you use TACC (e.g., Stampede2) to run heavier workflows.

## Overview
- Login vs compute: Login nodes for editing and submissions; compute nodes run jobs via Slurm.
- Filesystems: `/home` (small, backed up), `/work` (project space), `/scratch` (node-local, ephemeral).
- Scheduler: Slurm (`sbatch`, `squeue`, `sacct`).

## Accounts and SSH
1. Request a TACC account and project allocation; enroll Duo MFA.
2. Create SSH keys locally:
```bash
ssh-keygen -t ed25519 -C "your_email" -f ~/.ssh/tacc_ed25519
cat ~/.ssh/tacc_ed25519.pub
```
3. Add the public key to the TACC portal; test connection:
```bash
ssh -i ~/.ssh/tacc_ed25519 username@stampede2.tacc.utexas.edu
```

## Upload Code and Data
Use `rsync` for resumable transfers:
```bash
# Code
rsync -av --exclude .venv /Users/macbook/Downloads/Capstone/Capstone \
  username@stampede2.tacc.utexas.edu:/work/PROJECT/capstone/

# NetCDFs
rsync -av /path/to/netcdfs/ \
  username@stampede2.tacc.utexas.edu:/work/PROJECT/netcdfs/
```
Directory layout suggestion on TACC:
```
/work/PROJECT/
  capstone/            # repo
  netcdfs/             # inputs
  results/             # outputs
```

## Slurm Batch Script
Create `run_daymet.slurm` on TACC:
```bash
#!/bin/bash
#SBATCH -J daymet
#SBATCH -A PROJECT
#SBATCH -p skx-normal
#SBATCH -N 1
#SBATCH -n 48
#SBATCH -t 02:00:00
#SBATCH -o logs/%x-%j.out

module load python/3.10
source /work/PROJECT/capstone/.venv/bin/activate

python scripts/run_daymet_extract.py \
  --in /work/PROJECT/netcdfs/ \
  --out /work/PROJECT/results/daymet/
```
Submit and monitor:
```bash
sbatch run_daymet.slurm
squeue -u $USER
sacct -j <JOBID> --format=JobID,State,Elapsed
```

## Retrieve Results
```bash
rsync -av username@stampede2.tacc.utexas.edu:/work/PROJECT/results/ \
  /Users/macbook/Downloads/Capstone/results/
```

## Tips
- Compress NetCDF (`zlib=True`, `complevel=4`) to save space before upload.
- Prefer `/work` for persistent project data; avoid large files in `/home`.
- For large transfers, add `--partial --progress` to `rsync`.

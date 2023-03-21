#!/bin/bash
#SBATCH --job-name=16pred_2100
#SBATCH --output="./Study1/Data/study1_out.txt"
#SBATCH --ntasks=1
#SBATCH --time=30:00:00
#SBATCH --mem-per-cpu=20
#SBATCH --mail-user=j.a.nevedemevergnies@uu.nl
#SBATCH --mail-type=ALL
#SBATCH --array=1801-2100%25
srun Rscript "./Study1/Study1_execute.R" $SLURM_ARRAY_TASK_ID 16
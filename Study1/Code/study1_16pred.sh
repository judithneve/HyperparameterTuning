#!/bin/bash
#SBATCH --job-name=16pred_s1
#SBATCH --output="./Study1/Output/study1_out.txt"
#SBATCH --ntasks=1
#SBATCH --time=30:00:00
#SBATCH --mem-per-cpu=5
#SBATCH --array=1-3000
srun Rscript "./Study1/Code/Study1_execute.R" $SLURM_ARRAY_TASK_ID 16
#!/bin/bash
#SBATCH --job-name=s3
#SBATCH --output="./Study3/Output/study3_out.txt"
#SBATCH --ntasks=1
#SBATCH --time=06:00:00
#SBATCH --mem-per-cpu=5
#SBATCH --array=1-600
srun Rscript "./Study3/Code/Study3_execute.R" $SLURM_ARRAY_TASK_ID
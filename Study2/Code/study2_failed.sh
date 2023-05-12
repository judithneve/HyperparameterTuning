#!/bin/bash
#SBATCH --job-name=s2
#SBATCH --output="./Study2/Output/study2_out.txt"
#SBATCH --ntasks=1
#SBATCH --time=04:00:00
#SBATCH --mem-per-cpu=5
#SBATCH --array=788
srun Rscript "./Study2/Code/Study2_execute.R" $SLURM_ARRAY_TASK_ID
#!/bin/bash
#SBATCH --job-name=s2
#SBATCH --output="./Study2/Output/study2_out.txt"
#SBATCH --ntasks=1
#SBATCH --time=03:30:00
#SBATCH --mem-per-cpu=5
#SBATCH --array=1-6000
srun Rscript "./Study2/Code/Study2_execute.R" $SLURM_ARRAY_TASK_ID
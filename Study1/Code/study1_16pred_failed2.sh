#!/bin/bash
#SBATCH --job-name=16pred_s1
#SBATCH --output="./Study1/Output/study1_out.txt"
#SBATCH --ntasks=1
#SBATCH --time=35:00:00
#SBATCH --mem-per-cpu=5
#SBATCH --array=214,274,328,376,412,448,688,700,730,742,784,802,862
srun Rscript "./Study1/Code/Study1_execute.R" $SLURM_ARRAY_TASK_ID 16
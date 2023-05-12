#!/bin/bash
#SBATCH --job-name=s3_failed
#SBATCH --output="./Study3/Output/study3_out.txt"
#SBATCH --ntasks=1
#SBATCH --time=07:00:00
#SBATCH --mem-per-cpu=5
#SBATCH --array=80,296,380,476,524,560,584
srun Rscript "./Study3/Code/Study3_execute.R" $SLURM_ARRAY_TASK_ID
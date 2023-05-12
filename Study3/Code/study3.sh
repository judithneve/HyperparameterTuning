#!/bin/bash
#SBATCH --job-name=s3_10each
#SBATCH --output="./Study3/Data/study3_out.txt"
#SBATCH --ntasks=1
#SBATCH --time=06:00:00
#SBATCH --mem-per-cpu=20
#SBATCH --mail-user=j.a.nevedemevergnies@uu.nl
#SBATCH --mail-type=ALL
#SBATCH --array=1-12
srun Rscript "./Study3/Study3_execute.R" $SLURM_ARRAY_TASK_ID
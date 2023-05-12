#!/bin/bash
#SBATCH --job-name=s3_300each
#SBATCH --output="./Study3/Data/study3_out.txt"
#SBATCH --ntasks=1
#SBATCH --time=07:00:00
#SBATCH --mem-per-cpu=5
#SBATCH --mail-user=j.a.nevedemevergnies@uu.nl
#SBATCH --mail-type=ALL
#SBATCH --array=80,296,380,476,524,560,584
srun Rscript "./Study3/Study3_execute.R" $SLURM_ARRAY_TASK_ID
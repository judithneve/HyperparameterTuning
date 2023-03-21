#!/bin/bash
#SBATCH --job-name=s2_50each
#SBATCH --output="./Study2/Data/study2_out.txt"
#SBATCH --ntasks=1
#SBATCH --time=03:30:00
#SBATCH --mem-per-cpu=20
#SBATCH --mail-user=j.a.nevedemevergnies@uu.nl
#SBATCH --mail-type=ALL
#SBATCH --array=301-600
srun Rscript "./Study2/Study2_execute.R" $SLURM_ARRAY_TASK_ID
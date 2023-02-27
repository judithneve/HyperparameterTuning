#!/bin/bash

#SBATCH --job-name=jneve_study1
#SBATCH --output="./Study1/Data/sim/study1_out.txt"

#SBATCH --ntasks=1
#SBATCH --time=06:00:00
#SBATCH --mem-per-cpu=20
#SBATCH --mail-user=j.a.nevedemevergnies@uu.nl
#SBATCH --mail-type=ALL

#SBATCH --array=1-60

srun Rscript "./Study1/Study1_execute.R" $SLURM_ARRAY_TASK_ID 8
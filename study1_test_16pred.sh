#!/bin/bash

#SBATCH --job-name=jneve_study1
#SBATCH --output="./Study1/Data/sim/study1_out.txt"

#SBATCH --ntasks=1
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=20
#SBATCH --mail-user=j.a.nevedemevergnies@uu.nl
#SBATCH --mail-type=ALL

#SBATCH --array=2,8,10

srun Rscript "./Study1/Study1_onescenario_execute_test.R" $SLURM_ARRAY_TASK_ID
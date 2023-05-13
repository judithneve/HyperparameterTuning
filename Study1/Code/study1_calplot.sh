#!/bin/bash
#SBATCH --job-name=s1calplot
#SBATCH --output="./Study1/Output/calplot_coords.txt"
#SBATCH --ntasks=1
#SBATCH --time=06:00:00
#SBATCH --mem-per-cpu=5
#SBATCH --array=1-5
srun Rscript "./Study1/Code/CalPlotCoords.R" $SLURM_ARRAY_TASK_ID 1 8
srun Rscript "./Study1/Code/CalPlotCoords.R" $SLURM_ARRAY_TASK_ID 3 8
srun Rscript "./Study1/Code/CalPlotCoords.R" $SLURM_ARRAY_TASK_ID 5 8
srun Rscript "./Study1/Code/CalPlotCoords.R" $SLURM_ARRAY_TASK_ID 1 16
srun Rscript "./Study1/Code/CalPlotCoords.R" $SLURM_ARRAY_TASK_ID 3 16
srun Rscript "./Study1/Code/CalPlotCoords.R" $SLURM_ARRAY_TASK_ID 5 16

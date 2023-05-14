# Research archive for: Performance of clinical prediction models versus computational time for random forest hyperparameter tuning procedures: A simulation study

This study was a three-part simulation study conducted in part on personal computers and in part on high-performance computers. We make clear which parts were run where in the "What to run?" section of this readme.

This study was granted ethical approval by the FETC. It is filed under number 22-1808. All data used was simulated and presents no privacy concerns. The simulated data is present in this repository.

This project is under an MIT license. For more detail, see the LICENSE file.

## Permission and access

The archive is available in full on GitHub: https://github.com/judithneve/HyperparameterTuning. It will remain publicly available.

I, Judith Neve, am the sole person responsible for it. Please contact me at j.a.nevedemevergnies@students.uu.nl if you have any questions or remarks.

## What to run?

1. Personal computer. Run DGM/DGM_scenarios_betas.Rmd. This will reproduce the specifications for the data simulation scenarios. Estimated runtime: 4 hours.

2. Using slurm on a high performance computer, run Study1/Code/study1_8pred.sh, Study1/Code/study1_16pred.sh, Study2/Code/study2.sh, Study3/Code/study3.sh. The order does not matter. This will reproduce all data in the Data/preds/ and Data/perfs/ subfolders of Study1, Study2, Study3. Estimated runtime: 30 hours minimum (assuming everything starts running immediately).

3. Using slurm, check which jobs timed out, modify the array argument in Study1/Code/study1_16pred_failed.sh, Study2/Code/study2_failed.sh, Study3/Code/study3_failed.sh accordingly, and run these files. Repeat until there are no timed out jobs. If Study1 16-predictor jobs fail again, modify Study1/Code/study1_16pred_failed2.sh instead.

4. Using slurm on a high performance computer, run Study1/Code/study1_calplot.sh. This will reproduce all data in Study1/Data/coords/. Estimated runtime: 6 hours.

5. Personal computer. Run Study2/Code/CalPlotCoords.R and Study3/Code/CalPlotCoords.R. This will reproduce all data in Study2/Data/coords/ and Study3/Data/coords/. Estimated runtime: 6 hours.

6. Personal computer. Run Study1/Code/GenerateCalibrationPlot.R, Study2/Code/GenerateCalibrationPlot.R, Study3/Code/GenerateCalibrationPlot.R. This will reproduce all pdf files in Study1/Output/, Study2/Output/, Study3/Output/. Estimated runtime: 20 minutes.

7. Personal computer. Run Study1/Code/Results.Rmd, Study2/Code/Results.Rmd, Study3/Code/Results.Rmd. This will reproduce the Output/results.RData file in Study1, Study2, Study3. Estimated runtime: 15 minutes.

8. Personal computer. Knit Report/Report.Rmd and Report/SupplementaryMaterials.Rmd. This will reproduce the Report and Supplementary Materials pdf files, including all tables and figures using the files created in step 7. Any rendering of Report/Report.Rmd and Report/SupplementaryMaterials.Rmd with different data would produce different tables and figures. Estimated runtime: 10 minutes.

9. Personal computer. If desiring the title page, run Report/CombineTitlePageReport.R. This will add the title page in front of the report as in RM_Thesis_JudithNeve_0070661.pdf. Estimated runtime: 5 seconds.

10. Personal computer. The app can be recreated by running App/MakeAppData.Rmd, then App/app.R. Estimated runtime: 5 minutes.

Notes:

- Shell files can be duplicated and modified to run smaller batches of data generation by modifying the array argument.

- Exact file dependencies are detailed in the folder structure (later in this document).

- Data will not replicate exactly due to runtime being a variable, which depends heavily on tasks being computed in parallel and varies on its own. Additionally, data may differ between operating systems. The high-performance computer used in this project contains two types of processors (Xeon Gold and Xeon E5) which responded differently to seeds.

- R version and package information (step 1): DGM/DGM_scenarios_betas_sessionInfo.txt

- R version and package information (steps 2-4): HPC_requirements.txt

- R version and package information (steps 5-9): analysis_requirements.txt

## Folder structure

HyperparameterTuning/

|-- .gitignore                 - Specifies files and directories to be ignored by Git

|-- LICENSE                    - License file for the project

|-- README.md                  - Readme file rendering the html file containing information about the project

|-- README.html                - Readme file containing information about the project

|-- HyperparameterTuning.RProj - RStudio project file for the project

|-- HPC_requirements.txt       - Text file detailing R and R package versions used in data simulation

|-- analysis_requirements.txt  - Text file detailing R and R package versions used in data analysis

|-- App/

    |-- MakeAppData.Rmd - R Markdown file for creating appdata.RData | creates: appdata.RData | calls: all files in Study1/Data/perfs/, Study2/Data/perfs/, Study3/Data/perfs/

    |-- app.R           - R file containing the main code for the app | calls: appdata.RData

    |-- appdata.RData   - R data file including all data and functions necessary for the app

    |-- rsconnect/

        |-- shinyapps.io/

            |-- judithneve/

                |-- HyperparameterTuning.dcf - Configuration file for deploying the app on shinyapps.io

|-- DGM/

    |-- Code/

        |-- DGMFunctions.R - Helper functions and main functions for estimating coefficients used in the data generation

    |-- Data/

        |-- betas.RData            - R data file containing the matrix of coefficients for each scenario

        |-- betas_validation.RData - R data file containing information on the performance of the selected coefficients

        |-- scenarios.RData        - R data file containing the full information on scenarios (including sample size)

    |-- DGM_scenarios_betas.Rmd             - R script generating scenarios and coefficients for each scenario | creates: all files in DGM/Data/, DGM_scenarios_betas_sessionInfo.txt | calls: Code/DGMFunctions.R, RFunctions/DataSimFunctions.R

    |-- DGM_scenarios_betas_sessionInfo.txt - Text file detailing R and R package versions used in the scenario and coefficient computation

|-- RFunctions/

    |-- DataSimFunctions.R            - Helper functions and main functions used to simulate datasets

    |-- PerformanceMetricsFunctions.R - Helper functions and main functions used to assess the predictive performance of a model

    |-- TuningFunctions.R             - Helper functions and main functions used to tune random forests

|-- Report/

    |-- CombineTitlePageReport.R                    - R code to combine the title page and the report in one pdf | creates: RM_Thesis_JudithNeve_0070661.pdf | calls: TitlePage.pdf, Report.pdf

    |-- RM_Thesis_JudithNeve_0070661.pdf            - pdf file containing the report and the title page

    |-- RM_Thesis_JudithNeve_0070661_COMPRESSED.pdf - pdf file containing the report and the title page, compressed to under 2Mb

    |-- Report.Rmd                                  - R code compiling the report | creates: Report.pdf | calls: WileyNJD-AMA.bst, WileyNJD-v2.cls, bibliography.bib, DGM/Data/scenarios.RData, DGM/Data/betas_validation.RData, Study1/Output/results.RData, Study2/Output/results.RData, Study3/Output/results.RData, Study1/Output/plot_ef5.pdf, Study2/Output/plot_ef5.pdf, Study3/Output/plot_ef5.pdf

    |-- Report.pdf                                  - Rendered version of the report

    |-- SupplementaryMaterials.Rmd                  - R code compiling the supplementary materials | creates: SupplementaryMaterials.pdf | calls: Study1/Output/results.RData, Study2/Output/results.RData, Study3/Output/results.RData, Study1/Output/plot_ef1.pdf, Study2/Output/plot_ef1.pdf, Study3/Output/plot_ef1.pdf, Study1/Output/plot_ef3.pdf, Study2/Output/plot_ef3.pdf, Study3/Output/plot_ef3.pdf

    |-- SupplementaryMaterials.pdf                  - Rendered version of the supplementary materials

    |-- TitlePage.pdf                               - Rendered version of the title page

    |-- TitlePage.tex                               - LaTeX code generating the title page | creates: TitlePage.pdf

    |-- WileyNJD-AMA.bst                            - Formatting file for the report

    |-- WileyNJD-v2.cls                             - Formatting file for the report

    |-- bibliography.bib                            - Bibliography file for the report

|-- Study1/

    |-- Code/

        |-- CalPlotCoords.R           - R code generating coordinates used for the Study 1 calibration plots   runs through slurm | creates: all files in Study1/Data/coords/ | calls: all files in Study1/Data/preds/

        |-- GenerateCalibrationPlot.R - R code generating calibration plots for Study 1 | creates: all pdf files in Study1/Output/ | calls: all files in Study1/Data/coords/

        |-- Study1_execute.R          - R code generating observations for Study 1   runs through slurm | creates: all files in Study1/Data/perfs/, all files in Study1/Data/preds/ | calls: all files in RFunctions/, DGM/Data/scenarios.RData, DGM/Data/betas.RData

        |-- Study1_results.Rmd        - R code generating summary statistics and plots for Study 1 | creates: Study1/Output/results.RData | calls: all files in Study1/Data/perfs/

        |-- study1_calplot.sh         - Shell file to obtain all calibration plot coordinates in Study 1   runs through slurm | calls: CalPlotCoords.R

        |-- study1_16pred.sh          - Shell file to obtain all observations with 16 predictors in Study 1   runs through slurm | calls: Study1_execute.R

        |-- study1_16pred_failed.sh   - Shell file to obtain all failed jobs with 16 predictors in Study 1   runs through slurm | calls: Study1_execute.R

        |-- study1_16pred_failed2.sh  - Shell file to obtain jobs which failed twice with 16 predictors in Study 1   runs through slurm | calls: Study1_execute.R

        |-- study1_8pred.sh           - Shell file to obtain all observations with 8 predictors in Study 1   runs through slurm | calls: Study1_execute.R

    |-- Data/

        |-- coords/ - Folder containing all Study 1 RDS files used to generate the calibration plots

        |-- perfs/  - Folder containing all Study 1 RDS files containing the performance of tuning procedures on one dataset

        |-- preds/  - Folder containing all Study 1 RDS files containing predicted risk and the observed values

    |-- Output/

        |-- plot_ef1.pdf   - Calibration plots for all scenarios with an event fraction of 0.1 in Study 1

        |-- plot_ef3.pdf   - Calibration plots for all scenarios with an event fraction of 0.3 in Study 1

        |-- plot_ef5.pdf   - Calibration plots for all scenarios with an event fraction of 0.5 in Study 1

        |-- results.RData  - R Data file containing tables and figures presented in the final report

|-- Study2/

    |-- Code/

        |-- CalPlotCoords.R           - R code generating coordinates used for the Study 2 calibration plots | creates: all files in Study2/Data/coords/ | calls: all files in Study2/Data/preds/

        |-- GenerateCalibrationPlot.R - R code generating calibration plots for Study 2 | creates: all pdf files in Study2/Output/ | calls: all files in Study2/Data/coords/

        |-- Study2_execute.R          - R code generating observations for Study 2   runs through slurm | creates: all files in Study2/Data/perfs/, all files in Study2/Data/preds/ | calls: all files in RFunctions/, DGM/Data/scenarios.RData, DGM/Data/betas.RData

        |-- Study2_results.Rmd        - R code generating summary statistics and plots for Study 2 | creates: Study2/Output/results.RData | calls: all files in Study2/Data/perfs/

        |-- study2.sh                 - Shell file to obtain all observations in Study 2   runs through slurm | calls: Study2_execute.R

        |-- study2_failed.sh          - Shell file to obtain all failed observations in Study 2   runs through slurm | calls: Study2_execute.R

    |-- Data/

        |-- coords/ - Folder containing all Study 2 RDS files used to generate the calibration plots

        |-- perfs/  - Folder containing all Study 2 RDS files containing the performance of tuning procedures on one dataset

        |-- preds/  - Folder containing all Study 2 RDS files containing predicted risk and the observed values

    |-- Output/

        |-- plot_ef1.pdf   - Calibration plots for all scenarios with an event fraction of 0.1 in Study 2

        |-- plot_ef3.pdf   - Calibration plots for all scenarios with an event fraction of 0.3 in Study 2

        |-- plot_ef5.pdf   - Calibration plots for all scenarios with an event fraction of 0.5 in Study 2

        |-- results.RData  - R Data file containing tables and figures presented in the final report

|-- Study3/

    |-- Code/

        |-- CalPlotCoords.R           - R code generating coordinates used for the Study 3 calibration plots | creates: all files in Study3/Data/coords/ | calls: all files in Study3/Data/preds/

        |-- GenerateCalibrationPlot.R - R code generating calibration plots for Study 3 | creates: all pdf files in Study3/Output/ | calls: all files in Study3/Data/coords/

        |-- Study3_execute.R          - R code generating observations for Study 3   runs through slurm | creates: all files in Study3/Data/perfs/, all files in Study3/Data/preds/ | calls: all files in RFunctions/, DGM/Data/scenarios.RData, DGM/Data/betas.RData

        |-- Study3_results.Rmd        - R code generating summary statistics and plots for Study 3 | creates: Study3/Output/results.RData | calls: all files in Study3/Data/perfs/

        |-- study3.sh                 - Shell file to obtain all observations in Study 3   runs through slurm | calls: Study3_execute.R

        |-- study3_failed.sh          - Shell file to obtain all failed observations in Study 3   runs through slurm | calls: Study3_execute.R

    |-- Data/

        |-- coords/ - Folder containing all Study 3 RDS files used to generate the calibration plots

        |-- perfs/  - Folder containing all Study 3 RDS files containing the performance of tuning procedures on one dataset

        |-- preds/  - Folder containing all Study 3 RDS files containing predicted risk and the observed values

    |-- Output/

        |-- plot_ef1.pdf   - Calibration plots for all scenarios with an event fraction of 0.1 in Study 3

        |-- plot_ef3.pdf   - Calibration plots for all scenarios with an event fraction of 0.3 in Study 3

        |-- plot_ef5.pdf   - Calibration plots for all scenarios with an event fraction of 0.5 in Study 3

        |-- results.RData  - R Data file containing tables and figures presented in the final report

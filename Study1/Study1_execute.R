start <- Sys.time()

##### Setup #####
library(MASS)    # for mvrnorm
library(dplyr)   # data wrangling
library(tidyr)   # pivot functions
library(ranger)  # random forests
library(caret)   # tuning
library(pROC)    # AUC calculations
source("DataSimFunctions.R")
source("TuningFunctions.R")
source("PerformanceMetricsFunctions.R")

job_args <- 1

job_id <- job_args[1] %>% as.numeric()
start_seed <- job_id*100
set.seed(start_seed)

# load in scenario + coef
load("DGM_data/scenarios.RData")
load("DGM_data/betas.RData")
# randomise the order in which scenarios are run
scenario_permutation <- sample(1:nrow(scenarios), nrow(scenarios))
scenarios <- scenarios[scenario_permutation,]
# generate a seed for each dataset
dataset_seeds <- sample(1:1e9, 12)
# set size of the validation sets
large_sample <- 1e5
# make the hyperparameter combinations: each row is a combination - are we tuning the HP or not
hyperparameter_combinations <- expand.grid(
  mtry = TRUE,
  min.node.size = TRUE,
  sample.fraction = c(TRUE, FALSE),
  replace = c(TRUE, FALSE),
  splitrule = c(TRUE, FALSE)
)
# add a row: not tuning anything
hyperparameter_combinations <- rbind(rep(FALSE, ncol(hyperparameter_combinations)),
                                     hyperparameter_combinations)
# set up the CV function
ctrl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = deviance,
  classProbs = TRUE
)

nrow_output <- nrow(scenarios)*nrow(hyperparameter_combinations)
out <- data.frame(
  start_seed     = rep(start_seed, nrow_output),
  n              = rep(NA, nrow_output),
  n_prop         = rep(NA, nrow_output),
  p              = rep(NA, nrow_output),
  EF             = rep(NA, nrow_output),
  intercept      = rep(NA, nrow_output),
  beta           = rep(NA, nrow_output),
  gamma          = rep(NA, nrow_output),
  dataset        = rep(NA, nrow_output),
  dataset_seed   = rep(NA, nrow_output),
  hp_combination = rep(NA, nrow_output),
  fold_seed      = rep(NA, nrow_output),
  time           = rep(NA, nrow_output),
  AUC            = rep(NA, nrow_output),
  CalSlope       = rep(NA, nrow_output),
  CalIntercept   = rep(NA, nrow_output),
  BrierScore     = rep(NA, nrow_output),
  LogLoss        = rep(NA, nrow_output)
)
row_out <- 0

# start the simulation
for (scenario in 1:nrow(scenarios)) {
  print(scenarios[scenario,])
  ##### Generate data #####
  sample_size_temp    <- scenarios$n[scenario]
  n_prop_temp         <- scenarios$prop_sample_size[scenario]
  n_pred_temp         <- scenarios$n_pred[scenario]
  event_fraction_temp <- scenarios$event_fraction[scenario]
  betas <- betas_matrix[(betas_matrix[,"n_predictor"] == n_pred_temp) & (betas_matrix[,"prevalence"] == event_fraction_temp),3:5]
  
  set.seed(dataset_seeds[scenario])
  # generate a dataset for each scenario
  # generate a validation set for each scenario
  dat_temp     <- simulate_data(sample_size_temp, n_pred_temp, event_fraction_temp, betas)
  val_dat_temp <- simulate_data(large_sample,     n_pred_temp, event_fraction_temp, betas)
  
  ##### Tuning setup #####
  
  # randomise the order in which hyperparameter combinations are run
  hyperparameter_permutation <- sample(1:nrow(hyperparameter_combinations), nrow(hyperparameter_combinations))
  hyperparameter_comb_permuted <- hyperparameter_combinations[hyperparameter_permutation,]
  
  ##### Tuning procedure #####
  
  # for each combination, make a grid & tune
  for (combination in 1:nrow(hyperparameter_comb_permuted)) {
    print(hyperparameter_comb_permuted[combination,])
    row_out <- row_out + 1
    out[row_out,c("n", "n_prop", "p", "EF")] <- c(sample_size_temp, n_prop_temp, n_pred_temp, event_fraction_temp)
    out[row_out,c("intercept", "beta", "gamma")] <- betas
    out[row_out,"dataset"] <- start_seed + scenario
    out[row_out,"dataset_seed"] <- dataset_seeds[scenario]
    out[row_out,"hp_combination"] <- paste(colnames(hyperparameter_comb_permuted)[as.logical(hyperparameter_comb_permuted[combination,])], collapse = " + ")
    
    if (hyperparameter_comb_permuted[combination,"mtry"]) {
      mtry_candidates <- 1:n_pred_temp
    } else {
      mtry_candidates <- sqrt(n_pred_temp)
    }
    if (hyperparameter_comb_permuted[combination,"min.node.size"]) {
      min.node.size_candidates <- 1:10
    } else {
      min.node.size_candidates <- 1
    }
    if (hyperparameter_comb_permuted[combination,"splitrule"]) {
      splitrule_candidates <- c("gini", "hellinger", "extratrees")
    } else {
      splitrule_candidates <- "gini"
    }
    if (hyperparameter_comb_permuted[combination,"sample.fraction"]) {
      sample.fraction_candidates <- seq(0.1, 1, by = 0.1)
    } else {
      sample.fraction_candidates <- 1
    }
    if (hyperparameter_comb_permuted[combination,"replace"]) {
      replace_candidates <- c(TRUE, FALSE)
    } else {
      replace_candidates <- TRUE
    }
    
    tunegrid <- expand.grid(
      mtry = mtry_candidates,
      min.node.size = min.node.size_candidates,
      splitrule = splitrule_candidates
    )
    cat("\nNumber of rows of tunegrid:", nrow(tunegrid))
    
    start_tune <- Sys.time()
    mod <- tune_hyperparameters(dat_temp, tunegrid, sample.fraction_candidates, replace_candidates, ctrl)
    end_tune   <- Sys.time()
    print(end_tune)
    tuning_time <- difftime(end_tune, start_tune, units = "mins")
    
    ##### Assess performance #####
    
    # fit best model on the validation set
    pred <- predict(mod$model, newdata = val_dat_temp[,-ncol(val_dat_temp)], type = "prob")$pos
    # evaluate the performance based on my metrics
    perf <- performance(pred, val_dat_temp$Y)
    
    out[row_out,"fold_seed"] <- mod$fold_seed
    out[row_out,"time"] <- tuning_time
    out[row_out,14:18] <- perf
  }
}

##### Save #####

filename <- paste0("Study1/Data/sim/study1_allscenarios_run", job_id, ".rds")
saveRDS(out, file = filename)

end <- Sys.time()
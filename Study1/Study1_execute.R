job_args <- commandArgs(trailingOnly=TRUE)
print(job_args)

##### Setup #####
library(MASS)    # for mvrnorm
library(dplyr)   # data wrangling
library(ranger)  # random forests
library(caret)   # tuning
library(pROC)    # AUC calculations
library(psych)   # cohen's kappa
source("DataSimFunctions.R")
source("TuningFunctions.R")
source("PerformanceMetricsFunctions.R")

job_id <- job_args[1] %>% as.numeric()
p      <- job_args[2] %>% as.numeric()

start_seed <- job_id*100 + p
set.seed(start_seed)

# load in scenario + coef
load("DGM_data/scenarios.RData")
scenarios <- scenarios %>%
  filter(n_pred == p)
load("DGM_data/betas.RData")

selected_scenario <- ifelse(job_id %% 6 == 0, 6, job_id %% 6)

scenarios      <- scenarios[selected_scenario,]
sample_size    <- scenarios$n
n_prop         <- scenarios$prop_sample_size
n_pred         <- scenarios$n_pred
event_fraction <- scenarios$event_fraction
betas <- betas_matrix[(betas_matrix[,"n_predictor"] == n_pred) & (betas_matrix[,"prevalence"] == event_fraction),3:5]

# set size of the validation set
large_sample <- 1e5

##### Tuning setup #####

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
# randomise the order in which hyperparameter combinations are run
hyperparameter_permutation <- sample(1:nrow(hyperparameter_combinations), nrow(hyperparameter_combinations))
hyperparameter_combinations <- hyperparameter_combinations[hyperparameter_permutation,]

# set up the CV function
ctrl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = deviance,
  classProbs = TRUE
)

nrow_output <- nrow(hyperparameter_combinations)
out <- data.frame(
  start_seed     = rep(start_seed,     nrow_output),
  n              = rep(sample_size,    nrow_output),
  n_prop         = rep(n_prop,         nrow_output),
  p              = rep(n_pred,         nrow_output),
  EF             = rep(event_fraction, nrow_output),
  intercept      = rep(betas[1],       nrow_output),
  beta           = rep(betas[1],       nrow_output),
  gamma          = rep(betas[3],       nrow_output),
  hp_combination = rep(NA,             nrow_output),
  fold_seed      = rep(NA,             nrow_output),
  time           = rep(NA,             nrow_output),
  AUC            = rep(NA,             nrow_output),
  CalSlope       = rep(NA,             nrow_output),
  CalIntercept   = rep(NA,             nrow_output),
  BrierScore     = rep(NA,             nrow_output),
  LogLoss        = rep(NA,             nrow_output),
  Accuracy       = rep(NA,             nrow_output),
  CohensKappa    = rep(NA,             nrow_output)
)
row_out <- 0

nrow_pred <- nrow(hyperparameter_combinations)*large_sample
out_pred <- data.frame(
  start_seed     = rep(start_seed, nrow_pred),
  hp_combination = rep(NA,         nrow_pred),
  prob           = rep(NA,         nrow_pred),
  obs            = rep(NA,         nrow_pred)
)
row_pred <- 0

# start the simulation
##### Generate data #####
# generate a dataset for each scenario
# generate a validation set for each scenario
dat     <- simulate_data(sample_size,  n_pred, event_fraction, betas)
val_dat <- simulate_data(large_sample, n_pred, event_fraction, betas)

##### Tuning procedure #####

# for each combination, make a grid & tune
for (combination in 1:nrow(hyperparameter_combinations)) {
  print(hyperparameter_combinations[combination,])
  row_out <- row_out + 1
  hp_comb <- paste(colnames(hyperparameter_combinations)[as.logical(hyperparameter_combinations[combination,])], collapse = " + ")
  out[row_out,"hp_combination"] <- hp_comb
  out_pred[(row_pred + 1):(row_pred + large_sample),"hp_combination"] <- hp_comb
  
  if (hyperparameter_combinations[combination,"mtry"]) {
    mtry_candidates <- 1:n_pred
  } else {
    mtry_candidates <- sqrt(n_pred)
  }
  if (hyperparameter_combinations[combination,"min.node.size"]) {
    min.node.size_candidates <- 1:10
  } else {
    min.node.size_candidates <- 1
  }
  if (hyperparameter_combinations[combination,"splitrule"]) {
    splitrule_candidates <- c("gini", "hellinger", "extratrees")
  } else {
    splitrule_candidates <- "gini"
  }
  if (hyperparameter_combinations[combination,"sample.fraction"]) {
    sample.fraction_candidates <- seq(0.1, 1, by = 0.1)
  } else {
    sample.fraction_candidates <- 1
  }
  if (hyperparameter_combinations[combination,"replace"]) {
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
  mod <- tune_hyperparameters(dat, tunegrid, sample.fraction_candidates, replace_candidates, ctrl)
  end_tune   <- Sys.time()
  print(end_tune)
  tuning_time <- difftime(end_tune, start_tune, units = "mins")
  
  ##### Assess performance #####
  
  # fit best model on the validation set
  pred <- predict(mod$model, newdata = val_dat[,-ncol(val_dat)], type = "prob")$pos
  # evaluate the performance based on my metrics
  perf <- performance(pred, val_dat$Y)
  
  out[row_out,"fold_seed"] <- mod$fold_seed
  out[row_out,"time"] <- tuning_time
  out[row_out,12:18] <- perf
  
  out_pred[(row_pred + 1):(row_pred + large_sample),"prob"] <- pred
  out_pred[(row_pred + 1):(row_pred + large_sample),"obs"]  <- as.character(val_dat$Y)
  row_pred <- row_pred + large_sample
}

##### Save #####

filename <- paste0("Study1/Data/sim/study1_onescenario_run", job_id, "_", p, ".rds")
saveRDS(out, file = filename)

if ((job_id %% 10) == 0) {
  filename_pred <- paste0("Study1/Data/preds/study1_run", job_id, "_", p, ".rds")
  saveRDS(out_pred, file = filename_pred)
}
job_args <- commandArgs(trailingOnly=TRUE)
print(job_args)

##### Setup #####
library(MASS)         # for mvrnorm
library(dplyr)        # data wrangling
library(ranger)       # random forests
library(caret)        # tuning: grid search + random search
library(pROC)         # AUC calculations
library(psych)
library(tuneRanger)
source("DataSimFunctions.R")
source("PerformanceMetricsFunctions.R")
source("TuningFunctions.R")
source("simsalapar.R")

job_id <- job_args[1] %>% as.numeric()

start_seed <- job_id*100 + 3
set.seed(start_seed)

# load in scenario + coef
load("DGM_data/scenarios.RData")
load("DGM_data/betas.RData")

selected_scenario <- ifelse(job_id %% 12 == 0, 12, job_id %% 12)

scenarios      <- scenarios[selected_scenario,]
sample_size    <- scenarios$n
n_prop         <- scenarios$prop_sample_size
n_pred         <- scenarios$n_pred
event_fraction <- scenarios$event_fraction
betas <- betas_matrix[(betas_matrix[,"n_predictor"] == n_pred) & (betas_matrix[,"prevalence"] == event_fraction),3:5]

# set size of the validation set
large_sample <- 1e4

##### Tuning setup #####

# which algorithm are we using?
algorithm <- c("grid", "random", "mbo")

# randomise the order in which metrics are run
algorithm_permutation <- sample(1:length(algorithm), length(algorithm))
algorithm <- algorithm[algorithm_permutation]

nrow_output <- length(algorithm)
out <- data.frame(
  start_seed     = rep(start_seed,     nrow_output),
  n              = rep(sample_size,    nrow_output),
  n_prop         = rep(n_prop,         nrow_output),
  p              = rep(n_pred,         nrow_output),
  EF             = rep(event_fraction, nrow_output),
  intercept      = rep(betas[1],       nrow_output),
  beta           = rep(betas[1],       nrow_output),
  gamma          = rep(betas[3],       nrow_output),
  algorithm      = rep(NA,             nrow_output),
  time           = rep(NA,             nrow_output),
  AUC            = rep(NA,             nrow_output),
  CalSlope       = rep(NA,             nrow_output),
  CalIntercept   = rep(NA,             nrow_output),
  BrierScore     = rep(NA,             nrow_output),
  LogLoss        = rep(NA,             nrow_output),
  Accuracy       = rep(NA,             nrow_output),
  CohensKappa    = rep(NA,             nrow_output),
  warning        = rep("",             nrow_output)
)
row_out <- 0

nrow_pred <- length(algorithm)*large_sample
out_pred <- data.frame(
  start_seed = rep(start_seed, nrow_pred),
  algorithm  = rep(NA,         nrow_pred),
  prob       = rep(NA,         nrow_pred),
  obs        = rep(NA,         nrow_pred)
)
row_pred <- 0

# start the simulation
##### Generate data #####
# generate a dataset for each scenario
# generate a validation set for each scenario
dat     <- simulate_data(sample_size,  n_pred, event_fraction, betas)
val_dat <- simulate_data(large_sample, n_pred, event_fraction, betas)

##### Tuning procedure #####

# for each metric, make a grid & tune
for (alg in algorithm) {
  print(alg)
  row_out <- row_out + 1
  out[row_out,"algorithm"] <- alg
  out_pred[(row_pred + 1):(row_pred + large_sample),"algorithm"] <- alg
  
  # tuning
  if (alg == "grid") {
    ctrl <- trainControl(
      method = "cv",
      number = 5,
      summaryFunction = deviance,
      classProbs = TRUE,
      allowParallel = FALSE
    )
    # make the grid
    tunegrid <- expand.grid(
      mtry = 1:n_pred,
      min.node.size = 1:10,
      splitrule = "gini"
    )
    start_tune <- Sys.time()
    mod <- caret::train(Y ~ .,
                 data = dat,
                 method = "ranger",
                 metric = "Deviance",
                 maximize = FALSE,
                 trControl = ctrl,
                 tuneGrid = tunegrid)
    end_tune   <- Sys.time()
    print(end_tune)
    tuning_time <- difftime(end_tune, start_tune, units = "mins")
    
    # fit best model on the validation set
    pred <- predict(mod, newdata = val_dat[,-ncol(val_dat)], type = "prob")$pos
  }
  if (alg == "random") {
    ctrl <- trainControl(
      method = "cv",
      number = 5,
      summaryFunction = deviance,
      classProbs = TRUE,
      allowParallel = FALSE
    )
    # make the grid
    tunegrid <- expand.grid(
      mtry = 1:n_pred,
      min.node.size = 1:(sample_size*event_fraction),
      splitrule = "gini"
    )
    grid_select <- sample(1:nrow(tunegrid), 10*n_pred/2) # half the size of the full grid
    tunegrid <- tunegrid[grid_select,]
    
    start_tune <- Sys.time()
    mod <- caret::train(Y ~ .,
                        data = dat,
                        method = "ranger",
                        metric = "Deviance",
                        maximize = FALSE,
                        trControl = ctrl,
                        tuneGrid = tunegrid)
    end_tune   <- Sys.time()
    print(end_tune)
    tuning_time <- difftime(end_tune, start_tune, units = "mins")
    
    # fit best model on the validation set
    pred <- predict(mod, newdata = val_dat[,-ncol(val_dat)], type = "prob")$pos
  }
  if (alg == "mbo") {
    task <- makeClassifTask(data = dat,
                            target = "Y",
                            positive = "pos")
    start_tune <- Sys.time()
    mod <- tuneRanger(task = task,
                      measure = list(deviance_mbo),
                      tune.parameters = c("mtry", "min.node.size"),
                      num.trees = 500,
                      num.threads = 1,
                      parameters = list(
                        "splitrule" = "gini",
                        "sample.fraction" = 1,
                        "replace" = TRUE
                      )
                      ) %>% 
      tryCatch.W.E()
    end_tune   <- Sys.time()
    print(end_tune)
    tuning_time <- difftime(end_tune, start_tune, units = "mins")
    
    if (!is.null(mod$warning)) out[row_out,"warning"] <- mod$warning
    
    # fit best model on the validation set
    pred <- predict(mod$value$model, newdata = val_dat[,-ncol(val_dat)])$data$prob.pos
  }
  
  ##### Assess performance #####
  
  # evaluate the performance based on my metrics
  perf <- performance(pred, val_dat$Y)
  
  out[row_out,"time"] <- tuning_time
  out[row_out,11:17] <- perf
  
  out_pred[(row_pred + 1):(row_pred + large_sample),"prob"] <- pred
  out_pred[(row_pred + 1):(row_pred + large_sample),"obs"]  <- as.character(val_dat$Y)
  row_pred <- row_pred + large_sample
}

##### Save #####

filename <- paste0("Study3/Data/sim/study3_run", job_id, ".rds")
saveRDS(out, file = filename)

if ((ceiling(job_id / 12) %% 10) == 0) {
  filename_pred <- paste0("Study3/Data/preds/study3_preds_run", job_id, ".rds")
  saveRDS(out_pred, file = filename_pred)
}
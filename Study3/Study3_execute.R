job_args <- commandArgs(trailingOnly=TRUE)
print(job_args)

##### Setup #####
library(MASS)         # for mvrnorm
library(dplyr)        # data wrangling
library(ranger)       # random forests
library(pROC)         # AUC calculations
library(psych)
library(tuneRanger)
library(caret)        # tuning: grid search + random search
library(simsalapar)   # error catching
source("DataSimFunctions.R")
source("PerformanceMetricsFunctions.R")
source("TuningFunctions.R")
# # object to optimise deviance
# deviance_mbo <- makeMeasure(
#   "deviance_mbo",
#   minimize = TRUE,
#   fun = dev_mbo,
#   best = 0,
#   aggr = test.mean,
#   properties = c("classif", "req.prob")
# )

job_id <- job_args[1] %>% as.numeric()

# one run of this script generates 10 observations (10 datasets tuned 3 times)
# it's 10 so that we can just save the predictions for the last one
n_runs <- 10
# we don't want the seed generating the data to be the same in any study
# study 1 has seeds ending in 8 or 16
# study 2 has seeds ending in 00
# so study 3 can have seeds ending in 1-7, 9-15, 17-99
# to simplify, we pick 50-60
start_seed <- job_id*100 + 50
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

alg_list <- c("grid", "random", "mbo")
nrow_output <- length(alg_list)*n_runs
out <- data.frame(
  start_seed     = rep(start_seed,     nrow_output),
  seed           = rep(NA,             nrow_output),
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

nrow_pred <- length(alg_list)*large_sample*floor(n_runs/10)
out_pred <- data.frame(
  start_seed = rep(start_seed, nrow_output),
  seed       = rep(NA,         nrow_pred),
  algorithm  = rep(NA,         nrow_pred),
  prob       = rep(NA,         nrow_pred),
  obs        = rep(NA,         nrow_pred)
)
row_pred <- 0

# start the simulation
for (run in 1:n_runs) {
  seed <- start_seed + run
  set.seed(seed)
  
  ##### Tuning setup #####
  
  # which algorithm are we using?
  algorithm <- alg_list
  
  # randomise the order in which metrics are run
  algorithm_permutation <- sample(1:length(algorithm), length(algorithm))
  algorithm <- algorithm[algorithm_permutation]
  
  ##### Generate data #####
  # generate a dataset for each scenario
  # generate a validation set for each scenario
  dat     <- simulate_data(sample_size,  n_pred, event_fraction, betas)
  val_dat <- simulate_data(large_sample, n_pred, event_fraction, betas)
  
  out[(row_out+1):(row_out+length(algorithm)),"seed"] <- seed
  
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
        summaryFunction = logloss_opt,
        classProbs = TRUE,
        allowParallel = FALSE
      )
      # make the grid
      tunegrid <- expand.grid(
        mtry = 1:n_pred,
        min.node.size = 1:10,
        splitrule = "gini"
      )
      cat("Grid size: ", nrow(tunegrid), "\n")
      start_tune <- Sys.time()
      mod <- train(Y ~ .,
                   data = dat,
                   method = "ranger",
                   metric = "LogLoss",
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
        summaryFunction = logloss_opt,
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
      cat("Grid size: ", nrow(tunegrid), "\n")
      
      start_tune <- Sys.time()
      mod <- train(Y ~ .,
                   data = dat,
                   method = "ranger",
                   metric = "LogLoss",
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
                        measure = list(logloss),
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
    out[row_out,12:18] <- perf
    
    if ((run %% 10) == 0) {
      out_pred[(row_pred + 1):(row_pred + large_sample),"seed"] <- seed
      out_pred[(row_pred + 1):(row_pred + large_sample),"prob"] <- pred
      out_pred[(row_pred + 1):(row_pred + large_sample),"obs"]  <- as.character(val_dat$Y)
      row_pred <- row_pred + large_sample
    }
  }
}

##### Save #####

filename <- paste0("Study3/Data/sim/study3_run", job_id, ".rds")
saveRDS(out, file = filename)

filename_pred <- paste0("Study3/Data/preds/study3_preds_run", job_id, ".rds")
saveRDS(out_pred, file = filename_pred)
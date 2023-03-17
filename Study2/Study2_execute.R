job_args <- commandArgs(trailingOnly=TRUE)
print(job_args)

start <- Sys.time()
##### Setup #####
library(MASS)    # for mvrnorm
library(dplyr)   # data wrangling
# library(tidyr)   # pivot functions
library(ranger)  # random forests
library(caret)   # tuning
library(pROC)    # AUC calculations
library(psych)
source("DataSimFunctions.R")
source("TuningFunctions.R")
source("PerformanceMetricsFunctions.R")

job_id <- job_args[1] %>% as.numeric()
# p      <- job_args[2] %>% as.numeric()

start_seed <- job_id*100 #+ p + 2
set.seed(start_seed)

# load in scenario + coef
load("DGM_data/scenarios.RData")
# scenarios <- scenarios %>%
#   filter(n_pred == p)
load("DGM_data/betas.RData")

selected_scenario <- ifelse(job_id %% 12 == 0, 12, job_id %% 12)

scenarios      <- scenarios[selected_scenario,]
sample_size    <- scenarios$n
n_prop         <- scenarios$prop_sample_size
n_pred         <- scenarios$n_pred
event_fraction <- scenarios$event_fraction
betas <- betas_matrix[(betas_matrix[,"n_predictor"] == n_pred) & (betas_matrix[,"prevalence"] == event_fraction),3:5]

# set size of the validation set
large_sample <- 1e5

##### Tuning setup #####

# which metric are we optimising?
metrics <- c("Deviance", "BrierScore", "LogLoss", "AUC", "CalInt", "CalSlope", "Accuracy", "Kappa")
# names for calint and calslope

# randomise the order in which metrics are run
metrics_permutation <- sample(1:length(metrics), length(metrics))
metrics <- metrics[metrics_permutation]

# make the grid
tunegrid <- expand.grid(
  mtry = 1:n_pred,
  min.node.size = 1:10,
  splitrule = "gini"
)

nrow_output <- length(metrics)
out <- data.frame(
  start_seed     = rep(start_seed,     nrow_output),
  n              = rep(sample_size,    nrow_output),
  n_prop         = rep(n_prop,         nrow_output),
  p              = rep(n_pred,         nrow_output),
  EF             = rep(event_fraction, nrow_output),
  intercept      = rep(betas[1],       nrow_output),
  beta           = rep(betas[1],       nrow_output),
  gamma          = rep(betas[3],       nrow_output),
  metric         = rep(NA,             nrow_output),
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

nrow_pred <- length(metrics)*large_sample
out_pred <- data.frame(
  start_seed = rep(start_seed, nrow_pred),
  metric     = rep(NA,         nrow_pred),
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
for (metric in metrics) {
  print(metric)
  row_out <- row_out + 1
  out[row_out,"metric"] <- metric
  out_pred[(row_pred + 1):(row_pred + large_sample),"metric"] <- metric
  
  # set up the CV function
  if (metric == "Deviance") {
    ctrl <- trainControl(
      method = "cv",
      number = 5,
      summaryFunction = deviance,
      classProbs = TRUE,
      allowParallel = FALSE
    )
  }
  if (metric == "BrierScore") {
    ctrl <- trainControl(
      method = "cv",
      number = 5,
      summaryFunction = brier_opt,
      classProbs = TRUE,
      allowParallel = FALSE
    )
  }
  if (metric == "AUC") {
    ctrl <- trainControl(
      method = "cv",
      number = 5,
      summaryFunction = AUC_opt,
      classProbs = TRUE,
      allowParallel = FALSE
    )
  }
  if (metric == "CalInt") {
    ctrl <- trainControl(
      method = "cv",
      number = 5,
      summaryFunction = cal_int,
      classProbs = TRUE,
      allowParallel = FALSE
    )
  }
  if (metric == "CalSlope") {
    ctrl <- trainControl(
      method = "cv",
      number = 5,
      summaryFunction = cal_slope,
      classProbs = TRUE,
      allowParallel = FALSE
    )
  }
  if (metric == "LogLoss") {
    ctrl <- trainControl(
      method = "cv",
      number = 5,
      summaryFunction = logloss_opt,
      classProbs = TRUE,
      allowParallel = FALSE
    )
  }
  if (metric %in% c("Accuracy", "Kappa")) {
    ctrl <- trainControl(
      method = "cv",
      number = 5,
      classProbs = TRUE,
      allowParallel = FALSE
    )
  }
  
  # set a seed here
  start_tune <- Sys.time()
  mod <- train(Y ~ .,
               data = dat,
               method = "ranger",
               metric = metric,
               maximize = ifelse(metric %in% c("Accuracy", "Kappa", "AUC"), TRUE, FALSE),
               trControl = ctrl,
               tuneGrid = tunegrid)
  end_tune   <- Sys.time()
  print(end_tune)
  tuning_time <- difftime(end_tune, start_tune, units = "mins")
  
  ##### Assess performance #####
  
  # fit best model on the validation set
  pred <- predict(mod, newdata = val_dat[,-ncol(val_dat)], type = "prob")$pos
  # evaluate the performance based on my metrics
  perf <- performance(pred, val_dat$Y)
  
  out[row_out,"time"] <- tuning_time
  out[row_out,11:17] <- perf
  
  out_pred[(row_pred + 1):(row_pred + large_sample),"prob"] <- pred
  out_pred[(row_pred + 1):(row_pred + large_sample),"obs"]  <- as.character(val_dat$Y)
  row_pred <- row_pred + large_sample
}
end <- Sys.time()
##### Save #####
time <- end-start
timefile <- paste0("Study2/Data/study2_time", job_id, ".rds")
save(time, file = timefile)

filename <- paste0("Study2/Data/sim/study2_run", job_id, ".rds")
saveRDS(out, file = filename)

if ((ceiling(job_id / 6) %% 10) == 0) {
  filename_pred <- paste0("Study2/Data/preds/study2_preds_run", job_id, ".rds")
  saveRDS(out_pred, file = filename_pred)
}
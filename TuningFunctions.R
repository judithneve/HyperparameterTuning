# own metric to optimise: deviance
deviance <- function(data, lev = NULL, model = NULL) {
  obs  <- factor_to_outcome(data$obs)
  pred <- data$pos
  
  pred[pred == 0] <- 1e-16
  pred[pred == 1] <- 1-1e-16
  
  dev <- -2*sum(obs*log(pred) + (1-obs)*log(1-pred))
  
  c(Deviance = dev)
}

# own metric to optimise: Brier score
brier_opt <- function(data, lev = NULL, model = NULL) {
  obs  <- data$obs
  pred <- data$pos
  
  brier <- brier(pred, obs)
  
  c(BrierScore = brier)
}

# own metric to optimise: logarithmic loss
logloss_opt <- function(data, lev = NULL, model = NULL) {
  obs  <- data$obs
  pred <- data$pos
  
  ll <- logloss(pred, obs)
  
  c(LogLoss = ll)
}

# own metric to optimise: AUC
AUC_opt <- function(data, lev = NULL, model = NULL) {
  obs  <- data$obs
  pred <- data$pos
  
  auc <- 1 - roc(response = obs, predictor = pred, quiet = TRUE)$auc
  
  c(AUC = auc)
}

# own metric to optimise: calibration intercept
cal_int <- function(data, lev = NULL, model = NULL) {
  obs  <- data$obs
  pred <- data$pos
  
  if (sum(pred == 1) != 0){
    pred[pred == 1] <- 1 - ((1-max(pred[pred != 1]))/2)
  } 
  
  if (sum(pred == 0) != 0) {
    pred[pred == 0] <- min(pred[pred != 0]) /2 
  }
  
  cal_int <- glm(obs ~ 1,
                 offset = log(pred/(1-pred)),
                 family = "binomial") %>%
    coef()
  
  c(CalInt = cal_int^2)
}

# own metric to optimise: calibration slope
cal_slope <- function(data, lev = NULL, model = NULL) {
  obs  <- data$obs
  pred <- data$pos
  
  if (sum(pred == 1) != 0){
    pred[pred == 1] <- 1 - ((1-max(pred[pred != 1]))/2)
  } 
  
  if (sum(pred == 0) != 0) {
    pred[pred == 0] <- min(pred[pred != 0]) /2 
  }
  
  cal_sl <- glm(obs ~ log(pred/(1-pred)),
                 family = "binomial") %>%
    coef()[2]
  
  c(CalInt = cal_sl)
}



# hyperparameter tuning
tune_hyperparameters <- function(dataset, tunegrid, sf_candidates, replace_candidates, ctrl) {
  fold_seed <- sample(1:1e8, 1)
  cat("\nFold seed:", fold_seed)
  
  mods <- list()
  mod_performance <- data.frame()
  for (sf in sf_candidates) {
    print(sf)
    for (r in replace_candidates) {
      print(r)
      length_list <- length(mods) + 1
      set.seed(fold_seed)
      mods[[length_list]] <- train(Y ~ .,
                                   data = dataset,
                                   method = "ranger",
                                   metric = "Deviance",
                                   maximize = FALSE,
                                   trControl = ctrl,
                                   tuneGrid = tunegrid,
                                   num.trees = 500,
                                   replace = r,
                                   sample.fraction = sf)
      mod_performance <- rbind(mod_performance,
                               mods[[length_list]]$results %>%
                                 filter(Deviance == min(Deviance, na.rm = TRUE)))
    }
  }
  best_mod <- which.min(mod_performance$Deviance)
  final_mod <- mods[[best_mod]]
  # NOTE: COULD POSSIBLY SHORTEN TO OVERWRITE MODELS THAT ARE LESS GOOD
  
  return(list(model = final_mod, fold_seed = fold_seed))
}

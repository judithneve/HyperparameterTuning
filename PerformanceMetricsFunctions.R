# Performance metrics

factor_to_outcome <- function(factor) {
  outcome <- as.numeric(factor)
  if (!all(outcome %in% c(0, 1))) {
    outcome <- outcome - 1
  }
  return(outcome)
}

# Calibration curves + intercepts (without CI) -- taken from Ben van Calster -- https://github.com/benvancalster/classimb_calibration/blob/main/simulation%20study/Simulation/performance_measures_wo_eci.R
calibration <- function(probs, outcome){
  if (sum(probs == 1) != 0){
    probs[probs == 1] <- 1 - ((1-max(probs[probs != 1]))/2)
  } 
  
  if (sum(probs == 0) != 0) {
    probs[probs == 0] <- min(probs[probs != 0]) /2 
  }
  
  slope_model <- glm(outcome ~ log(probs/(1-probs)), family = "binomial")
  slope <- coef(slope_model)[2]
  attributes(slope) <- NULL
  intercept_model <- glm(outcome ~ 1, 
                         offset = log(probs/(1-probs)), 
                         family = "binomial")
  intercept <- coef(intercept_model)
  attributes(intercept) <- NULL
  cal_results <- c(intercept, slope)
  
  return(cal_results)
}

# Brier score -- taken from Ben van Calster -- https://github.com/benvancalster/classimb_calibration/blob/main/simulation%20study/Simulation/performance_measures_wo_eci.R
brier <- function(probs, outcome) {
  outcome <- factor_to_outcome(outcome)
  mean((outcome - probs)^2)
}

# Logloss
logloss <- function(probs, outcome) {
  outcome <- factor_to_outcome(outcome)
  
  probs[probs == 0] <- 1e-16
  probs[probs == 1] <- 1-1e-16
  
  -mean(outcome*log(probs) + (1-outcome)*log(1-probs))
}

# Accuracy
accuracy <- function(pred, outcome) {
  correct_predictions <- sum(pred == outcome)
  total_predictions <- length(pred)
  correct_predictions / total_predictions
}

# all metrics
performance <- function(probs, outcome) {
  AUC <- roc(response = outcome, predictor = probs, quiet = TRUE)$auc
  calib <- calibration(probs, outcome)
  CalSlope <- calib[2]
  CalInt <- calib[1]
  Brier <- brier(probs, outcome)
  LogLoss <- logloss(probs, outcome)
  
  pred <- ifelse(probs < 0.5, 0, 1) %>%
    factor(levels = c(0, 1),
           labels = c("neg", "pos"))
  acc <- accuracy(pred, outcome)
  kappa <- cohen.kappa(cbind(pred, outcome))$weighted.kappa
  
  c(
    AUC = AUC,
    CalibrationSlope = CalSlope,
    CalibrationIntercept = CalInt,
    BrierScore = Brier,
    LogarithmicLoss = LogLoss,
    Accuracy = acc,
    CohensKappa = kappa
    )
}

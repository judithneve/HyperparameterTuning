###### Sensitivity analyses to determine the grid of HPs in study 1 ######

# load packages
library(caret)
library(MASS)
library(dplyr)
library(tidyr)
library(ranger)

start_seed <- 150

# load scenario information, regression coefficients, data simulation functions
load("DGM_data/scenarios.RData")
scenarios <- scenarios %>%
  filter(n_pred == 16)
load("DGM_data/betas.RData")
betas_matrix <- betas_matrix[4:6,]
source("DataSimFunctions.R")

# define a function for the deviance - we optimise using this
deviance <- function(data, lev = NULL, model = NULL) {
  obs  <- as.numeric(data$obs) - 1
  pred <- data$pos
  
  pred[pred == 0] <- min(0.0001, min(pred[pred != 0]))
  pred[pred == 1] <- max(0.9999, max(pred[pred != 1]))
  
  dev <- -2*sum(obs*log(pred) + (1-obs)*log(1-pred))
  
  c(Deviance = dev)
}

# define a function for minimum.node.size
mtry_sensitivity <- function(scenarios, betas, n_datasets = 50) {
  ctrl <- trainControl(
    method = "cv",
    number = 5,
    summaryFunction = deviance,
    classProbs = TRUE
  )
  
  out <- matrix(NA, nrow = 0, ncol = 6) # what should this be
  colnames(out) <- c("dataset_id", "p", "n", "EF", "min.node.size", "deviance")
  
  for (scenario in 1:nrow(scenarios)) {
    n_temp <- scenarios$n[scenario]
    p_temp <- scenarios$n_pred[scenario]
    EF_temp <- scenarios$event_fraction[scenario]
    beta <- betas[betas[,"prevalence"] == EF_temp,3:5]
    
    tunegrid <- expand.grid(mtry = 1:p_temp,
                            splitrule = "gini",
                            min.node.size = 1)
    
    for (it in 1:n_datasets) {
      print(it)
      dataset <- paste0(scenario, "_", it)
      dat_temp <- simulate_data(n_temp,
                                p_temp,
                                EF_temp,
                                beta)
      mod_temp <- train(Y ~ ., data = dat_temp,
                        method = "ranger",
                        metric = "Deviance",
                        maximize = FALSE,
                        trControl = ctrl,
                        tuneGrid = tunegrid)$results$Deviance
      out_temp <- data.frame(
        dataset_id = dataset,
        p = p_temp,
        n = n_temp,
        EF = EF_temp,
        mtry = 1:p_temp,
        deviance = mod_temp
      )
      
      out <- rbind(
        out, out_temp
        )
    }
  }
  return(out)
}

# generate a data frame with the deviance for each minimum node size for each dataset
##### TODO: set a seed #####
set.seed(start_seed)
start <- Sys.time()
select_mtry <- mtry_sensitivity(
  scenarios = scenarios,
  betas = betas_matrix,
  n_datasets = 100
)
end <- Sys.time()

# save the resulting data frames
save(start_seed, select_mtry, file = "Study1/Data/select_mtry.RData")
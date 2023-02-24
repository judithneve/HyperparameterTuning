###### Sensitivity analyses to determine the grid of HPs in study 1 ######

# load packages
library(caret)
library(MASS)
library(dplyr)
library(tidyr)
library(ranger)

# load scenario information, regression coefficients, data simulation functions
load("Study1/Data/scenarios.RData")
load("Study1/Data/betas.RData")
source("Study1/DataSimFunctions.R")

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
sample.fraction_sensitivity <- function(scenarios, betas, n_datasets = 50) {
  dat <- sim_data(scenarios, betas, nsim = n_datasets, seed = sample(1:10000, 1))
  
  ctrl <- trainControl(
    method = "cv",
    number = 5,
    summaryFunction = deviance,
    classProbs = TRUE
  )
  
  out <- matrix(NA, nrow = 0, ncol = 6)
  colnames(out) <- c("dataset_id", "p", "n", "EF", "sample.fraction", "deviance")
  
  for (dataset in unique(dat[,"dataset_id"])) {
    print(dataset)
    print(Sys.time())
    EF_temp <- dat[dat[,"dataset_id"] == dataset,] %>%
      as.data.frame() %>% 
      pull(event_fraction) %>% 
      unique() %>% 
      as.numeric()
    
    dat_temp <- dat[dat[,"dataset_id"] == dataset,] %>%
      as.data.frame() %>%
      mutate(Pred_value = as.numeric(Pred_value),
             Y = factor(Y,
                        levels = c(0, 1),
                        labels = c("neg", "pos"))) %>% 
      pivot_wider(names_from = Pred_number, values_from = Pred_value) %>% 
      dplyr::select(-c("id", "sample_size_prop", "n_pred", "event_fraction", "dataset_id"))
    n_temp <- nrow(dat_temp)
    p_temp <- ncol(dat_temp) - 1
    
    tunegrid <- expand.grid(mtry = sqrt(p_temp),
                            splitrule = "gini",
                            min.node.size = 1)
    fold_seed <- sample(1:10000, 1)
    print(fold_seed)
    
    mod_temp <- rep(NA, 100)
    for (sf in 1:100) {
      set.seed(fold_seed)
      # print(sf)
      temp <- try(train(
        Y ~ ., data = dat_temp,
        method = "ranger",
        metric = "Deviance",
        maximize = FALSE,
        trControl = ctrl,
        tuneGrid = tunegrid,
        sample.fraction = sf/100
      )$results$Deviance)
      if (class(temp) == "try-error") {temp <- NA}
      # print(temp)
      mod_temp[sf] <- temp
    }
    
    out_temp <- data.frame(
      dataset_id = dataset,
      p = p_temp,
      n = n_temp,
      EF = EF_temp,
      sample.fraction = seq(0.01, 1, by = 0.01),
      deviance = mod_temp
    )
    
    out <- rbind(
      out, out_temp
    )
  }
  return(out)
}

# generate a data frame with the deviance for each sample.fraction for each dataset
set.seed(2582198)
start_sf <- Sys.time()
select_sample.frac <- sample.fraction_sensitivity(
  scenarios = scenarios,
  betas = betas_matrix,
  n_datasets = 25
)
end_sf <- Sys.time()
# 
# # save the resulting data frames
# save(select_sample.frac, file = "Study1/Data/select_samplefrac.RData")

# generate a data frame with the deviance for each sample.fraction for each dataset
set.seed(2582198*2)
start_sf <- Sys.time()
select_sample.frac2 <- sample.fraction_sensitivity(
  scenarios = scenarios,
  betas = betas_matrix,
  n_datasets = 25
)
end_sf <- Sys.time()

# save the resulting data frames
# save(select_sample.frac2, file = "Study1/Data/select_samplefrac2.RData")
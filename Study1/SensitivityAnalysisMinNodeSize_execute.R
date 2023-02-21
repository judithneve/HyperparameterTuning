###### Sensitivity analyses to determine the grid of HPs in study 1 ######

# load packages
library(caret)
library(MASS)
library(dplyr)
library(tidyr)
library(ranger)

start_seed <- as.numeric(job_args[1])

# load scenario information, regression coefficients, data simulation functions
load("Study1/Data/scenarios.RData")
load("Study1/Data/betas.RData")
source("Study1/DataSimFunctions.R")

# define a function for the deviance - we optimise using this
deviance <- function(data, lev = NULL, model = NULL) {
  obs  <- as.numeric(data$obs) - 1
  pred <- data$pos
  
  dev <- -2*sum(obs*log(pred) + (1-obs)*log(1-pred))
  
  c(Deviance = dev)
}

# define a function for minimum.node.size
min.node.size_sensitivity <- function(scenarios, betas, n_datasets = 50) {
  dat <- sim_data(scenarios, betas, nsim = n_datasets)
  
  ctrl <- trainControl(
    method = "cv",
    number = 5,
    summaryFunction = deviance,
    classProbs = TRUE
  )
  
  out <- matrix(NA, nrow = 0, ncol = 6) # what should this be
  colnames(out) <- c("dataset_id", "p", "n", "EF", "min.node.size", "deviance")
  
  for (dataset in unique(dat[,"dataset_id"])) {
    print(dataset)
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
                            min.node.size = 1:(round(EF_temp*n_temp)))
    
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
      min.node.size = 1:length(mod_temp),
      deviance = mod_temp
    )
    
    out <- rbind(
      out, out_temp
    )
  }
  return(out)
}

# generate a data frame with the deviance for each minimum node size for each dataset
##### TODO: set a seed #####
set.seed(start_seed)
select_min.node.size <- min.node.size_sensitivity(
  scenarios = scenarios,
  betas = betas_matrix,
  n_datasets = 10
)

# save the resulting data frames
save(start_seed, select_min.node.size, file = "Study1/Data/select_minnodesize.RData")
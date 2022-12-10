# Tuning functions

# make a HP grid for a given combination of hyperparameters to tune
hp_grid <- function(hp_combination, p) {    # input: a logical vector of whether HPs are tuned or not
  tuning_ranges <- list(
    mtry = 1:p,             # CHANGE - correspond to p
    sample.fraction = seq(0.1, 1, by = 0.1),
    # num.trees = c(100, 500),        # CHANGE - correspond to range in protocol
    replace = c(TRUE, FALSE),
    min.node.size = 1:10,    # CHANGE - correspond to N
    splitrule = c("gini", "hellinger", "extratrees")
  )
  
  default_values <- list(
    mtry = sqrt(p),               # change -- sqrt(p)
    sample.fraction = 1,
    # num.trees = 500,
    replace = TRUE,
    min.node.size = 1,
    splitrule = "gini"
  )
  
  hp_values <- list()
  for (hp in 1:length(hp_combination)) {
    if (hp_combination[hp] == TRUE) {
      hp_values[[hp]] <- tuning_ranges[[hp]]
    } else {
      hp_values[[hp]] <- default_values[[hp]]
    }
    names(hp_values)[[hp]] <- names(hp_combination)[[hp]]
  }
  return(expand.grid(hp_values))
}

# gets the best set of hyperparameters for the data
tune_hyperparameters <- function(combination, dataset) {
  n_pred <- as.integer(unique(dataset$n_pred))
  
  tuneGrid_full <- hp_grid(combination, p = n_pred)
  tuneGrid_red  <- tuneGrid_full %>%
    select(mtry, splitrule, min.node.size) %>%
    unique()
  
  start <- Sys.time()
  mods <- list()
  for (sf in unique(tuneGrid_full$sample.fraction)) {
    cat("sample fraction = ", sf, "\n")
    # for (nt in unique(tuneGrid_full$num.trees)) {
    #   cat("num.trees = ", nt, "\n")
      for (r in unique(tuneGrid_full$replace)) {
        cat("replace = ", r, "\n")
        length_list <- length(mods) + 1
        mods[[length_list]] <- train(
          x = as.data.frame(dataset[,7:(6+n_pred)]),
          y = as.factor(dataset$Y),
          method = "ranger",
          trControl = ctrl,
          tuneGrid = tuneGrid_red,
          num.trees = 500,
          replace = r,
          sample.fraction = sf
        ) # TODO: only store results? maybe I can use this to get the best model to use for prediction too
      }
    # }
  }
  end <- Sys.time()
  tuning_time <- difftime(end, start, units = "mins")
  
  # extract results for best tune
  all_mods <- matrix(NA, nrow = 0, ncol = 9) %>% as.data.frame()
  mod  <- 0
  for (sf in unique(tuneGrid_full$sample.fraction)) {
    # for (nt in unique(tuneGrid_full$num.trees)) {
      for (r in unique(tuneGrid_full$replace)) {
        mod <- mod + 1
        all_mods <- rbind(all_mods,
                          mods[[mod]]$results %>% 
                            mutate(sample.fraction = sf,
                                   # num.trees = nt,
                                   replace = r))
      }
    # }
  }
  c((all_mods %>% arrange(desc(Accuracy)))[1,c(1:3, 8:9)], time = tuning_time)
}

validation_prep <- function(datasets, dataset_id) {
  datasets[datasets[,"dataset_id"] == dataset_id,] %>%
    as.data.frame() %>%
    select(Y, Pred_number, Pred_value, id) %>%
    mutate(Pred_value = as.numeric(Pred_value),
           Y = as.factor(Y)) %>%
    pivot_wider(names_from = Pred_number, values_from = Pred_value) %>% 
    select(-id)
}

validate_model <- function(dataset, best_hp) {
  ranger(as.factor(Y) ~ .,
         data = dataset %>% select(-id, -sample_size_prop, -n_pred, -event_fraction, -dataset_id),
         num.trees = 500,
         mtry = best_hp$mtry,
         splitrule = best_hp$splitrule,
         min.node.size = best_hp$min.node.size,
         sample.fraction = best_hp$sample.fraction,
         replace = best_hp$replace,
         probability = TRUE)
}
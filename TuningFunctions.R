# own metric to optimise: deviance
deviance <- function(data, lev = NULL, model = NULL) {
  obs  <- factor_to_outcome(data$obs)
  pred <- data$pos
  
  dev <- -2*sum(obs*log(pred) + (1-obs)*log(1-pred))
  
  c(Deviance = dev)
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
      print(mod_performance)
    }
  }
  best_mod <- which.min(mod_performance$Deviance)
  print(best_mod)
  final_mod <- mods[[best_mod]]
  # NOTE: COULD POSSIBLY SHORTEN TO OVERWRITE MODELS THAT ARE LESS GOOD
  
  return(list(model = final_mod, fold_seed = fold_seed))
}

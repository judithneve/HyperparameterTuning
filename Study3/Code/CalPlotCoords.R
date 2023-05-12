# for each event fraction, generate a dataset with the coordinates for 200 points in each dataset/metric combination

library(purrr)
library(dplyr)
library(ggplot2)

ef_id <- c(1, 3, 5)

nrow_coords <- 200*200*3

loess_regression <- function(dataframe){
  
  x <- c(seq(0, 1, length.out = 200))
  # start_seed <- as.numeric(unique(temp$start_seed))
  
  mod  <- dataframe %>% loess(formula = obs ~ prob)
  y <- predict(mod, newdata = x)
  
  out  <- tibble(# iter = rep(start_seed, 200), 
    x  = x, y  = y)
  return(out)
}

load("DGM/Data/scenarios.RData")
scenarios <- scenarios %>% 
  mutate(sc = 1:12)

job_ids <- 1:600

scenario <- job_ids %% 12
scenario[scenario == 0] <- 12

for (ef in ef_id) {
  print(ef)
  seed_to_scenario <- data.frame(
    job_id = job_ids,
    sc     = scenario
  ) %>% 
    inner_join(scenarios) %>% 
    filter(event_fraction == (ef/10))
  
  predictions <- paste0("./Study3/Data/preds/study3_preds_run", seed_to_scenario$job_id, ".rds")
  # lf <- list.files("./Study1/Data/preds/") %>%  paste0("./Study1/Data/preds/", .)
  # predictions <- predictions[predictions %in% lf][1:10]
  
  coords <- data.frame(
    start_seed = rep(NA, nrow_coords),
    algorithm  = rep(NA, nrow_coords),
    x          = rep(NA, nrow_coords),
    y          = rep(NA, nrow_coords)
  )
  
  row <- 0
  
  for (pred in predictions) {
    temp <- readRDS(pred) %>% 
      mutate(obs = ifelse(obs == "neg", 0, 1)) %>%
      group_by(seed, algorithm) %>% 
      group_modify(~loess_regression(.))
    
    coords[(row + 1):(row + nrow(temp)),] <- temp
    
    row <- row + nrow(temp)
  }
  
  file_path <- paste0("./Study3/Data/coords/study3_ef", ef, ".rds")
  saveRDS(coords, file = file_path)
}


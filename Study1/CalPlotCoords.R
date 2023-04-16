job_args <- commandArgs(trailingOnly=TRUE)
print(job_args)

# for each event fraction, generate a dataset with the coordinates for 200 points in each dataset/metric combination

library(purrr)
library(dplyr)
# library(ggplot2)

run   <- as.numeric(job_args[1])
ef_id <- as.numeric(job_args[2])
p     <- as.numeric(job_args[3])

nrow_coords <- 200*20*9 # 200 observations for each of the 20 datasets with the chosen p and EF (100 datasets in 5 batches), 9 combinations

loess_regression <- function(dataframe){
  
  x <- c(seq(0, 1, length.out = 200))
  # start_seed <- as.numeric(unique(temp$start_seed))
  
  mod  <- dataframe %>% loess(formula = obs ~ prob)
  y <- predict(mod, newdata = x)
  
  out  <- tibble(# iter = rep(start_seed, 200), 
    x  = x, y  = y)
  return(out)
}

load("DGM_data/scenarios.RData")
scenarios <- scenarios %>% 
  filter(n_pred == p) %>% 
  mutate(sc = 1:6)

job_ids <- 1:3000
job_ids <- job_ids[(ceiling(job_ids / 6) %% 10) == 0]

scenario <- job_ids %% 6
scenario[scenario == 0] <- 6

seed_to_scenario <- data.frame(
  job_id = job_ids,
  sc     = scenario
) %>% 
  inner_join(scenarios) %>% 
  filter(event_fraction == (ef_id/10))

predictions <- paste0("./Study1/Data/preds/study1_run", seed_to_scenario$job_id, "_", p, ".rds")[(1+20*(run-1)):(20*run)]
# lf <- list.files("./Study1/Data/preds/") %>%  paste0("./Study1/Data/preds/", .)
# predictions <- predictions[predictions %in% lf][1:10]

coords <- data.frame(
  start_seed      = rep(NA, nrow_coords),
  hp_combination  = rep(NA, nrow_coords),
  x               = rep(NA, nrow_coords),
  y               = rep(NA, nrow_coords)
)

row <- 0

for (pred in predictions) {
  # start <- Sys.time()
  temp <- readRDS(pred) %>% 
    mutate(obs = ifelse(obs == "neg", 0, 1)) %>%
    group_by(start_seed, hp_combination) %>% 
    group_modify(~loess_regression(.))
  # end <- Sys.time()
  
  coords[(row + 1):(row + nrow(temp)),] <- temp
  
  row <- row + nrow(temp)
}

file_path <- paste0("./Study3/Data/coords/study3_ef", ef_id, "_pred", p, "_batch", run, ".rds")
saveRDS(coords, file = file_path)


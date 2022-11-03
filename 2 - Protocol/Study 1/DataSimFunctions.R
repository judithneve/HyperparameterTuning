# We generate data under a logistic regression model with strong interactions.
# 
# Vary:
# * number of predictors
# * event fraction
# * sample size

# Minimum sample size calculation based on Riley et al., 2020

make_scenarios <- function(n_pred, event_fraction, sample_size, cstat = 0.8) {
  req_n <- data.frame(
    n_pred         = rep(n_pred,         each  = length(event_fraction)),
    event_fraction = rep(event_fraction, times = length(n_pred)),
    req_n          = rep(NA, length(n_pred)*length(event_fraction))
  )
  
  for (p in n_pred) {
    for (ef in event_fraction) {
      req_n$req_n[(req_n$n_pred) == p & (req_n$event_fraction == ef)] <- pmsampsize(
        type = "b",
        parameters = p,
        prevalence = ef,
        cstatistic = cstat
      )$sample_size
    }
  }
  
  n_cols <- matrix(
    NA,
    nrow = nrow(req_n),
    ncol = length(sample_size)
  ) %>%
    as.data.frame()
  
  colnames(n_cols) <- sample_size
  
  for (j in 1:length(sample_size)) {
    n_cols[,j] <- req_n$req_n * sample_size[j]
  }

  scenarios <- cbind(req_n[,-3], n_cols) %>%
    pivot_longer(
      `0.5`:`2`,
      names_to = "prop_min_n",
      values_to = "n"
    ) %>% 
    mutate(n = ceiling(n))
  
  return(scenarios)
}

##########################################################################

## Simulate data

sim_data <- function(scenarios, nsim = 1000, seed = 0070661) {
  set.seed(seed)
  dat  <- matrix(NA, nrow = 0, ncol = 7)
  
  for (i in 1:nrow(scenarios)) {
    sample_size_temp <- scenarios$n[i]
    n_pred_temp      <- scenarios$n_pred[i]
    
    # TODO: how do I set this and this intercept to ensure an AUC of 0.8 (or some other number)
    #       include interaction terms
    beta <- runif(n_pred_temp + 1, -5, 5)
    
    for (j in 1:nsim) {
      
      dat_temp <- matrix(
        NA,
        nrow = sample_size_temp,
        ncol = n_pred_temp + 1
      )
      colnames(dat_temp) <- c(paste0("X", 1:n_pred_temp), "Y")
      
      X <- mvrnorm(
        n = sample_size_temp,
        mu = rep(0, n_pred_temp),
        Sigma = matrix(0.2, nrow = n_pred_temp, ncol = n_pred_temp) + diag(0.8, n_pred_temp)
      )
      
      dat_temp[,1:n_pred_temp] <- X
      
      ### TODO: include interactions ###
      X <- cbind(1, X) %>% as.matrix()
      
      prob <- exp(X %*% beta) / (1 + exp(X %*% beta))
      dat_temp[,"Y"] <- rbinom(sample_size_temp, 1, prob)
      
      dat_temp <- dat_temp %>%
        as.data.frame() %>%
        pivot_longer(-Y, names_to = "Pred_number", values_to = "Pred_value") %>%
        mutate(sample_size_prop = scenarios$prop_min_n[i],
               n_pred           = n_pred_temp,
               event_fraction   = scenarios$event_fraction[i],
               dataset_id       = paste(i, j, sep = "_")) %>%
        as.matrix()
      
      dat <- rbind(
        dat,
        dat_temp
      )
    }
  }
  return(dat)
}


  
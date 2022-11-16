# We generate data under a logistic regression model with strong interactions.
# 
# Vary:
# * number of predictors
# * event fraction
# * sample size

# Minimum sample size calculation based on Riley et al., 2020

make_scenarios <- function(n_pred, event_fraction, sample_size, cstat = 0.8) {
  scenarios <- cbind(
    expand.grid(n_pred = n_pred, event_fraction = event_fraction, prop_sample_size = sample_size),
    n = NA
  )
  
  for (p in n_pred) {
    for (ef in event_fraction) {
      scenarios$n[(scenarios$n_pred == p) & (scenarios$event_fraction == ef)] <- pmsampsize(
        type = "b",
        parameters = p,
        prevalence = ef,
        cstatistic = cstat
      )$sample_size
    }
  }

  scenarios <- scenarios %>% 
    mutate(n = ceiling(n*prop_sample_size))
  
  return(scenarios)
}

##########################################################################

## Simulate data

sim_data <- function(scenarios, nsim = 1000, seed = 0070661) {
  set.seed(seed)
  dat  <- matrix(NA, nrow = 0, ncol = 8)
  
  for (i in 1:nrow(scenarios)) {
    sample_size_temp <- scenarios$n[i]
    n_pred_temp      <- scenarios$n_pred[i]
    
    # TODO: how do I set this and this intercept to ensure an AUC of 0.8 (or some other number)
    #       include interaction terms
    # beta <- runif(n_pred_temp + 1, -5, 5)
    
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
      X_int <- matrix(NA, nrow = sample_size_temp, ncol = n_pred_temp*0.25)
      interacted_with <- n_pred_temp*0.5
      for (int in 1:ncol(X_int)) {
        X_int[,int] <- X[,int]*X[,interacted_with]
        interacted_with <- interacted_with + 1
      }
      X <- cbind(1, X, X_int) %>% as.matrix()
      
      intercept <- betas_matrix[1,3] # define this
      beta <- betas_matrix[1,4] # define this
      gamma <- betas_matrix[1,5]
      beta <- c(
        intercept,
        rep(beta, n_pred_temp),
        rep(gamma, n_pred_temp*0.25)
      )
      prob <- exp(X %*% beta) / (1 + exp(X %*% beta))
      dat_temp[,"Y"] <- rbinom(sample_size_temp, 1, prob)
      
      dat_temp <- dat_temp %>%
        as.data.frame() %>%
        mutate(id = 1:sample_size_temp) %>% 
        pivot_longer(-c(Y, id), names_to = "Pred_number", values_to = "Pred_value") %>%
        mutate(sample_size_prop = scenarios$prop_sample_size[i],
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


  
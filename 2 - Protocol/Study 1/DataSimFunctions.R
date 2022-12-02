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
        parameters = p*1.25,
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

## Make interactions

interaction_matrix <- function(X, n_pred = ncol(X), prop_int = 0.25) {
  int_term_2 <- n_pred/2
  n_int      <- n_pred*prop_int
  
  X_int      <- matrix(NA, nrow = nrow(X), ncol = n_int)
  
  for (i in 1:n_int) {
    X_int[,i] <- X[,i]*X[,i+int_term_2]
  }
  
  return(X_int)
}

## Simulate data

sim_data <- function(scenarios, coefs, nsim = 1000, seed = 0070661) {
  set.seed(seed)
  dat  <- matrix(NA, nrow = 0, ncol = 8)
  
  for (i in 1:nrow(scenarios)) {
    sample_size_temp    <- scenarios$n[i]
    n_pred_temp         <- scenarios$n_pred[i]
    event_fraction_temp <- scenarios$event_fraction[i]
    betas <- coefs[coefs[,"n_predictor"] == n_pred_temp & near(coefs[,"prevalence"], event_fraction_temp),]
    
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
      
      X_int <- interaction_matrix(X)
      X <- cbind(1, X, X_int) %>% as.matrix()
      
      intercept <- betas[3]
      beta <- betas[4]
      gamma <- betas[5]
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


  
# based on work by Zoe Dunias

#########################################
## large sample approximation of betas ##
#########################################

# function to optimise: get intercept & slope for a chosen AUC & prevalence
loss_function <- function(betas){ # this is a vector of length 2: intercept and slope
  AUC_previous <- AUC

  # Calculate probabilities
  linear <- betas[1] +
    betas[2]*rowSums(X)
  prob <- 1 / (1 + exp(-linear))
  true_y <- rbinom(n = example_n, size = 1, prob = prob)

  prev <- mean(true_y)
  if (prev == 0){
    AUC <- AUC_previous
  }
  else {
    AUC <<- roc(response = true_y, predictor = prob, quiet = TRUE)$auc
  }
  
  e <- (AUC - AUC_target)^2 + 1 * (prev - prev_target)^2 # this is what we optimise

  return(e)
}

# second function to optimise: using the intercept and slope, we optimise gamma (for 2-way interactions)
loss_function_interactions <- function(gamma){ # this is a single number
  AUC_previous <- AUC
  
  # Calculate probabilities
  linear <- optim_betas[1] +
    optim_betas[2]*rowSums(X) +
    gamma*rowSums(X_int)
  
  prob <- 1 / (1 + exp(-linear))
  true_y <- rbinom(n = example_n, size = 1, prob = prob)
  
  prev <- mean(true_y)
  if (prev == 0){
    AUC <- AUC_previous
  }
  else {
    AUC <<- roc(response = true_y, predictor = prob, quiet = TRUE)$auc
  }
  
  e <- (AUC - AUC_target_int)^2 + 1 * (prev - prev_target)^2 # function to optimise
  
  return(e)
}

# intercept
loss_function_intercept <- function(b0){ # this is a single number
  AUC_previous <- AUC
  
  # Calculate probabilities
  linear <- b0 +
    optim_betas[2]*rowSums(X) +
    optim_gamma*rowSums(X_int)
  
  prob <- 1 / (1 + exp(-linear))
  true_y <- rbinom(n = example_n, size = 1, prob = prob)
  
  prev <- mean(true_y)
  if (prev == 0){
    AUC <- AUC_previous
  }
  else {
    AUC <<- roc(response = true_y, predictor = prob, quiet = TRUE)$auc
  }
  
  e <- (AUC - AUC_target_int)^2 + 1 * (prev - prev_target)^2 # function to optimise
  
  return(e)
}

# function to make betas: calls the loss functions above
generate_betas <- function(example_n,       # a large number: we generate betas based on a large sample
                           n_predictors,    # a vector of the numbers of predictors we consider
                           prevalences,     # a vector of the numbers of prevalences we consider
                           initial_betas,   # a vector of length 2?
                           initial_gammas){ # a single number?
  AUC_target <<- 0.7
  AUC_target_int <<- 0.8
  
  betas_matrix <- matrix(NA, ncol = 2+3, nrow = length(prevalences)*length(n_predictors)) # empty matrix to put coefs in
  betas_matrix_row <- 0
  
  for (j in 1:length(n_predictors)){
    current_predictor <<- n_predictors[j]
    cat("predictor = ", current_predictor, "\n")
    
    # Specify means of the predictors
    mus <- c(rep(0, times = current_predictor)) 
    
    # Create covariance matrix of the predictors
    covariance_matrix <- matrix(0.2, nrow = current_predictor, ncol = current_predictor) # Covariances of 0.2
    diag(covariance_matrix) <- rep(1, times = current_predictor) # Replace diagonal of matrix with 1's (variances)
    
    # Generate predictors
    X <<- mvrnorm(n = example_n, mu = mus, Sigma = covariance_matrix)
    
    # TODO: make a function to generate interactions (instead of this, since it's repeated in many places)
    X_int <<- matrix(NA, nrow = example_n, ncol = current_predictor*0.25)
    interacted_with <- current_predictor*0.5
    
    for (int in 1:ncol(X_int)) {
      X_int[,int] <<- X[,int]*X[,interacted_with]
      interacted_with <- interacted_with + 1
    }
    
    # generate for the prevalences
    for (i in 1:length(prevalences)){
      betas_matrix_row <- betas_matrix_row + 1
      AUC <<- 0.5
      prev_target <<- prevalences[i]
      cat("prevalence = ", prev_target, "\n")
      
      initial_beta <- initial_betas[((j-1)*length(n_predictors) + i),]
      optim_betas <<- optim(par = initial_beta, 
                           fn = loss_function,
                           control = list(maxit = 1e4))$par
      AUC <<- 0.7 # TODO: is this correct?
      initial_gamma <- initial_gammas[((j-1)*length(n_predictors) + i)]
      optim_gamma <<- optim(par = initial_gamma,
                           fn = loss_function_interactions,
                           control = list(maxit = 1e4),
                           lower = 0,
                           upper = 1,
                           method = "Brent")$par
      AUC <<- 0.7 # TODO: is this correct?
      initial_b0 <- optim_betas[1]
      optim_intercept <- optim(par = initial_b0,
                           fn = loss_function_intercept,
                           control = list(maxit = 1e4),
                           lower = -5,
                           upper = 5,
                           method = "Brent")$par
      
      # store the optimal values and associated settings
      betas_matrix[betas_matrix_row, 3]   <- optim_intercept
      betas_matrix[betas_matrix_row, 4] <- optim_betas[2]
      betas_matrix[betas_matrix_row, 5]   <- optim_gamma
      betas_matrix[betas_matrix_row, 1]   <- current_predictor
      betas_matrix[betas_matrix_row, 2]   <- prev_target
    }
  }
  colnames(betas_matrix) <- c("n_predictor","prevalence","intercept", "beta", "gamma")
  return(betas_matrix)
}

mean_multiple_betas <- function(n_predictors,       # a vector of the numbers of predictors we're considering
                                prevalences,        # a vector of the numbers of prevalences we're considering
                                n_beta_repetitions, # a number of how many times we're fitting betas
                                example_n,          # a number of a large sample we're using
                                start_seed){        # a starting seed that will change over the iterations
  # A row per predictor/event fraction scenario
  betas_matrix_total <- matrix(0, nrow = length(n_predictors)*length(prevalences), ncol = 5)
  initial_betas <- matrix(0, nrow = length(n_predictors)*length(prevalences), ncol = 2)
  initial_gammas <- rep(0, length(n_predictors)*length(prevalences))
  betas_matrices <- list()
  
  for (k in 1:n_beta_repetitions){
    set.seed(start_seed + k)
    cat("seed = ", start_seed + k, "\n")
    betas_matrix <- generate_betas(
      example_n = example_n, n_predictors = n_predictors, prevalences = prevalences,
      initial_betas = initial_betas, initial_gammas = initial_gammas
    )
    initial_betas <- betas_matrix[,3:4] # change the initial value for the next generation of beta
    initial_gammas <- betas_matrix[,5]  # change the initial value for the next generation of gamma
    print(betas_matrix)
    betas_matrices[[k]] <- betas_matrix
    betas_matrix_total <- betas_matrix_total + betas_matrix
  }
  mean_betas_matrix <- betas_matrix_total / n_beta_repetitions
  
  return(list(mean_matrix = mean_betas_matrix, betas_matrices = betas_matrices))
}

# a function to check the coefficients give the required AUC & prevalences
validate_betas <- function(betas_matrix, example_n, n_predictors, prevalences){
  validated_AUC_prev_matrix <- matrix(NA, ncol = 2+3+2, nrow = length(prevalences)*length(n_predictors))
  
  for (j in 1:length(n_predictors)){
    current_predictor <- n_predictors[j]
    cat("predictor = ", current_predictor, "\n")
    
    # Specify means of the predictors
    mus <- c(rep(0, times = current_predictor)) 
    
    # Create covariance matrix of the predictors
    covariance_matrix <- matrix(0.2, ncol = current_predictor, nrow = current_predictor)
    diag(covariance_matrix) <- rep(1, times = current_predictor)  # Replace diagonal of matrix with 1's (variances)
    
    X_valid <- mvrnorm(n = example_n, mu = mus, Sigma = covariance_matrix)
    # TODO: make this in accordance with above
    X_int_valid <- matrix(NA, nrow = example_n, ncol = current_predictor*0.25)
    interacted_with <- current_predictor*0.5
    for (int in 1:ncol(X_int_valid)) {
      X_int_valid[,int] <- X_valid[,int]*X_valid[,interacted_with]
      interacted_with <- interacted_with + 1
    }
    
    for (i in 1:length(prevalences)){
      prev_target <- prevalences[i]
      cat("prevalence = ", prev_target, "\n")
      
      betas <- betas_matrix[((j-1)*length(n_predictors) + i),3:5]
      # Calculate probabilities
      linear <- betas[1] +
        betas[2]*rowSums(X_valid) +
        betas[3]*rowSums(X_int_valid)
        
      prob <- 1 / (1 + exp(-linear))
      true_y <- rbinom(n = example_n, size = 1, prob = prob)
      
      prev_valid <- mean(prob)
      if (prev_valid == 0){
        AUC_valid <- NA
      }
      else {
        AUC_valid <<- roc(response = true_y, predictor = prob, quiet = TRUE)$auc
      }
      validated_AUC_prev_matrix[((j-1)*length(n_predictors) + i),6:7] <- c(AUC_valid, prev_valid)
      validated_AUC_prev_matrix[((j-1)*length(n_predictors) + i),3:5] <- betas
      
      validated_AUC_prev_matrix[((j-1)*length(n_predictors) + i),1] <- current_predictor
      validated_AUC_prev_matrix[((j-1)*length(n_predictors) + i),2] <- prev_target
      
    }
  }
  
  colnames(validated_AUC_prev_matrix) <- c("n_predictor","prevalence",
                                           "intercept", "beta", "gamma",
                                           "validation AUC", "validation prevalence")
  return(validated_AUC_prev_matrix)
}

# as above, without the interactions - to check the AUC is lower
validate_betas_noint <- function(betas_matrix, example_n, n_predictors, prevalences){
  validated_AUC_prev_matrix <- matrix(NA, ncol = 2+3+2, nrow = length(prevalences)*length(n_predictors))
  
  for (j in 1:length(n_predictors)){
    current_predictor <- n_predictors[j]
    cat("predictor = ", current_predictor, "\n")
    
    # Specify means of the predictors
    mus <- c(rep(0, times = current_predictor)) 
    
    # Create covariance matrix of the predictors
    covariance_matrix <- matrix(0.2, ncol = current_predictor, nrow = current_predictor)
    diag(covariance_matrix) <- rep(1, times = current_predictor)  # Replace diagonal of matrix with 1's (variances)
    
    X_valid <- mvrnorm(n = example_n, mu = mus, Sigma = covariance_matrix)
    # TODO: make this in accordance with above
    X_int_valid <- matrix(NA, nrow = example_n, ncol = current_predictor*0.25)
    interacted_with <- current_predictor*0.5
    for (int in 1:ncol(X_int_valid)) {
      X_int_valid[,int] <- X_valid[,int]*X_valid[,interacted_with]
      interacted_with <- interacted_with + 1
    }
    
    for (i in 1:length(prevalences)){
      prev_target <- prevalences[i]
      cat("prevalence = ", prev_target, "\n")
      
      betas <- betas_matrix[((j-1)*length(n_predictors) + i),3:5]
      # Calculate probabilities
      linear <- betas[1] +
        betas[2]*rowSums(X_valid)
      
      prob <- 1 / (1 + exp(-linear))
      
      linear <- betas[1] +
        betas[2]*rowSums(X_valid) +
        betas[3]*rowSums(X_int_valid)
      true_prob <- 1 / (1 + exp(-linear))
      true_y <- rbinom(n = example_n, size = 1, prob = true_prob)
      
      prev_valid <- mean(true_prob)
      if (prev_valid == 0){
        AUC_valid <- NA
      }
      else {
        AUC_valid <<- roc(response = true_y, predictor = prob, quiet = TRUE)$auc
      }
      validated_AUC_prev_matrix[((j-1)*length(n_predictors) + i),6:7] <- c(AUC_valid, prev_valid)
      validated_AUC_prev_matrix[((j-1)*length(n_predictors) + i),3:5] <- betas
      
      validated_AUC_prev_matrix[((j-1)*length(n_predictors) + i),1] <- current_predictor
      validated_AUC_prev_matrix[((j-1)*length(n_predictors) + i),2] <- prev_target
      
    }
  }
  
  colnames(validated_AUC_prev_matrix) <- c("n_predictor","prevalence",
                                           "intercept", "beta", "gamma",
                                           "validation AUC", "validation prevalence")
  return(validated_AUC_prev_matrix)
}

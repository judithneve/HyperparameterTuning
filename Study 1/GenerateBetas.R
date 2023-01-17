# based on work by Zoe Dunias

#########################################
## large sample approximation of betas ##
#########################################

# function to optimise: get intercept & slope for a chosen AUC & prevalence
loss_function <- function(betas, X, X_int = NULL, interaction = FALSE, fixed_betas = NULL, AUC, AUC_target = 0.7, prev_target){
  AUC_previous <- AUC

  # Calculate probabilities
  if (interaction == FALSE) {
    linear <- betas[1] +
      betas[2]*rowSums(X)
  } else {
    linear <- betas[1] +
      fixed_betas*rowSums(X) +
      betas[2]*rowSums(X_int)
  }
  
  prob <- 1 / (1 + exp(-linear))
  true_y <- rbinom(n = example_n, size = 1, prob = prob)

  prev <- mean(true_y)
  if (prev == 0){
    AUC <- AUC_previous
  }
  else {
    AUC <- roc(response = true_y, predictor = prob, quiet = TRUE)$auc
  }
  
  e <- (AUC - AUC_target)^2 + (prev - prev_target)^2

  return(e)
}

# function to make betas: calls the loss function above
generate_betas <- function(example_n,       # a large number: we generate betas based on a large sample
                           n_predictors,    # a vector of the numbers of predictors we consider
                           prevalences,     # a vector of the numbers of prevalences we consider
                           initial_betas,   # a vector of length 3
                           AUC_target = 0.7,
                           AUC_target_int = 0.8){
  
  betas_matrix <- matrix(NA, ncol = 2+3, nrow = length(prevalences)*length(n_predictors)) # empty matrix to put coefs in
  betas_matrix_row <- 0
  
  for (j in 1:length(n_predictors)){
    current_predictor <- n_predictors[j]
    cat("predictor = ", current_predictor, "\n")
    
    # Specify means of the predictors
    mus <- c(rep(0, times = current_predictor)) 
    
    # Create covariance matrix of the predictors
    covariance_matrix <- matrix(0.2, nrow = current_predictor, ncol = current_predictor) # Covariances of 0.2
    diag(covariance_matrix) <- rep(1, times = current_predictor) # Replace diagonal of matrix with 1's (variances)
    
    # Generate predictors
    X <- mvrnorm(n = example_n, mu = mus, Sigma = covariance_matrix)
    X_int <- interaction_matrix(X)
    
    # generate for the prevalences
    for (i in 1:length(prevalences)){
      betas_matrix_row <- betas_matrix_row + 1
      AUC <- 0.5
      prev_target <- prevalences[i]
      cat("prevalence = ", prev_target, "\n")
      
      initial_beta <- initial_betas[((j-1)*length(n_predictors) + i),] # initial beta for the correct p-EF combo
      optim_betas <- optim(par = initial_beta[1:2], 
                           fn = loss_function,
                           control = list(maxit = 1e4),
                           AUC = AUC,
                           AUC_target = AUC_target,
                           prev_target = prev_target,
                           interaction = FALSE,
                           X = X
                           )$par
      initial_beta[1:2] <- optim_betas
      optim_gamma <- optim(par = initial_beta[c(1,3)],
                           fn = loss_function,
                           control = list(maxit = 1e4),
                           AUC = AUC,
                           AUC_target = AUC_target_int,
                           prev_target = prev_target,
                           interaction = TRUE,
                           fixed_betas = initial_beta[2],
                           X = X,
                           X_int = X_int
                           )$par
      
      # store the optimal values and associated settings
      betas_matrix[betas_matrix_row, 3] <- optim_gamma[1]
      betas_matrix[betas_matrix_row, 4] <- optim_betas[2]
      betas_matrix[betas_matrix_row, 5] <- optim_gamma[2]
      betas_matrix[betas_matrix_row, 1] <- current_predictor
      betas_matrix[betas_matrix_row, 2] <- prev_target
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
  initial_betas <- matrix(0, nrow = length(n_predictors)*length(prevalences), ncol = 3)
  # betas_matrices <- list()
  
  for (k in 1:n_beta_repetitions){
    set.seed(start_seed + k)
    cat("seed = ", start_seed + k, "\n")
    betas_matrix <- generate_betas(
      example_n = example_n, n_predictors = n_predictors, prevalences = prevalences,
      initial_betas = initial_betas# , initial_gammas = initial_gammas
    )
    initial_betas <- betas_matrix[,3:5] # change the initial value for the next generation of beta
    print(betas_matrix)
    # betas_matrices[[k]] <- betas_matrix
    betas_matrix_total <- betas_matrix_total + betas_matrix
  }
  mean_betas_matrix <- betas_matrix_total / n_beta_repetitions
  
  return(list(mean_matrix = mean_betas_matrix, betas_matrices = betas_matrices))
}

# a function to check the coefficients give the required AUC & prevalences
validate_betas <- function(betas_matrix, example_n, n_predictors, prevalences, target_AUC = c(0.7, 0.8)){
  validated_AUC_prev_matrix <- matrix(NA, ncol = 2+3+3, nrow = length(prevalences)*length(n_predictors))
  
  validation_row <- 0
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
    X_int_valid <- interaction_matrix(X_valid)
    
    for (i in 1:length(prevalences)){
      validation_row <- validation_row + 1
      prev_target <- prevalences[i]
      cat("prevalence = ", prev_target, "\n")
      
      betas <- betas_matrix[((j-1)*length(n_predictors) + i),3:5]
      # Calculate probabilities
      linear_int <- betas[1] +
        betas[2]*rowSums(X_valid) +
        betas[3]*rowSums(X_int_valid)
      linear <- betas[1] +
        betas[2]*rowSums(X_valid)
        
      prob_int <- 1 / (1 + exp(-linear_int))
      prob <- 1 / (1 + exp(-linear))
      true_y <- rbinom(n = example_n, size = 1, prob = prob_int)
      
      prev_valid <- mean(true_y)
      if (prev_valid == 0){
        AUC_valid <- NA
        AUC_valid_int <- NA
      }
      else {
        AUC_valid <- roc(response = true_y, predictor = prob, quiet = TRUE)$auc
        AUC_valid_int <- roc(response = true_y, predictor = prob_int, quiet = TRUE)$auc
      }
      validated_AUC_prev_matrix[validation_row,6:8] <- c(AUC_valid, AUC_valid_int, prev_valid)
      validated_AUC_prev_matrix[validation_row,3:5] <- betas
      
      validated_AUC_prev_matrix[validation_row,1] <- current_predictor
      validated_AUC_prev_matrix[validation_row,2] <- prev_target
      
    }
  }
  
  colnames(validated_AUC_prev_matrix) <- c("n_predictor","prevalence",
                                           "intercept", "beta", "gamma",
                                           "validation AUC (no interaction)", "validation AUC (interaction)",
                                           "validation prevalence")
  return(validated_AUC_prev_matrix)
}

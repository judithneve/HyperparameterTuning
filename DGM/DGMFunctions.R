# based on work by Zoe Dunias

# Create scenario info using a minimum sample size calculation based on Riley et al., 2020

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

loss_function <- function(betas, X, AUC, AUC_target = 0.7, AUC_target_int = 0.8, prev_target){
  AUC_previous <- AUC
  AUC_int_previous <- AUC
  
  # Calculate probabilities
  linear_int <- linear(betas, X)
  linear_comb <- linear(betas, X, interaction = FALSE)
  
  prob_int <- 1 / (1 + exp(-linear_int))
  prob <- 1 / (1 + exp(-linear_comb))
  true_y <- rbinom(n = example_n, size = 1, prob = prob_int)
  
  prev <- mean(true_y)
  if (prev == 0){
    AUC <- AUC_previous
    AUC_int <- AUC_int_previous
  }
  else {
    AUC <- roc(response = true_y, predictor = prob, quiet = TRUE)$auc
    AUC_int <- roc(response = true_y, predictor = prob_int, quiet = TRUE)$auc
  }
  
  e <- (AUC - AUC_target)^2 + (AUC_int - AUC_target_int)^2 + (prev - prev_target)^2
  
  return(e)
}

# function to make betas: calls the loss function above
generate_betas <- function(example_n,       # a large number: we generate betas based on a large sample
                           n_predictors,    # a vector of the numbers of predictors we consider
                           prevalences,     # a vector of the numbers of prevalences we consider
                           initial_betas,   # a matrix of betas
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
    
    # generate for the prevalences
    for (i in 1:length(prevalences)){
      betas_matrix_row <- betas_matrix_row + 1
      AUC <- 0.5
      prev_target <- prevalences[i]
      cat("prevalence = ", prev_target, "\n")
      
      initial_beta <- initial_betas[((j-1)*length(n_predictors) + i),] # initial beta for the correct p-EF combo
      optim_betas <- optim(par = initial_beta, 
                           fn = loss_function,
                           control = list(maxit = 1e4),
                           # method = "L-BFGS-B", # constrain optimisation
                           # lower = c(-5, -1, -1),
                           # upper = c(5, 1, 1),
                           AUC = AUC,
                           AUC_target = AUC_target,
                           AUC_target_int = AUC_target_int,
                           prev_target = prev_target,
                           X = X
      )$par
      
      # store the optimal values and associated settings
      betas_matrix[betas_matrix_row, 3:5] <- optim_betas
      betas_matrix[betas_matrix_row, 1] <- current_predictor
      betas_matrix[betas_matrix_row, 2] <- prev_target
    }
  }
  colnames(betas_matrix) <- c("n_predictor","prevalence","intercept", "beta", "gamma")
  return(betas_matrix)
}

gen_multiple_betas <- function(n_predictors,       # a vector of the numbers of predictors we're considering
                               prevalences,        # a vector of the numbers of prevalences we're considering
                               n_beta_repetitions, # a number of how many times we're fitting betas
                               example_n,          # a number of a large sample we're using
                               start_seed){        # a starting seed that will change over the iterations
  # A row per predictor/event fraction scenario
  initial_betas <- matrix(0, nrow = length(n_predictors)*length(prevalences), ncol = 3)
  betas_matrices <- list()
  
  for (k in 1:n_beta_repetitions){
    set.seed(start_seed + k)
    cat("seed = ", start_seed + k, "\n")
    betas_matrix <- generate_betas(
      example_n = example_n, n_predictors = n_predictors, prevalences = prevalences,
      initial_betas = initial_betas# , initial_gammas = initial_gammas
    )
    initial_betas <- betas_matrix[,3:5] # change the initial value for the next generation of beta
    print(betas_matrix)
    betas_matrices[[k]] <- betas_matrix
  }
  
  return(betas_matrices)
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
    
    for (i in 1:length(prevalences)){
      validation_row <- validation_row + 1
      prev_target <- prevalences[i]
      cat("prevalence = ", prev_target, "\n")
      
      betas <- betas_matrix[validation_row,3:5]
      # Calculate probabilities
      linear_int <- linear(betas, X_valid)
      linear_comb <- linear(betas, X_valid, interaction = FALSE)
        
      prob_int <- 1 / (1 + exp(-linear_int))
      prob <- 1 / (1 + exp(-linear_comb))
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

recover_betas <- function(
    betas_matrix,        # matrix with column titles n_predictor, prevalence, intercept, beta, gamma
    example_n = 1e5
) {
  out <- matrix(NA, nrow = 5*nrow(betas_matrix), ncol = 7)
  row_out <- 0
  for (i in 1:nrow(betas_matrix)) {
    print(betas_matrix[i,])
    
    p_temp  <- betas_matrix[i,"n_predictor"]
    EF_temp <- betas_matrix[i,"prevalence"]
    
    dat_temp <- simulate_data(example_n, p_temp, EF_temp, betas_matrix[i,3:5])
    
    form <- ifelse(p_temp == 8,
                   "Y ~ . + X1:X3 + X2:X4",
                   "Y ~ . + X1:X5 + X2:X6 + X3:X7 + X4:X8")
    target_beta <- c(betas_matrix[i,"intercept"],
                     rep(betas_matrix[i,"beta"], p_temp/2),
                     rep(2*betas_matrix[i,"beta"], p_temp/4),
                     rep(0, p_temp/4),
                     rep(betas_matrix[i,"gamma"], p_temp/4))
    mod <- glm(form, family = binomial(), data = dat_temp)
    diff_beta <- abs(target_beta - coef(mod))
    
    out[(row_out+1):(row_out+5),1] <- p_temp
    out[(row_out+1):(row_out+5),2] <- EF_temp
    out[(row_out+1):(row_out+5),3] <- mean(as.numeric(dat_temp$Y) - 1)
    out[(row_out+1):(row_out+5),5] <- unique(target_beta)
    out[(row_out+1):(row_out+5),6] <- c(coef(mod)[1],
                                        coef(mod)[1+which.max(diff_beta[2:(1+p_temp/2)])],
                                        coef(mod)[1+p_temp/2+which.max(diff_beta[(2+p_temp/2):(1+p_temp/2+p_temp/4)])],
                                        coef(mod)[1+p_temp*3/4+which.max(diff_beta[(2+p_temp/2+p_temp/4):(p_temp+1)])],
                                        coef(mod)[1+p_temp+which.max(diff_beta[(2+p_temp):(1+p_temp+p_temp/4)])])
    out[(row_out+1):(row_out+5),7] <- abs(out[(row_out+1):(row_out+5),5] - out[(row_out+1):(row_out+5),6])
    
    row_out <- row_out + 5
  }
  out[,4] <- rep(c("intercept", "main effect", "2*main effect", "no effect", "interaction"), nrow(betas_matrix))
  colnames(out) <- c("n_pred", "prevalence", "obtained_prevalence", "effect", "true_effect", "max_dev_effect", "abs_diff")
  return(out)
}

interaction_matrix <- function(X, n_pred = ncol(X), prop_int = 0.25) {
  int_term_2 <- n_pred/4
  n_int      <- n_pred*prop_int
  
  X_int      <- matrix(NA, nrow = nrow(X), ncol = n_int)
  
  for (i in 1:n_int) {
    X_int[,i] <- X[,i]*X[,i+int_term_2]
  }
  
  return(X_int)
}

linear <- function(betas, X, interaction = TRUE) {
  X_int <- interaction_matrix(X)
  if (interaction) {
    out <- betas[1] +
      betas[2]*rowSums(X[,1:(ncol(X)/2)]) +
      2*betas[2]*rowSums(X[,(ncol(X)/2+1):(ncol(X)*3/4)]) +
      betas[3]*rowSums(X_int)
  } else {
    out <- betas[1] +
      betas[2]*rowSums(X[,1:(ncol(X)/2)]) +
      2*betas[2]*rowSums(X[,(ncol(X)/2+1):(ncol(X)*3/4)])
  }
  return(out)
}

simulate_data <- function(n, p, EF, betas) {
  dat <- matrix(
    NA,
    nrow = n,
    ncol = p + 1
  ) %>% 
    as.data.frame()
  colnames(dat) <- c(paste0("X", 1:p), "Y")
  
  X <- mvrnorm(
    n = n,
    mu = rep(0, p),
    Sigma = matrix(0.2, nrow = p, ncol = p) + diag(0.8, p)
  )
  
  dat[,1:p] <- X
  
  linear_comb <- linear(betas, X)
  prob <- 1 / (1 + exp(-linear_comb))
  dat[,"Y"] <- rbinom(n, 1, prob) %>%
    factor(levels = c(0, 1),
           labels = c("neg", "pos"))
  
  return(dat)
}

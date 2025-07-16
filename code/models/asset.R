# models/asset.R
# Asset price simulation

source("models/volatility.R")

# Generate single asset price path
simulate_asset_path <- function(rho, a, b, r, J, coeffs, config) {
  N <- config$simulation$N
  dt <- config$simulation$dt
  S0 <- config$simulation$S0
  c <- coeffs$c
  k <- coeffs$k
  
  # Generate correlated Brownian motions
  BM <- generate_correlated_brownian(N, dt, rho)
  
  # Simulate OU processes
  Z <- simulate_ou_processes(N, J, k, BM$dWV, dt)
  
  # Compute rough volatility
  crV <- compute_rough_volatility(Z, c, k, a, b)
  
  # Simulate logarithmic returns
  log_returns <- (r - 0.5 * crV[-length(crV)]^2) * dt + crV[-length(crV)] * BM$dWcA
  
  # Calculate logarithmic prices
  log_S <- cumsum(c(log(S0), log_returns))
  
  # Convert back to prices
  S <- exp(log_S)
  
  return( S )
}

# Generate multiple asset price paths
simulate_multiple_paths <- function(num_paths, rho, a, b, r, J, coeffs, config) {
  paths <- matrix(0, nrow = config$simulation$N + 1, ncol = num_paths)
  
  for (i in 1:num_paths) {
    paths[, i] <- simulate_asset_path(rho, a, b, r, J, coeffs, config)
  }
  
  return( paths )
}

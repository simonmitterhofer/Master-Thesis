# models/option_pricing.R
# Option pricing and payoff calculations

# Compute option payoffs
compute_call_payoff <- function(S, strikes) {
  pmax(S - strikes, 0)
}

compute_put_payoff <- function(S, strikes) {
  pmax(strikes - S, 0)
}

# Compute discounted payoff for single path
compute_discounted_payoffs <- function(asset_path, strikes, expirations, r, config) {
  dt <- config$simulation$dt
  
  num_expirations <- length(expirations)
  num_strikes <- length(strikes)
  
  call_payoffs <- matrix(0, nrow = num_expirations, ncol = num_strikes)
  put_payoffs <- matrix(0, nrow = num_expirations, ncol = num_strikes)
  
  for (i in seq_along(expirations)) {
    tt <- expirations[i]
    time_idx <- round(tt / dt) + 1
    
    # Ensure valid index
    time_idx <- min(time_idx, length(asset_path))
    
    spot_price <- asset_path[time_idx]
    
    # Compute both call and put discounted payoffs
    call_payoffs[i, ] <- compute_call_payoff(spot_price, strikes) * exp(-r * tt)
    put_payoffs[i, ] <- compute_put_payoff(spot_price, strikes) * exp(-r * tt)
  }
  
  # Set row and column names
  rownames(call_payoffs) <- paste0("T", expirations)
  colnames(call_payoffs) <- paste0("K", strikes)
  rownames(put_payoffs) <- paste0("T", expirations)
  colnames(put_payoffs) <- paste0("K", strikes)
  
  return(list(call = call_payoffs, put = put_payoffs))
}

# Price options using Monte Carlo
price_options_mc <- function(scenario, config, strikes_maturities) {
  H <- scenario$H
  rho <- scenario$rho
  gamma1 <- scenario$gamma1
  gamma2 <- scenario$gamma2
  a <- scenario$a
  b <- scenario$b
  r <- scenario$r
  J <- scenario$J
  strikes <- strikes_maturities$strikes
  expirations <- strikes_maturities$expirations
  
  num_strikes <- length(strikes)
  num_expirations <- length(expirations)
  num_paths <- config$simulation$num_paths
  
  # Set seed for reproducibility
  set.seed(config$simulation$seed)
  
  if (config$output$progress) {
    cat("Pricing options:\n    ", "H = ", H, ", rho = ", rho, ", gamma1 = ", gamma1, 
        ", gamma2 = ", gamma2, ", a = ", a, ", b = ", b, ", r = ", r, ", J = ", J, "\n", sep = "")
  }
  
  # Compute approximation coefficients
  coeffs <- compute_approximation_coeffs(J, gamma1, gamma2)
  
  # Check for computation errors
  if (any(is.na(coeffs$c)) || any(is.na(coeffs$k))) {
    warning("Invalid approximation coefficients")
    return(list(call = NULL, put = NULL))
  }
  
  # Initialize option value matrices
  call_values <- matrix(0, nrow = num_expirations, ncol = num_strikes)
  put_values <- matrix(0, nrow = num_expirations, ncol = num_strikes)
  
  # Monte Carlo simulation
  for (path in 1:num_paths) {
    # Generate single asset path
    asset_path <- simulate_asset_path(rho, a, b, r, J, coeffs, config)
    
    # Compute payoffs for this path
    payoffs <- compute_discounted_payoffs(asset_path, strikes, expirations, r, config)
    
    # Accumulate payoffs
    call_values <- call_values + payoffs$call
    put_values <- put_values + payoffs$put
    
    # Progress reporting
    if (config$output$progress && (path %% 1000 == 0)) {
      cat("  Generated", path, "of", num_paths, "paths\n")
    }
  }
  
  # Average over all paths to get option values
  call_values <- call_values / num_paths
  put_values <- put_values / num_paths
  
  return( list(call = call_values, put = put_values) )
}

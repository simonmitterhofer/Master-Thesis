# models/implied_volatility.R
# Implied volatility calculations and Black Scholes inversion

# Black-Scholes option pricing
bs_price <- function(S, K, r, tt, sigma, type = c("call", "put")) {
  type <- match.arg(type)
  
  if (tt <= 0 || sigma <= 0) return(NA)
  
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * tt) / (sigma * sqrt(tt))
  d2 <- d1 - sigma * sqrt(tt)
  
  if (type == "call") {
    S * pnorm(d1) - K * exp(-r * tt) * pnorm(d2)
  } else {
    K * exp(-r * tt) * pnorm(-d2) - S * pnorm(-d1)
  }
}

# Compute implied volatility with robust error handling
compute_implied_volatility <- function(price, S, K, r, tt, type = c("call", "put")) {
  type <- match.arg(type)
  
  # Check for valid inputs
  if (is.na(price) || price <= 0 || tt <= 0 || K <= 0 || S <= 0) return(NA)
  
  # Check for intrinsic value bounds
  intrinsic <- switch(type,
                      "call" = max(S - K * exp(-r * tt), 0),
                      "put" = max(K * exp(-r * tt) - S, 0))
  
  if (price < intrinsic * 0.999) return(NA)  # Small tolerance for numerical errors
  
  # Define objective function for root finding
  objective <- function(sigma) {
    tryCatch({
      theoretical_price <- bs_price(S, K, r, tt, sigma, type)
      if (is.na(theoretical_price)) return(Inf)
      return(theoretical_price - price)
    }, error = function(e) Inf)
  }
  
  # Use uniroot to find implied volatility
  tryCatch({
    result <- uniroot(objective, interval = c(1e-6, 10), tol = 1e-8)
    if (result$root > 0 && result$root < 10) {
      return(result$root)
    } else {
      return(NA)
    }
  }, error = function(e) {
    return(NA)
  })
}

# Compute implied volatility matrix for a given option type
compute_implied_vol_matrix <- function(option_values, r, config, option_type) {
  S0 <- config$simulation$S0
  
  # Extract maturities and strikes from matrix dimnames
  expirations <- as.numeric(sub("T", "", rownames(option_values)))
  strikes <- as.numeric(sub("K", "", colnames(option_values)))
  
  # Initialize implied volatility matrix
  implied_vol <- matrix(NA, nrow = nrow(option_values), ncol = ncol(option_values))
  
  # Compute implied volatility for each strike-maturity combination
  for (i in seq_along(expirations)) {
    for (j in seq_along(strikes)) {
      price <- option_values[i, j]
      tt <- expirations[i]
      K <- strikes[j]
      
      implied_vol[i, j] <- compute_implied_volatility(price, S0, K, r, tt, option_type)
    }
    
    if (config$output$progress && i %% 5 == 0) {
      cat("  Computed", option_type, "implied vol for", i, "of", length(expirations), "maturities\n")
    }
  }
  
  # Set row and column names
  rownames(implied_vol) <- rownames(option_values)
  colnames(implied_vol) <- colnames(option_values)
  
  return(implied_vol)
}

# Linearly interpolate the implied volatility surface
linear_interpolation <- function(iv_matrix, max_gap = 4) {
  library(zoo)
  nc <- ncol(iv_matrix)
  dn <- dimnames(iv_matrix)
  
  # Fill short gaps along columns (maturities)
  iv_matrix <- apply(iv_matrix, 2, function(c)
    na.approx(c, maxgap = max_gap, na.rm = FALSE, rule = 2))
  
  # Exclude empirically irrelevant regions
  iv_matrix[1, c(1:3, (nc-2):nc)] <- NA
  iv_matrix[2, c(1:2, (nc-1):nc)] <- NA
  iv_matrix[3, c(1, nc)] <- NA
  iv_matrix[4, c(1, nc)] <- NA
  iv_matrix[5, c(1, nc)] <- NA
  
  # Keep names
  dimnames(iv_matrix) <- dn
  
  return( iv_matrix )
}


# Compute implied volatility matrices for both call and put options
compute_implied_vol_matrices <- function(option_values, r, config) {
  
  if (is.null(option_values$call) || is.null(option_values$put)) {
    warning("Missing option values for call or put")
    return(list(call = NULL, put = NULL))
  }
  
  if (config$output$progress) {
    cat("Computing implied volatilities...\n")
  }
  
  # Compute implied volatilities for calls
  call_iv <- compute_implied_vol_matrix(option_values$call, r, config, "call")
  
  # Compute implied volatilities for puts
  put_iv <- compute_implied_vol_matrix(option_values$put, r, config, "put")
  
  return(list(call = call_iv, put = put_iv))
}

# Aggregate put-call implied volatility
aggregate_put_call_iv <- function(iv_matrices, type = c("mean", "combn")) {
  
  iv_matrix <- apply(simplify2array(iv_matrices), c(1, 2), mean, na.rm = TRUE)
  iv_matrix[is.nan(iv_matrix)] <- NA
  
  iv_matrix <- linear_interpolation(iv_matrix)
  
  if (type == "mean") {
    return( iv_matrix )
  } else if (type == "combn") {
    iv_matrix_c <- cbind(iv_matrices$put[,1:(atm_idx-1)], iv_matrix[,atm_idx], iv_matrices$call[,(atm_idx+1):n_strikes])
    return( iv_matrix )
  }
}

# Compute ATM skew
compute_atm_skew <- function(iv_matrix, config) {
  
  S0 <- config$simulation$S0
  strikes <- as.numeric(gsub("K", "", colnames(iv_matrix)))
  moneyness <- strikes / S0
  maturities <- as.numeric(gsub("T", "", rownames(iv_matrix)))
  atm_idx <- which.min(abs(moneyness - 1))
  
  atm_skew <- vapply(seq_along(maturities), function(i) {
    d_vol <- iv_matrix[i, atm_idx + 1] - iv_matrix[i, atm_idx - 1]
    d_mon <- moneyness[atm_idx+1] - moneyness[atm_idx-1]
    return(d_vol/d_mon)
  }, numeric(1))
  names(atm_skew) <- paste0("T", maturities)
  
  return( atm_skew )
}
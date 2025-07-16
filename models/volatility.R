# models/volatility.R
# Rough volatility model implementation

# Mixing measure
mu_measure <- function(x, g1, g2) {
  return( (1 + x)^(-g1) * x^(-g2) )
}

# Compute weights for Ornstein-Uhlenbeck processes
compute_ou_weights <- function(J, xi, g1, g2) {
  weights <- vapply(1:J, function(j) {
    integrate(mu_measure, lower = xi[j], upper = xi[j+1], g1 = g1, g2 = g2)$value
  }, numeric(1))
  return( weights )
}

# Compute mean reversion speeds for OU processes
compute_mean_reversion_speeds <- function(J, xi, g1, g2, weights) {
  numerators <- vapply(1:J, function(j) {
    integrate(function(x) x * mu_measure(x, g1, g2), lower = xi[j], upper = xi[j+1])$value
  }, numeric(1))
  return( numerators / weights )
}

# Compute approximation coefficients for OU processes
compute_approximation_coeffs <- function(J, gamma1, gamma2) {
  H <- gamma1 + gamma2 - 0.5
  q <- J^(4/J)
  xi <- J^(-2*H - 1) * q^(0:J)
  
  c <- compute_ou_weights(J, xi, gamma1, gamma2)
  k <- compute_mean_reversion_speeds(J, xi, gamma1, gamma2, c)
  
  return( list(c = c, k = k) )
}

# Generate correlated Brownian motion increments
generate_correlated_brownian <- function(N, dt, rho) {
  dWV <- rnorm(N, mean = 0, sd = sqrt(dt))
  dWA <- rnorm(N, mean = 0, sd = sqrt(dt))
  dWcA <- rho * dWV + sqrt(1 - rho^2) * dWA
  
  return( list(dWV = dWV, dWcA = dWcA) )
}

# Simulate Ornstein-Uhlenbeck processes
simulate_ou_processes <- function(N, J, k, dWV, dt) {
  Z <- matrix(0, nrow = N + 1, ncol = J)
  
  exp_kdt <- exp(-k * dt)
  sqrt_var <- sqrt((1 - exp(-2 * k * dt)) / (2 * k * dt))
  
  for (i in 1:N) {
    Z[i + 1, ] <- Z[i, ] * exp_kdt + sqrt_var * rep(dWV[i], J)
  }
  
  return( Z )
}

# Compute rough volatility process
compute_rough_volatility <- function(Z, c, k, a, b) {
  ltVar <- sum(outer(c, c, "*") / outer(k, k, "+"))
  WrV <- c(Z %*% c) / sqrt(ltVar)
  crV <- b * exp(a * WrV)
  
  return( crV )
}

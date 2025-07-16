# config.R
# Configuration and parameter management

create_config <- function() {
  config <- list(
    simulation = list(
      num_paths = 10000,  # Sample paths
      dt = 0.001,  # Time increment
      T0 = 0,  # Initial time
      TN = 2,  # Final time
      S0 = 100,  # Initial asset price
      seed = 1   # Random seed
    ),
    
    output = list(
      save_data = TRUE,
      output_dir = "SimulationData",
      progress = TRUE
    )
  )
  
  # Computed parameters
  config$simulation$N <- with(config$simulation, (TN - T0) / dt)
  
  return(config)
}

create_scenarios <- function() {
  scenarios <- list()
  
  # Simulation scenarios with parameters J, H, gamma2, rho, a
  scenarios <- expand.grid(
    a = 1,
    b = c(0.2, 0.25),
    r = c(0.02, 0.03),
    rho = -0.4,
    gamma2 = 0.3,
    H = c(0.1, 0.49),
    J = 20,
    
    # a = c(0.5, 0.7, 1, 1.4),  # Volatility scale parameter
    # b = c(0.2, 0.3),  # Volatility level parameter
    # r = c(0.02, 0.03, 0.04),  # Risk-free rate
    # rho = c(-0.5, -0.4, -0.3, -0.2),  # Correlation
    # gamma2 = c(0.10, 0.20, 0.30, 0.40),  # Persistence
    # H = c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30),  # Hurst parameter
    # J = c(10, 20, 30, 40, 50),  # Dimension of OU approximation
    stringsAsFactors = FALSE
  )
  
  # Add gamma1 = H - gamma2 + 0.5
  scenarios$gamma1 = scenarios$H - scenarios$gamma2 + 0.5
  
  # Reorder columns
  scenarios <- scenarios[, c("a", "b", "r", "rho", "gamma2", "gamma1", "H", "J")]
  
  # Convert to list of scenarios
  scenarios <- lapply(seq(nrow(scenarios)), function(i) scenarios[i,])
  scenarios <- scenarios[1:8]
  
  return(scenarios)
}

setup_strikes_maturities <- function(config) {
  S0 <- config$simulation$S0
  
  strikes <- S0 * c(0.9, 0.95, 0.975, 0.99, 0.995, 1.0, 1.005, 1.01, 1.025, 1.05, 1.1)
  
  expirations <- c(1, 2, 3, 5, 10, 15, 21, 42, 63, 84, 126, 252, 504, 756)/252
  expirations <- round(expirations[expirations <= config$simulation$TN], 3)
  
  return(list(strikes = strikes, expirations = expirations))
}

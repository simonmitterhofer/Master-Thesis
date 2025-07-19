# config.R
# Configuration and parameter management

create_config <- function() {
  config <- list(
    simulation = list(
      num_paths = 100000,  # Sample paths
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
    a = c(0.5, 0.7, 1.0, 1.4, 2),  # Volatility scale parameter
    b = c(0.10, 0.15, 0.20, 0.25),  # Volatility level parameter
    r = c(0.02),  # Risk-free rate
    rho = c(-0.5, -0.4, -0.3, -0.2),  # Correlation
    gamma2 = c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45),  # Persistence
    H = c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40),  # Hurst parameter
    J = c(20),  # Dimension of OU approximation
    stringsAsFactors = FALSE
  )
  
  # Add gamma1 = H - gamma2 + 0.5
  scenarios$gamma1 = scenarios$H - scenarios$gamma2 + 0.5
  
  # Reorder columns
  scenarios <- scenarios[, c("a", "b", "r", "rho", "gamma2", "gamma1", "H", "J")]
  
  # Convert to list of scenarios
  scenarios <- lapply(seq(nrow(scenarios)), function(i) scenarios[i,])

  return(scenarios)
}

setup_strikes_maturities <- function(config) {
  S0 <- config$simulation$S0
  
  strikes <- S0 * c(0.500, 0.700, 0.800, 0.900, 0.950, 0.975, 0.990, 0.995, 
                    1.000, 1.005, 1.010, 1.025, 1.050, 1.100, 1.200, 1.400, 2.000)
  
  expirations <- c(1, 2, 3, 5, 10, 15, 21, 42, 63, 84, 126, 252, 378, 504, 756)/252
  expirations <- round(expirations[expirations <= config$simulation$TN], 3)
  
  return(list(strikes = strikes, expirations = expirations))
}
# main.R
# Simulation for rough volatility option pricing

# Load packages
library(parallel)

# Source files
source("config.R")
source("models/volatility.R")
source("models/asset.R")
source("models/option_pricing.R")
source("models/implied_volatility.R")
source("utils/data_management.R")

# Setup
cat("Starting simulation...\n")
cat("Time:", as.character(Sys.time()), "\n")

# Get cluster cores
n_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK", unset = "4"))
cat("Using", n_cores, "cores\n")

# Create config - enhanced for cluster
config <- create_config()
config$simulation$num_paths <- 100000  # Adapt simulation paths
config$output$progress <- TRUE  # Adapt progress output

# Create scenarios and strikes/maturities
scenarios <- create_scenarios()
strikes_maturities <- setup_strikes_maturities(config)

cat("Configuration:\n")
cat("- Scenarios:", length(scenarios), "\n")
cat("- Paths per scenario:", config$simulation$num_paths, "\n")
cat("- Time horizon:", config$simulation$TN, "years\n\n")

est_runtime <- ceiling(length(scenarios) / n_cores) * config$simulation$num_paths * config$simulation$TN / 600000
if (est_runtime < 1) {
  cat("Estimated runtime:", round(est_runtime * 60), "minutes.\n\n")
} else if (est_runtime < 24) {
  cat("Estimated runtime:", round(est_runtime), "hour(s).\n\n")
} else {
  cat("Estimated runtime:", est_runtime %/% 24, "day(s) and", est_runtime %% 24, "hour(s).\n\n")
}

# Function to run scenarios in parallel
run_cluster_scenarios <- function(scenarios_list, config, strikes_maturities, n_cores) {
  n_scenarios <- length(scenarios_list)
  
  if (n_scenarios == 0) {
    cat("No scenarios to process\n")
    return(NULL)
  }
  
  cat("Processing", n_scenarios, "scenarios with", min(n_cores-1, n_scenarios, 64), "cores\n")
  
  # Setup parallel cluster
  cl <- makeCluster(min(n_cores-1, n_scenarios, 64))
  
  tryCatch({
    # Load functions on workers
    clusterEvalQ(cl, {
      source("config.R")
      source("models/volatility.R") 
      source("models/asset.R")
      source("models/option_pricing.R")
      source("models/implied_volatility.R")
    })
    
    # Export necessary objects
    clusterExport(cl, c("scenarios_list", "strikes_maturities", "config"), 
                  envir = environment())
    
    # Run parallel computation
    cat("Running parallel computation...\n")
    cat("Option pricing...\n")
    option_values <- parLapply(cl, seq_along(scenarios_list), function(i) {
      scenario <- scenarios_list[[i]]
      tryCatch({
        price_options_mc(
          scenario = scenario,
          config = config,
          strikes_maturities = strikes_maturities
        )
      }, error = function(e) {
        warning(paste("Option pricing scenario", i, "failed:", e$message))
        return( NULL )
      })
    })
    
    cat("Option pricing completed.\n")
    cat("Black-Scholes inversion...\n")
    
    implied_vols <- parLapply(cl, seq_along(scenarios_list), function(i) {
      scenario <- scenarios_list[[i]]
      if(!is.null(option_values[[i]])) {
        tryCatch({
          compute_implied_vol_matrices(
            option_values = option_values[[i]],
            r = scenario$r,
            config = config
          )
        }, error = function(e) {
          warning(paste("Implied volatility calculation scenario", i, "failed:", e$message))
          return( NULL )
        })
      } else {
        return( NULL )
      }
    })
    
    cat("Black-Scholes inversion completed.\n")
    
    stopCluster(cl)
    
    cat("Parallel computation completed.\n")
    
    # Create results structure
    results <- list(
      config = config,
      option_values = option_values,
      implied_vols = implied_vols,
      scenarios = scenarios_list
    )
    
    # Save results
    save_simulation_results(results, config)
    
    return( results )
    
  }, error = function(e) {
    cat("Cluster processing failed:", e$message, "\n")
    if (exists("cl")) try(stopCluster(cl), silent = TRUE)
    return( NULL )
  })
}

# Process options
cat("\n=== Processing Scenarios ===\n")
results <- run_cluster_scenarios(scenarios, config, strikes_maturities, n_cores)

# Summary
cat("\n=== Simulation Summary ===\n")
cat("Simulation completed at:", as.character(Sys.time()), "\n")

cat("Results saved in SimulationData\n")
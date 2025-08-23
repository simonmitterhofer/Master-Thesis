# analysis.R
# Analysis of rough volatility option pricing simulation results

# Setup -----------------------------------------------------

# rm(list = ls())   # Uncomment if you want to clear the workspace

# Working directory
setwd("~/QFin/Master Thesis/code")

# Load helper scripts
source("utils/data_management.R")
source("utils/filtering.R")
source("utils/plotting.R")
source("utils/calibration.R")
source("models/implied_volatility.R")

# Input & output directories
data_dir   <- "SimulationData"
output_dir <- "Analysis"

# Load Simulation Results -----------------------------------

results <- load_simulation_results(data_dir)
cat("=== SIMULATION ANALYSIS ===\n")

df <- do.call(rbind, results$scenarios)

parameter_values <- lapply(names(results$scenarios[[1]]), function(col) {
  unique(df[[col]])
})
names(parameter_values) <- names(results$scenarios[[1]])
parameter_values


# 1. Scenario Analysis --------------------------------------

cat("1. Running scenario analysis...\n")

scenario_analysis <- create_scenario_analysis(results)
parameter_summary <- create_parameter_summary(scenario_analysis)

# Ensure output directory exists
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Save results
write.table(
  scenario_analysis,
  file = file.path(output_dir, "analysis_scenarios.csv"),
  sep = ";", row.names = TRUE, col.names = NA
)

cat("   - Scenarios analyzed:        ", nrow(scenario_analysis), "\n")
cat("   - Scenarios passing all tests:", sum(scenario_analysis$AllPassed), "\n")


# 2. Market Calibration -------------------------------------

cat("\n2. Running market calibration...\n")

calibration_results <- run_calibration(
  results,
  scenario_analysis,
  c("2023-03-01", "2023-03-31")
)

market_iv <- calibration_results$market_iv_matrix
config    <- results$config

# Validation checks
check_smiles_convex(market_iv, config)
check_atm_skew_negative(market_iv, config)
check_atm_skew_increasing(market_iv, config)
check_atm_skew_power_law(market_iv, config)

# Plots
# save_plot_vol_surface(market_iv, NULL, config, "6.1 Market Surface")
# save_plot_vol_smiles(market_iv, NULL, config, "6.1 Market Surface")
# save_plot_atm_skew(market_iv, NULL, config, "6.1 Market Surface")
# save_plot_log_atm_skew(market_iv, NULL, config, "6.1 Market Surface")

# Add calibration metrics to scenario analysis
scenario_analysis$MSE  <- calibration_results$MSE
scenario_analysis$RMSE <- calibration_results$RMSE

# Show top results
cat("\nTop 20 Best Fitting Models:\n")
top_models <- head(
  scenario_analysis[order(scenario_analysis$MSE),
                    c(1:8, 14, ncol(scenario_analysis)-1, ncol(scenario_analysis))], 20)
top_passed <- head(
  scenario_analysis[scenario_analysis$AllPassed,
                    c(1:8, 14, ncol(scenario_analysis)-1, ncol(scenario_analysis))][
                      order(scenario_analysis$MSE[scenario_analysis$AllPassed]), ], 20)

print(top_models)
print(top_passed)

# Save calibrated results
write.table(
  scenario_analysis,
  file = file.path(output_dir, "analysis_with_calibration.csv"),
  sep = ";", row.names = TRUE, col.names = NA
)


# 3. Parameter Influence Analysis ---------------------------

cat("\n3. Creating parameter analysis...\n")

par(mfrow = c(1, 3))
for (param in names(parameter_summary)) {
  
  # RMSE vs parameter
  plot(scenario_analysis[[param]], scenario_analysis$RMSE,
       main = paste0("'", param, "' vs. RMSE"),
       xlab = paste0("Parameter '", param, "'"),
       ylab = "RMSE")
  
  # RMSE vs parameter (restricted to AllPassed)
  plot(scenario_analysis[scenario_analysis$AllPassed, ][[param]],
       scenario_analysis[scenario_analysis$AllPassed, ]$RMSE,
       main = paste0("'", param, "' vs. RMSE (AllPassed)"),
       xlab = paste0("Parameter '", param, "'"),
       ylab = "RMSE")
  
  # Pass/Fail vs parameter
  mosaicplot(scenario_analysis[[param]] ~ scenario_analysis$AllPassed,
             main  = paste0("'", param, "' vs. AllPassed"),
             xlab  = paste0("Parameter '", param, "'"),
             ylab  = "All Criteria Passed",
             color = TRUE,
             cex   = 0.8)
}
par(mfrow = c(1, 1))
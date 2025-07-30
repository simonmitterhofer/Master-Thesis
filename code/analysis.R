# analysis.R
# Analysis of rough volatility option pricing simulation results

rm(list = ls())

# Set working directory
setwd("~/Master Thesis/6_Rough Volatility Option Pricing Simulation")

# Source files
source("utils/data_management.R")
source("utils/filtering.R")
source("utils/plotting.R")
source("utils/calibration.R")
source("models/implied_volatility.R")

# Load simulation results
data_dir <- "SimulationData_2025-07-20"
# data_dir <- "SimulationData"
output_dir <- "Analysis"
results <- load_simulation_results(data_dir)

cat("=== SIMULATION ANALYSIS ===\n")

df <- do.call(rbind, results$scenarios)
parameter_values <- list()
for (col_name in names(results$scenarios[[1]])) {
  parameter_values[[col_name]] = unique(df[[col_name]])
}
parameter_values


# 1. Basic scenario analysis ----------------------------------------------

cat("1. Running scenario analysis...\n")
scenario_analysis <- create_scenario_analysis(results)
parameter_summary <- create_parameter_summary(scenario_analysis)

# Save basic analysis
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
# write.table(scenario_analysis, file = file.path(output_dir, "analysis_scenarios.csv"), sep = ";", row.names = TRUE, col.names = NA)
# save_filtered_scenarios(results, subset(scenario_analysis, a == 1 & b == 0.2), output_dir)

cat("   - Scenarios analyzed:", nrow(scenario_analysis), "\n")
cat("   - Scenarios passing all tests:", sum(scenario_analysis$AllPassed), "\n")


# 2. Statistical analysis of parameter influence --------------------------

cat("Creating parameter anaylsis....\n")

par(mfrow = c(2, 2))
for (param in names(parameter_summary)) {
  # plot(scenario_analysis[[param]], scenario_analysis$RMSE,
  #      main = paste0("'", param, "' vs. RMSE"),
  #      xlab = paste0("Parameter '", param, "'"),
  #      ylab = "RMSE")
  # 
  # plot(scenario_analysis[scenario_analysis$AllPassed,][[param]], 
  #      scenario_analysis[scenario_analysis$AllPassed,]$RMSE,
  #      main = paste0("'", param, "' vs. RMSE (AllPassed)"),
  #      xlab = paste0("Parameter '", param, "'"),
  #      ylab = "RMSE")
  
  mosaicplot(scenario_analysis[[param]] ~ scenario_analysis$AllPassed,
             main = paste0("'", param, "' vs. AllPassed"),
             xlab = paste0("Parameter '", param, "'"),
             ylab = "All Criteria Passed",
             color = TRUE,
             cex = 0.8)
}
par(mfrow = c(1, 1))


# 3. Ceteris paribus parameter analysis -----------------------------------

cat("\n4. Running ceteris paribus parameter analysis...\n")

summary(subset(scenario_analysis, b == 0.2 & H == 0.05))

scenario_analysis_filtered <- subset(scenario_analysis, AllPassed & a == 1 & H == 0.1)
parameter_summary_filtered <- create_parameter_summary(scenario_analysis_filtered)

# fixed_combinations_H <- create_parameter_analysis("H", results, subset(scenario_analysis, b == 0.2 & a == 1), parameter_summary)

# fixed_combinations_gamma2 <- create_parameter_analysis("gamma2", results, 
#                                                        subset(scenario_analysis, 
#                                                               H == 0.15 & a == 1 & b == 0.2 & gamma2 %in% c(0.05, 0.25, 0.45)), 
#                                                        parameter_summary)
# 
# # fixed_combinations_rho <- create_parameter_analysis("rho", results, subset(scenario_analysis, a == 1 & b == 0.2), parameter_summary)
# fixed_combinations_a <- create_parameter_analysis("a", results, subset(scenario_analysis, 
#                                                               H == 0.2 & a == 1 & b == 0.2 & gamma2 == 0.2, 
#                                                        parameter_summary)
# fixed_combinations_b <- create_parameter_analysis("b", results, subset(scenario_analysis, 
#                                                                        H == 0.15 & a == 1), parameter_summary)
# fixed_combinations_b <- create_parameter_analysis("b", results, scenario_analysis, parameter_summary)
# fixed_combinations_r <- create_parameter_analysis("r", results, subset(scenario_analysis, 
#                                                                        H == 0.2 & a == 1 & b == 0.2 & gamma2 == 0.2), 
#                                                                        parameter_summary)
# fixed_combinations_J <- create_parameter_analysis("J", results, 
#                                                   subset(scenario_analysis, 
#                                                   H == 0.2 & a == 1 & b == 0.2 & gamma2 == 0.2), 
#                                                   parameter_summary)

# fixed_combinations_H_gamma2 <- create_parameter_analysis(c("H", "gamma2"), results, scenario_analysis, parameter_summary)
# fixed_combinations_a_b <- create_parameter_analysis(c("a", "b"), results, scenario_analysis, parameter_summary)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved to:", output_dir, "\n")








# 4. Market calibration ---------------------------------------------------

cat("\n2. Running market calibration...\n")

# Run calibration
calibration_results <- run_calibration(results, scenario_analysis, c("2023-03-01", "2023-03-31"))
plot_vol_surface(calibration_results$market_iv_matrix, NULL, results$config)
plot_vol_smiles(calibration_results$market_iv_matrix, NULL, results$config)
plot_atm_skew(calibration_results$market_iv_matrix, NULL, results$config)
plot_log_atm_skew(calibration_results$market_iv_matrix, NULL, results$config)

save_plot_vol_surface(calibration_results$market_iv_matrix, NULL, results$config, "Analysis_Market")
save_plot_vol_smiles(calibration_results$market_iv_matrix, NULL, results$config, "Analysis_Market")
save_plot_atm_skew(calibration_results$market_iv_matrix, NULL, results$config, "Analysis_Market")
save_plot_log_atm_skew(calibration_results$market_iv_matrix, NULL, results$config, "Analysis_Market")

# Add calibration results to scenario analysis
scenario_analysis$MSE <- calibration_results$MSE
scenario_analysis$RMSE <- calibration_results$RMSE

# Find best scenario
best_scenario <- scenario_analysis[which.min(scenario_analysis$MSE), ]
best_i <- as.integer(rownames(best_scenario))

cat("   - Best scenario:", best_i, "with RMSE =", round(best_scenario$RMSE, 4), "\n")

# 3. Results visualization
cat("\n3. Creating visualizations...\n")

# Plot market vs best model comparison
plot_calibration_results(results, best_i, calibration_results$market_iv_matrix)

# Save top models
cat("\nTop 5 Best Fitting Models:\n")
top_models <- head(scenario_analysis[order(scenario_analysis$MSE), 
                                     c(1:8, 14, ncol(scenario_analysis)-1, ncol(scenario_analysis))], 10)
top_passed <- head(scenario_analysis[scenario_analysis$AllPassed,
                                     c(1:8, 14, ncol(scenario_analysis)-1, ncol(scenario_analysis))
                                     ][order(scenario_analysis$MSE[scenario_analysis$AllPassed]), ], 10)
print(top_models)
print(top_passed)

# Save final results
# write.table(scenario_analysis, file = file.path(output_dir, "analysis_with_calibration.csv"), sep = ";", row.names = TRUE, col.names = NA)





market_iv <- calibration_results$market_iv_matrix
config <- results$config
check_smiles_convex(market_iv, config)
check_atm_skew_negative(market_iv, config)
check_atm_skew_increasing(market_iv, config)
check_atm_skew_power_law(market_iv, config)
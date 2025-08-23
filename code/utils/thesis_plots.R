# thesis_plots.R
# Produce plots to be used in the thesis

# Setup -----------------------------------------------------

rm(list = ls())

# Working directory
setwd("~/QFin/Master Thesis/code")

# Load helper scripts
source("utils/data_management.R")
source("utils/filtering.R")
# source("utils/plotting.R")
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


# 5.2 Individual Parameter Effect ---------------------------

for (i in c(0.1, 0.4)) {
  save_filtered_scenarios(
    results,
    subset(scenario_analysis,
           H == i &
             gamma2 == 0.25 &
             rho == -0.4 &
             a == 0.7 &
             b == 0.25 &
             r == 0.02 &
             J == 20),
    "5.2 Individual Parameter Effects",
    filename = paste0("H=", sprintf("%.2f", i))
  )
}

for (i in c(0.1, 0.4)) {
  save_filtered_scenarios(
    results,
    subset(scenario_analysis,
           H == 0.25 &
             gamma2 == i &
             rho == -0.4 &
             a == 0.7 &
             b == 0.25 &
             r == 0.02 &
             J == 20),
    "5.2 Individual Parameter Effects",
    filename = paste0("gamma2=", sprintf("%.2f", i))
  )
}

for (i in c(-0.6, -0.2)) {
  save_filtered_scenarios(
    results,
    subset(scenario_analysis,
           H == 0.25 &
             gamma2 == 0.25 &
             rho == i &
             a == 0.7 &
             b == 0.25 &
             r == 0.02 &
             J == 20),
    "5.2 Individual Parameter Effects",
    filename = paste0("rho=", sprintf("%.1f", i))
  )
}

for (i in c(0.5, 1)) {
  save_filtered_scenarios(
    results,
    subset(scenario_analysis,
           H == 0.25 &
             gamma2 == 0.25 &
             rho == -0.4 &
             a == i &
             b == 0.25 &
             r == 0.02 &
             J == 20),
    "5.2 Individual Parameter Effects",
    filename = paste0("a=", sprintf("%.1f", i))
  )
}

for (i in c(0.1, 0.4)) {
  save_filtered_scenarios(
    results,
    subset(scenario_analysis,
           H == 0.25 &
             gamma2 == 0.25 &
             rho == -0.4 &
             a == 0.7 &
             b == i &
             r == 0.02 &
             J == 20),
    "5.2 Individual Parameter Effects",
    filename = paste0("b=", sprintf("%.1f", i))
  )
}

for (i in c(0.01, 0.02, 0.05)) {
  save_filtered_scenarios(
    results,
    subset(scenario_analysis,
           H == 0.25 &
             gamma2 == 0.25 &
             rho == -0.4 &
             a == 0.7 &
             b == 0.25 &
             r == i &
             J == 20),
    "5.2 Individual Parameter Effects",
    filename = paste0("r=", sprintf("%.2f", i))
  )
}

for (i in c(2, 5, 10, 20, 50)) {
  save_filtered_scenarios(
    results,
    subset(scenario_analysis,
           H == 0.25 &
             gamma2 == 0.25 &
             rho == -0.4 &
             a == 0.7 &
             b == 0.25 &
             r == 0.02 &
             J == i),
    "5.2 Individual Parameter Effects",
    filename = paste0("J=", sprintf("%.0f", i))
  )
}


# 5.4 Optimal Parameter Ranges ------------------------------

save_filtered_scenarios(
  results,
  subset(scenario_analysis,
         H == 0.25 &
           gamma2 == 0.25 &
           rho == -0.4 &
           a == 0.7 &
           b == 0.25 &
           r == 0.02 &
           J == 20),
  "5.4 Optimal Parameter Ranges",
  filename = "optimal_example"
)


# 6.1 Empirical Volatility Surface --------------------------

calibration_results <- run_calibration(
  results, scenario_analysis, c("2023-03-01", "2023-03-31")
)

save_plot_vol_surface(calibration_results$market_iv_matrix, NULL, results$config, "6.1 Market Surface")
save_plot_vol_smiles(calibration_results$market_iv_matrix, NULL, results$config, "6.1 Market Surface")
save_plot_atm_skew(calibration_results$market_iv_matrix, NULL, results$config, "6.1 Market Surface")
save_plot_log_atm_skew(calibration_results$market_iv_matrix, NULL, results$config, "6.1 Market Surface")


# 6.2 Best Fit ----------------------------------------------

save_filtered_scenarios(
  results,
  subset(scenario_analysis,
         H == 0.05 &
           gamma2 == 0.20 &
           rho == -0.7 &
           a == 1.0 &
           b == 0.20 &
           r == 0.02 &
           J == 20),
  "6.2 Best Fit Surface",
  filename = "best_fit1"
)

save_filtered_scenarios(
  results,
  subset(scenario_analysis,
         H == 0.10 &
           gamma2 == 0.15 &
           rho == -0.7 &
           a == 1.0 &
           b == 0.20 &
           r == 0.02 &
           J == 20),
  "6.2 Best Fit Surface",
  filename = "best_fit2"
)


# A.1 Market April 2023 -------------------------------------

# calibration_results2 <- run_calibration(
#   results, scenario_analysis, c("2023-04-01", "2023-04-30")
# )
# 
# save_plot_vol_surface(calibration_results2$market_iv_matrix, NULL, results$config, "A.1 Market Surface")
# save_plot_vol_smiles(calibration_results2$market_iv_matrix, NULL, results$config, "A.1 Market Surface")
# save_plot_atm_skew(calibration_results2$market_iv_matrix, NULL, results$config, "A.1 Market Surface")
# save_plot_log_atm_skew(calibration_results2$market_iv_matrix, NULL, results$config, "A.1 Market Surface")

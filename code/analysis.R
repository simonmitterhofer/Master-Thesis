# analysis.R
# Analysis of rough volatility option pricing simulation results

rm(list = ls())

# Source files
#source("models/implied_volatility.R")
source("utils/filtering.R")
source("utils/plotting.R")
source("utils/data_management.R")

# Load results
data_dir <- "SimulationData_2025-07-16"
output_dir <- "Analysis"

results <- load_simulation_results(data_dir)

scenario_analysis <- create_scenario_analysis(results)
parameter_summary <- create_parameter_summary(scenario_analysis)

save_filtered_scenarios(results, scenario_analysis, output_dir)

write.table(scenario_analysis, file = file.path(output_dir, "analysis_scenarios.csv"), sep = ";", row.names = TRUE, col.names = NA)

fixed_combinations_H_gamma1 <- create_parameter_analysis("H", results, scenario_analysis, parameter_summary)
fixed_combinations_gamma1_gamma2 <- create_parameter_analysis("gamma2", results, scenario_analysis, parameter_summary)
fixed_combinations_rho <- create_parameter_analysis("rho", results, scenario_analysis, parameter_summary)
fixed_combinations_a <- create_parameter_analysis("a", results, scenario_analysis, parameter_summary)
fixed_combinations_b <- create_parameter_analysis("b", results, scenario_analysis, parameter_summary)
fixed_combinations_r <- create_parameter_analysis("r", results, scenario_analysis, parameter_summary)
fixed_combinations_J <- create_parameter_analysis("J", results, scenario_analysis, parameter_summary)

fixed_combinations_H_gamma2 <- create_parameter_analysis(c("H", "gamma2"), results, scenario_analysis, parameter_summary)
fixed_combinations_a_b <- create_parameter_analysis(c("a", "b"), results, scenario_analysis, parameter_summary)
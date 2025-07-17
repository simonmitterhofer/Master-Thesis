# utils/data_management.R
# Data saving and loading utilities

source("utils/plotting.R")
source("utils/filtering.R")

# Ensure output directory exists
ensure_output_dir <- function(config) {
  output_dir <- config$output$output_dir
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("\nCreated output directory:", output_dir, "\n")
  }
}

# Save simulation results
save_simulation_results <- function(results, config, timestamp = Sys.Date()) {
  if (!config$output$save_data) return(NULL)
  
  ensure_output_dir(config)
  
  filename <- file.path(config$output$output_dir, 
                        paste0("results_", timestamp, ".rds"))
  saveRDS(results, filename)
  cat("\nSaved results to:", filename, "\n")
  filename
}

# Save simulation results
load_simulation_results <- function(data_dir) {
  result_files <- list.files(data_dir, pattern = "results.*\\.rds", full.names = TRUE)
  results <- readRDS(result_files[1])
  
  return( results )
}

# Save plot of implied volatility surface
save_plot_vol_surface <- function(iv_matrix, scenario, config,
                                  output_dir = NULL, filename = NULL) {
  if ( is.null(filename) ) {
    filename = paste0("scenario", rownames(scenario), "_",
                      paste(names(scenario), round(scenario, 2), sep = "=", collapse = "_"),
                      "_iv_surface.png")
  }
  if ( filename == "rev" ) {
    filename = paste0("iv_surf_S", rownames(scenario), "_",
                      paste(names(scenario), round(scenario, 2), sep = "=", collapse = "_"),
                      ".png")
  }
  
  if ( !dir.exists(output_dir) ) dir.create(output_dir, recursive = TRUE)
  
  png(file.path(output_dir, filename), width = 800, height = 600, res = 100)
  plot_vol_surface(iv_matrix, scenario, config)
  dev.off()
}

# Save plot of volatility smiles
save_plot_vol_smiles <- function(iv_matrix, scenario, config,
                                 output_dir = NULL, filename = NULL) {
  if ( is.null(filename) ) {
    filename = paste0("scenario", rownames(scenario), "_",
                      paste(names(scenario), round(scenario, 2), sep = "=", collapse = "_"),
                      "_iv_smiles.png")
  }
  if ( filename == "rev" ) {
    filename = paste0("iv_smiles_S", rownames(scenario), "_",
                      paste(names(scenario), round(scenario, 2), sep = "=", collapse = "_"),
                      ".png")
  }
  
  if ( !dir.exists(output_dir) ) dir.create(output_dir, recursive = TRUE)
  
  png(file.path(output_dir, filename), width = 800, height = 600, res = 100)
  plot_vol_smiles(iv_matrix, scenario, config)
  dev.off()
}

# Save plot of ATM-skew term structure
save_plot_atm_skew <- function(iv_matrix, scenario, config,
                               output_dir = NULL, filename = NULL) {
  if ( is.null(filename) ) {
    filename = paste0("scenario", rownames(scenario), "_",
                      paste(names(scenario), round(scenario, 2), sep = "=", collapse = "_"),
                      "_atm_skew.png")
  }
  if ( filename == "rev" ) {
    filename = paste0("skew_S", rownames(scenario), "_",
                      paste(names(scenario), round(scenario, 2), sep = "=", collapse = "_"),
                      ".png")
  }
  
  if ( !dir.exists(output_dir) ) dir.create(output_dir, recursive = TRUE)
  
  png(file.path(output_dir, filename), width = 800, height = 600, res = 100)
  plot_atm_skew(iv_matrix, scenario, config)
  dev.off()
}

# Save plot of ATM-skew term structure
save_plot_log_atm_skew <- function(iv_matrix, scenario, config, 
                                   output_dir = NULL, filename = NULL) {
  if ( is.null(filename) ) {
    filename = paste0("scenario", rownames(scenario), "_",
                      paste(names(scenario), round(scenario, 2), sep = "=", collapse = "_"),
                      "_atm_skew_log.png")
  }
  if ( filename == "rev" ) {
    filename = paste0("skew_log_S", rownames(scenario), "_",
                      paste(names(scenario), round(scenario, 2), sep = "=", collapse = "_"),
                      ".png")
  }
  
  if ( !dir.exists(output_dir) ) dir.create(output_dir, recursive = TRUE)
  
  png(file.path(output_dir, filename), width = 800, height = 600, res = 100)
  plot_log_atm_skew(iv_matrix, scenario, config)
  dev.off()
}

# Save plots of filtered scenarios
save_filtered_scenarios <- function(results, analysis_df,
                                    output_dir = "SimulationAnalysis",
                                    filename = NULL,
                                    # Plot type controls
                                    plot_vol_surface = TRUE,
                                    plot_vol_smiles = TRUE,
                                    plot_atm_skew = TRUE,
                                    plot_log_atm_skew = TRUE,
                                    # Filter settings
                                    filter_put_call_parity = TRUE,
                                    filter_smile_convex = TRUE,
                                    filter_skew_negative = TRUE,
                                    filter_slope_increasing = TRUE,
                                    filter_skew_powerlaw_fit = TRUE) {
  config <- results$config
  iv_matrices <- results$implied_vols
  
  if (filter_put_call_parity) analysis_df <- analysis_df[analysis_df$PutCallParity, ]
  if (filter_smile_convex) analysis_df <- analysis_df[analysis_df$SmileConvex, ]
  if (filter_skew_negative) analysis_df <- analysis_df[analysis_df$SkewNegative, ]
  if (filter_slope_increasing) analysis_df <- analysis_df[analysis_df$SkewIncreasing, ]
  if (filter_skew_powerlaw_fit) analysis_df <- analysis_df[analysis_df$SkewPowerLawFit, ]
  
  if (nrow(analysis_df) > 0) {
    for (i in seq(nrow(analysis_df))) {
      
      scenario <- analysis_df[i, 1:8]
      scenario_idx <- as.integer(rownames(scenario))
      scenario <- data.frame(lapply(lapply(scenario, as.character), as.numeric))
      rownames(scenario) <- scenario_idx
      iv_matrix <- aggregate_put_call_iv(iv_matrices[[scenario_idx]], "mean")
      
      if (plot_vol_surface) save_plot_vol_surface(iv_matrix, scenario, config, output_dir, filename)
      if (plot_vol_smiles) save_plot_vol_smiles(iv_matrix, scenario, config, output_dir, filename)
      if (plot_atm_skew) save_plot_atm_skew(iv_matrix, scenario, config, output_dir, filename)
      if (plot_log_atm_skew) save_plot_log_atm_skew(iv_matrix, scenario, config, output_dir, filename)
    }
  }
}

# Save plots for ceteris paribus parameter analysis
create_parameter_analysis <- function(variable_pars, results, analysis_df, summary_df,
                                      output_dir = "Analysis") {
  
  params = c("H", "gamma1", "gamma2", "rho", "a", "b", "r", "J")
  
  if(sum(c("H", "gamma1", "gamma2") %in% variable_pars) == 1) {
    if ("gamma1" %in% variable_pars) variable_pars <- c(variable_pars, "H")
    else variable_pars <- c(variable_pars, "gamma1")
  }
  fixed_pars <- setdiff(params, variable_pars)
  variable_pars <- setdiff(params, fixed_pars)
  
  if (length(variable_pars) > 1) output_dir = "Analysis"
  output_dir <- paste(output_dir, "_", paste(variable_pars, collapse = "_"), sep = "")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  fixed_pars_combinations <- unique(analysis_df[, fixed_pars, drop = FALSE])
  fixed_combination_summary <- list()
  
  for (param in variable_pars) {
    data <- cbind(rownames(summary_df[[param]]), summary_df[[param]])
    names(data)[1] <- param
    write.table(data, sep = ";", row.names = FALSE, col.names = TRUE,
                file = file.path(output_dir, paste0("analysis_passing_rates_parameter_", param, ".csv")))
  }
  
  for (i in seq(nrow(fixed_pars_combinations))) {
    
    fixed_combination <- data.frame(lapply(lapply(fixed_pars_combinations[i, ], as.character), as.numeric))
    
    subfolder <- paste(names(fixed_combination), fixed_combination, sep = "=", collapse = "_")
    
    conditions_list <- lapply(fixed_pars, function(fixed_par) {
      analysis_df[[fixed_par]] == fixed_combination[[fixed_par]]
    })
    
    filtered_analysis <- analysis_df[Reduce("&", conditions_list), ]
    
    fixed_combination_summary[[subfolder]] <- filtered_analysis
    
    save_filtered_scenarios(results, filtered_analysis,
                            file.path(output_dir, subfolder),
                            filename = "rev",
                            # Plot type controls
                            plot_vol_surface = TRUE,
                            plot_vol_smiles = TRUE,
                            plot_atm_skew = TRUE,
                            plot_log_atm_skew = TRUE,
                            # Filter settings
                            filter_put_call_parity = TRUE,
                            filter_smile_convex = TRUE,
                            filter_skew_negative = TRUE,
                            filter_slope_increasing = TRUE,
                            filter_skew_powerlaw_fit = TRUE)
  }
  return(fixed_combination_summary)
}
# utils/calibration.R
# Market calibration functions (separate file)

# Load required packages for calibration
library(DBI)
library(RPostgres)
library(dplyr)
library(akima)

# Main calibration function
run_calibration <- function(results, scenario_analysis, period) {
  
  start_period <- min(as.Date(period))
  end_period <- max(as.Date(period))
  
  cat("   - Downloading market data...\n")
  market_data <- download_sp500_market_data(start_date = start_period, end_date = end_period)
  
  cat("   - Processing market data...\n")
  market_iv_matrix <- process_market_data(market_data, results$config)
  
  cat("   - Computing calibration errors...\n")
  calibration_errors <- compute_calibration_errors(results, scenario_analysis, market_iv_matrix)
  
  return(list(
    MSE = calibration_errors$MSE,
    RMSE = calibration_errors$RMSE,
    market_iv_matrix = market_iv_matrix,
    market_data = market_data
  ))
}

# Download market data from WRDS
download_sp500_market_data <- function(start_date, end_date) {
  # WRDS connection
  readRenviron(".Renviron")
  
  cat(Sys.getenv("WRDS_USER"), "\n")
  cat(Sys.getenv("WRDS_PASSWORD"), "\n")
  
  con <- dbConnect(RPostgres::Postgres(),
                   host = "wrds-pgdata.wharton.upenn.edu",
                   port = 9737,
                   dbname = "wrds",
                   user = Sys.getenv("WRDS_USER"),
                   password = Sys.getenv("WRDS_PASSWORD"),
                   sslmode = "require")
  
  # Options query
  query_options <- paste0("
  SELECT secid, date, cp_flag, strike_price, exdate,
         (exdate - date) AS days_to_expiry, impl_volatility
  FROM optionm.opprcd2023
  WHERE date BETWEEN '", start_date, "' AND '", end_date, "'
    AND secid = 108105
    AND impl_volatility IS NOT NULL
    AND impl_volatility > 0
    AND exdate > date
    AND strike_price > 0
  ORDER BY date, exdate, strike_price")
  
  options_data <- dbGetQuery(con, query_options)
  
  # Spot price query
  query_spot <- paste0("
  SELECT date, Open, Close
  FROM optionm.secprd
  WHERE date BETWEEN '", start_date, "' AND '", end_date, "'
    AND secid = 108105")
  
  spot_data <- dbGetQuery(con, query_spot)
  
  # Merge and clean up
  options_data <- merge(options_data, spot_data, by = "date", all.x = TRUE)
  dbDisconnect(con)
  
  return(options_data)
}

# Process market data and create IV matrix
process_market_data <- function(options_data, config) {
  
  # Data processing
  options_data$days_to_expiry <- as.numeric(options_data$days_to_expiry)
  options_data$maturity <- options_data$days_to_expiry / 365
  options_data$strike_price <- options_data$strike_price / 1000
  options_data$moneyness <- options_data$strike_price / options_data$close
  options_data$log_moneyness <- log(options_data$moneyness)
  
  # Clean data
  options_data_clean <- options_data %>%
    filter(
      impl_volatility > 0,
      impl_volatility <= 1,
      log_moneyness > -1.2,
      log_moneyness <= 1.2,
      maturity >= 0,
      maturity <= 4
    ) %>%
    na.omit()
  
  cat("Observations:", sum(!is.na(options_data_clean$impl_volatility)), "\n")
  cat("Maturity:", range(options_data_clean$maturity), "\n")
  cat("Moneyness:", exp(range(options_data_clean$log_moneyness)), "\n")
  
  # Binning and averaging
  options_data_clean <- options_data_clean %>%
    mutate(
      log_moneyness_bin = cut(log_moneyness, breaks = seq(-1.2, 1.2, length.out = 18)^3),
      maturity_bin = cut(maturity, breaks = exp(seq(log(0.0019), log(3.5), length.out = 21)))
    )
  
  iv_summary <- options_data_clean %>%
    group_by(log_moneyness_bin, maturity_bin) %>%
    summarise(
      iv_mean = mean(impl_volatility, na.rm = TRUE),
      count = n(),
      log_moneyness_center = mean(log_moneyness, na.rm = TRUE),
      maturity_center = mean(maturity, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(count >= 1)
  
  # Get simulation grid
  S0 <- config$simulation$S0
  sample_iv_matrix <- results$implied_vols[[1]][[1]]
  
  moneyness <- as.numeric(gsub("K", "", colnames(sample_iv_matrix))) / S0
  maturities <- as.numeric(gsub("T", "", rownames(sample_iv_matrix)))
  log_moneyness <- log(moneyness)
  
  # Interpolate to simulation grid
  iv_interp <- interp(
    x = iv_summary$log_moneyness_center,
    y = iv_summary$maturity_center,
    z = iv_summary$iv_mean,
    xo = log_moneyness,
    yo = maturities,
    linear = TRUE,
    duplicate = "mean"
  )
  
  # Create market IV matrix
  iv_matrix_market <- t(iv_interp$z)
  colnames(iv_matrix_market) <- paste0("K", round(exp(iv_interp$x) * S0, 2))
  rownames(iv_matrix_market) <- paste0("T", round(iv_interp$y, 3))
  
  return(iv_matrix_market)
}

# Compute calibration errors for all scenarios
compute_calibration_errors <- function(results, scenario_analysis, market_iv_matrix) {
  
  MSE <- rep(NA, nrow(scenario_analysis))
  RMSE <- rep(NA, nrow(scenario_analysis))
  
  for (i in 1:nrow(scenario_analysis)) {
    iv_matrices <- results$implied_vols[[i]]
    iv_matrix_sim <- aggregate_put_call_iv(iv_matrices, "mean")
    
    # Compute error
    diff_vec <- as.vector(iv_matrix_sim - market_iv_matrix)
    valid <- !is.na(diff_vec)
    mse <- mean(diff_vec[valid]^2)
    
    MSE[i] <- round(mse, 5)
    RMSE[i] <- round(sqrt(mse), 4)
  }
  
  return(list(MSE = MSE, RMSE = RMSE))
}

# Plot calibration results
plot_calibration_results <- function(results, best_i, market_iv_matrix) {
  
  # Get best model
  iv_matrices_best <- results$implied_vols[[best_i]]
  iv_matrix_best <- aggregate_put_call_iv(iv_matrices_best, "mean")
  
  # Create grid for plotting
  S0 <- results$config$simulation$S0
  moneyness <- as.numeric(gsub("K", "", colnames(iv_matrix_best))) / S0
  maturities <- as.numeric(gsub("T", "", rownames(iv_matrix_best)))
  log_moneyness <- log(moneyness)
  
  # Side-by-side comparison
  par(mfrow = c(1, 2))
  
  persp(log_moneyness, maturities, t(market_iv_matrix),
        zlim = c(0.1, 0.7), theta = 40, phi = 20, expand = 0.5,
        col = "lightblue", xlab = "Log-Moneyness", ylab = "Time to Maturity", 
        zlab = "Implied Volatility", ticktype = "detailed", main = "Market IV Surface")
  
  persp(log_moneyness, maturities, t(iv_matrix_best),
        zlim = c(0.1, 0.7), theta = 40, phi = 20, expand = 0.5,
        col = "orange", xlab = "Log-Moneyness", ylab = "Time to Maturity", 
        zlab = "Implied Volatility", ticktype = "detailed", 
        main = paste("Best Model - Scenario", best_i))
  
  par(mfrow = c(1, 1))
}

# Plot calibration results
plot_market_surface <- function(market_iv_matrix, config) {
  
  # Create grid for plotting
  S0 <- config$simulation$S0
  moneyness <- as.numeric(gsub("K", "", colnames(market_iv_matrix))) / S0
  maturities <- as.numeric(gsub("T", "", rownames(market_iv_matrix)))
  log_moneyness <- log(moneyness)
  
  persp(log_moneyness, maturities, t(market_iv_matrix),
        zlim = c(0.1, 1), theta = 40, phi = 20, expand = 0.5,
        col = "lightblue", xlab = "Log-Moneyness", ylab = "Time to Maturity", 
        zlab = "Implied Volatility", ticktype = "detailed", main = "Market IV Surface")
}
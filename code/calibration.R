# calibration/calibration.R
# Implied volatility surface calibration

# Source files
source("models/implied_volatility.R")
source("utils/filtering.R")
# source("utils/plotting.R")
source("utils/data_management.R")

# Load required packages
library(DBI)
library(RPostgreSQL)
library(dplyr)
library(akima)
library(plotly)
#library(ggplot2)

# WRDS connection setup
# You'll need to modify these connection details for your WRDS account
wrds_host <- "wrds-pgdata.wharton.upenn.edu"
wrds_port <- 9737
wrds_dbname <- "wrds"
wrds_user <- username  # Replace with your WRDS username
wrds_password <- password  # Replace with your WRDS password

# Connect to WRDS
con <- dbConnect(PostgreSQL(),
                 host = wrds_host,
                 port = wrds_port,
                 dbname = wrds_dbname,
                 user = wrds_user,
                 password = wrds_password)

# Define date range (all March 1999)
# start_date <- "1999-03-01"
# end_date   <- "1999-03-31"

# Define date range (February 15, 2023)
start_date <- "2023-02-15"
end_date   <- "2023-02-15"

# Query S&P500 options over date range
query_options <- paste0("
SELECT 
    secid,
    date,
    cp_flag,
    strike_price,
    exdate,
    (exdate - date) AS days_to_expiry,
    impl_volatility
FROM optionm.opprcd2023
WHERE 
    date BETWEEN '", start_date, "' AND '", end_date, "'
    AND secid = 108105
    AND impl_volatility IS NOT NULL
    AND impl_volatility > 0
    AND exdate > date
    AND strike_price > 0
ORDER BY date, exdate, strike_price
")

options_data <- dbGetQuery(con, query_options)

# Query S&P500 spot prices over the same dates
query_spot <- paste0("
SELECT date, Open, Close
FROM optionm.secprd
WHERE date BETWEEN '", start_date, "' AND '", end_date, "'
AND secid = 108105
")

spot_data <- dbGetQuery(con, query_spot)

# Merge spot prices by date
options_data <- merge(
  options_data,
  spot_data,
  by = "date",
  all.x = TRUE
)

original_data <- options_data

# Close connection
dbDisconnect(con)

options_data <- original_data

# Ensure correct data types
options_data$days_to_expiry <- as.numeric(options_data$days_to_expiry)
options_data$maturity <- options_data$days_to_expiry / 365
options_data$strike_price <- options_data$strike_price / 1000

# Compute moneyness
options_data$moneyness <- options_data$strike_price / options_data$close
options_data$log_moneyness <- log(options_data$moneyness)

# Remove rows with missing values
options_data <- na.omit(options_data)

# Filter out implausible options
options_data_clean <- options_data %>%
  filter(
    impl_volatility > 0,
    impl_volatility < 1,
    log_moneyness > -2,
    log_moneyness < 2,
    maturity >= 0,
    maturity <= 2
  )

# Binning
options_data_clean <- options_data_clean %>%
  mutate(
    log_moneyness_bin = cut(log_moneyness, breaks = seq(-2, 2, by=0.1)),
    maturity_bin = cut(maturity, breaks = seq(0, 2, by=0.1))
  )

# Compute averages
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

# Dense grid for visualization
iv_interp_dense <- interp(
  x = iv_summary$log_moneyness_center,
  y = iv_summary$maturity_center,
  z = iv_summary$iv_mean,
  linear = TRUE,
  duplicate = "mean"
)

# Define custom grid for moneyness and times to maturity
M <- c(0.30, 0.60, 0.80, 0.85, 0.90, 0.95, 0.975, 1.00, 1.025, 1.05, 1.10, 1.15, 1.20, 1.30, 1.50, 1.80)
TT <- c(0.0274, 0.0548, 0.0822, 0.1644, 0.2466, 0.4110, 0.5753, 0.7397, 1.0000, 1.5000, 2.0000)
log_M <- log(M)

# Custom target grid
iv_interp_custom <- interp(
  x = iv_summary$log_moneyness_center,
  y = iv_summary$maturity_center,
  z = iv_summary$iv_mean,
  xo = log_M,
  yo = TT,
  linear = TRUE,
  duplicate = "mean"
)

par(mfrow = c(1, 2))  # 1 row, 2 columns

# Dense grid
persp(
  x = iv_interp_dense$x,
  y = iv_interp_dense$y,
  z = iv_interp_dense$z,
  zlim = c(0.1, 1),
  theta = 40, phi = 20,
  expand = 0.5,
  col = "lightblue",
  xlab = "Log-Moneyness",
  ylab = "Time to Maturity",
  zlab = "IV",
  ticktype = "detailed",
  main = "Dense Grid Surface"
)

# Custom grid
persp(
  x = iv_interp_custom$x,
  y = iv_interp_custom$y,
  z = iv_interp_custom$z,
  zlim = c(0.1, 1),
  theta = 40, phi = 20,
  expand = 0.5,
  col = "orange",
  xlab = "Log-Moneyness",
  ylab = "Time to Maturity",
  zlab = "IV",
  ticktype = "detailed",
  main = "Custom Grid Surface"
)

# Reset layout
par(mfrow = c(1,1))

cat("Interpolated implied volatilities at your grid:\n")
print(round(iv_interp_custom$z, 4))





# Load results
data_dir <- "SimulationData_2025-07-06"
results <- load_simulation_results(data_dir)


iv_matrix_market <- t(iv_interp_custom$z)
colnames(iv_matrix_market) <- paste0("K", exp(iv_interp_custom$x)*100)
rownames(iv_matrix_market) <- paste0("T", round(iv_interp_custom$y, 3))








compute_error <- function(sim_iv_matrix, market_iv_matrix, metric = c("MSE", "RMSE", "MAE"),
                          mean_correction = FALSE) {
  moneyness <- intersect(colnames(sim_iv_matrix), colnames(market_iv_matrix))
  maturities <- intersect(rownames(sim_iv_matrix), rownames(market_iv_matrix))
  sim_iv_matrix <- sim_iv_matrix[maturities, moneyness]
  market_iv_matrix <- market_iv_matrix[maturities, moneyness]
  
  if (mean_correction) {
    sim_iv_matrix <- sim_iv_matrix - mean(sim_iv_matrix, na.rm = TRUE)
    market_iv_matrix <- market_iv_matrix - mean(market_iv_matrix, na.rm = TRUE)
  }
  
  diff_vec <- as.vector(sim_iv_matrix - market_iv_matrix)
  valid <- !is.na(diff_vec)
  
  if (metric == "MSE") return( mean(diff_vec[valid]^2) )
  if (metric == "RMSE") return( sqrt(mean(diff_vec[valid]^2)) )
  if (metric == "MAE") return( mean(abs(diff_vec[valid])) )
}



scenario_analysis <- create_scenario_analysis(results)

passed_idx <- as.integer(rownames(scenario_analysis[scenario_analysis$AllPassed,]))


for (i in passed_idx) {
  iv_matrices <- results$implied_vols[[i]]
  iv_matrix_sim <- aggregate_put_call_iv(iv_matrices, "mean")
  
  mse <- compute_error(iv_matrix_sim, iv_matrix_market, metric = "MSE", mean_correction = TRUE)
  rmse <- compute_error(iv_matrix_sim, iv_matrix_market, metric = "RMSE", mean_correction = TRUE)
  mae <- compute_error(iv_matrix_sim, iv_matrix_market, metric = "MAE", mean_correction = TRUE)
  
  cat("Scenario ", i, ":",
      "\n    RMSE = ", round(rmse, 4),
      "\n     MSE = ", round(mse, 5),
      "\n     MAE = ", round(mae, 4), "\n", sep = "")
}




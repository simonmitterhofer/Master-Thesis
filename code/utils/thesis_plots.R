# thesis_plots.R
# Produce plots to be used in the thesis

# Setup -----------------------------------------------------

rm(list = ls())

# Working directory
setwd("~/QFin/Master Thesis/code")

# Load helper scripts
source("utils/data_management.R")
source("utils/filtering.R")
source("utils/plotting.R")
source("utils/calibration.R")
source("models/volatility.R")
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


# A.1 Process Dynamics Example ------------------------------

set.seed(3120)

N <- 3000
dt <- 0.001

H <- 0.1
gamma2 <- 0.2
gamma1 <- H + 0.5 - gamma2
rho <- -0.4
a <- 1
b <- 0.2
r <- 0.02
J <- 10

coeffs <- compute_approximation_coeffs(J, gamma1, gamma2)
c <- coeffs$c
k <- coeffs$k

BM <- generate_correlated_brownian(N, dt, rho)
Z <- simulate_ou_processes(N, J, k, BM$dWV, dt)

long_term_variance <- sum(outer(c, c, "*") / outer(k, k, "+"))
signal <- c(Z %*% c) / sqrt(long_term_variance)
crV <- b * exp(a * signal - 0.5 * a^2)

log_returns <- (r - 0.5 * crV[-length(crV)]^2) * dt + crV[-length(crV)] * BM$dWcA
log_S <- cumsum(c(log(100), log_returns))
S <- exp(log_S)

time_vector <- seq(0, N * dt, by = dt)

# OU processes plot
png(file.path("~/QFin/Master Thesis/code/A.1 Process Dynamics", 
              "ou_processes.png"), width = 800, height = 600, res = 100)
par(mar = c(4, 4, 3, 2))
plot(time_vector, Z[, 1], type = "l", col = rainbow(J)[1],
     xlim = c(0, max(time_vector)), ylim = range(Z),
     main = "Ornstein-Uhlenbeck Processes",
     xlab = "", ylab = "")

for (j in 2:J) {
  lines(time_vector, Z[, j], col = rainbow(J)[j])
}

legend("topright", legend = paste0("Z", 1:J),
       col = rainbow(J), lty = 1, ncol = 2, cex = 0.8)
grid(col = "gray90", lty = 3)
dev.off()

# Signal process plot
png(file.path("~/QFin/Master Thesis/code/A.1 Process Dynamics", 
              "signal_process.png"), width = 800, height = 600, res = 100)
par(mar = c(4, 4, 3, 2))
plot(time_vector, signal, type = "l", col = "blue", lwd = 1,
     main = "Signal Process",
     xlab = "", ylab = "")
grid(col = "gray90", lty = 3)
abline(h = 0, col = "black", lty = 2, lwd = 1)
dev.off()

# Volatility process plot
png(file.path("~/QFin/Master Thesis/code/A.1 Process Dynamics", 
              "volatility_process.png"), width = 800, height = 600, res = 100)
par(mar = c(4, 4, 3, 2))
plot(time_vector, volatility_process, type = "l", col = "red", lwd = 1,
     main = "Volatility Process",
     xlab = "", ylab = "")
grid(col = "gray90", lty = 3)
abline(h = mean(volatility_process), col = "black", lty = 2, lwd = 1)
dev.off()

# Asset price plot
png(file.path("~/QFin/Master Thesis/code/A.1 Process Dynamics", 
              "asset_price.png"), width = 800, height = 600, res = 100)
par(mar = c(4, 4, 3, 2))
plot(time_vector, S, type = "l", col = "darkgreen", lwd = 1,
     main = "Asset Price Process",
     xlab = "", ylab = "")
grid(col = "gray90", lty = 3)
dev.off()


# A.2 Market April 2023 -------------------------------------

calibration_results2 <- run_calibration(
  results, scenario_analysis, c("2023-04-01", "2023-04-30")
)

save_plot_vol_surface(calibration_results2$market_iv_matrix, NULL, results$config, "A.2 Market Surface")
save_plot_vol_smiles(calibration_results2$market_iv_matrix, NULL, results$config, "A.2 Market Surface")
save_plot_atm_skew(calibration_results2$market_iv_matrix, NULL, results$config, "A.2 Market Surface")
save_plot_log_atm_skew(calibration_results2$market_iv_matrix, NULL, results$config, "A.2 Market Surface")


# A.3 Calibration Diagnostics -------------------------------

market_matrix <- market_iv
simulated_matrix <- aggregate_put_call_iv(results$implied_vols[[20]], "mean")

png(file.path("~/QFin/Master Thesis/code/A.3 Calibration Diagnostics",
              "market_best_fit_heatmap.png"), width = 800, height = 600, res = 100)

# Set up layout with space for color bar on the right
layout(matrix(c(1,2,3), nrow=1, byrow=TRUE), widths=c(4,4,1))

# Market heatmap
par(mar = c(5, 4, 4, 1))
image(1:17, 1:15, t(market_matrix),
      col = colorRampPalette(c("lightblue", "black"))(50),
      zlim = market_model_range,
      main = "Market IV Surface",
      xlab = "Moneyness", ylab = "Time to Maturity",
      axes = FALSE)
axis(1, at = seq(1, 17), labels = as.numeric(gsub("K", "", colnames(market_matrix)))/100, las = 2, cex.axis = 0.7)
axis(2, at = seq(1, 15), labels = as.numeric(gsub("T", "", rownames(market_matrix))), las = 2, cex.axis = 0.7)

# Model heatmap
par(mar = c(5, 4, 4, 1))
image(1:17, 1:15, t(simulated_matrix),
      col = colorRampPalette(c("lightblue", "black"))(50),
      zlim = market_model_range,
      main = "Simluated IV Surface",
      xlab = "Moneyness", ylab = "Time to Maturity",
      axes = FALSE)
axis(1, at = seq(1, 17), labels = as.numeric(gsub("K", "", colnames(simulated_matrix)))/100, las = 2, cex.axis = 0.7)
axis(2, at = seq(1, 15), labels = as.numeric(gsub("T", "", rownames(simulated_matrix))), las = 2, cex.axis = 0.7)

# Color bar on the right
par(mar = c(5, 1, 4, 4))
color_seq <- seq(0, 1, length.out = 50)
image(1, color_seq, matrix(color_seq, nrow = 1),
      col = colorRampPalette(c("lightblue", "black"))(50),
      axes = FALSE, xlab = "", ylab = "")
axis(4, las = 1, cex.axis = 0.8)
mtext("Implied Volatility", side = 4, line = 2.5, cex = 0.7)

dev.off()

# Reset layout
par(mfrow = c(1, 1))

png(file.path("~/QFin/Master Thesis/code/A.3 Calibration Diagnostics",
              "market_best_fit_mse_heatmap.png"), width = 800, height = 600, res = 100)

# Separate plot for MSE difference with color bar on the right
mse_diff <- (market_matrix - simulated_matrix)^2
diff <- (market_matrix - simulated_matrix)

layout(matrix(c(1,2), nrow=1), widths=c(4,1))

par(mar = c(5, 4, 4, 1))
image(1:17, 1:15, t(mse_diff),
      col = colorRampPalette(c("lightblue", "black"))(50),
      main = "MSE: (Market - Model)²",
      xlab = "Moneyness", ylab = "Time to Maturity",
      axes = FALSE)
axis(1, at = seq(1, 17), labels = as.numeric(gsub("K", "", colnames(mse_diff)))/100, las = 2, cex.axis = 0.7)
axis(2, at = seq(1, 15), labels = as.numeric(gsub("T", "", rownames(mse_diff))), las = 2, cex.axis = 0.7)

# Color bar on the right
par(mar = c(5, 1, 4, 4))
mse_seq <- seq(0, 1, length.out = 50)
image(1, mse_seq, matrix(mse_seq, nrow = 1),
      col = colorRampPalette(c("lightblue", "black"))(50),
      axes = FALSE, xlab = "", ylab = "")
axis(4, las = 1, cex.axis = 0.8)
mtext("Mean Squared Error", side = 4, line = 2.5, cex = 0.7)

dev.off()

# Reset layout
par(mfrow = c(1, 1))









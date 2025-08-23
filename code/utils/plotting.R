# utils/plotting.R
# Plotting and visualization functions

source("models/implied_volatility.R")

# Plot implied volatility surface
plot_vol_surface <- function(iv_matrix, scenario, config) {
  scenario <- scenario[-2]
  
  strikes <- as.numeric(gsub("K", "", colnames(iv_matrix)))
  moneyness <- strikes / config$simulation$S0
  maturities <- as.numeric(gsub("T", "", rownames(iv_matrix)))
  
  title_sub = paste0(names(scenario), sep = " = ", scenario, collapse = ", ")
  if (is.null(scenario)) title_sub = "Market (March 2023)"
  
  par(mfrow = c(1,1))
  
  persp(log(moneyness), maturities, t(iv_matrix),
        theta = 40, phi = 20,
        expand = 0.5,
        box = TRUE,
        col = "lightblue",
        border = "darkblue",
        lwd = 0.5,
        xlab = "Log-Moneyness",
        ylab = "Time to Maturity",
        zlab = "Implied Volatility",
        zlim = c(0, 1),
        ticktype = "detailed",
        main = paste("Implied Volatility Surface", title_sub, sep = "\n"),
        cex.main = 1.2,
        cex.lab = 0.8,
        cex.axis = 0.6)
}

# Plot volatility smiles
plot_vol_smiles <- function(iv_matrix, scenario, config) {
  scenario <- scenario[-2]
  
  strikes <- as.numeric(gsub("K", "", colnames(iv_matrix)))
  moneyness <- strikes / config$simulation$S0
  log_moneyness <- log(moneyness)
  maturities <- as.numeric(gsub("T", "", rownames(iv_matrix)))
  
  n_cols <- ceiling(sqrt(length(maturities)))
  n_rows <- ceiling(length(maturities) / n_cols)
  
  old_par <- par(no.readonly = TRUE)
  par(mfrow = c(n_rows, n_cols), 
      oma = c(2, 2, 4, 2),
      mar = c(3, 3, 2, 1))
  
  title_sub = paste0(names(scenario), sep = " = ", scenario, collapse = ", ")
  if (is.null(scenario)) title_sub = "Market (March 2023)"
  
  for (i in seq_along(maturities)) {
    plot(log_moneyness, iv_matrix[i, ], main = paste0("T = ", maturities[i]), 
         xlab = "", ylab = "", 
         xlim = range(log_moneyness, na.rm = TRUE), ylim = c(0, 1),
         type = "o", lwd = 1, col = "blue")
    grid()
  }
  mtext(paste("Implied Volatility Smiles", title_sub, sep = "\n"), 
        outer = TRUE, line = 0.5, cex = 1.2)
  
  par(old_par)
}

# Plot ATM-skew term structure
plot_atm_skew <- function(iv_matrix, scenario, config) {
  scenario <- scenario[-2]
  
  maturities <- as.numeric(gsub("T", "", rownames(iv_matrix)))
  atm_skew <- compute_atm_skew(iv_matrix, config)
  
  title_sub = paste0(names(scenario), sep = " = ", scenario, collapse = ", ")
  if (is.null(scenario)) title_sub = "Market (March 2023)"
  
  par(mfrow = c(1,1))
  
  plot(maturities, atm_skew, 
       main = paste('Term Structure of ATM-Skew', title_sub, sep = "\n"), 
       xlab = 'Time to Maturity', ylab = 'ATM-Skew',
       xlim = c(0.003, 3), ylim = c(-3.5, 0),
       type = 'b', lwd = 2, col = "blue")
  grid()
}

# Plot ATM-skew term structure (log-log)
plot_log_atm_skew <- function(iv_matrix, scenario, config) {
  scenario <- scenario[-2]
  
  maturities <- as.numeric(gsub("T", "", rownames(iv_matrix)))
  atm_skew <- compute_atm_skew(iv_matrix, config)
  logfit <- lm(log(abs(atm_skew)) ~ log(maturities))
  
  title_sub = paste0(names(scenario), sep = " = ", scenario, collapse = ", ")
  if (is.null(scenario)) title_sub = "Market (March 2023)"
  
  par(mfrow = c(1,1))
  
  plot(maturities, abs(atm_skew), 
       main = paste('Term Structure of ATM-Skew (log-log)', title_sub, sep = "\n"),
       sub = paste0("Slope = ", round(coef(logfit)[2], 3), ", R^2 = ", 
                    round(summary(logfit)$r.squared, 3)),
       xlab = 'Time to Maturity (log)', ylab = 'ATM-Skew (log)', log = 'xy',
       xlim = c(0.003, 3), ylim = c(0.05, 3.5),
       type = 'b', lwd = 2, col = 'blue')
  grid()
  
  lines(as.numeric(gsub("T", "", names(exp(predict(logfit))))), exp(predict(logfit)), 
        col = "red", lwd = 2)
}


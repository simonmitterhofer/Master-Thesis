# utils/plotting.R
# Plotting and visualization functions

source("models/implied_volatility.R")

# Plot implied volatility surface
plot_vol_surface <- function(iv_matrix, scenario, config) {
  
  strikes <- as.numeric(gsub("K", "", colnames(iv_matrix)))
  moneyness <- strikes / config$simulation$S0
  maturities <- as.numeric(gsub("T", "", rownames(iv_matrix)))
  
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
        zlim = c(0, 0.6),
        ticktype = "detailed",
        main = paste("Implied Volatility Surface", 
                     paste0(names(scenario), sep = " = ", 
                            scenario, collapse = ", "), 
                     sep = "\n"),
        cex.main = 1.2,
        cex.lab = 0.8,
        cex.axis = 0.6)
}

# Plot volatility smiles
plot_vol_smiles <- function(iv_matrix, scenario, config) {
  
  strikes <- as.numeric(gsub("K", "", colnames(iv_matrix)))
  moneyness <- strikes / config$simulation$S0
  maturities <- as.numeric(gsub("T", "", rownames(iv_matrix)))
  
  n_cols <- ceiling(sqrt(length(maturities)))
  n_rows <- ceiling(length(maturities) / n_cols)
  
  old_par <- par(no.readonly = TRUE)
  par(mfrow = c(n_rows, n_cols), 
      oma = c(2, 2, 4, 2),
      mar = c(3, 3, 2, 1))
  
  for (i in seq_along(maturities)) {
    plot(moneyness, iv_matrix[i, ], main = paste0("T = ", maturities[i]), 
         xlab = "", ylab = "", 
         xlim = range(moneyness, na.rm = TRUE), ylim = range(iv_matrix, na.rm = TRUE),
         type = "o", lwd = 1, col = "blue")
    grid()
  }
  mtext(paste("Implied Volatility Smiles", 
              paste0(names(scenario), sep = " = ", 
                     scenario, collapse = ", "), 
              sep = "\n"), 
        outer = TRUE, line = 0.5, cex = 1.2)
  
  par(old_par)
}

# Plot ATM-skew term structure
plot_atm_skew <- function(iv_matrix, scenario, config) {
  
  maturities <- as.numeric(gsub("T", "", rownames(iv_matrix)))
  atm_skew <- compute_atm_skew(iv_matrix, config)
  
  plot(maturities, atm_skew, 
       main = paste('Term Structure of ATM-Skew', 
                    paste0(names(scenario), sep = " = ", 
                           scenario, collapse = ", "), 
                    sep = "\n"), 
       xlab = 'Time to Maturity', ylab = 'ATM-Skew', ylim = c(-1, 0),
       type = 'b', lwd = 2, col = "blue")
  grid()
}

# Plot ATM-skew term structure (log-log)
plot_log_atm_skew <- function(iv_matrix, scenario, config) {
  
  maturities <- as.numeric(gsub("T", "", rownames(iv_matrix)))
  atm_skew <- compute_atm_skew(iv_matrix, config)
  logfit <- lm(log(abs(atm_skew)) ~ log(maturities))
  
  plot(maturities, abs(atm_skew), 
       main = paste('Term Structure of ATM-Skew (log-log)', 
                    paste0(names(scenario), sep = " = ", 
                           scenario, collapse = ", "), 
                    sep = "\n"),
       sub = paste0("Slope = ", round(coef(logfit)[2], 3), ", R² = ", 
                    round(summary(logfit)$r.squared, 3)),
       xlab = 'Time to Maturity (log)', ylab = 'ATM-Skew (log)', log = 'xy',
       type = 'b', lwd = 2, col = 'blue')
  grid()
  lines(maturities, exp(predict(logfit)), col = "red", lwd = 2)
}

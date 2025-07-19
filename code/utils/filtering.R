# utils/filtering.R
# Filtering and analysis functions

source("models/implied_volatility.R")

# Check if put-call-parity holds
check_put_call_parity <- function(ov_matrices, scenario, config) {
  PutCallParity <- FALSE
  
  S0 <- config$simulation$S0
  r <- scenario$r
  strikes <- as.numeric(gsub("K", "", colnames(ov_matrices$call)))
  maturities <- as.numeric(gsub("T", "", rownames(ov_matrices$call)))
  
  pcp_violation <- ov_matrices$call - ov_matrices$put - S0 + exp(-r * maturities) %*% t(strikes)
  
  pcp_stats <- list(
    min = round(min(abs(pcp_violation))/S0*100, 3),
    mean = round(mean(abs(pcp_violation))/S0*100, 3),
    max = round(max(abs(pcp_violation))/S0*100, 3)
  )
  
  if ( (pcp_stats$mean < 0.1) && (pcp_stats$max < 0.5) ) {
    PutCallParity <- TRUE
  }
  
  return( PutCallParity )
}

# Check if volatility smiles are convex
check_smiles_convex  <- function(iv_matrix, scenario, config) {
  SmileConvex <- FALSE
  
  r <- scenario$r
  S0 <- config$simulation$S0
  strikes <- as.numeric(gsub("K", "", colnames(iv_matrix)))
  maturities <- as.numeric(gsub("T", "", rownames(iv_matrix)))
  atm_idx <- which(strikes == S0)
  
  vol_smile <- vapply(seq_along(maturities), function(i) {
    itm_wing <- iv_matrix[i, seq(1, atm_idx-1)]
    otm_wing <- iv_matrix[i, seq(atm_idx+1, length(strikes))]
    
    vol_itm <- mean(head(itm_wing[!is.na(itm_wing)], 2))
    vol_atm <- iv_matrix[i,atm_idx]
    vol_otm <- mean(tail(otm_wing[!is.na(otm_wing)], 2))
    
    return( (vol_atm < vol_itm) && (vol_atm < vol_otm) )
  }, numeric(1))
  
  if ( (vol_smile[1]) && (mean(vol_smile) > 0.2) ) {
    SmileConvex <- TRUE
  }
  
  return( SmileConvex )
}

# Check if ATM-skew is negative
check_atm_skew_negative <- function(iv_matrix, config) {
  SkewNegative <- FALSE
  
  atm_skew <- compute_atm_skew(iv_matrix, config)
  
  if ( (mean(atm_skew < -0.05) > 0.8) && (atm_skew[1] < -0.8) ) { 
    SkewNegative <- TRUE
  }
  
  return( SkewNegative )
}

# Check if ATM-skew is increasing
check_atm_skew_increasing <- function(iv_matrix, config) {
  SkewIncreasing <- FALSE
  
  maturities <- as.numeric(gsub("T", "", rownames(iv_matrix)))
  atm_skew <- compute_atm_skew(iv_matrix, config)
  skew_slope <- coef(lm(atm_skew ~ maturities))[2]
  
  if (skew_slope > 0) {
    SkewIncreasing <- TRUE
  }
  
  return( SkewIncreasing )
}

# Check if ATM-skew is power-law
check_atm_skew_power_law <- function(iv_matrix, config) {
  PowerLawFit <- FALSE
  
  maturities <- as.numeric(gsub("T", "", rownames(iv_matrix)))
  atm_skew <- compute_atm_skew(iv_matrix, config)
  logfit <- lm(log(abs(atm_skew)) ~ log(maturities))
  
  if ( summary(logfit)$r.squared > 0.9 ) {
    PowerLawFit <- TRUE
  }
  
  return( PowerLawFit )
}  

# Check if all criteria passed
check_criteria <- function(ov_matrices, iv_matrices, scenario, config) {
  
  PutCallParity <- check_put_call_parity(ov_matrices, scenario, config)
  iv_matrix <- aggregate_put_call_iv(iv_matrices, "mean")
  SmileConvex <- check_smiles_convex(iv_matrix, scenario, config)
  SkewNegative <- check_atm_skew_negative(iv_matrix, config)
  SkewIncreasing <- check_atm_skew_increasing(iv_matrix, config)
  SkewPowerLawFit <- check_atm_skew_power_law(iv_matrix, config)
  
  AllPassed <- all(PutCallParity, SmileConvex, SkewNegative, SkewIncreasing, SkewPowerLawFit)
  
  return( list(
    PutCallParity = PutCallParity,
    SmileConvex = SmileConvex,
    SkewNegative = SkewNegative,
    SkewIncreasing = SkewIncreasing,
    SkewPowerLawFit = SkewPowerLawFit,
    AllPassed = AllPassed
  ) )
}

# Create a scenario-criteria summary
create_scenario_analysis <- function(results) {
  config <- results$config
  ov_matrices <- results$option_values
  iv_matrices <- results$implied_vols
  scenarios <- results$scenarios
  
  scenarios_df <-  do.call(rbind, lapply(scenarios, data.frame))
  scenarios_df <- scenarios_df[, c("H", "gamma1", "gamma2", "rho", "a", "b", "r", "J")]
  
  criteria_df <- do.call(rbind, lapply(seq_along(scenarios), function(idx) {
    criteria <- check_criteria(ov_matrices[[idx]], iv_matrices[[idx]],
                               scenarios[[idx]], config)
    return( as.data.frame(criteria) )
  }))
  return( cbind(lapply(scenarios_df, factor), criteria_df) )
}

# Create a summary of parameter criteria passing rates
create_parameter_summary <- function(analysis_df, 
                                     parameters = c("H", "gamma1", "gamma2", "rho", "a", "b", "r", "J"),
                                     criteria = c("PutCallParity", "SmileConvex", "SkewNegative", 
                                                  "SkewIncreasing", "SkewPowerLawFit", "AllPassed")) {
  param_analysis <- lapply(parameters, function(param) {
    passing_rates <- lapply(criteria, function(criterion) {
      round(proportions( table(scenario_analysis[[param]], 
                               factor(scenario_analysis[[criterion]], levels = c(FALSE, TRUE))), margin = 1 )[,'TRUE'], 2)
    })
    names(passing_rates) <- criteria
    return( data.frame(passing_rates) )
  })
  names(param_analysis) <- parameters
  return( param_analysis )
}
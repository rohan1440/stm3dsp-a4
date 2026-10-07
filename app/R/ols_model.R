# Task: Ordinary Least Squares (OLS) Regression
# Author: Lan Anh Bui

fit_ols <- function(data, x_vars, y_var) {
  # Build formula dynamically
  formula <- reformulate(x_vars, response = y_var)
  
  # Fit OLS linear regression model
  model <- lm(formula, data = data)
  return(model)
}

# Extract coefficient table
get_ols_coefficients <- function(model) {
  # Extract coefficient matrix from summary
  coef_summary <- summary(model)$coefficients
  df <- as.data.frame(coef_summary)
  
  # Format table output
  df$Term <- rownames(df)
  df <- df[, c("Term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
  df[, 2:5] <- round(df[, 2:5], 4)
  rownames(df) <- NULL
  
  return(df)
}


# Append fitted values and residuals
get_ols_fitted_data <- function(data, model) {
  data$Fitted <- fitted(model)
  data$Residuals <- residuals(model)
  return(data)
}


# Extract summary statistics
get_ols_summary_stats <- function(model) {
  s <- summary(model)
  list(
    r_squared = round(s$r.squared, 4),
    adjusted_r_squared = round(s$adj.r.squared, 4),
    residual_standard_error = round(s$sigma, 4)
  )
}
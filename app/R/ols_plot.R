# Plots: OLS Plotting
# Author: Lan Anh Bui

library(ggplot2)

# Plot OLS regression fit or actual vs fitted
plot_ols_fit <- function(data, x_vars, y_var, ols_result) {
  
  # Append fitted and residuals columns to dataset
  fitted_data <- get_ols_fitted_data(data, ols_result)
  
  if (length(x_vars) == 1) {
    # Single X: Plot X vs Y with fitted regression line
    p <- ggplot(fitted_data, aes(x = .data[[x_vars[1]]], y = .data[[y_var]])) +
      geom_point(color = "#9b59b6", alpha = 0.7, size = 2.5) +
      geom_line(aes(y = Fitted), color = "#e84393", linewidth = 1.2) +
      labs(
        title = paste("OLS Regression Fit:", y_var, "vs", x_vars[1]),
        x = x_vars[1],
        y = y_var
      ) +
      theme_minimal()
  } else {
    # Multiple X predictors: Plot Actual vs Fitted values
    p <- ggplot(fitted_data, aes(x = Fitted, y = .data[[y_var]])) +
      geom_point(color = "#9b59b6", alpha = 0.7, size = 2.5) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#e84393", linewidth = 1) +
      labs(
        title = "OLS Model: Actual vs Fitted Values",
        x = "Fitted Values",
        y = paste("Actual", y_var)
      ) +
      theme_minimal()
  }
  
  return(p)
}


# Plot residuals vs fitted values
plot_ols_residuals <- function(data, x_vars = NULL, y_var = NULL, ols_result) {
  
  # Append Fitted and Residuals columns to dataset
  fitted_data <- get_ols_fitted_data(data, ols_result)
  
  # Plot Residuals vs Fitted values
  ggplot(fitted_data, aes(x = Fitted, y = Residuals)) +
    geom_point(color = "#9b59b6", alpha = 0.7, size = 2.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#e84393", linewidth = 1) +
    labs(
      title = "OLS Residuals vs Fitted Values",
      x = "Fitted Values",
      y = "Residuals"
    ) +
    theme_minimal()
}
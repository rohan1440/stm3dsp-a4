# Module: OLS vs LMS Comparison
# Author: Gaurab Bhusal

# Plot both fitted models for direct comparison.
plot_comparison <- function(data, x_vars, y_var, ols_model, lms_result) {
  ols_fitted <- get_ols_fitted_data(data, ols_model)
  lms_fitted <- get_lms_fitted_data(data, x_vars, y_var, lms_result)

  if (length(x_vars) == 1) {
    x_variable <- x_vars[1]
    ols_fitted <- ols_fitted[order(ols_fitted[[x_variable]]), , drop = FALSE]
    lms_fitted <- lms_fitted[order(lms_fitted[[x_variable]]), , drop = FALSE]

    ggplot(data, aes(x = .data[[x_variable]], y = .data[[y_var]])) +
      geom_point(color = "#2c3e50", alpha = 0.7, size = 2.5) +
      geom_line(
        data = ols_fitted,
        aes(y = Fitted, color = "OLS"),
        linewidth = 1.2
      ) +
      geom_line(
        data = lms_fitted,
        aes(y = Fitted, color = "LMS"),
        linewidth = 1.2,
        linetype = "dashed"
      ) +
      scale_color_manual(
        name = "Model",
        values = c("OLS" = "#9b59b6", "LMS" = "#e67e22")
      ) +
      labs(
        title = paste("OLS vs LMS Fit:", y_var, "vs", x_variable),
        x = x_variable,
        y = y_var
      ) +
      theme_minimal(base_size = 13)
  } else {
    ggplot() +
      geom_point(
        data = ols_fitted,
        aes(x = Fitted, y = .data[[y_var]], color = "OLS"),
        alpha = 0.65,
        size = 2.5
      ) +
      geom_point(
        data = lms_fitted,
        aes(x = Fitted, y = .data[[y_var]], color = "LMS"),
        alpha = 0.65,
        size = 2.5,
        shape = 17
      ) +
      geom_abline(
        intercept = 0,
        slope = 1,
        linetype = "dashed",
        color = "gray50"
      ) +
      scale_color_manual(
        name = "Model",
        values = c("OLS" = "#9b59b6", "LMS" = "#e67e22")
      ) +
      labs(
        title = "OLS vs LMS: Actual vs Fitted Values",
        x = "Fitted Values",
        y = paste("Actual", y_var)
      ) +
      theme_minimal(base_size = 13)
  }
}

# Compare coefficients side by side.
compare_coefficients <- function(x_vars, ols_model, lms_result) {
  ols_coefficients <- coef(ols_model)
  lms_coefficients <- lms_result$par

  data.frame(
    Term = c("(Intercept)", x_vars),
    OLS_Estimate = round(as.numeric(ols_coefficients), 4),
    LMS_Estimate = round(as.numeric(lms_coefficients), 4),
    row.names = NULL,
    check.names = FALSE
  )
}

# Module: Least Median of Squares (LMS) Regression

# Fit an LMS regression model using numerical optimisation.
fit_lms <- function(data, x_vars, y_var,
                    n_jitter = 20,
                    n_elemental = 300,
                    n_refine = 5,
                    seed = 2026) {
  y <- data[[y_var]]
  x_matrix <- as.matrix(data[, x_vars, drop = FALSE])
  
  if (any(!is.finite(y)) || any(!is.finite(x_matrix))) {
    stop("LMS requires finite values in all selected variables.")
  }
  
  design_matrix <- cbind(1, x_matrix)
  number_of_parameters <- ncol(design_matrix)
  
  lms_objective <- function(params) {
    residuals <- y - as.numeric(design_matrix %*% params)
    median(residuals^2)
  }
  
  zero_start <- rep(0, number_of_parameters)
  median_start <- c(median(y), rep(0, ncol(x_matrix)))
  ols_start <- as.numeric(qr.coef(qr(design_matrix), y))
  ols_start[!is.finite(ols_start)] <- 0
  
  candidate_starts <- list(zero_start, median_start, ols_start)
  
  random_starts <- with_local_seed(seed, {
    jitter_scale <- pmax(abs(ols_start), sd(y) / 10)
    jittered <- lapply(seq_len(n_jitter), function(i) {
      ols_start + rnorm(number_of_parameters, sd = jitter_scale)
    })
    
    elemental <- lapply(seq_len(n_elemental), function(i) {
      rows <- sample(nrow(design_matrix), number_of_parameters)
      tryCatch(
        as.numeric(solve(design_matrix[rows, , drop = FALSE], y[rows])),
        error = function(e) NULL
      )
    })
    
    c(jittered, Filter(Negate(is.null), elemental))
  })
  
  candidate_starts <- c(candidate_starts, random_starts)
  
  start_scores <- vapply(candidate_starts, lms_objective, numeric(1))
  best_starts <- candidate_starts[order(start_scores)][seq_len(
    min(n_refine, length(candidate_starts))
  )]
  
  candidate_fits <- lapply(best_starts, function(starting_values) {
    tryCatch(
      optim(
        par = starting_values,
        fn = lms_objective,
        method = "Nelder-Mead",
        control = list(maxit = 10000, reltol = 1e-10)
      ),
      error = function(error_message) NULL
    )
  })
  
  candidate_fits <- Filter(
    function(result) {
      !is.null(result) && is.finite(result$value) && all(is.finite(result$par))
    },
    candidate_fits
  )
  
  if (length(candidate_fits) == 0) {
    stop("The LMS numerical optimisation could not find a finite solution.")
  }
  
  objective_values <- vapply(
    candidate_fits,
    function(result) result$value,
    numeric(1)
  )
  
  result <- candidate_fits[[which.min(objective_values)]]
  names(result$par) <- c("(Intercept)", x_vars)
  result$n_starts <- length(candidate_starts)
  result
}

with_local_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(expr)
  }
  
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = globalenv())
  }
  
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  })
  
  set.seed(seed)
  expr
}


# Extract a tidy coefficient table from an LMS model.
get_lms_coefficients <- function(lms_result) {
  data.frame(
    Term = names(lms_result$par),
    Estimate = round(as.numeric(lms_result$par), 4),
    row.names = NULL,
    check.names = FALSE
  )
}

# Add fitted values and residuals to the dataset.
get_lms_fitted_data <- function(data, x_vars, y_var, lms_result) {
  x_matrix <- as.matrix(data[, x_vars, drop = FALSE])
  intercept <- lms_result$par[1]
  slopes <- lms_result$par[-1]

  data$Fitted <- as.numeric(intercept + x_matrix %*% slopes)
  data$Residuals <- data[[y_var]] - data$Fitted
  data
}

# Calculate descriptive summary statistics. LMS minimises the median squared
# residual, so these OLS-style values are included for description only.
get_lms_summary_stats <- function(lms_result, data, x_vars, y_var) {
  fitted_data <- get_lms_fitted_data(data, x_vars, y_var, lms_result)

  ss_res <- sum(fitted_data$Residuals^2)
  ss_tot <- sum((fitted_data[[y_var]] - mean(fitted_data[[y_var]]))^2)
  r_squared <- 1 - (ss_res / ss_tot)

  n <- nrow(data)
  p <- length(x_vars)
  adjusted_r_squared <- 1 - (1 - r_squared) * (n - 1) / (n - p - 1)
  residual_standard_error <- sqrt(ss_res / (n - p - 1))

  list(
    r_squared = round(r_squared, 4),
    adjusted_r_squared = round(adjusted_r_squared, 4),
    residual_standard_error = round(residual_standard_error, 4)
  )
}

# Plot the LMS regression fit or actual versus fitted values.
plot_lms_fit <- function(data, x_vars, y_var, lms_result) {
  fitted_data <- get_lms_fitted_data(data, x_vars, y_var, lms_result)

  if (length(x_vars) == 1) {
    fitted_data <- fitted_data[order(fitted_data[[x_vars[1]]]), , drop = FALSE]

    ggplot(fitted_data, aes(x = .data[[x_vars[1]]], y = .data[[y_var]])) +
      geom_point(color = "#2c3e50", alpha = 0.7, size = 2.5) +
      geom_line(aes(y = Fitted), color = "#e67e22", linewidth = 1.2) +
      labs(
        title = paste("LMS Fit:", y_var, "vs", x_vars[1]),
        x = x_vars[1],
        y = y_var
      ) +
      theme_minimal(base_size = 13)
  } else {
    ggplot(fitted_data, aes(x = Fitted, y = .data[[y_var]])) +
      geom_point(color = "#2c3e50", alpha = 0.7, size = 2.5) +
      geom_abline(
        intercept = 0,
        slope = 1,
        linetype = "dashed",
        color = "#e67e22",
        linewidth = 1
      ) +
      labs(
        title = "LMS Model: Actual vs Fitted Values",
        x = "Fitted Values",
        y = paste("Actual", y_var)
      ) +
      theme_minimal(base_size = 13)
  }
}

# Plot residuals against fitted values.
plot_lms_residuals <- function(data, x_vars, y_var, lms_result) {
  fitted_data <- get_lms_fitted_data(data, x_vars, y_var, lms_result)

  ggplot(fitted_data, aes(x = Fitted, y = Residuals)) +
    geom_point(color = "#34495e", alpha = 0.7, size = 2.5) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "#e67e22",
      linewidth = 1
    ) +
    labs(
      title = "LMS Residuals vs Fitted Values",
      x = "Fitted Values",
      y = "Residuals"
    ) +
    theme_minimal(base_size = 13)
}

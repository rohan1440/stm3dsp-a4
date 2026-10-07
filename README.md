# Robust Regression Explorer

## Project Description

Robust Regression Explorer is an interactive R Shiny application developed for STM3DSP Assessment 2.

The application demonstrates how Ordinary Least Squares (OLS) and Least Median of Squares (LMS) regression can behave differently, particularly when a dataset contains outliers. Users can upload their own CSV dataset, select variables, fit both models, and compare the results through outputs, visualisations, and explanations.

## Group Members

1.  Khalid Hasan Rohan — 21448762
2.  Lan Anh Bui — 21290580
3.  Gaurab Bhusal — 22173528

## Application Features

The application allows users to:

-   Upload a CSV dataset
-   Specify whether the file contains column names
-   Select the appropriate column separator
-   Preview the uploaded dataset
-   View summaries of numeric variables
-   Select one response variable
-   Select one or more explanatory variables
-   Fit an Ordinary Least Squares regression model
-   Fit a Least Median of Squares regression model
-   Review model results and visualisations
-   Compare the behaviour of OLS and LMS
-   Learn about robust regression and the effect of outliers

## Understanding OLS and LMS

### Ordinary Least Squares

Ordinary Least Squares, or OLS, is a commonly used regression method. It finds the regression model that minimises the sum of the squared residuals.

A residual is the difference between an observed response value and the value predicted by the model.

Because OLS squares the residuals, observations with large residuals receive greater importance. As a result, a small number of extreme outliers can noticeably change the fitted model.

### Least Median of Squares

Least Median of Squares, or LMS, is a robust regression method. Instead of minimising the sum of all squared residuals, LMS minimises the median of the squared residuals.

The median is less affected by extreme values. This means the LMS model can remain more stable when some observations are unusual or contain outliers.

### Comparison

| OLS | LMS |
|------------------------------------|------------------------------------|
| Minimises the sum of squared residuals | Minimises the median of squared residuals |
| Uses information from all observations | Focuses on the typical residual |
| Can be strongly affected by outliers | Is more resistant to outliers |
| Commonly used and computationally efficient | Useful for demonstrating robust regression |

LMS is not automatically better than OLS in every situation. Comparing both models helps users understand whether unusual observations are influencing the OLS result.

## Suitable Dataset Format

The uploaded dataset should:

-   Be saved as a CSV file
-   Contain one observation per row
-   Contain column names
-   Include at least two numeric variables
-   Include enough complete observations for regression
-   Use one numeric variable as the response
-   Use one or more numeric variables as explanatory variables

## Software Requirements

-   R
-   RStudio
-   Shiny package
-   ggplot2 package

Install the required packages if necessary:

``` r
install.packages(c("shiny", "ggplot2"))
```

The regression calculations use base R functions including `lm()` and `optim()`. Visualisations are created with `ggplot2`.

## Project Structure

-   `Assignment2/app.R` — Shiny user interface and server logic
-   `Assignment2/ols_model.R` — OLS model functions
-   `Assignment2/ols_plot.R` — OLS plotting functions
-   `Assignment2/R/lms_model.R` — LMS model, summaries, and plots
-   `Assignment2/R/comparison.R` — OLS versus LMS comparisons
-   `Assignment2/sample_data/` — sample CSV for testing

## How to Run the Application

1.  Download or clone the repository.
2.  Open `Assignment 2.Rproj` in RStudio.
3.  Open `Assignment2/app.R`.
4.  Click **Run App**.

Alternatively, run the following command from the repository’s root directory:

``` r
shiny::runApp("Assignment2")
```

## How to Use the Application

1.  Click **Browse** and upload a suitable CSV dataset.
2.  Choose the correct header and separator options.
3.  Open the **Data Preview** tab to check the uploaded data.
4.  Select one response variable.
5.  Select one or more explanatory variables.
6.  Click **Fit and Compare Models**.
7.  Review the OLS and LMS results.
8.  Use the comparison outputs and explanations to interpret how outliers affect the fitted models.

## Purpose

This application is designed as an interactive educational demonstration of robust regression. It helps users with an introductory understanding of regression explore the differences between OLS and LMS in a clear and accessible way.

It is not intended to be a general-purpose regression package.

## Collaborative Development

The application was developed collaboratively using a private GitHub repository. Group members used separate branches and regular commits to document their contributions before integrating and testing the final application.

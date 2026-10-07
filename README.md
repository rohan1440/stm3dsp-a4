# Robust Regression Explorer – Assessment 4

STM3DSP Assessment 4 (individual). This project extends the Assessment 2
group application (OLS vs Least Median of Squares regression) with:

- a built-in sample dataset (the classic 1920s *cars* stopping-distance data),
- neural-network regression trained with the `neuralnet` package, and
- a computational experiment comparing OLS, LMS and neural networks under
  increasing levels of data contamination.

> Work in progress – the experiment plan, scripts and results are added progressively

## How to run the application

1. Open `STM3DSP-A4.Rproj` in RStudio.
2. Install the packages once:

   ```r
   install.packages(c("shiny", "ggplot2", "neuralnet"))
   ```

3. Run the app from the repository root:

   ```r
   shiny::runApp("app")
   ```

## Repository structure

| Path | Purpose |
|------|---------|
| `app/app.R` | Shiny user interface and server logic |
| `app/R/` | All helper functions (OLS, LMS, comparison, plotting) |
| `app/data/` | Built-in sample datasets |

## Origin

The starting point is the Assessment 2 group project (Group G: Khalid Hasan
Rohan, Lan Anh Bui, Gaurab Bhusal). All Assessment 4 changes are individual
work by Khalid Hasan Rohan.

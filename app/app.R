# STM3DSP Assessment 4
# Robust Regression Explorer: OLS, LMS and neural networks
# Extended individually from the Assessment 2 group application.


library(shiny)
library(ggplot2)

app_directory <- if (file.exists("app.R")) "." else "app"

helper_files <- list.files(
  file.path(app_directory, "R"),
  pattern = "\\.R$",
  full.names = TRUE
)
for (helper_file in helper_files) {
  source(helper_file, local = TRUE)
}


# -----------------------------
# USER INTERFACE
# -----------------------------

ui <- fluidPage(
  
  titlePanel("Robust Regression Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("1. Upload Dataset"),
      
      fileInput(
        inputId = "csv_file",
        label = "Choose a CSV file",
        accept = c(".csv", "text/csv")
      ),
      
      helpText(
        "Upload a CSV file containing at least two numeric variables."
      ),
      
      checkboxInput(
        inputId = "header",
        label = "The file contains column names",
        value = TRUE
      ),
      
      selectInput(
        inputId = "separator",
        label = "Column separator",
        choices = c(
          "Comma" = ",",
          "Semicolon" = ";",
          "Tab" = "\t"
        ),
        selected = ","
      ),
      
      tags$hr(),
      
      h4("2. Select Variables"),
      
      selectInput(
        inputId = "y_var",
        label = "Response variable (Y)",
        choices = NULL
      ),
      
      selectInput(
        inputId = "x_vars",
        label = "Explanatory variable(s) (X)",
        choices = NULL,
        multiple = TRUE
      ),
      
      helpText(
        "Select one response variable and one or more explanatory variables."
      ),
      
      actionButton(
        inputId = "run_models",
        label = "Fit and Compare Models",
        class = "btn-primary"
      ),
      
      tags$hr(),
      
      helpText(
        "Only numeric variables are available for regression."
      )
    ),
    
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          title = "Instructions",
          
          h3("How to Use the Application"),
          
          tags$ol(
            tags$li("Upload a CSV dataset."),
            tags$li("Check the uploaded data."),
            tags$li("Select one response variable."),
            tags$li("Select one or more explanatory variables."),
            tags$li("Click the Fit and Compare Models button."),
            tags$li("Compare the model results and visualisations.")
          ),
          
          tags$hr(),
          
          h4("About This Application"),
          
          p(
            "This application is an interactive demonstration of robust
            regression. It will compare Ordinary Least Squares regression
            with Least Median of Squares regression."
          ),
          
          p(
            "The OLS and LMS models are fitted to the same complete observations ",
            "so their coefficients, fitted values and sensitivity to outliers ",
            "can be compared directly."
          )
        ),
        
        
        tabPanel(
          title = "Data Preview",
          
          h3("Uploaded Dataset"),
          
          h4("File Information"),
          
          verbatimTextOutput("file_status"),
          
          h4("First 10 Rows"),
          
          tableOutput("data_preview"),
          
          h4("Numeric Variable Summary"),
          
          tableOutput("numeric_summary")
        ),
        
        
        tabPanel(
          title = "OLS Results",
          
          h3("Ordinary Least Squares Regression"),
          
          fluidRow(
    column(6,
      h4("Model Coefficients & Summary"),
      verbatimTextOutput("ols_summary"),
      p(em("The Estimate column shows the fitted intercept and slope. The slope 
          tells you how much the response variable is expected to change for 
          each one unit increase in the explanatory variable. The p-value 
          (Pr(>|t|)) tests whether this relationship is statistically 
          significant; a value below 0.05 is typically considered significant. 
          Adjusted R-squared indicates the proportion of variation in the 
          response variable explained by the model, adjusted for the number of 
          predictors; values closer to 1 indicate a stronger fit."))
    ),
    column(6,
      h4("OLS Regression Fit Plot"),
      plotOutput("ols_plot"),
      p(em("The solid line shows the fitted OLS regression line. The shaded band 
          represents the 95% confidence interval for this line, indicating the 
          range of plausible fitted lines given the uncertainty in the data."))
    )
  ),
  hr(),
  fluidRow(
    column(12,
      h4("Residual Diagnostics"),
      plotOutput("ols_residual_plot"),
      p(em("This plot shows the residuals (the difference between observed and 
      predicted values) against the fitted values. The red dashed line 
      marks zero residual. The orange dotted curve is a smoothed trend 
      through the residuals, used to detect any systematic pattern. If 
      residuals are scattered randomly around zero with no clear pattern, 
      the linear model's assumptions are reasonably well satisfied. A 
      curved trend, or residuals that spread out more at higher fitted 
      values, may suggest the linear model does not fully capture the 
      relationship, or that the assumption of constant variance is 
      violated.")),
      
      p(em("If a small number of points appear far from the others in these 
      plots, they may be exerting a disproportionate influence on the OLS 
      fit. Compare these results with the Least Median of Squares results 
      to see whether the robust method produces a noticeably different 
      fitted line."))
    )
  )
),
        
        
        tabPanel(
          title = "LMS Results",
          
          h3("Least Median of Squares Regression"),
          
          fluidRow(
            column(
              width = 5,
              h4("LMS Coefficients"),
              tableOutput("lms_coefficients"),
              h4("Model Summary"),
              verbatimTextOutput("lms_summary"),
              p(em(
                "LMS chooses the coefficients that minimise the median of the ",
                "squared residuals. Unlike OLS, standard errors and p-values are ",
                "not reported because this numerical LMS demonstration does not ",
                "use the usual OLS inference formulas."
              ))
            ),
            column(
              width = 7,
              h4("LMS Regression Fit Plot"),
              plotOutput("lms_plot")
            )
          ),
          tags$hr(),
          h4("LMS Residual Diagnostics"),
          plotOutput("lms_residual_plot"),
          p(em(
            "Residuals are observed values minus LMS fitted values. Points far ",
            "from zero may be unusual observations, while the LMS fit is driven ",
            "by the typical squared residual rather than their total."
          ))
        ),
        
        
        tabPanel(
          title = "Model Comparison",
          
          h3("Current Variable Selection"),
          
          verbatimTextOutput("selection_status"),
          
          tags$hr(),
          
          fluidRow(
            column(
              width = 7,
              h4("OLS and LMS Visual Comparison"),
              plotOutput("comparison_plot")
            ),
            column(
              width = 5,
              h4("Coefficient Comparison"),
              tableOutput("coefficient_comparison"),
              h4("How to interpret this comparison"),
              p(
                "A large difference between the OLS and LMS coefficients or ",
                "fitted values suggests that unusual observations may be ",
                "influencing the OLS result."
              ),
              verbatimTextOutput("comparison_summary")
            )
          )
        ),
        
        
        tabPanel(
          title = "Methodology",
          
          h3("Robust Regression Methodology"),

          h4("What is robust regression?"),
          p(
            "Robust regression refers to statistical methods that are designed to ",
            "produce reliable results even when a dataset contains unusual or ",
            "extreme observations, known as outliers. Ordinary Least Squares (OLS) ",
            "regression, the standard method for fitting a line to data, can be ",
            "strongly influenced by even a single outlier. Least Median of Squares ",
            "(LMS) regression is a robust alternative that resists this influence."
          ),

          tags$hr(),
          
          withMathJax(
            
            h4("Ordinary Least Squares (OLS)"),
            
            p("Ordinary least squares regression fits a linear model of the form 
       \\(y = ax + b\\) to a set of data points 
       \\((x_1, y_1), (x_2, y_2), \\ldots, (x_n, y_n)\\)."),
            
            p("For each data point, the residual is defined as the difference between 
       the observed value and the value predicted by the model:"),
            
            p("$$r_i = y_i - ax_i - b$$"),
            
            p("OLS finds the values of \\(a\\) and \\(b\\) that minimise the sum of 
       squared residuals. This is expressed as the objective function:"),
            
            p("$$F(a,b) = \\sum_{i=1}^n (y_i - ax_i - b)^2$$"),
            
            p("Squaring each residual removes the effect of sign, so that positive 
       and negative errors do not cancel out, and it places a larger penalty 
       on bigger errors. Because this objective function has a closed form 
       solution, it can be found exactly using calculus, and this is the 
       method used internally by the R function ", code("lm()"), 
              ". It can also be approximated numerically using the ", code("optim()"), 
              " function."),
            
            p("One important limitation of OLS is that it is not robust. Because the 
       objective function relies on a sum of squared residuals, a single 
       extreme observation can have a large effect on the fitted line. This 
       motivates the robust alternative demonstrated below, least median of 
       squares regression.")
          ),
          
          tags$hr(),
          
          h4("How Least Median of Squares (LMS) works"),
          p(
            "Both OLS and LMS work by finding the straight line that best fits a ",
            "set of data points. The difference lies in how each method measures ",
            "\"best fit\"."
          ),
          p(
            "OLS finds the line that minimises the sum of the squared residuals ",
            "(the squared vertical gaps between each data point and the fitted ",
            "line). Because it adds up every squared gap, a single data point that ",
            "is far from the rest can dominate this sum and pull the fitted line ",
            "strongly toward it."
          ),
          p(
            "LMS instead finds the line that minimises the median of the squared ",
            "residuals. Rather than adding up every gap, it only considers the ",
            "middle value once all the squared gaps are sorted. This makes LMS ",
            "far less sensitive to any single extreme point, since one unusual ",
            "value has little effect on the median."
          ),
          
          h4("Why LMS resists outliers"),
          p(
            "This behaviour mirrors the difference between a mean and a median in ",
            "basic statistics. If one very large value is added to a small set of ",
            "numbers, the mean shifts substantially, while the median barely ",
            "moves, because the median only depends on which value sits in the ",
            "middle, not on how extreme any single value is. LMS applies this same ",
            "principle to regression: because it optimises a median rather than a ",
            "sum, an outlier has little ability to distort the fitted line."
          ),
          p(
            "Unlike OLS, which has a direct mathematical formula for its solution, ",
            "LMS has no simple closed-form solution. This application finds the ",
            "LMS fit using numerical optimisation, searching for the intercept and ",
            "slope values that minimise the median squared residual."
          ),
          
          h4("Comparing OLS and LMS"),
          p(
            "On a clean dataset with no unusual observations, OLS and LMS will ",
            "typically produce similar fitted lines. The difference between the ",
            "two methods becomes clear once an outlier is present: OLS's fitted ",
            "line will shift noticeably toward the outlier, while LMS's fitted ",
            "line will remain close to the trend followed by the majority of the ",
            "data. The Comparison tab in this application allows you to see both ",
            "fitted lines together on your uploaded dataset, and observe this ",
            "behaviour directly."
          ),
          
          h4("Interpreting the results"),
          p(
            "When examining the results for your own dataset, consider whether ",
            "the OLS and LMS fitted lines are similar or noticeably different. A ",
            "large difference between the two suggests that one or more ",
            "observations in your data may be disproportionately influencing the ",
            "OLS fit. In this case, the LMS result may better represent the ",
            "underlying trend followed by most of your data, while the OLS result ",
            "may be distorted by those unusual points."
          ),
          p(
            "It is worth noting that robustness comes with a trade-off: LMS is ",
            "computationally more demanding than OLS, and does not have the same ",
            "well-established statistical inference tools (such as standard errors ",
            "and p-values) that are available for OLS."
          )
        )
      )
    )
  )
)


# -----------------------------
# SERVER
# -----------------------------

server <- function(input, output, session) {
  
  
  # Read the uploaded CSV file
  uploaded_data <- reactive({
    
    req(input$csv_file)
    
    tryCatch(
      
      read.csv(
        file = input$csv_file$datapath,
        header = input$header,
        sep = input$separator,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        na.strings = c("", "NA", "N/A")
      ),
      
      error = function(error_message) {
        
        showNotification(
          paste(
            "The CSV file could not be read:",
            error_message$message
          ),
          type = "error",
          duration = 8
        )
        
        return(NULL)
      }
    )
  })
  
  
  # Return the names of numeric columns
  numeric_columns <- reactive({
    
    data <- uploaded_data()
    
    req(!is.null(data))
    
    names(data)[
      vapply(
        data,
        is.numeric,
        logical(1)
      )
    ]
  })
  
  
  # Update variable selections after a file is uploaded
  observeEvent(uploaded_data(), {
    
    data <- uploaded_data()
    
    req(!is.null(data))
    
    numeric_names <- names(data)[
      vapply(
        data,
        is.numeric,
        logical(1)
      )
    ]
    
    if (length(numeric_names) > 0) {
      
      selected_y <- numeric_names[1]
      
      available_x <- setdiff(
        numeric_names,
        selected_y
      )
      
      selected_x <- if (length(available_x) > 0) {
        available_x[1]
      } else {
        character(0)
      }
      
      updateSelectInput(
        session = session,
        inputId = "y_var",
        choices = numeric_names,
        selected = selected_y
      )
      
      updateSelectInput(
        session = session,
        inputId = "x_vars",
        choices = available_x,
        selected = selected_x
      )
      
    } else {
      
      updateSelectInput(
        session = session,
        inputId = "y_var",
        choices = character(0)
      )
      
      updateSelectInput(
        session = session,
        inputId = "x_vars",
        choices = character(0)
      )
    }
  })
  
  
  # Remove the selected response variable from X choices
  observeEvent(input$y_var, {
    
    req(input$y_var)
    
    numeric_names <- numeric_columns()
    
    available_x <- setdiff(
      numeric_names,
      input$y_var
    )
    
    current_selection <- intersect(
      isolate(input$x_vars),
      available_x
    )
    
    if (
      length(current_selection) == 0 &&
      length(available_x) > 0
    ) {
      current_selection <- available_x[1]
    }
    
    updateSelectInput(
      session = session,
      inputId = "x_vars",
      choices = available_x,
      selected = current_selection
    )
  })
  
  
  # Validate the uploaded dataset
  validated_data <- reactive({
    
    data <- uploaded_data()
    
    validate(
      need(
        !is.null(data),
        "Please upload a valid CSV file."
      ),
      
      need(
        nrow(data) > 0,
        "The uploaded dataset contains no observations."
      ),
      
      need(
        ncol(data) > 0,
        "The uploaded dataset contains no variables."
      ),
      
      need(
        all(nzchar(trimws(names(data)))),
        "Every column in the dataset must have a name."
      ),
      
      need(
        !anyDuplicated(names(data)),
        "Column names must be unique."
      )
    )
    
    number_of_numeric_columns <- sum(
      vapply(
        data,
        is.numeric,
        logical(1)
      )
    )
    
    validate(
      need(
        number_of_numeric_columns >= 2,
        paste(
          "The dataset must contain at least two numeric",
          "columns for regression."
        )
      )
    )
    
    data
  })
  
  
  # Display file information
  output$file_status <- renderText({
    
    data <- validated_data()
    
    paste0(
      "File name: ",
      input$csv_file$name,
      "\nNumber of observations: ",
      nrow(data),
      "\nNumber of variables: ",
      ncol(data),
      "\nNumeric variables: ",
      paste(numeric_columns(), collapse = ", ")
    )
  })
  
  
  # Display the first 10 rows
  output$data_preview <- renderTable({
    
    data <- validated_data()
    
    head(data, 10)
    
  },
  striped = TRUE,
  bordered = TRUE,
  spacing = "s"
  )
  
  
  # Safely calculate the mean
  safe_mean <- function(variable) {
    
    if (all(is.na(variable))) {
      return(NA_real_)
    }
    
    mean(variable, na.rm = TRUE)
  }
  
  
  # Safely calculate the standard deviation
  safe_sd <- function(variable) {
    
    available_values <- variable[!is.na(variable)]
    
    if (length(available_values) < 2) {
      return(NA_real_)
    }
    
    sd(available_values)
  }
  
  
  # Display a summary of numeric variables
  output$numeric_summary <- renderTable({
    
    data <- validated_data()
    
    numeric_data <- data[
      vapply(
        data,
        is.numeric,
        logical(1)
      )
    ]
    
    summary_table <- data.frame(
      
      Variable = names(numeric_data),
      
      Missing_Values = vapply(
        numeric_data,
        function(variable) sum(is.na(variable)),
        integer(1)
      ),
      
      Mean = vapply(
        numeric_data,
        safe_mean,
        numeric(1)
      ),
      
      Standard_Deviation = vapply(
        numeric_data,
        safe_sd,
        numeric(1)
      ),
      
      Minimum = vapply(
        numeric_data,
        function(variable) {
          
          if (all(is.na(variable))) {
            NA_real_
          } else {
            min(variable, na.rm = TRUE)
          }
        },
        numeric(1)
      ),
      
      Maximum = vapply(
        numeric_data,
        function(variable) {
          
          if (all(is.na(variable))) {
            NA_real_
          } else {
            max(variable, na.rm = TRUE)
          }
        },
        numeric(1)
      )
    )
    
    summary_table$Mean <- round(
      summary_table$Mean,
      3
    )
    
    summary_table$Standard_Deviation <- round(
      summary_table$Standard_Deviation,
      3
    )
    
    summary_table$Minimum <- round(
      summary_table$Minimum,
      3
    )
    
    summary_table$Maximum <- round(
      summary_table$Maximum,
      3
    )
    
    summary_table
    
  },
  striped = TRUE,
  bordered = TRUE,
  spacing = "s"
  )
  
  
  # Display the selected variables
  output$selection_status <- renderText({
    
    validated_data()
    
    if (
      is.null(input$y_var) ||
      input$y_var == "" ||
      length(input$x_vars) == 0
    ) {
      
      return(
        paste(
          "Select one response variable and at least",
          "one explanatory variable."
        )
      )
    }
    
    paste0(
      "Response variable (Y): ",
      input$y_var,
      "\nExplanatory variable(s) (X): ",
      paste(input$x_vars, collapse = ", ")
    )
  })
  
  
  # Check selections when the button is clicked
  observeEvent(input$run_models, {
    
    if (is.null(input$csv_file)) {
      
      showNotification(
        "Please upload a CSV file before fitting the models.",
        type = "error"
      )
      
      return()
    }
    
    data <- uploaded_data()
    
    if (is.null(data)) {
      
      showNotification(
        "Please upload a valid CSV file.",
        type = "error"
      )
      
      return()
    }
    
    if (nrow(data) == 0 || ncol(data) == 0) {
      
      showNotification(
        "The uploaded dataset must contain observations and variables.",
        type = "error"
      )
      
      return()
    }
    
    if (
      !all(nzchar(trimws(names(data)))) ||
      anyDuplicated(names(data))
    ) {
      
      showNotification(
        "Every column must have a unique, non-empty name.",
        type = "error"
      )
      
      return()
    }
    
    numeric_names <- names(data)[
      vapply(
        data,
        is.numeric,
        logical(1)
      )
    ]
    
    if (length(numeric_names) < 2) {
      
      showNotification(
        "The dataset must contain at least two numeric columns.",
        type = "error"
      )
      
      return()
    }
    
    if (
      is.null(input$y_var) ||
      input$y_var == ""
    ) {
      
      showNotification(
        "Please select a response variable.",
        type = "error"
      )
      
      return()
    }
    
    if (length(input$x_vars) == 0) {
      
      showNotification(
        "Please select at least one explanatory variable.",
        type = "error"
      )
      
      return()
    }
    
    if (input$y_var %in% input$x_vars) {
      
      showNotification(
        paste(
          "The response variable cannot also be used",
          "as an explanatory variable."
        ),
        type = "error"
      )
      
      return()
    }
    
    selected_variables <- c(
      input$y_var,
      input$x_vars
    )
    
    if (!all(selected_variables %in% numeric_names)) {
      
      showNotification(
        "All selected variables must be numeric columns in the dataset.",
        type = "error"
      )
      
      return()
    }
    
    selected_data <- data[
      selected_variables
    ]
    
    missing_rows <- sum(
      !complete.cases(
        selected_data
      )
    )
    
    complete_data <- selected_data[
      complete.cases(selected_data),
      ,
      drop = FALSE
    ]
    
    minimum_rows <- length(input$x_vars) + 2
    
    if (nrow(complete_data) < minimum_rows) {
      
      showNotification(
        paste(
          "At least",
          minimum_rows,
          "complete rows are required for the selected variables."
        ),
        type = "error",
        duration = 8
      )
      
      return()
    }
    
    constant_variables <- names(complete_data)[
      vapply(
        complete_data,
        function(variable) length(unique(variable)) < 2,
        logical(1)
      )
    ]
    
    if (length(constant_variables) > 0) {
      
      showNotification(
        paste(
          "These variables have no variation:",
          paste(constant_variables, collapse = ", ")
        ),
        type = "error",
        duration = 8
      )
      
      return()
    }
    
    if (missing_rows > 0) {
      
      showNotification(
        paste(
          missing_rows,
          "rows contain missing values in the selected variables."
        ),
        type = "warning",
        duration = 7
      )
    }
    
    showNotification(
      paste(
        "The data and variables are ready.",
        "Open the OLS, LMS and Model Comparison tabs to view the results."
      ),
      type = "message",
      duration = 6
    )
  })

  # Use the same complete observations for both models so their results can
  # be compared directly.
  model_data <- eventReactive(input$run_models, {
    data <- validated_data()
    req(input$y_var, input$x_vars)

    selected_variables <- unique(c(input$y_var, input$x_vars))

    validate(
      need(
        all(selected_variables %in% names(data)),
        "The selected variables are not available in the uploaded dataset."
      ),
      need(
        all(vapply(data[selected_variables], is.numeric, logical(1))),
        "All selected variables must be numeric."
      )
    )

    complete_rows <- complete.cases(data[selected_variables])
    complete_data <- data[complete_rows, , drop = FALSE]
    minimum_rows <- length(input$x_vars) + 2

    validate(
      need(
        nrow(complete_data) >= minimum_rows,
        paste("At least", minimum_rows, "complete rows are required.")
      ),
      need(
        all(vapply(
          complete_data[selected_variables],
          function(variable) length(unique(variable)) >= 2,
          logical(1)
        )),
        "Every selected variable must contain at least two different values."
      )
    )

    complete_data
  }, ignoreInit = TRUE)

  # Fit the OLS model when the user clicks Fit and Compare Models.
  ols_model <- eventReactive(input$run_models, {
    data <- model_data()
    req(data, input$y_var, input$x_vars)
    fit_ols(data, input$x_vars, input$y_var)
  }, ignoreInit = TRUE)
  
  # 2. Hiển thị bảng summary mô hình OLS
  output$ols_summary <- renderPrint({
    req(ols_model())
    summary(ols_model())
  })
  
  # 3. Vẽ biểu đồ OLS Fit (Sử dụng ggplot2 và màu tím #9b59b6 đặc trưng của OLS)
  output$ols_plot <- renderPlot({
    req(ols_model(), validated_data())
    data <- validated_data()
    x_first <- input$x_vars[1]
    
    ggplot(data, aes_string(x = x_first, y = input$y_var)) +
      geom_point(color = "#3498db", size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", color = "#9b59b6", fill = "#e84393", se = TRUE) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("OLS Regression Fit:", input$y_var, "vs", x_first),
        x = x_first,
        y = input$y_var
      )
  })
  
  # 4. Vẽ biểu đồ Residual Diagnostics
  output$ols_residual_plot <- renderPlot({
    req(ols_model())
    model <- ols_model()
    
    res_df <- data.frame(
      Fitted = fitted(model),
      Residuals = residuals(model)
    )
    
    ggplot(res_df, aes(x = Fitted, y = Residuals)) +
      geom_point(color = "#9b59b6", size = 2.5, alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#e74c3c", linewidth = 1) +
      geom_smooth(method = "loess", color = "#e67e22", se = FALSE, linetype = "dotted") +
      theme_minimal(base_size = 14) +
      labs(
        title = "Residuals vs Fitted Values",
        x = "Fitted Values",
        y = "Residuals"
      )
  })

  # Fit the LMS model using the same observations as OLS.
  lms_result <- eventReactive(input$run_models, {
    data <- model_data()
    req(data, input$y_var, input$x_vars)

    result <- fit_lms(data, input$x_vars, input$y_var)

    validate(
      need(
        is.list(result) && !is.null(result$par),
        "The LMS optimisation did not return model coefficients."
      )
    )

    result
  }, ignoreInit = TRUE)

  output$lms_coefficients <- renderTable({
    req(lms_result())
    get_lms_coefficients(lms_result())
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$lms_summary <- renderPrint({
    data <- model_data()
    result <- lms_result()
    req(data, result)

    statistics <- get_lms_summary_stats(
      result,
      data,
      input$x_vars,
      input$y_var
    )

    cat("Observations used:", nrow(data), "\n")
    cat("Median squared residual:", round(result$value, 4), "\n")
    cat("R-squared (descriptive):", statistics$r_squared, "\n")
    cat("Adjusted R-squared (descriptive):", statistics$adjusted_r_squared, "\n")
    cat("Residual standard error:", statistics$residual_standard_error, "\n")
    cat("Optimisation convergence code:", result$convergence, "\n")
    cat("(A convergence code of 0 indicates successful optimisation.)\n")
  })

  output$lms_plot <- renderPlot({
    req(lms_result(), model_data())
    plot_lms_fit(
      model_data(),
      input$x_vars,
      input$y_var,
      lms_result()
    )
  })

  output$lms_residual_plot <- renderPlot({
    req(lms_result(), model_data())
    plot_lms_residuals(
      model_data(),
      input$x_vars,
      input$y_var,
      lms_result()
    )
  })

  output$comparison_plot <- renderPlot({
    req(ols_model(), lms_result(), model_data())
    plot_comparison(
      model_data(),
      input$x_vars,
      input$y_var,
      ols_model(),
      lms_result()
    )
  })

  output$coefficient_comparison <- renderTable({
    req(ols_model(), lms_result())
    compare_coefficients(input$x_vars, ols_model(), lms_result())
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$comparison_summary <- renderText({
    req(ols_model(), lms_result())

    coefficient_table <- compare_coefficients(
      input$x_vars,
      ols_model(),
      lms_result()
    )

    largest_difference <- max(
      abs(coefficient_table$OLS_Estimate - coefficient_table$LMS_Estimate),
      na.rm = TRUE
    )

    paste0(
      "Largest absolute coefficient difference: ",
      round(largest_difference, 4),
      ". Compare the fitted lines or actual-versus-fitted points to decide ",
      "whether unusual observations are materially changing the OLS fit."
    )
  })
  
}


# -----------------------------
# RUN THE APPLICATION
# -----------------------------

shinyApp(
  ui = ui,
  server = server
)

#library(shinya11y) # Used to check accessibility
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(DT)
library(shinycssloaders)
library(plotly)
   

enableBookmarking("url")


# Read in Energy Consumption data
energy_df <- read.csv("Energy_consumption_dataset.csv")

# Factorise categorical data to enable use in linear modelling
energy_df$DayOfWeek <- factor(energy_df$DayOfWeek,
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) # Set order of weekdays
energy_df$Holiday <- as.factor(energy_df$Holiday)
energy_df$HVACUsage <- as.factor(energy_df$HVACUsage)
energy_df$LightingUsage <- as.factor(energy_df$LightingUsage)


# Amended labels for display
variables <- c(
  "Month" = "Month", # 1-12
  "Hour" = "Hour",
  "Day of Week" = "DayOfWeek",
  "Holiday" = "Holiday",
  "Temperature °C" = "Temperature",
  "Humidity" = "Humidity",
  "Square Footage" = "SquareFootage",
  "Occupancy" = "Occupancy",
  "HVAC Usage" = "HVACUsage",
  "Lighting Usage" = "LightingUsage",
  "Renewable Energy" = "RenewableEnergy"
)


ui <- function(request) {
  
    dashboardPage(
      skin = "blue",
      dashboardHeader(
        titleWidth = 300,
        title = tags$div(
          class = "logo",
          style = "display: flex; align-items: center; width: 100%; white-space: nowrap;",
          tags$img(src = 'LR-logo.png', height = "50px", alt = "Linear Regression Logo", `aria-label` = "logo")
          )
        ),

    dashboardSidebar(
      width = 300,  # Set sidebar width to 300
        sidebarMenu(
          id = "sidebarMenu", `aria-label` = "Main Navigation",
          menuItem("Home", tabName = "home", icon = icon("home")),
          menuItem("Learn Linear Regression", tabName = "learn", icon = icon("book")),
          menuItem("Exploratory Data Analysis", tabName = "explore", icon = icon("chart-simple")),
          menuItem("Linear Model Building", tabName = "build", icon = icon("gears")),
          menuItem("Prediction", tabName = "predict", icon = icon("bullseye")),
          br(),
          bookmarkButton()
        )
    ),
    
    dashboardBody(
      tags$html(lang="en"), # Explicitly stated the language to aid accessibility     
      #use_tota11y(), # Used to assess and improve accessibility 
            
      tags$head(
        tags$style(HTML("  
        
        # Used custom styling to edit theme 
        # Used contrasting colours to enhance accessibility
        # Blue/Gray 2C3E52, Turquoise #42e6ff, Gray #4c5a64
        
          .skin-blue .logo {
          background-color: #2C3E52 !important;
          }
          .skin-blue .main-header .logo { 
          background-color: #2C3E52; 
          }
          .skin-blue .main-header .navbar { 
          background-color: #2C3E52; 
          }
          .skin-blue .main-header .logo:hover { 
          background-color: #2C3E52; 
          }
          .skin-blue .main-header .navbar .sidebar-toggle:hover { 
          background-color: #4c5a64; 
          }
          .skin-blue .main-sidebar { 
          background-color: #2C3E52; 
          }
          .skin-blue .sidebar-menu li a { 
          color: #ffffff;
          }
          .skin-blue .sidebar-menu > li.active > a { 
          background-color: #4c5a64; 
          }
          .skin-blue .sidebar-menu > li:hover > a { 
          border-left-color: #42e6ff; 
          }
          .sidebar-menu li a i { 
          margin-right: 8px; 
          }
          .btn-custom-nav { 
          background-color: #2C3E52; color: white; border-color: #2C3E52; 
          }
          .btn-custom-nav:hover, .btn-custom-nav:focus, .btn-custom-nav:active { 
          background-color: #3d5269; color: white; border-color: #2C3E52; 
          }
          .equation-container { 
          overflow-x: auto; max-width: 100%; padding: 10px 0; # Formatting equation container to use a L-R scroll bar
          }
          .MathJax_Display { 
          overflow-x: auto; overflow-y: hidden; # Applied styling for equation formation
          }
          .noUi-connect {
          background: #42e6ff !important; # Custom syling for slider inputs
          }
        "))
      ),
      
        tabItems(

### HOME TAB
          
          tabItem(
            tabName = "home",
            tags$h1("Linear Regression"),
            
            # Used combination of columns and boxes inside fluid row to achieve layout and a dynamic fit
            fluidRow( 
              box(
                width = 12,
                # App intro text and button to download user guide
                tags$h2("Learn how to model and understand linear regression."),
                tags$a("Download User Guide", href = "lra-user-guide.pdf", target = "_blank",  class = "btn btn-custom-nav", `aria-label` = "Download User Guide")
              )
            ),
            
            # Used navigation call to action (CTA) boxes to link to the key tabs, guiding users to content
            fluidRow(
              # Learn CTA 
              column(
                width = 6,
                box(
                  width = NULL,
                  style = "display: flex; flex-direction: column; height: 260px;", # Set height for all boxes for consistent size, independent of contents 
                  div(style = "text-align: center",
                      tags$i(class = "fa fa-book fa-5x", alt ="book icon", style = "color: #42e6ff; width: 100%; max-width: 100px;"), # Included icons to aid understanding of what is being linked to 
                      tags$div(style = "margin-bottom: 15px;") # margin to improve layout
                  ),
                  div(style = "text-align: center; font-size: 20px;", "Learn the theory behind linear regression!"), # Text to explain purpose of linked content 
                  div(style = "margin-top: auto;", actionButton("goto_learn", "LEARN THEORY", class = "btn btn-custom-nav btn-block", `aria-label` = "link to Learn"))  #Link to the tab
                )
              ),            
              # EDA CTA
              column(
                width = 6,
                box(
                  width = NULL,
                  style = "display: flex; flex-direction: column; height: 260px;",
                  div(style = "text-align: center",
                      tags$i(class = "fa fa-chart-simple fa-5x",alt ="graph icon", style = "color: #42e6ff; width: 100%; max-width: 100px;"),
                      tags$div(style = "margin-bottom: 15px;")
                  ),
                  div(style = "text-align: center; font-size: 20px;", "Carry out exploratory data analysis!"),
                  div(style = "margin-top: auto;", actionButton("goto_explore", "EXPLORE DATA", class = "btn btn-custom-nav btn-block", `aria-label` = "link to Explore EDA"))
                )
              )
            ),
            fluidRow(
              # Build CTA
              column(
                width = 6,
                box(
                  width = NULL,
                  style = "display: flex; flex-direction: column; height: 260px;", 
                  div(style = "text-align: center;",
                      tags$i(class = "fa fa-gears fa-5x",  alt ="gears icon", style = "color: #42e6ff; width: 100%; max-width: 100px;"),
                      tags$div(style = "margin-bottom: 15px;")
                  ),
                  div(style = "text-align: center; font-size: 20px;", "Test impact of variables on statistics, equation and plots!"),
                  div(style = "margin-top: auto;", actionButton("goto_build", "BUILD MODEL", class = "btn btn-custom-nav btn-block", `aria-label` = "link to Build Model"))
                )
              ),
              # Predict CTA
              column(
                width = 6,
                box(
                  width = NULL,
                  style = "display: flex; flex-direction: column; height: 260px;",
                  div(style = "text-align: center;",
                      tags$i(class = "fa fa-bullseye fa-5x", alt ="a bullseye", style = "color: #42e6ff; width: 100%; max-width: 100px;"),
                      tags$div(style = "margin-bottom: 15px;")
                  ),
                  div(style = "text-align: center; font-size: 20px;", "Set variable values and get a prediction!"),
                  div(style = "margin-top: auto;", actionButton("goto_predict", "PREDICT RESULTS", class = "btn btn-custom-nav btn-block", `aria-label` = "link to Prediction"))
                )
              )
            )
          ),

### LEARN TAB

          tabItem(
            tabName = "learn",
            tags$h1("Linear Regression Theory"),
            fluidRow( 
              box(
                width = 12,
                # Added tabbed on page content to allow easy navigation of information without needing to scroll
                tabsetPanel( 
                  tabPanel("Introduction", 
                           tags$h2("What is Linear Regression?"),
                           tags$p("Linear regression is used to model the relationship between a dependent variable and one (or more) independent variables by fitting a linear equation."),
                           tags$p("This is the form of a linear regression equation with one independent variable:"),
                           withMathJax(HTML("<p>$$Y = \\beta_0 + \\beta_1 X + \\varepsilon$$</p>")),
                           tags$p("Where:"),
                           tags$ul(
                             tags$li("Y is the dependent variable and is determined by the value of the independent variable, X."),
                             tags$li("X is the independent variable or variables."),
                             tags$li("Bo is the intercept. It is the value of Y when X = 0)."),
                             tags$li("B1 represents the slope of the regression line. It represents how the value of Y changes for a one unit change in X)."),
                             tags$li("E is the error term (the amount of variation that the model doesn't explain).")
                           ),
                           tags$p("View the linear regression equations that are created when selecting different variables on the model building page (model equation tab) ")
                           ),
                  
                  tabPanel("Key Assumptions", 
                           tags$h2("Assumptions of Linear Regression"),
                           tags$p("For linear regression to be valid, several assumptions should be met:"),
                           tags$ul(
                             tags$li("Linearity: There is a linear relationship between the dependent and independent variables."),
                             tags$li("Normality: The residuals are normally distributed."),
                             tags$li("Independence: The observations are independent of each other."),
                             tags$li("Homoscedasticity: The residuals have an even variance."),
                             tags$li("No Multicollinearity: The variables are not highly correlated with each other.")
                           ),
                           tags$p("Use the diagnostic plots on the model building page to check if these assumptions are met.")
                           ),
                  
                  tabPanel("Variable Selection", 
                           tags$h2("How to Select Variables"),
                           tags$p("Selecting the right variable is important for building a good regression model:"),
                           tags$ul(
                             tags$li("Theoretical relevance: In context, variables should have a logical connection to what you're predicting."),
                             tags$li("Statistical significance: Look for predictors with low p-values (typically < 0.05)."),
                             tags$li("Adjusted R-squared: A higher adjusted r-squared value generally indicates a better model."),
                             tags$li("Parsimony: Simpler models using only the necessary variables often perform better and avoid overfitting."),
                             tags$li("Multicollinearity: Avoid using highly correlated predictors in the same model.")
                           ),
                           tags$p("Use the Exploratory Data Anaylsis tool to see what effect inidividual variables have on the model.")
                           ), 
                  
                  tabPanel("Interpreting Results", 
                           tags$h2("How to Interpret Regression Output"),
                           tags$ul(
                             tags$li("Coefficients: The coefficient for each variable represents the average change in the dependent variable for a one-unit change in the variable."),
                             tags$li("p-values: A p-value < 0.05 indicates that the variable is significant"),
                             tags$li("R-squared: The amount of variance in the dependent variable that can be predicted from the independent variable. The value ranges from 0 - 1, the closer to 1, the more accurate the prediction."),
                             tags$li("Adjusted R-squared: Adjusts the R value based on the number of predictors in the model.")
                             ),
                           tags$p("Use the Model Building tool to see what impact individual variables have on the output")
                           )
                  )
                )
              )
            ),


### EDA TAB

          tabItem(
            tabName = "explore",
            tags$h1("Exploratory Data Analysis"),
            fluidRow(
              column( # Used combination of columns and boxes inside fluid row to achieve layout with a dynamic fit
                width = 6,
                box(
                  width = NULL,
                  tags$p("Choose a variable from the dropdown below to see the summary statistics, scatterplot against energy consumption, and Boxplot and Histogram of the selected variable."),  
                  selectInput("eda_var", "Select Variable:", choices = variables, selected = "Temperature"),
                  tags$h2("\nSummary Statistics"), 
                  verbatimTextOutput("eda_summary_stats")
                )
              ),
              column(
                width = 6,
                box(
                  title = "Scatterplot",          
                  width = NULL,
                  withSpinner(plotlyOutput("eda_scatter", height = "400px"), color = "#42e6ff") # Applied spinner and plotly attributes
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                box(
                  title = "Boxplot", 
                  width = NULL,
                  withSpinner(plotlyOutput("eda_box", height = "400px"), color = "#42e6ff") # Applied spinner and plotly attributes
                )
              ),
              column(
                width = 6,
                box(
                  title = "Histogram",                 
                  width = NULL,
                  withSpinner(plotlyOutput("eda_hist", height = "400px"), color = "#42e6ff") # Applied spinner and plotly attributes
                )
              )
            )
          ),

### MODEL BUILD TAB

          tabItem(
            tabName = "build",
            tags$h1("Model Building"),
            fluidRow( 
              box(
                width = 12,
                # Page intro text to guide user
                tags$p("Use the plots and statistical information below to find out how different variables impact the predicted energy consumption."),
                tags$ul(
                  tags$li("To start, the temperature data has been selected. Is there a relationship between energy consumption and temperature?"),
                  tags$li("Select another variable to see the impact on the linear regression model."),
                  tags$li("Try different combinations of variables. What impact are they having on the model?"),
                  tags$li("Investigate the model summary details. What is happening to the R-Squared and P-Value?"),
                  tags$li("Use the variable selection with summary statistics to perform backward and forward regression selection to define effective models.")
              )
            )
          ),      
            fluidRow(
              box(
                width = 12, 
                 fluidRow(
                  column(
                    width = 4,
                    box(
                      h2("Linear Regression Inputs"),
                      width = NULL,
                      # Checkbox input to select variables for outputs
                      checkboxGroupInput("build_variables", "Choose Variables:", choices = variables, selected = "Temperature"),
                      fluidRow(
                        column(12, actionButton("select_all", "Select All", class = "btn btn-custom-nav btn-sm")) # Select All Button, with custom colour
                      ),
                      fluidRow(
                        style = "margin-top: 4px;", # Added margin to improve spacing of buttons
                        column(12, actionButton("deselect_all", "Deselect All", class = "btn-default btn-sm")) # Deselect All Button
                      )
                    )
                  ),
                  column(
                    width = 8,
                    box(
                      width = NULL,
                      title = "Linear Regression Scatterplot",
                      # Applied spinner and plotly attributes to linear regression plot output
                      withSpinner(plotlyOutput("lr_plot", height = "400px"), color = "#42e6ff") 
                    )
                  )
                 )
              )
            ),
            fluidRow(
              box(
                title = "Regression Model Details",
                width = 12,
                tabsetPanel( 
                  # Applied spinners. Text output for the model summary. uiOutput to display dynamic model equation based on selected variables.
                  tabPanel("Model Summary", withSpinner(verbatimTextOutput("model_summary"), color = "#42e6ff"), 
                           tags$p("The summary output above shows the coefficients, their significance, and overall model fit statistics.")),
                  tabPanel("Model Equation", withSpinner(uiOutput("model_equation"), color = "#42e6ff"), 
                           tags$p("This equation shows the mathematical relationship between the response variable and predictors."))
                )
              )
            ),
            fluidRow(
              # Added diagnostic plot outputs and applied spinners.
              # Information about how to interpret the plots is detailed below each one. 
              box(title = "Residual v Fitted Plot", width = 6, withSpinner(plotlyOutput("rvfPlot", height = "400px"), color = "#42e6ff"),
                  tags$div(
                tags$h4("Interpreting Residual Plots:"),
                tags$p("Residual plots helps to assess the degree of homoscedasticity and linearity."),
                tags$ul(
                  tags$li("Random scatter: Indicates constant variance."),
                  tags$li("Funnel shape: Suggests heteroscedasticity."),
                  tags$li("Curved pattern: May indicate non-linearity.")
                )
              )),
              box(title = "Normal QQ Plot", width = 6, withSpinner(plotlyOutput("qqPlot", height = "400px"), color = "#42e6ff"),
                  tags$div(
                    tags$h4("Interpreting QQ Plots:"),
                    tags$p("The QQ (Quantile-Quantile) plot helps to assess if the residuals follow a normal distribution."),
                    tags$ul(
                      tags$li("Straight diagonal line: Residuals are normally distributed."),
                      tags$li("S-shaped curve: Indicates skewness."),
                      tags$li(("Curling at ends: May indicate heavy or light tails.")
                    )
                  ))
            ),
            fluidRow(
              box(title = "Scale Location Plot", width = 6, withSpinner(plotlyOutput("sclLocPlot", height = "400px"), color = "#42e6ff"),
                  tags$div(
                    tags$h4("Interpreting Scale-Location Plot:"),
                    tags$p("The Scale-Location plot shows the square root of the standardised residuals agianst the fitted values, which helps assess the spread of residuals."),
                    tags$ul(
                      tags$li("Horizontal line: Indicates a consistant spread (homoscedasticity)."),
                      tags$li("Upward/Downward trend: Suggests increasing or decreasing variance (heteroscedasticity)."),
                      tags$li("Scattered points: Random spreading of points suggests the model fits well across the range of fitted values.")
                    )
                  )),
              box(title = "Residual v Leverage Plot", width = 6, withSpinner(plotlyOutput("rvlevPlot", height = "400px"), color = "#42e6ff"),
                  tags$div(
                    tags$h4("Interpreting Residuals v Leverage Plot:"),
                    tags$p("The Residual v Leverage plot helps identify influential data points by combining information about residuals and leverage."),
                    tags$ul(
                      tags$li("High leverage points: Data points that have unusual predictor values can disproportionately affect the model."),
                      tags$li("Large residuals: May highlight poor fit or potential outliers."),
                      tags$li("Cook's Distance: Points with high Cook's Distance (bigger sized points) highlight influential observations that can impact on the model estimates.")
                    )
                  ))
            )
          )),

### PREDICT TAB

          tabItem(
            tabName = "predict",
            tags$h1("Predict Energy Consumption"),
            fluidRow(
              box(
                width = 12,
                tags$p("Set predictor variables and estimate energy consumption."),
                tags$p("Enter values for all predictors below, then click 'Predict Energy Consumption' to see the results.")
              )
            ),
            # Used combination of nested columns and boxes inside fluid row to achieve layout and a dynamic fit
            fluidRow(
              box(
                width = 12,
                style = "background-color: white; padding: 20px; margin: 10px;",
                fluidRow(
                  column(
                    width = 6,
                    box(
                      width = NULL,
                      # Added borders and padding for clarity
                      # Slider fill colours set in CSS styling to match theme style
                      # Used customisable noUiSliderInput to allow user to set values for prediction output
                      # Start value set to mid points, and step and markers levels picked for ease of use and clarity
                      # Min/max scale correlates to data
                      style = "padding: 10px;",
                      # Slider inputs for numeric predictors

                      div(style = "border: 2px solid #42e6ff; padding: 15px; margin-bottom: 10px;",
                          noUiSliderInput(
                            inputId = "hour_input",
                            label = div(style = "margin-top: 15px; font-weight: bold;", "Hour (0-23):"),
                            min = 0,
                            max = 23,
                            value = 12,
                            step = 1,
                            pips = list(mode = "steps", density = 4)
                          )
                      ),
                      div(style = "border: 2px solid #42e6ff; padding: 15px; margin-bottom: 10px;",
                          noUiSliderInput(
                            inputId = "sf_input",
                            label = div(style = "margin-top: 15px; font-weight: bold;", "Square Footage:"),
                            min = 1000,
                            max = 2000,
                            value = 1500,
                            step = 100,
                            pips = list(mode = "steps", density = 4)
                          )
                      ),
                      div(style = "border: 2px solid #42e6ff; padding: 15px;",
                          noUiSliderInput(
                            inputId = "humidity_input",
                            label = div(style = "margin-top: 15px; font-weight: bold;", "Humidity (%):"),
                            min = 30,
                            max = 60,
                            value = 45,
                            step = 5,
                            pips = list(mode = "steps", density = 4)
                          )
                      )
                    )
                  ),
                  column(
                    width = 6,
                    box(
                      width = NULL,
                      style = "padding: 10px;",
                      div(style = "border: 2px solid #42e6ff; padding: 15px; margin-bottom: 10px;",
                          noUiSliderInput(
                            inputId = "renewable_input",
                            label = div(style = "margin-top: 10px; font-weight: bold;", "Renewable Energy (%):"),
                            min = 0,
                            max = 30,
                            value = 15,
                            step = 1,
                            pips = list(mode = "steps", density = 4)
                          )
                      ),
                      div(style = "border: 2px solid #42e6ff; padding: 15px; margin-bottom: 10px;",
                          noUiSliderInput(
                            inputId = "temperature_input",
                            label = div(style = "margin-top: 15px; font-weight: bold;", "Temperature °C:"),
                            min = 20,
                            max = 30,
                            value = 25,
                            step = 1,
                            pips = list(mode = "steps", density = 4)
                          )
                      ),
                      #Dropdown select input chosen for month and day of week input
                      selectInput("month_input", "Month:",
                                  choices = setNames(1:12, c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                                  selected = 1),
                      selectInput("day_input", "Day of Week:",
                                  choices = levels(energy_df$DayOfWeek),
                                  selected = levels(energy_df$DayOfWeek)[1])
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    tags$div(
                      #Radio buttons selected for input of holiday, HVAC and Lighting variables, all set to positive
                      style = "display: inline-block; margin-right: 20px;",
                      radioButtons("holiday_input", "Holiday:",
                                   choices = c("Yes", "No"), selected = "Yes", inline = TRUE)
                    ),
                    tags$div(
                      style = "display: inline-block; margin-right: 20px;",
                      radioButtons("hvac_input", "HVAC Usage:",
                                   choices = c("On", "Off"), selected = "On", inline = TRUE)
                    ),
                    tags$div(
                      style = "display: inline-block; margin-right: 20px;",
                      radioButtons("lighting_input", "Lighting Usage:",
                                   choices = c("On", "Off"), selected = "On", inline = TRUE)
                    ),
                    br(),
                    box(
                      width = 12,
                      tags$p("Set variable values above, then click button to return Energy Consumption prediction."),
                      # Button actions the collection of the variable inputs and outputs the event reactive prediction result
                      actionButton("predict_btn", "Predict Energy Consumption", class = "btn btn-custom-nav btn-block", style = "margin-top: 10px;"),
                      h2("Prediction Results"),
                      style = "border: 2px solid #2C3E52;",
                      withSpinner(verbatimTextOutput("prediction_result"), color = "#42e6ff") # Text output with spinner
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    box(
                      width = NULL,
                      title = "How to Interpret These Results",
                      solidHeader = TRUE,
                      tags$p("The prediction shows the estimated energy consumption based on your inputs, with a 95% confidence interval."),
                      tags$ul(
                        tags$li("Point Estimate: This is the most likely energy consumption value based on the selected input values."),
                        tags$li("Confidence Interval: This is the range in which the actual value is likely to fall, with 95% confidence."),
                        tags$li("Wide Interval: A wide confidence interval indicates more uncertainty in the prediction."),
                        tags$li("Narrow Interval: A narrow confidence interval suggests higher prediction precision.")
                      ),
                      tags$p("Try adjusting different values to see how they impact energy consumption estimate and confidence range.")
                    )
                  )
                )
              )
            )
          )
        ),

    ))
}
           
# Define Server function
server <- function(input, output, session) {


    
  # Navigation button links to tabs from homepage boxes
  observeEvent(input$goto_learn, { 
    updateTabItems(session, "sidebarMenu", "learn") 
    })
  observeEvent(input$goto_explore, {
    updateTabItems(session, "sidebarMenu", "explore")
    })
  observeEvent(input$goto_build, {
    updateTabItems(session, "sidebarMenu", "build")
    })
  observeEvent(input$goto_predict, {
    updateTabItems(session, "sidebarMenu", "predict")
    })
  
  # Observe when select all and deselect all buttons are actioned for Model Building
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "build_variables", selected = unname(variables))
    })
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "build_variables", selected = character(0))
    })
  
### Exploratory Data Analysis
  # Compile and display relevant summary statistics for EDA
  # Concatenate strings for display
  output$eda_summary_stats <- renderPrint({ 
    req(input$eda_var)
    if (!is.numeric(energy_df[[input$eda_var]])) { # Check that the selected variable is numeric
      cat("Summary unavailable for non-numeric variables.") #If not numeric, handle with explanation
    } else {
      cat("Summary Statistics for", input$eda_var, ":\n")
      print(summary(energy_df[[input$eda_var]]))  # use the print function to display the summary statistics
      cat("\nStandard Deviation for", input$eda_var, ":\n", round(sd(energy_df[[input$eda_var]], na.rm = TRUE), 3)) #Calculate standard deviation and round to 3 decimal places
      formula_str <- paste("EnergyConsumption ~", input$eda_var)
      model <- lm(as.formula(formula_str), data = energy_df) #Fit the linear model to the formula
      model_summary <- summary(model) # Create summary to generate statistical output
      intercept <- coef(model)[1] #Coefficient for the intercept
      slope     <- coef(model)[2] #Coefficient for the slope
      r_squared <- model_summary$r.squared #Rsquared value
      adj_r2    <- model_summary$adj.r.squared #Adjusted Rsquared value 
      cat("\n\nRegression Model Results:") #Concatenate and print model results
      cat("\nFormula:", formula_str)
      cat("\nIntercept:", round(intercept, 3))
      cat("\nSlope:", round(slope, 3))
      cat("\nR-squared:", round(r_squared, 3))
      cat("\nAdjusted R-squared:", round(adj_r2, 3))
    }
  })


  # Scatterplot for EDA based on variable input selected 
  output$eda_scatter <- renderPlotly({
    req(input$eda_var) #Require input from EDA var
    eda_scat_plot <- ggplot(energy_df, aes_string(x = input$eda_var, y = "EnergyConsumption")) +
      geom_point(alpha = 0.7) +
      labs(x = input$eda_var, y = "Energy Consumption") +
      theme_minimal()
    ggplotly(eda_scat_plot) #Convert to plotly
  })
  

  # Histogram for EDA based on variable input selected (error handling for non-numeric inputs)
  output$eda_hist <- renderPlotly({
    req(input$eda_var) #Require input from EDA var
    if (!is.numeric(energy_df[[input$eda_var]])) {
      eda_hist_plot <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Histogram unavailable for non-numeric variable", hjust = 0.5, vjust = 0.5) +
        theme_void()
      return(ggplotly(eda_hist_plot)) 
    }
    eda_hist_plot <- ggplot(energy_df, aes_string(x = input$eda_var)) +
      geom_histogram(fill = "#caf6fc", color = "#2C3E52", bins = 20) +
      labs(x = input$eda_var, y = "Count") +
      theme_minimal() 
    ggplotly(eda_hist_plot) #Convert to plotly
  })
  

  # Boxplot for EDA based on variable input selected (error handling for non-numeric inputs)
  output$eda_box <- renderPlotly({
    req(input$eda_var) #Require input from EDA var
    if (!is.numeric(energy_df[[input$eda_var]])) {
      eda_box_plot <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Boxplot unavailable for non-numeric variable") +
        theme_minimal()
      return(ggplotly(eda_box_plot)) 
    }
    eda_box_plot <- ggplot(energy_df, aes_string(x = "1", y = input$eda_var)) +
      geom_boxplot(fill = "#caf6fc", color = "#2C3E52") +
      labs(x = "", y = input$eda_var) +
      theme_minimal() +
      theme(axis.title = element_text(size = 12))
    ggplotly(eda_box_plot) #Convert to plotly
  })
  

  
  ### Model Building

    # Build linear model based on selection of variables from reactive checkbox input 
    build_model <- reactive({ 
    req(input$build_variables, length(input$build_variables) > 0) #Requires build_variable inputs
    formula_lr <- paste("EnergyConsumption ~", paste(input$build_variables, collapse = " + "))
    lm(as.formula(formula_lr), data = energy_df)
  })
  
    # Create summary of linear model to display
  output$model_summary <- renderPrint({
    model <- build_model()
    req(model)
    summary(model)
  })
  

  # Build equation of linear model and use mathjax to render
  output$model_equation <- renderUI({
    model <- build_model()
    req(model) 
    coefs <- coef(model)
    coefs_round <- round(coefs, 3)
    eq <- "\\text{Energy Consumption} = ["
    eq <- paste0(eq, coefs_round[1], "]")
    for (i in 2:length(coefs_round)) {
      coef_label <- names(coefs_round)[i]
      coef_value <- coefs_round[i]
      if (coef_value >= 0) {
        eq <- paste0(eq, " + [", coef_value, "] \\cdot \\text{", coef_label, "}") 
      } else {
        eq <- paste0(eq, " - [", abs(coef_value), "] \\cdot \\text{", coef_label, "}")
      }
    }
    div(class = "equation-container", withMathJax(HTML(paste0("<h4>$$", eq, "$$</h4>"))))
  })
 
  # Plot linear regression as scatterplot, utlising plotly
  output$lr_plot <- renderPlotly({
    req(input$build_variables) #Requires build_variable inputs
    n <- length(input$build_variables) #Count number of variables selected
    if(n == 0) {
      plot_lrm <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Please select at least one variable") + # If none, prompt selection
        theme_void()
    } else if(n == 1) { # 1 Variable selected
      plot_lrm <- ggplot(energy_df, aes_string(x = input$build_variables, y = "EnergyConsumption")) + # Build scatterplot
        geom_point(alpha = 0.7) +
        geom_smooth(method = "lm", se = TRUE, color = "#42e6ff") +
        labs(x = input$build_variables, y = "Energy Consumption (kWh)") +
        theme_minimal()
    } else { # More than 1 variable, build a multiple regression model
      model <- build_model()
      req(model)
      plot_df <- data.frame( #Create df with EnergyConsumption values and predicted values from model
        Observed = energy_df$EnergyConsumption,
        Fitted = predict(model, newdata = energy_df)
      )
      plot_lrm <- ggplot(plot_df, aes(x = Fitted, y = Observed)) + #Create plot of fitted v observed values with abline
        geom_point(alpha = 0.7) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#42e6ff", linewidth = 1) +
        labs(x = "Fitted Energy Consumption (kWh)", y = "Observed Energy Consumption (kWh)") +
        theme_minimal()
    }
    ggplotly(plot_lrm) #Convert to interactive plotly plot
  })
  

  # Create and render diagnostic plots, utlising plotly
  diagPlot <- function(model){
    mod_df <- data.frame( # Dataframe to capture required information
      fitted = fitted(model), # Predicted values from model
      resid = residuals(model), # Residuals from model
      stdresid = rstandard(model), # Residuals scaled by standard deviation
      hat = hatvalues(model), # Leverage values
      cooksd = cooks.distance(model) #Observation influence
    )
    p1 <- ggplot(mod_df, aes(x = fitted, y = resid)) +
      geom_point(alpha = 0.5) +
      stat_smooth(method="loess", color="#42e6ff") + # Display trend
      geom_hline(yintercept=0, col="purple", linetype="dashed") + # Display residual distribution
      xlab("Fitted values") +
      ylab("Residuals") +
      theme_minimal()
    
    p2 <- ggplot(mod_df, aes(sample = stdresid)) +
      stat_qq(alpha = 0.5, na.rm = TRUE) + # plot QQ points
      stat_qq_line(na.rm = TRUE, color = "#42e6ff") + #QQ line for reference
      xlab("Theoretical Quantiles") +
      ylab("Standardised Residuals") +
      theme_minimal()
    
    p3 <- ggplot(mod_df, aes(x = fitted, y = sqrt(abs(stdresid)))) +
      geom_point(alpha = 0.5, na.rm = TRUE) + #Plot data points
      stat_smooth(method="loess",na.rm = TRUE, color="#42e6ff") + #Add trend line
      xlab("Fitted Value") +
      ylab("√|Standardised residuals|") +
      theme_minimal()

    p4 <- ggplot(mod_df, aes(x = hat, y = stdresid, size = cooksd)) +
      geom_point(alpha = 0.5, na.rm = TRUE) + #Plot data points
      stat_smooth(aes(x = hat, y = stdresid, group = 1), #Add trend line
                  method = "loess", na.rm = TRUE, color = "#42e6ff",
                  inherit.aes = FALSE) +
      xlab("Leverage") +
      ylab("Standardised Residuals") +
      scale_size_continuous("Cook's Distance", range = c(1, 5)) +
      theme_minimal() +
      theme(legend.position = "bottom")

      list(rvfPlot = ggplotly(p1), #Return plotly for all
         qqPlot = ggplotly(p2),
         sclLocPlot = ggplotly(p3),
         rvlevPlot = ggplotly(p4))
  }
  
  # Block to observe all diagnostic plots, refreshes when reactive inputs change
  observe({
    model <- build_model()
    req(model)
    diag_plots <- diagPlot(model)
    output$rvfPlot <- renderPlotly({ diag_plots$rvfPlot })
    output$qqPlot <- renderPlotly({ diag_plots$qqPlot })
    output$sclLocPlot <- renderPlotly({ diag_plots$sclLocPlot })
    output$rvlevPlot <- renderPlotly({ diag_plots$rvlevPlot })
  })
  
  
  ### Prediction Tab
  
  # Collect all input values into a data frame for prediction
  create_pred_data <- function() {
    data.frame(
      Hour = input$hour_input,
      SquareFootage = input$sf_input,
      RenewableEnergy = input$renewable_input,
      Temperature = input$temperature_input,
      Humidity = input$humidity_input,
      Month = as.numeric(input$month_input),
      DayOfWeek = as.factor(input$day_input),
      Holiday = as.factor(input$holiday_input),
      HVACUsage = as.factor(input$hvac_input),
      LightingUsage = as.factor(input$lighting_input)
    )
  }
  
  # Prediction model. Reacts and generates output when button pressed 
  predict_model_result <- eventReactive(input$predict_btn, {
    formula_lr <- "EnergyConsumption ~ Hour + SquareFootage + RenewableEnergy + Temperature + Humidity + Month + DayOfWeek + Holiday + HVACUsage + LightingUsage"
    lm_model <- lm(as.formula(formula_lr), data = energy_df)
    pred_data <- create_pred_data()
    predict(lm_model, newdata = pred_data, interval = "prediction")
  })
  
  output$prediction_result <- renderPrint({
    prediction <- predict_model_result()
    cat("Point Estimate:", round(prediction[1, "fit"], 2), "kWh\n") # Rounding output to 2 decimal places
    cat("Lower 95% Bound:", round(prediction[1, "lwr"], 2), "kWh\n")
    cat("Upper 95% Bound:", round(prediction[1, "upr"], 2), "kWh\n")
    cat("Confidence Range: ±", round((prediction[1, "upr"] - prediction[1, "lwr"]) / 2, 2), "kWh\n") # Calculating range of confidence interval
  })

}
# Create as Shiny app
shinyApp(ui, server)


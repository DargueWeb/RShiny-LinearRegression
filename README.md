# R Shiny Linear Regression Dashboard

This repository contains an interactive R Shiny web application designed to explore and visualize linear regression results using an energy consumption dataset. The app allows users to select variables, fit models, and interactively view the results in a clear, visual format.

## Contents

The repository includes the following:

- `LinearRegressionApp_v1.R` – The main Shiny app file
- `Energy_consumption_dataset.csv` – The dataset used in the app
- `Assignment.Rproj` – RStudio project file
- `www/LR-logo.png` – Logo image used in the app interface
- `www/lra-user-guide.pdf` – Supporting documentation
- `LR-logo.png` – Additional reference logo
- `lra-user-guide.pptx` – Presentation slides with user guide and documentation

## Features

- User-selectable predictor variables
- Model summary and coefficient output
- Interactive plots using `ggplot2` and `plotly`
- Factor handling for categorical predictors (e.g., day of week, holiday)
- Custom theming and layout via `shinythemes` and `shinydashboard`

## Running the App

To run this app locally:

1. Clone the repository or download the ZIP
2. Open `LinearRegressionApp_v1.R` in RStudio
3. Run the app with:

```R
shiny::runApp()

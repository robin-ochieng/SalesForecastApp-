# Predictive Analytics Model

This Shiny application is designed to provide a predictive analytics dashboard that forecasts daily sum and count of sales using multiple machine learning models. The app integrates several R packages and modular components to offer a customizable and interactive user interface.

## Overview

The application leverages the following key features:
- **Forecasting Models:** Offers multiple forecast options including CatBoost, XGBoost, LightGBM, RandomForest, and GLMNet.
- **Custom Dashboard:** Built using `bs4Dash` and `bslib` for a modern and responsive design.
- **Interactive Visualizations:** Displays forecasts through line plots, bar plots, and prediction tables.
- **Dynamic Inputs:** Allows users to set forecast start/end dates, choose the forecast model, and toggle between sum and count forecasts.

## Modules

The app is divided into several modular components:

- **Data Preparation Module (`dataPreparationModule.R`):** Processes and prepares the daily sales data.
- **Forecasting Module (`forecastingModule.R`):** Generates forecast data based on user-selected models and date ranges.
- **Value Boxes Module (`valueboxesModule.R`):** Displays key summary metrics.
- **Line Plot Module (`linePlotModule.R`):** Visualizes forecast data using line plots.
- **Bar Plot Module (`barPlotModule.R`):** Visualizes forecast data using bar charts.
- **Prediction Table Module (`predictionTableModule.R`):** Displays forecast results in a detailed table.

Additional utility functions are sourced from `functions.R`.

## File Structure

```plaintext
├── app.R                   # Main Shiny application script
├── modules
│   ├── dataPreparationModule.R
│   ├── forecastingModule.R
│   ├── functions.R
│   ├── valueboxesModule.R
│   ├── linePlotModule.R
│   ├── barPlotModule.R
│   └── predictionTableModule.R
├── www
│   └── css
│       └── custom_styles.css
├── js
│   └── custom.js
└── favicon
    └── kenbright.ico

##Installation

Install the necessary R packages by running:
```R
install.packages(c(
  "shiny", "bs4Dash", "bslib", "tidyverse", "lubridate", "readr",
  "catboost", "xgboost", "lightgbm", "caret", "imputeTS", "scales",
  "xts", "readxl", "DT", "plotly", "shinycssloaders", "shinyWidgets",
  "glmnet", "ranger"
))
```

### Interact with the Dashboard

- **Date Inputs:** Use the sidebar to select forecast start and end dates.
- **Model Selection:** Choose from CatBoost, XGBoost, LightGBM, RandomForest, or GLMNet.
- **Forecast Type:** Toggle between forecasting the sum or count of sales.
- **Visualizations:** Explore forecast outcomes via dynamic value boxes, line plots, bar plots, and a detailed prediction table.


### Customization

- **Theming:**  
  The dashboard uses a custom theme defined with `bslib`. You can modify colors, fonts, and other style elements in the `my_theme` object in the main app script.

- **Modules:**  
  Each module is designed to be independent. You can modify or extend the functionality by editing the corresponding R script files in the `modules` folder.

- **Assets:**  
  Custom CSS and JavaScript files are included to enhance the UI. Modify these in the `www/css/custom_styles.css` and `js/custom.js` files as needed.


This README provides a clear overview of the project, how to install and run it, and a breakdown of the modular components used. Customize further as needed for your repository.

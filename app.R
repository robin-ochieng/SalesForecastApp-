library(shiny)
library(bs4Dash)
library(bslib)
library(tidyverse)
library(lubridate)
library(readr)
library(catboost)
library(xgboost)
library(lightgbm)
library(caret)
library(imputeTS)
library(scales)
library(xts)
library(readxl)
library(DT)
library(plotly)
library(shinycssloaders)
library(shinyWidgets)
library(glmnet)
library(ranger)



source("modules/dataPreparationModule.R", local = TRUE)[1]
source("modules/forecastingModule.R", local = TRUE)[1]  
source("modules/functions.R", local = TRUE)[1] 
source("modules/valueboxesModule.R", local = TRUE)[1] 
source("modules/dataPreparationModule.R", local = TRUE)
source("modules/linePlotModule.R", local = TRUE)  
source("modules/barPlotModule.R", local = TRUE)
source("modules/predictionTableModule.R", local = TRUE)

# Define a custom theme using bslib
my_theme <- bs_theme(
  bg = "#202123", 
  fg = "#E1E1E1", 
  primary = "#EA80FC", 
  info = "#17a2b8",
  secondary = "#00BFA5",
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish"),
  navbar_bg = "#333333",  
  navbar_fg = "#ffffff"  
)

# Define the UI using bs4dash components
ui <- dashboardPage(
  title = "Sales Forecasting Model",
  freshTheme = my_theme,
  dark = NULL,
  help = NULL,
  fullscreen = FALSE,
  scrollToTop = TRUE,
  header = dashboardHeader(
    status = "white",
    fixed = TRUE,
    sidebarIcon = NULL,
    controlbarIcon = NULL,
    titleWidth = 250,
    border = FALSE,
    title = dashboardBrand(
      title = tags$div(
        class = "text-center header-title-container",
        tags$h4("Sales Forecasting Model", class = "header-title")
      ),
      color = "white"
    ),
    tags$li(
      class = "clock-container",
    tags$span(
      id = "dynamic-clock"
      ),
    )
  ),
  sidebar = dashboardSidebar(
    width = 12,
    minified = FALSE,
    tags$div(
      class = "menu-container",
    sidebarMenu(
    dateInput("startDate", "Select Forecast start date:",value = Sys.Date() , min = Sys.Date()),
    dateInput("endDate", "Select Forecast end date:", value = Sys.Date()+ days(7), min = Sys.Date()),
    # DROPDOWN for model selection
    selectInput(
      inputId  = "modelChoice",
      label    = "Select the Forecast Model:",
      choices  = c("CatBoost", "XGBoost", "LightGBM", "RandomForest", "GLMNet"),
      selected = "XGBoost"
    ), 
    tags$h6("Forecast Sum or Count:", style = "padding-left: 20px;  font-weight: bold; margin-top: 20px;"),
    # SwitchButton for Forecast Type selection
    switchInput(
      inputId = "forecastType", 
      label = NULL, 
      value = FALSE, 
      onLabel = "Sum", 
      offLabel = "Count", 
      onStatus = "gray-dark", 
      offStatus = "gray-dark"
      ),
    tags$style(HTML("
      #forecastType .switch-input {
        width: 200px;  # Increase width as needed
      }
    ")) 
     )
    )
  ),
  body = dashboardBody(
    tags$head(
      includeCSS("www/css/custom_styles.css"),      
      tags$script(src = "js/custom.js"),
      tags$link(rel = "shortcut icon", href = "favicon/kenbright.ico", type = "image/x-icon"),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Nunito:wght@400;700&display=swap", 
        rel = "stylesheet")
    ),
    valueBoxesModuleUI("boxes_id"),
    fluidRow(
      LinePlotUI("linePlot_id")
    ),
    fluidRow(
      predictionTableUI("table_id")
    ),
    fluidRow(
      barPlotUI("barPlot_id")
    )
  ),
  #controlbar = dashboardControlbar(), # Optional, add if needed
  footer = dashboardFooter()
)

server <- function(input, output, session) {

  # 1) Get the daily_sales from the data prep module
  dailySalesRV <- modDataPrepServer("dataPrep")
  
  # 2) Generate the forecast data from the forecasting module
  forecastDataRV <- modForecastServer(
    id = "forecast_id", 
    dailySalesReactive = dailySalesRV,
    startDateReactive = reactive({ input$startDate }),
    endDateReactive   = reactive({ input$endDate }),
    forecastTypeReactive = reactive({ input$forecastType }),
    modelChoiceReactive  = reactive({ input$modelChoice }) 
    )

  # 3) Module for Our Value boxes
  valueBoxesModuleServer(
    id = "boxes_id",
    forecastData = forecastDataRV,     
    startDate    = reactive({ input$startDate }),
    endDate      = reactive({ input$endDate }),
    forecastTypeReactive  = reactive({ input$forecastType })
  )
   
  # 4) Now call the table, line plot, and bar plot modules, passing forecast data
  predictionTableServer(
    id           = "table_id",
    forecastData = forecastDataRV,
    forecastTypeReactive = reactive({ input$forecastType })
  )
  
  LinePlotServer(
    id           = "linePlot_id",
    forecastData = forecastDataRV,
    forecastTypeReactive  = reactive({ input$forecastType })
  )
  
  barPlotServer(
    id           = "barPlot_id",
    forecastData = forecastDataRV,
    forecastTypeReactive = reactive({ input$forecastType })
  )

}

shinyApp(ui, server)

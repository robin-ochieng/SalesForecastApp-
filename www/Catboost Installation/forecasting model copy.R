library(shiny)
library(bs4Dash)
library(bslib)
library(tidyverse)
library(lubridate)
library(readr)
library(xgboost)
library(caret)
library(imputeTS)
library(scales)
library(xts)
library(readxl)
library(DT)
library(plotly)
library(shinyWidgets)

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
  navbar_bg = "#333333",  # Darker background for the navbar for contrast
  navbar_fg = "#ffffff"  # White text color for readability
)

# Load the data
sales_data <- read_csv("./data/Data.csv", 
                       col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                        Sales = col_number()))

# Preprocess the data
sales_data <- sales_data %>% 
  mutate(Date = ymd(Date)) %>%
  arrange(Date) %>%
  mutate(Sales = as.numeric(gsub(",", "", Sales, fixed = TRUE)))

# Aggregate sales data to daily frequency
daily_sales <- sales_data %>% 
  group_by(Date = floor_date(Date, unit = "day")) %>% 
  summarise(Sales = sum(!is.na(Sales))) %>%
  filter(Sales != 0) %>%
  drop_na()

# Calculate IQR and define bounds
Q1 <- quantile(daily_sales$Sales, 0.15)
Q3 <- quantile(daily_sales$Sales, 0.85)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Remove Outliers
daily_sales <- daily_sales %>%
  filter(Sales >= lower_bound & Sales <= upper_bound)

# Adding time-based features to the data
daily_sales <- daily_sales %>% 
  mutate(
    year = year(Date), 
    month = month(Date),
    day = day(Date), 
    weekday = wday(Date), 
    quarter = quarter(Date))

# Creating lag features
for (i in 1:31) {
  daily_sales <- daily_sales %>% mutate(!!paste0("sales_lag_", i) := lag(Sales, i))
}

# Removing rows with missing values
daily_sales <- drop_na(daily_sales)

formatKES <- function(x) {
  paste(formatC(x, format = "f", big.mark = ",", digits = 0))
}

# Define the UI using bs4dash components
ui <- dashboardPage(
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
    ),
    dateInput("startDate", "Select Forecast start date:",value = Sys.Date() , min = Sys.Date()),
    dateInput("endDate", "Select Forecast end date:", value = Sys.Date()+ days(7), min = Sys.Date())
  )),
  body = dashboardBody(
    tags$head(
      includeCSS("www/css/custom_styles.css"),      
      tags$script(src = "js/custom.js"),
      tags$link(rel = "shortcut icon", href = "favicon/kenbright.ico", type = "image/x-icon"),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Nunito:wght@400;700&display=swap", 
        rel = "stylesheet")
    ),
    fluidRow(
      valueBoxOutput("totalSales", width = 4),
      valueBoxOutput("avgSales", width = 4),
      valueBoxOutput("daysForecasted", width = 4)
    ),
    fluidRow(
      bs4Card(
        title = "Predicted Sales by Day",
        status = "white",
        solidHeader = TRUE,
        width = 12,
        plotlyOutput("linePlot")
      )
    ),
    fluidRow(
      bs4Card(
        title = "Sales Prediction Table",
        status = "white",
        solidHeader = TRUE,
        width = 12,
        DTOutput("predictionsTable")
      )
    ),
    fluidRow(
      box(title = " Predicted Sales by Day of the Week",
          status = "white",
          solidHeader = TRUE, 
          plotlyOutput("barPlot"),
          width = 12)
    )
  ),
  #controlbar = dashboardControlbar(), # Optional, add if needed
  footer = dashboardFooter()
)


server <- function(input, output, session) {

  # 1) Train XGBoost model once at app startup
  model <- reactive({
    # We can simply re-use daily_sales data here directly
    X <- select(daily_sales, -c(Sales, Date))
    y <- daily_sales$Sales

    xgboost(
      data = as.matrix(X),
      label = y,
      nrounds = 1000,
      objective = "reg:squarederror",
      max_depth = 5,
      eta = 0.1,
      nthread = 1,
      verbose = 0
    )
  })

  # 2) Create a reactive that produces forecasts based on the currently selected dates
  forecastData <- reactive({
    req(input$startDate, input$endDate)
    
    future_dates <- seq(as.Date(input$startDate), as.Date(input$endDate), by = "day")
    future_data <- data.frame(
      Date     = future_dates,
      year     = year(future_dates),
      month    = month(future_dates),
      day      = day(future_dates),
      weekday  = wday(future_dates),
      quarter  = quarter(future_dates)
    )
    
    # For demonstration, we will simply borrow the last known 31 days from daily_sales
    # to fill the lag columns. In practice, you might refine how you handle lags
    for (i in 1:31) {
      # tail(daily_sales$Sales, 31 + i - 1) gets the last (31 + i - 1) rows,
      # then we pick the first 'n' to match the future_data size
      future_data[paste0("sales_lag_", i)] <- tail(daily_sales$Sales, 31 + i - 1)[1:length(future_dates)]
    }
    
    # Now predict
    xgb_model <- model()
    future_data$PredictedSales <- predict(xgb_model, as.matrix(future_data[, -1]))
    
    # Return just the date + predicted sales
    future_data %>% select(Date, PredictedSales)
  })

  # 3) Create outputs using the forecast
  output$predictionsTable <- DT::renderDT({
    forecast <- forecastData()
    # Create a new column showing the day of the week
    forecast[["Day of Week"]] <- weekdays(forecast$Date)
    # Format the Date column
    forecast$Date <- format(forecast$Date, "%Y-%m-%d")
    # Add a formatted Sales Count
    forecast[["Sales Count"]] <- scales::comma(forecast$PredictedSales, accuracy = 1)
    # Drop the numeric column (PredictedSales) if you only want the formatted version
    forecast <- forecast[, c("Date", "Day of Week", "Sales Count")]
    
    datatable(
      forecast,
      options = list(
        pageLength = 14,
        autoWidth = FALSE,
        lengthChange = FALSE,
        paging = TRUE,
        searching = FALSE,
        info = FALSE,
        initComplete = JS("
          function(settings, json) {
            $(this.api().table().header()).css({
              'background-color': '#17a2b8',
              'color': '#FFFFFF'
            });
          }
        ")
      )
    )
  }, server = FALSE) 
    
    # After predictions
    output$totalSales <- renderValueBox({
      forecast <- forecastData()
      valueBox(
        value = formatKES(sum(forecast$PredictedSales)),
        subtitle = "Daily Sales Count",
        icon = NULL,
        color = "white"
      )
    })
    
    output$avgSales <- renderValueBox({
      forecast <- forecastData()
      valueBox(
        value = formatKES(mean(forecast$PredictedSales)),
        subtitle = "Average Daily Sales",
        icon = NULL,
        color = "white"
      )
    })
    
    output$daysForecasted <- renderValueBox({
      days_count <- as.integer(difftime(as.Date(input$endDate), as.Date(input$startDate), units = "days")) + 1
      valueBox(
        value = paste(days_count, "Days"),
        subtitle = "Forecast Duration",
        icon = NULL,
        color = "white"
      )
    })
    
    
    
    output$linePlot <- renderPlotly({
      forecast <- forecastData()
      plot_ly(
        forecast,
        x = ~Date,
        y = ~PredictedSales, 
        type = 'scatter', 
        mode = 'lines+markers+text',
        text = ~round(PredictedSales),  # This will display the Sales values
        textposition = 'top center',  # Adjust this as needed (e.g., 'top', 'bottom', 'middle')
        line = list(color = '#1CA4F8'),
        marker = list(color = '#0d6efd')
        ) %>%
        layout(
          title = "",
          xaxis = list(title = "Date", tickfont = list(size = 10, color = "#333333")),
          yaxis = list(title = "Count of Sales", tickfont = list(size = 10, color = "#333333")),
          font = list(family = "Mulish", color = "#333333"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })
    
    
    output$barPlot <- renderPlotly({
      forecast <- forecastData() %>%
        mutate(
          DayOfWeek = weekdays(Date)) %>%
        group_by(DayOfWeek) %>%
        summarise(Sales = sum(PredictedSales))
      
      plot_ly(
        forecast,
        x = ~DayOfWeek, 
        y = ~Sales, 
        type = 'bar',
        text = ~paste(scales::comma(Sales, accuracy = 1)),
        textposition = 'outside',
        hoverinfo = 'text',
        textfont = list(size = 9, color = "black"),
        marker = list(color = '#1CA4F8')
        ) %>%
        layout(
          title = "",
          xaxis = list(title = "Day of the Week", tickfont = list(size = 10, color = "#333333")),
          yaxis = list(title = "Total Sales", tickfont = list(size = 10, color = "#333333")), # Changed from "Count of Predicted Sales"
          font = list(family = "Mulish", color = "#333333"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        ) 
    })
    
}

shinyApp(ui, server)

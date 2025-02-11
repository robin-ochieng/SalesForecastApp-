# File: modules/accuracyModule.R

# =============== UI ===============
accuracyModuleUI <- function(id) {
  ns <- NS(id)
  
  bs4Card(
    title = "Forecast Accuracy",
    status = "white",
    solidHeader = TRUE,
    width = 12,
    # You can display metrics in text, or use valueBox, infoBox, etc.
    fluidRow(
      column(
        width = 12,
        tableOutput(ns("accuracyTable"))
      )
    )
  )
}

# =============== Server ===============
accuracyModuleServer <- function(
  id,
  dailySalesReactive,   # Actual historical data
  forecastDataReactive  # Forecasted data (PredictedSales)
) {
  moduleServer(id, function(input, output, session) {
    
    output$accuracyTable <- renderTable({
      req(dailySalesReactive())
      req(forecastDataReactive())
      
      # 1) Actual data
      actualData <- dailySalesReactive()  # e.g. columns: Date, ActualSales, ...
      print(actualData)
      # 2) Forecast data
      forecastData <- forecastDataReactive() # e.g. columns: Date, PredictedSales, ModelName, ...
      
      # ----- JOIN BY DATE (or your relevant key) -----
      # If your dailySalesReactive() doesn’t already have the relevant
      # forecast time horizon, you’ll need to handle that with a train/test split or
      # see how you define "actual" for the same period as your forecast.
      
      accuracy_df <- forecastData %>%
        inner_join(actualData, by = "Date")  %>%
        mutate(
          # Example: let's assume actual col is "ActualSales" 
          Error       = Sales - PredictedSales,
          AbsError    = abs(Error),
          SqError     = Error^2,
          AbsPercError= abs(Error / (Sales + 1e-8)) * 100  # Avoid divide-by-zero
        )
      
      # If your forecastData includes multiple models, you can group_by model
      # and compute metrics per model:
      accuracy_summary <- accuracy_df %>%
        group_by(ModelName) %>%  # If you have a "ModelName" column
        summarise(
          MAE  = mean(AbsError, na.rm = TRUE),
          RMSE = sqrt(mean(SqError, na.rm = TRUE)),
          MAPE = mean(AbsPercError, na.rm = TRUE)
        ) %>%
        ungroup()
      
      # Round your metrics for clarity
      accuracy_summary <- accuracy_summary %>%
        mutate(
          MAE  = round(MAE, 2),
          RMSE = round(RMSE, 2),
          MAPE = paste0(round(MAPE, 2), "%")
        )
      
      # Return the summarized dataframe
      accuracy_summary
    })
  })
}

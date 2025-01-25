# modules/linePlotModule.R

LinePlotUI <- function(id) {
  ns <- NS(id)
    bs4Card(
        class = "highlight-effect",
        title = "Predicted Sales by Day",
        status = "white",
        solidHeader = TRUE,
        width = 12,
        plotlyOutput(ns("linePlot"))
      )
}

LinePlotServer <- function(id, forecastData, forecastTypeReactive) {
  moduleServer(id, function(input, output, session) {
    
    output$linePlot <- renderPlotly({
      req(forecastData())
      forecast <- forecastData()

      # Decide if it's "Count of Sales" or "Sum of Sales"
      yTitle <- if (forecastTypeReactive() == FALSE) {
        "Count of Sales" 
      } else {
        "Sum of Sales"
      }     
      
      plot_ly(
        data = forecast,
        x = ~Date,
        y = ~PredictedSales, 
        type = 'scatter', 
        mode = 'lines+markers+text',
        text = ~format(round(PredictedSales), big.mark = ",", scientific = FALSE),
        textposition = 'top center',
        line = list(color = '#8E8E8E'),
        marker = list(color = '#525252')
      ) %>%
      layout(
        title = "",
        xaxis = list(title = "Date", tickfont = list(size = 10, color = "#333333")),
        yaxis = list(title = yTitle, tickfont = list(size = 10, color = "#333333")),
        font = list(family = "Mulish", color = "#333333"),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
    })
    
  })
}

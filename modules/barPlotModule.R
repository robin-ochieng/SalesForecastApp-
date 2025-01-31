# modules/barPlotModule.R

barPlotUI <- function(id) {
  ns <- NS(id)
      box(
        class = "highlight-effect",
        title = " Predicted Sales by Day of the Week",
        status = "white",
        solidHeader = TRUE, 
        plotlyOutput(ns("barPlot")) %>% withSpinner(type = 5),
        width = 12)
}

barPlotServer <- function(id, forecastData, forecastTypeReactive) {
  moduleServer(id, function(input, output, session) {
    
    output$barPlot <- renderPlotly({
      req(forecastData())
      forecast <- forecastData() %>%
        mutate(DayOfWeek = weekdays(Date)) %>%
        group_by(DayOfWeek) %>%
        summarise(AverageSales = mean(PredictedSales))
      
            # Decide y-axis label
      yAxisTitle <- if (forecastTypeReactive() == FALSE) {
        "Average Sales Count"
      } else {
        "Average Sales Value"
      }

      plot_ly(
        forecast,
        x = ~DayOfWeek, 
        y = ~AverageSales, 
        type = 'bar',
        text = ~paste(scales::comma(AverageSales, accuracy = 1)),
        textposition = 'outside',
        hoverinfo = 'text',
        textfont = list(size = 9, color = "black"),
        marker = list(color = '#8E8E8E')
      ) %>%
      layout(
        title = "",
        xaxis = list(title = "Day of the Week", tickfont = list(size = 10, color = "#333333")),
        yaxis = list(title = yAxisTitle, tickfont = list(size = 10, color = "#333333")),
        font = list(family = "Mulish", color = "#333333"),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
    })
    
  })
}

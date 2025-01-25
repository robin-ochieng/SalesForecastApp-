# modules/barPlotModule.R

barPlotUI <- function(id) {
  ns <- NS(id)
      box(
        class = "highlight-effect",
        title = " Predicted Sales by Day of the Week",
        status = "white",
        solidHeader = TRUE, 
        plotlyOutput(ns("barPlot")),
        width = 12)
}

barPlotServer <- function(id, forecastData, forecastTypeReactive) {
  moduleServer(id, function(input, output, session) {
    
    output$barPlot <- renderPlotly({
      req(forecastData())
      forecast <- forecastData() %>%
        mutate(DayOfWeek = weekdays(Date)) %>%
        group_by(DayOfWeek) %>%
        summarise(Sales = sum(PredictedSales))
      
            # Decide y-axis label
      yAxisTitle <- if (forecastTypeReactive() == FALSE) {
        "Sales Count"
      } else {
        "Sales Sum"
      }

      plot_ly(
        forecast,
        x = ~DayOfWeek, 
        y = ~Sales, 
        type = 'bar',
        text = ~paste(scales::comma(Sales, accuracy = 1)),
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

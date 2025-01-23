# dataPreparationModule.R
modDataPrepServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # -- 1) Load your data
    #    (Adjust paths or code as needed; you can also make them configurable)
    sales_data <- read_csv(
      "./data/Data.csv",
      col_types = cols(Date = col_date(format = "%m/%d/%Y"), Sales = col_number())
    )
    
    # -- 2) Preprocess the data
    sales_data <- sales_data %>%
      mutate(Date = ymd(Date)) %>%
      arrange(Date) %>%
      mutate(Sales = as.numeric(gsub(",", "", Sales, fixed = TRUE)))
    
    # Aggregate sales data to daily frequency
    daily_sales <- sales_data %>%
      group_by(Date = floor_date(Date, unit = "day")) %>%
      summarise(
        SalesCount = sum(!is.na(Sales)),   # Number of rows (counts)
        SalesSum   = sum(Sales, na.rm=TRUE)  # Actual sum of sales
      ) %>%
      filter(SalesCount != 0) %>%
      drop_na()
    
    # Calculate IQR and define bounds
    Q1_count  <- quantile(daily_sales$SalesCount, 0.10)
    Q3_count  <- quantile(daily_sales$SalesCount, 0.90)
    IQR_count <- Q3_count - Q1_count
    lower_bound_count <- Q1_count - 1.5 * IQR_count
    upper_bound_count <- Q3_count + 1.5 * IQR_count

    Q1_sum  <- quantile(daily_sales$SalesSum, 0.15)
    Q3_sum  <- quantile(daily_sales$SalesSum, 0.85)
    IQR_sum <- Q3_sum - Q1_sum
    lower_bound_sum <- Q1_sum - 1.5 * IQR_sum
    upper_bound_sum <- Q3_sum + 1.5 * IQR_sum
    
    # Keep days that pass both checks
    daily_sales <- daily_sales %>%
      filter(
        SalesCount >= lower_bound_count & SalesCount <= upper_bound_count,
        SalesSum   >= lower_bound_sum   & SalesSum   <= upper_bound_sum
      )

    # Adding time-based features
    daily_sales <- daily_sales %>%
      mutate(
        year = year(Date), 
        month = month(Date),
        day = day(Date), 
        weekday = wday(Date),
        quarter = quarter(Date)
      )
    
    # Creating lag features
    for (i in 1:31) {
      daily_sales <- daily_sales %>%
        mutate(!!paste0("count_lag_", i) := dplyr::lag(SalesCount, i),
               !!paste0("sum_lag_", i)   := dplyr::lag(SalesSum,   i))
    }
    
    # Remove rows with missing values
    daily_sales <- drop_na(daily_sales)
    
    # We return daily_sales as a reactive expression
    # so other modules or the main server can use it.
    # We'll store it in a reactiveVal container
    dailySalesRV <- reactiveVal(daily_sales)
    
    # Return the reactiveVal
    return(dailySalesRV)
  })
}

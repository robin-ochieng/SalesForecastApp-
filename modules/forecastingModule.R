# forecastingModule.R
modForecastServer <- function(id, dailySalesReactive, startDateReactive, endDateReactive, forecastTypeReactive, modelChoiceReactive) {
  moduleServer(id, function(input, output, session) {
    
    # 1) Train XGBoost model once, using the daily_sales data from module 1
    model <- reactive({
      req(dailySalesReactive())
      daily_sales <- dailySalesReactive()

      if (forecastTypeReactive() == FALSE) {
        # forecastTypeReactive() == FALSE => forecast "count"
        daily_sales <- daily_sales %>%
          mutate(Sales = SalesCount)

        # Also rename count_lag_i => sales_lag_i
        for (i in 1:31) {
          old_col <- paste0("count_lag_", i)
          new_col <- paste0("sales_lag_", i)
          # Only rename if it exists
          if (old_col %in% names(daily_sales)) {
            daily_sales <- daily_sales %>%
              rename(!!new_col := all_of(old_col))
          }
        }  

      } else {
        # forecastTypeReactive() == TRUE => forecast "sum"
        daily_sales <- daily_sales %>%
          mutate(Sales = SalesSum)
        
        # Also rename sum_lag_i => sales_lag_i
        for (i in 1:31) {
          old_col <- paste0("sum_lag_", i)
          new_col <- paste0("sales_lag_", i)
          if (old_col %in% names(daily_sales)) {
            daily_sales <- daily_sales %>%
              rename(!!new_col := all_of(old_col))
          }
        }
      }

      # =============  B) SELECT the columns we actually want  =============
      # We'll keep "Sales" + time-based features + any "sales_lag_i" columns
      trainCols <- names(daily_sales)[
        grepl("^sales_lag_", names(daily_sales)) |        # all sales_lag_i
          names(daily_sales) %in% c("Sales", "year", "month", "day", "weekday", "quarter")
      ]

      trainDF <- daily_sales[, trainCols, drop = FALSE]

      # Now you have daily_sales with columns: "Sales", "sales_lag_1..31", plus time-based features
      # Drop columns that are not needed (Date, SalesCount, SalesSum)
      X <- dplyr::select(trainDF, -Sales)
      y <- trainDF$Sales

       # -- B) Check which model user chose
      if (modelChoiceReactive() == "CatBoost") {     

      # Train CatBoost (example)
      pool <- catboost.load_pool(data = X, label = y)
      fit <- catboost.train(pool, NULL,
        params = list(
          loss_function = "RMSE",
          iterations    = 1000,
          depth         = 5,
          learning_rate = 0.1
        )
      )
      fit
    } else {
        # XGBoost pipeline
        fit <- xgboost::xgboost(
          data        = as.matrix(X),
          label       = y,
          nrounds     = 1000,
          objective   = "reg:squarederror",
          max_depth   = 5,
          eta         = 0.1,
          verbose     = 0
        )
        fit
      }
    })

    ################
    # 2) FORECAST/Prediction Step
    ################
    
    # 2) Compute a forecast based on date inputs from the main UI
    forecastData <- reactive({
      req(startDateReactive(), endDateReactive())
      
      # A) Create future dates / Build skeleton for future_data 
      future_dates <- seq(
        from = as.Date(startDateReactive()),
        to   = as.Date(endDateReactive()), 
        by = "day")
      
      # Basic time-based features
      future_data <- data.frame(
        Date     = future_dates,
        year     = lubridate::year(future_dates),
        month    = lubridate::month(future_dates),
        day      = lubridate::day(future_dates),
        weekday  = lubridate::wday(future_dates),
        quarter  = lubridate::quarter(future_dates)
      )
      
      # B) Rename columns in daily_sales (same as training) 
      daily_sales <- dailySalesReactive()

      #Count
      if (forecastTypeReactive() == FALSE) {
        daily_sales <- daily_sales %>%
          mutate(Sales = SalesCount)

        for (i in 1:31) {
          old_col <- paste0("count_lag_", i)
          new_col <- paste0("sales_lag_", i)
          if (old_col %in% names(daily_sales)) {
            daily_sales <- daily_sales %>%
              rename(!!new_col := all_of(old_col))
          }
        }
      } else {
        #Sum
        daily_sales <- daily_sales %>%
          mutate(Sales = SalesSum)

        for (i in 1:31) {
          old_col <- paste0("sum_lag_", i)
          new_col <- paste0("sales_lag_", i)
          if (old_col %in% names(daily_sales)) {
            daily_sales <- daily_sales %>%
              rename(!!new_col := all_of(old_col))
          }
        }
      }
      # C) Fill future_data with "sales_lag_i" columns
      nFutureDays <- length(future_dates)
      # Create the same "sales_lag_i" columns (because we always call it "Sales")
      for (i in 1:31) {
        future_data[[paste0("sales_lag_", i)]] <-
          tail(daily_sales$Sales, 31 + i - 1)[1:nFutureDays]
      }

      # =============  D) Keep the same columns we used for training (minus "Sales") =============
      # Grab the training columns from the trained model, but we only have direct access to them
      # if we saved them or re-generated them. So let's do the same approach here:
      predCols <- names(daily_sales)[
        grepl("^sales_lag_", names(daily_sales)) | 
          names(daily_sales) %in% c("year", "month", "day", "weekday", "quarter")
      ]
      
      # Then subset future_data (which we just created) to these columns
      # We do NOT include "Sales" here because that's the target in training.
      future_data_final <- future_data[, predCols, drop = FALSE]
      
      # E) Use whichever model is trained
      finalModel <- model()

      # Predict
      # If it's CatBoost
      if (modelChoiceReactive() == "CatBoost") {
        pool <- catboost.load_pool(data = future_data_final)
        preds <- catboost.predict(finalModel, pool)
        
      } else {
        # XGBoost
        preds <- predict(finalModel, as.matrix(future_data_final))
      }
      
      # Return a data frame with a single final column name "PredictedSales"
      data.frame(
        Date = future_dates,
        PredictedSales = preds
      )
    })
    
    # Return the forecast data as a reactive
    return(forecastData)
  })
}

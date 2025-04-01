# forecastingModule.R
modForecastServer <- function(id, dailySalesReactive, startDateReactive, endDateReactive, forecastTypeReactive, modelChoiceReactive) {
  moduleServer(id, function(input, output, session) {
    
    # 1) Train XGBoost model once, using the daily_sales data from module 1
    model <- reactive({
      req(dailySalesReactive())
      withProgress(message = "Training model...", value = 0, {
        
      incProgress(0.1, detail = "Fetching data...")      
      daily_sales <- dailySalesReactive()

      # A) unify count vs. sum => rename to "Sales", rename lag columns => "sales_lag_i"
      incProgress(0.2, detail = "Preparing columns...")
      if (forecastTypeReactive() == FALSE) {
        # forecastTypeReactive() == FALSE => forecast "count"
        daily_sales <- daily_sales %>% mutate(Sales = SalesCount)

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
      incProgress(0.4, detail = "Building training matrix...")
      trainCols <- names(daily_sales)[
        grepl("^sales_lag_", names(daily_sales)) |        # all sales_lag_i
          names(daily_sales) %in% c("Sales", "year", "month", "day", "weekday", "quarter")
      ]

      trainDF <- daily_sales[, trainCols, drop = FALSE]

      # Now you have daily_sales with columns: "Sales", "sales_lag_1..31", plus time-based features
      # Drop columns that are not needed (Date, SalesCount, SalesSum)
      X <- dplyr::select(trainDF, -Sales)
      y <- trainDF$Sales

       # -- C) Check which model user chose
      incProgress(0.6, detail = paste("Fitting", modelChoiceReactive(), "..."))
      fit <- NULL

      if (modelChoiceReactive() == "GBM") {  
        # Train GBM
        fit <- gbm::gbm(
          formula = Sales ~ .,
          data = trainDF,
          distribution = "gaussian",
          n.trees = 1000,
          interaction.depth = 5,
          shrinkage = 0.1,
          bag.fraction = 0.5,
          cv.folds = 5,
          verbose = FALSE
        )  
        incProgress(0.9, detail = "GBM finished.")    
    } else if (modelChoiceReactive() == "XGBoost") {
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
        incProgress(0.9, detail = "XGBoost finished.")
        
      } else if (modelChoiceReactive() == "LightGBM") {
        # --- LightGBM
        # 1) Create dataset
        lgb_train <- lightgbm::lgb.Dataset(data = as.matrix(X), label = y)
        # 2) Train model
        fit <- lightgbm::lgb.train(
          params = list(
            objective = "regression",
            learning_rate = 0.1,
            num_leaves = 31 
          ),
          data    = lgb_train,
          nrounds = 1000,
          verbose = 0
        )
        incProgress(0.9, detail = "LightGBM finished.")
        
        
      } else if (modelChoiceReactive() == "RandomForest") {
          # -- RandomForest via ranger
          # Make sure to install.packages("ranger") and library(ranger)
          incProgress(0.7, detail = "RandomForest in progress...")
          # We can combine X, y into a single data.frame, as ranger uses a formula or data approach:
          df_rf <- cbind(X, Sales = y)
          fit <- ranger::ranger(
            Sales ~ .,
            data       = df_rf,
            num.trees  = 500,    # e.g. 500 trees
            mtry       = floor(sqrt(ncol(X))),  # typical default
            importance = "impurity"
          )
          incProgress(0.9, detail = "RandomForest finished.")
          
        } else if (modelChoiceReactive() == "GLMNet") {
          # -- GLMNet
          # Make sure to install.packages("glmnet") and library(glmnet)
          incProgress(0.7, detail = "Fitting GLMNet (Elastic Net)...")
          fit <- glmnet::glmnet(
            x     = as.matrix(X),
            y     = y,
            alpha = 0.5,     # 0.5 = halfway between ridge/lasso
            family = "gaussian"
          )
          # Optionally you can do cross-validation with cv.glmnet
          incProgress(0.9, detail = "GLMNet finished.")
          
        } else {
          validate(need(FALSE, "Unsupported modelChoice selected."))
        }

        incProgress(1, detail = "Done.")
        fit
      }) # end withProgress
    })
    
    

    ################
    # 2) FORECAST/Prediction Step
    ################
    
    # 2) Compute a forecast based on date inputs from the main UI
    forecastData <- reactive({
      req(startDateReactive(), endDateReactive())
      
      withProgress(message = "Generating forecast...", value = 0, {

        incProgress(0.1, detail = "Preparing future data...")
        # A) Create future dates / Build skeleton for future_data 
        future_dates <- seq(
          as.Date(startDateReactive()),
          as.Date(endDateReactive()), 
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
        incProgress(0.2, detail = "Renaming daily sales for forecast...")
        daily_sales <- dailySalesReactive()

        #Count
        if (forecastTypeReactive() == FALSE) {
          daily_sales <- daily_sales %>% mutate(Sales = SalesCount)

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
          daily_sales <- daily_sales %>% mutate(Sales = SalesSum)
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
        incProgress(0.4, detail = "Adding lag columns to future data...")
        nFutureDays <- length(future_dates)
        # Create the same "sales_lag_i" columns (because we always call it "Sales")
        for (i in 1:31) {
          future_data[[paste0("sales_lag_", i)]] <-
            tail(daily_sales$Sales, 31 + i - 1)[1:nFutureDays]
        }

        # =============  D) Keep the same columns we used for training (minus "Sales") =============
        # Grab the training columns from the trained model, but we only have direct access to them
        # if we saved them or re-generated them. So let's do the same approach here:
        incProgress(0.6, detail = "Subsetting columns...")
        predCols <- names(daily_sales)[
          grepl("^sales_lag_", names(daily_sales)) | 
            names(daily_sales) %in% c("year", "month", "day", "weekday", "quarter")
        ]
        
        # Then subset future_data (which we just created) to these columns
        # We do NOT include "Sales" here because that's the target in training.
        future_data_final <- future_data[, predCols, drop = FALSE]
        
        # E) Use whichever model is trained
        incProgress(0.7, detail = "Loading trained model...")
        finalModel <- model()

        # F) Predict on future_data_final
        incProgress(0.8, detail = "Predicting on new data...")
        preds <- NULL


        if (modelChoiceReactive() == "GBM") {
          # gbm predict
          preds <- predict(finalModel, newdata = future_data_final, n.trees = 1000)

        } else if (modelChoiceReactive() == "XGBoost") {
          # xgboost predict
          preds <- predict(finalModel, as.matrix(future_data_final))
          
        } else if (modelChoiceReactive() == "LightGBM") {
          # lightGBM predict
          preds <- predict(finalModel, as.matrix(future_data_final))
          
        } else if (modelChoiceReactive() == "RandomForest") {
          # ranger predict
          preds <- predict(finalModel, data = future_data_final)$predictions
          
        } else if (modelChoiceReactive() == "GLMNet") {

          bestLambda <- tail(finalModel$lambda, 1)
          preds <- predict(finalModel, newx = as.matrix(future_data_final), s = bestLambda)
          preds <- as.numeric(preds)
          
        } else {
          validate(need(FALSE, "Unsupported modelChoice in forecast step."))
        }

        
        # Return a data frame with a single final column name "PredictedSales"
        incProgress(0.95, detail = "Finalizing forecast...")
        df <- data.frame(
          Date = future_dates,
          PredictedSales = preds
        )

        incProgress(1, detail = "Done.")
        df  
        }) # end withProgress
      })
      
      # Return the forecast data as a reactive
      return(forecastData)
  })
}


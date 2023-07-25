# Script for forecasting in years

# Author: Peter Boshe
# Version: 2023-07-14

# Packages
library(tidyverse)

# Parameters

# ============================================================================




forecast_facility_arima <- function(data, forecast_period, session) {

  data |> then(~{
    data_df <- .
    forecast_period <- forecast_period * 6
    df <- data_df |>
      as_tsibble(index = date, key = c(program,product_code))
    #separate train and test data
    train <- df |> filter_index(~ paste0(max(data_df$date) - 6))

    # train the model and record execution time
    print("training our model")
    execution_time <- system.time(
      model <- train |>
        #convert to work with arimas
        mutate(date = as.numeric(date)) |>
        model(
          arima = ARIMA(adj_consumption_in_packs)
        )
    )
    print("making our forecasts")
    forecast_values <- model |>
      #include the 6 months of test data
      forecast(h = 3 + forecast_period) |>
      mutate(date = yearmonth(date))

    forecast_demand <- forecast_values |>
      filter_index(paste0(max(data_df$date)) ~ .) |>
      as_tibble() |>
      group_by(product_code) |>
      summarise(demand = round(sum(.mean)))


    print("function complete")

    # calculating model accuracy
    actual <- data_df %>% mutate(date = yearmonth(date)) |>
      filter(date > max(train$date)) %>%
      select(product_code, date, adj_consumption_in_packs) %>%
      rename(actual = adj_consumption_in_packs)

    #get the accuracy metrics
    accuracy_metrics <- forecast_values %>%
      as_tibble() |>
      rename(forecast = .mean) |>
      inner_join(actual, by = c("product_code", "date")) %>%
      group_by(product_code) %>%
      summarise(
        actual = sum(actual),
        forecast = sum(forecast)
      ) |>
      mutate(forecast = round(as.numeric(forecast), 2)) |>
      mutate(
        PE = ((actual - forecast) / (actual)) * 100,
        PE = round(case_when(
          PE == Inf | PE == -Inf ~ 100,
          is.nan(PE) ~ 0,
          TRUE ~ PE
        ),1),
        APE = abs(PE)
      )

    group_accuracies <- accuracy_metrics %>%
      summarise(
        APE_less_than_10 = mean(APE < 10) * 100,
        APE_less_than_20 = mean(APE < 20) * 100,
        APE_less_than_30 = mean(APE < 30) * 100
      )

    mean_accuracy_metrics <- accuracy_metrics |>
      summarise(MAPE = mean(APE),
                MPE = mean(PE))

    # Use forecastData for the table part of your Shiny app
    session$output$forecastTable <- renderDT({


      forecast_values |>
        as_tibble() |>
        mutate(.mean = round(.mean, 1)) |>
        pivot_wider(id_cols = c(program, product_code), names_from = date, values_from = .mean) |>
        mutate(demand = round(rowSums(across(where(is.numeric))),1)) |>
        custom_datatable(selection = list(mode = 'multiple', target = 'row'),
                         datatable_options = list(scrollY = "400px")) |>
        formatStyle("product_code", backgroundColor = "#C2ECED",
                    fontWeight = "bold")
    })

    observeEvent(session$input$forecastButton, {
      # print(forecast_object$time["elapsed"] |> as.numeric())
      session$output$modelSpeed <- renderText({
        paste0("Training time: ",round(execution_time["elapsed"] |> as.numeric(), 1) , " seconds")

      })
      session$output$modelAccuracyPercent <- renderText({
        paste0("MAPE: ", round(mean_accuracy_metrics$MAPE,1), "%\nMPE: ", round(mean_accuracy_metrics$MPE,1), "%")
      })
      session$output$modelAccuracySummary <- renderText({
        paste0("Products with APE < 10%: ", round(group_accuracies$APE_less_than_10, 1), "%\n\nProducts with APE < 20%: ", round(group_accuracies$APE_less_than_20,1), "%\n\nProducts with APE < 30%: ", round(group_accuracies$APE_less_than_30,1),"%")
      })

      session$output$accuracyTable <- renderDT({
        accuracy_metrics |>
          as.data.frame() |>
          custom_datatable()
      })

      observeEvent(session$input$forecastTable_rows_selected,{
        req(session$input$forecastTable_rows_selected)
        pivoted_wider <- data_df |>
          mutate(date = yearmonth(date)) |>
          mutate(date = as.character(date)) |>
          pivot_wider(id_cols = c(product_code), names_from = date, values_from = adj_consumption_in_packs)

        selRow <- pivoted_wider[session$input$forecastTable_rows_selected, ]

        session$output$forecastChart <- renderEcharts4r({
          draw_forecast_trendline(forecast_values, selRow[[1]], data_df)
        })

      })

    })


  })

}

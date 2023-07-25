# Script for fiscal year forecasting function

# Author: Peter Boshe
# Version: 2023-07-14

# Packages
library(tidyverse)

# Parameters

# ============================================================================

forecast_facility_fiscal_arima <- function(data, financialYear, session) {



  data |> then(~ {
    data_df <- .
    financialIndex <- paste0(substr(financialYear, 1, 4),"-07-01")
    df <- data_df |> as_tsibble(index = date, key = c(program,product_code))
    print(df)
    train <- df |> filter_index(~ financialIndex)

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
      #forecasting for one financial year
      forecast(h = 6) |>
      mutate(date = yearmonth(date))

    forecasted <- as.data.frame(forecast_values) %>%
      rename(forecast = .mean)

    actual <- df |>
      filter(date > max(train$date) & date < yearmonth(paste0(as.numeric(substr(financialYear, 1, 4)) + 1,"-07-01"))) |>
      select(product_code, item_description, date, adj_consumption_in_packs, program) |>
      rename(actual = adj_consumption_in_packs)

    # print("this is my forecasted data")
    # print(head(forecasted))
    # print("this is my actual data")
    # print(head(actual))

    accuracy_metrics <- forecasted %>%
      left_join(actual, by = c("product_code", "date", "program")) %>%
      ungroup() |>
      group_by(product_code) %>%
      summarise(
        actual = sum(actual, na.rm = T),
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

    forecast_demand <- forecast_values |>
      as_tibble() |>
      group_by(product_code) |>
      summarise(demand = round(sum(.mean)))

    if (nrow(actual) < 1) {
      mean_accuracy_metrics <- mean_accuracy_metrics * NA
      group_accuracies <- group_accuracies * NA
      accuracy_metrics <- accuracy_metrics |>
        mutate(actual = NA,
               PE = NA,
               APE = NA)
      # accuracy_metrics <- accuracy_metrics * NA
    }

    print("function complete")
    print(execution_time)
    print(model)
    print(forecast_demand)
    print(forecast_values)
    print(accuracy_metrics)
    print(group_accuracies)
    print(mean_accuracy_metrics)



    # Use forecastData for the table part of your Shiny app
     someVariable <- reactive({
       session$output$forecastTable <- renderDT({
        
        # print("these are my forecasts depending on my choice of method")
        # print(class(forecast_object()$forecast))
        
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
    })
     
     print(someVariable())
    
    # observe({
    #   req(session$output$forecastTable)
    #   
    # })


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
        print("the following is my selected row")
        print(selRow)

        session$output$forecastChart <- renderEcharts4r({
          draw_forecast_trendline(forecast_values, selRow[[1]], data_df)
        })

      })

      # output$buqAccuracyTable <- renderDT({
      #   head(buqData)
      #
      # })
    })

    # execution_time <-  reactive({execution_time})
    #     model <-  reactive({model})
    # forecast_demand <-  reactive({forecast_demand})
    # forecast_values <-  reactive({forecast_values})
    # accuracy_metrics <-  reactive({accuracy_metrics})
    # group_accuracies <-  reactive({group_accuracies})
    # mean_accuracy_metrics <-  reactive({mean_accuracy_metrics})

    # return(list(
    #   time = execution_time,
    #   model = model,
    #   demand = forecast_demand,
    #   forecast = forecast_values,
    #   accuracy_table = accuracy_metrics,
    #   summary = group_accuracies,
    #   mean_accuracy = mean_accuracy_metrics
    # ))

  })


}



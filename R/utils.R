# Helper functions for the quantification workshop app

# Author: Peter
# Version: 2023-05-24

# Packages
# library(tidyverse)

# Parameters

# ============================================================================

#############################################################################
#  fill gaps with median
#############################################################################



#
# replace_missing_variables <- function(df, facility) {
#   # get the summary statistics to replace the missing values ----------------
#
#   df |>
#     filter(facility_name == facility) |>
#     mutate(year_month = yearmonth(year_month, format = "%Y %b")) |>
#     arrange(year_month) |>
#     complete(product_code, facility_code, year_month) |>
#     group_by(facility_code, product_code, fiscal_year) |>
#     summarise(mean = mean(adj_consumption_in_packs, na.rm = T),
#               median = median(adj_consumption_in_packs, na.rm = T),
#               .groups = "drop") |>
#     mutate(median = ifelse(is.na(median), 0, median),
#            mean = ifelse(is.na(mean), 0, mean)) -> summary_stats_df
#
#
#   # replace the missing values with median ----------------------------------
#
#   df <- df |>
#     filter(facility_name == facility ) |>
#     mutate(year_month = yearmonth(year_month, format = "%Y %b")) |>
#     arrange(year_month) |>
#     complete(product_code, facility_code, year_month) |>
#     group_by(facility_code, product_code, .drop = T) |>
#     select(product_code, year_month, fiscal_year, adj_consumption_in_packs) |>
#     left_join(summary_stats_df, by = c("facility_code", "product_code", "fiscal_year")) |>
#     mutate(adj_consumption_in_packs_filled = ifelse(is.na(adj_consumption_in_packs), median, adj_consumption_in_packs)) |>
#     select(- adj_consumption_in_packs, -mean, -median, -fiscal_year) |>
#     pivot_wider(names_from = year_month, values_from = adj_consumption_in_packs_filled)
#     # mutate(total_consumption = sum(c_across(where(is.numeric)))) |>
#
#     return(df)
# }


#############################################################################
# draw charts for drilling down
#############################################################################

create_trend_line_chart <- function(df, products, facility) {

  df |>
    filter(facility_name == facility) |>
    mutate(year_month = yearmonth(year_month, format = "%Y %b")) |>
    arrange(year_month) |>
    complete(product_code, facility_code, fiscal_year, year_month) |>
    group_by(facility_code, product_code, fiscal_year) |>
    summarise(mean = mean(adj_consumption_in_packs, na.rm = T),
              median = median(adj_consumption_in_packs, na.rm = T),
              .groups = "drop") |>
    mutate(median = ifelse(is.na(median), 0, median),
           mean = ifelse(is.na(mean), 0, mean)) -> summary_stats_df

  df |>
    filter(facility_name == facility ) |>
    filter(product_code %in% products) |>
    mutate(year_month = yearmonth(year_month, format = "%Y %b")) |>
    arrange(year_month) |>
    complete(product_code, facility_code, year_month) |>
    group_by(facility_code, product_code, .drop = T) |>
    select(product_code, year_month, adj_consumption_in_packs) |>
    left_join(summary_stats_df, by = c("facility_code", "product_code")) |>
    mutate(adj_consumption_in_packs_filled = if_else(is.na(adj_consumption_in_packs), median, adj_consumption_in_packs),
           adj_consumption_in_packs_filled = replace_outliers_with_median(adj_consumption_in_packs_filled)) |>
    select(- adj_consumption_in_packs, - mean, - median) |>
    ggplot(aes(year_month, adj_consumption_in_packs_filled, group = product_code)) +
    geom_line(aes(color = product_code)) +
    labs(title = "Trendlines for Selected Products",
         x = "Reporting Month",
         y = "Consumption in Packs")

}

#############################################################################
#  Replace outliers with median
#############################################################################



# function to replace outliers with median
replace_outliers_with_median <- function(x, coef = 1.5) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  fence_low <- q1 - coef * iqr
  fence_high <- q3 + coef * iqr
  x[x < fence_low | x > fence_high] <- median(x, na.rm = TRUE)
  return(x)
}

#
# replace_facility_outlier_with_median <- function(df, facility) {
#
#   df |>
#     filter(facility_name == facility) |>
#     mutate(year_month = yearmonth(year_month, format = "%Y %b")) |>
#     arrange(year_month) |>
#     complete(product_code, facility_code, year_month) |>
#     group_by(facility_code, product_code, fiscal_year) |>
#     summarise(mean = mean(adj_consumption_in_packs, na.rm = T),
#               median = median(adj_consumption_in_packs, na.rm = T),
#               .groups = "drop") |>
#     mutate(median = ifelse(is.na(median), 0, median),
#            mean = ifelse(is.na(mean), 0, mean)) -> summary_stats_df
#
#
#   # replace the missing values with median ----------------------------------
#
#   df <- df |>
#     filter(facility_name == facility ) |>
#     mutate(year_month = yearmonth(year_month, format = "%Y %b")) |>
#     arrange(year_month) |>
#     complete(product_code, facility_code, year_month) |>
#     group_by(facility_code, product_code, .drop = T) |>
#     select(product_code, year_month, fiscal_year, adj_consumption_in_packs) |>
#     left_join(summary_stats_df, by = c("facility_code", "product_code", "fiscal_year")) |>
#     mutate(adj_consumption_in_packs_filled = ifelse(is.na(adj_consumption_in_packs), median, adj_consumption_in_packs),
#            adj_consumption_in_packs_filled = replace_outliers_with_median(adj_consumption_in_packs_filled)) |>
#     select(- adj_consumption_in_packs, -mean, -median, -fiscal_year) |>
#     pivot_wider(names_from = year_month, values_from = adj_consumption_in_packs_filled)
#
#
#     return(df)
# }

#############################################################################
#  Test Model Accuracy
#############################################################################

measure_model_accuracy <- function(df, facility_name, test_period) {

  options(scipen = 999)

  replace_facility_outlier_with_median(df, facility_name) |>
    ungroup() |>
    select(-facility_code) |>
    pivot_longer(cols = where(is.numeric),
                 names_to = "year_month",
                 values_to = "consumption") |>
    mutate(year_month = yearmonth(year_month, format = "%Y %b")) |>
    as_tsibble(
      index = year_month,
      key = c(product_code)
    ) |> aggregate_key(product_code, total_consumption = sum(consumption)) |>
    relocate(year_month) -> df_ts



  df_ts <- df_ts |>
    filter(!is_aggregated(product_code))

  print("seperating our training set")

  train <- df_ts %>%
    group_by(product_code) %>%
    arrange(year_month) %>%
    mutate(row_number = row_number()) %>%
    filter(row_number <= (n() - test_period)) %>%
    ungroup() %>%
    select(-row_number)

  print("training out ets model...")

  execution_time <- system.time({
    model <- train |>
      model(
        ets = ETS(total_consumption)
      )
  })

  print("finished training our ets model...")
  print("making predictions...")

  forecast_values <- model |>
    forecast(h = test_period)

  print("finished making our predictions")

  # Extract the forecasted values and actual values for each product
  forecasted <- as.data.frame(forecast_values) %>%
    rename(forecast = .mean)

  actual <- df_ts %>%
    filter(year_month > max(train$year_month)) %>%
    select(product_code, year_month, total_consumption) %>%
    rename(actual = total_consumption)

  # Calculate accuracy metrics for each product



  accuracy_metrics <- forecasted %>%
    left_join(actual, by = c("product_code", "year_month")) %>%
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

  return(list(accuracy_table = accuracy_metrics, mean_accuracy = mean_accuracy_metrics, time_taken = execution_time, summary = group_accuracies))

}


#############################################################################
#  forecasting for new periods
#############################################################################

forecast_demand <- function(df, facility, financial_year, forecast_period) {
  options(scipen = 999)

  replace_facility_outlier_with_median(df, facility) |>
    ungroup() |>
    select(-facility_code) |>
    pivot_longer(cols = where(is.numeric),
                 names_to = "year_month",
                 values_to = "consumption") |>
    mutate(year_month = yearmonth(year_month, format = "%Y %b")) |>
    as_tsibble(
      index = year_month,
      key = c(product_code)
    ) |> aggregate_key(product_code, total_consumption = sum(consumption)) |>
    relocate(year_month) -> df_ts



  df_ts <- df_ts |>
    filter(!is_aggregated(product_code)) |>
    mutate(fiscal_year = fiscal_year(yearquarter(year_month, fiscal_start = 7))) |>
    filter(fiscal_year < financial_year) |>
    select(-fiscal_year)

  # print("seperating our training set")
  #
  # train <- df_ts %>%
  #   group_by(product_code) %>%
  #   arrange(year_month) %>%
  #   mutate(row_number = row_number()) %>%
  #   filter(row_number <= (n() - test_period)) %>%
  #   ungroup() %>%
  #   select(-row_number)

  print("training out ets model...")

  execution_time <- system.time({
    model <- df_ts |>
      model(
        ets = ETS(total_consumption)
      )
  })

  print("finished training our ets model...")
  print("making predictions...")

  forecast_values <- model |>
    forecast(h = forecast_period)

  print("finished making our predictions")


  forecasted <- as.data.frame(forecast_values) %>%
       rename(forecast = .mean)


  forecast <- forecasted |>
    group_by(product_code) |>
    summarise(forecasted_demand = round(sum(forecast)))



  beginning_of_data = min(df_ts$year_month)
  end_of_data = max(df_ts$year_month)
  beginning_of_forecast = min(forecast_values$year_month)
  end_of_forecast = max(forecast_values$year_month)


  return(list(bod = beginning_of_data, eod = end_of_data, bof = beginning_of_forecast, eof = end_of_forecast, demand = forecast))
}


#############################################################################
#  create trend lines for the products in e_charts
#############################################################################


products_trendline_chart <- function(df, session) {
  echartsColors <- c("#0c7c8c", "#5ac2e8", "#DF8B0D", "#768948", "#548C2F")
  df |> then(~ {
    data <- .

    rows <- c(session$input$explorationTable_rows_selected)
    # print("this is the row object")
    # print(class(rows))
    # print(rows)


    selProduct <- data |>
      mutate(date = as.character(yearmonth(date))) |>
      pivot_wider(
        id_cols = product_code,
        names_from = date,
        values_from = adj_consumption_in_packs
      ) |>
      filter(row_number() %in% rows) |>
      pull(product_code)

    print("the following are the selected products")
    print(selProduct)

    data |>
    mutate(date = yearmonth(date)) |>
      mutate(date = as.character(date)) |>
      mutate(color = ifelse(outlier == TRUE, "red", "black"),
             size = 20) |>
      filter(product_code %in% selProduct) |>
      group_by(product_code) |>
      e_charts(date) |>
      e_line(serie = adj_consumption_in_packs, symbolSize = 8,
             symbol = "roundRect") |>
      e_add_nested("itemStyle", color, size) |>
      e_tooltip(trigger = "axis") |>
      e_color(echartsColors) |>
      e_title(text = "Trends of products selected",
              # subtext = "to populate the chart, select items from the table above",
              left = 10, up = 5) |>
      e_legend(left = "right", bottom = 20, type = "scroll") |>
      e_toolbox_feature("dataZoom") |>
      e_toolbox_feature("reset") |>    # feature = "reset"
      e_toolbox_feature("dataView") |>
      e_toolbox_feature("saveAsImage")
  })

  # df %...>%
  #     mutate(date = yearmonth(date)) %...>%
  #     mutate(date = as.character(date)) %...>%
  #     mutate(color = ifelse(outlier == TRUE, "red", "black"),
  #            size = 20) %...>%
  #     filter(product_code %in% products) %...>%
  #     group_by(product_code) %...>%
  #     e_charts(date) %...>%
  #     e_line(serie = adj_consumption_in_packs, symbolSize = 8,
  #            symbol = "roundRect") %...>%
  #     e_add_nested("itemStyle", color, size) %...>%
  #     e_tooltip(trigger = "axis") %...>%
  #     e_color(echartsColors) %...>%
  #     e_title(text = "Trends of products selected",
  #             # subtext = "to populate the chart, select items from the table above",
  #             left = 10, up = 5) %...>%
  #     e_legend(left = "right", bottom = 20, type = "scroll") %...>%
  #     e_toolbox_feature("dataZoom") %...>%
  #     e_toolbox_feature("reset") %...>%    # feature = "reset"
  #     e_toolbox_feature("dataView") %...>%
  #     e_toolbox_feature("saveAsImage")




}

#############################################################################
#  function to mark the outliers
#############################################################################

isnt_out_tukey <- function(x, k = 1.5, na.rm = TRUE) {
  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(quar)

  (quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr)
}

#############################################################################
#  train model for selected facility
#############################################################################
#
#
# forecast_facility_arima <- function(data, forecast_period) {
#   forecast_period <- forecast_period * 6
#   df <- data |>
#     as_tsibble(index = date, key = c(program,product_code))
#   #separate train and test data
#   train <- df |> filter_index(~ paste0(max(data$date) - 6))
#
#   # train the model and record execution time
#   print("training our model")
#   execution_time <- system.time(
#     model <- train |>
#       #convert to work with arimas
#       mutate(date = as.numeric(date)) |>
#       model(
#       arima = ARIMA(adj_consumption_in_packs)
#     )
#   )
#   print("making our forecasts")
#   forecast_values <- model |>
#     #include the 6 months of test data
#     forecast(h = 3 + forecast_period) |>
#     mutate(date = yearmonth(date))
#
#   forecast_demand <- forecast_values |>
#     filter_index(paste0(max(data$date)) ~ .) |>
#     as_tibble() |>
#     group_by(product_code) |>
#     summarise(demand = round(sum(.mean)))
#
#   # forecast_plot <- arima_forecast_values |>
#   #   filter(product_code == "10010022MD") |>
#   #   autoplot(df)
#   print("function complete")
#
#   # calculating model accuracy
#   actual <- data %>% mutate(date = yearmonth(date)) |>
#     filter(date > max(train$date)) %>%
#     select(product_code, date, adj_consumption_in_packs) %>%
#     rename(actual = adj_consumption_in_packs)
#
#   #get the accuracy metrics
#   accuracy_metrics <- forecast_values %>%
#     as_tibble() |>
#     rename(forecast = .mean) |>
#     inner_join(actual, by = c("product_code", "date")) %>%
#     group_by(product_code) %>%
#     summarise(
#       actual = sum(actual),
#       forecast = sum(forecast)
#     ) |>
#     mutate(forecast = round(as.numeric(forecast), 2)) |>
#     mutate(
#       PE = ((actual - forecast) / (actual)) * 100,
#       PE = round(case_when(
#         PE == Inf | PE == -Inf ~ 100,
#         is.nan(PE) ~ 0,
#         TRUE ~ PE
#       ),1),
#       APE = abs(PE)
#     )
#
#   group_accuracies <- accuracy_metrics %>%
#     summarise(
#       APE_less_than_10 = mean(APE < 10) * 100,
#       APE_less_than_20 = mean(APE < 20) * 100,
#       APE_less_than_30 = mean(APE < 30) * 100
#     )
#
#   mean_accuracy_metrics <- accuracy_metrics |>
#     summarise(MAPE = mean(APE),
#               MPE = mean(PE))
#
#   return(list(
#     time = execution_time,
#     model = model,
#     demand = forecast_demand,
#     forecast = forecast_values,
#     accuracy_table = accuracy_metrics,
#     mean_accuracy = mean_accuracy_metrics,
#     summary = group_accuracies
#   ))
# }

#############################################################################
#  customizing datatables
#############################################################################

custom_datatable <- function(..., datatable_options = list()) {
  datatable(
    extensions = list('Buttons' = NULL, 'FixedColumns' = NULL),
    class = 'cell-border hover nowrap compact',
    # selection = list(mode = 'multiple', target = 'row'),
    options = list(
      # columnDefs = list(list(width = '350px', targets = 1)),
      scrollX = "8000px",
      scrollY = "400px",
      paging = FALSE,
      dom = "Blfrtip",
      buttons = list(
        "copy", list(
          extend = "collection",
          buttons = c("csv", "excel", "pdf"),
          text = "Download"
        )
      ),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#DF8B0D', 'color': '#000'});",
        "}"),
      datatable_options
    ),
    ...
  )
}


#############################################################################
#  creating forecast trend line
#############################################################################


draw_forecast_trendline <- function(forecast_values, products, data) {
  # print("our forecast values")
  # print(forecast_values)
  # df <- data |>
  #   as_tsibble(index = date, key = product_code)
  # print("our df")
  # print(df)
  #
  # forecast_values |>
  #   filter(product_code %in% products) |>
  #   autoplot(df)
  #
  print("this is the data the charts are drawn from")
  print(data)
  print("these are our forecast values")
  print(forecast_values)
  echartsColors <- c("#0c7c8c", "#5ac2e8", "#DF8B0D", "#768948", "#548C2F")
  data |>
    filter(product_code %in% products) |>
    mutate(consumption = adj_consumption_in_packs) |>
    bind_rows(forecast_values |>
                filter(product_code %in% products) |>
                hilo() |>
                unpack_hilo(`80%`) |>
                mutate(forecasts = .mean), .id = "group") |>
    mutate(date = as.Date(date)) |>
    group_by(product_code, group)|>
    e_chart(date) |>
    e_line(serie = consumption) |>
    e_line(serie = forecasts, lineStyle = list(type = 'dashed')) |>
    e_tooltip(trigger = "axis")|>
    e_color(echartsColors) |>
    e_title(text = "Forecasts of products selected",
            # subtext = "to populate the chart, select items from the table above",
            left = 10, up = 5) |>
    e_legend(left = "right", bottom = 20, type = "scroll") |>
    e_toolbox_feature("dataZoom") |>
    e_toolbox_feature("reset") |>  # feature = "reset"
    e_toolbox_feature("dataView") |>
    e_toolbox_feature("saveAsImage")
}

#############################################################################
#  function to forecast by financial years
#############################################################################

#
#
# forecast_facility_fiscal_arima <- function(data, financialYear, session) {
#
#
#
# data |> then(~ {
#     data_df <- .
#   financialIndex <- paste0(substr(financialYear, 1, 4),"-07-01")
#     df <- data_df |> as_tsibble(index = date, key = c(program,product_code))
#   train <- df |> filter_index(~ financialIndex)
#
#   # train the model and record execution time
#   print("training our model")
#   execution_time <- system.time(
#     model <- train |>
#       #convert to work with arimas
#       mutate(date = as.numeric(date)) |>
#       model(
#         arima = ARIMA(adj_consumption_in_packs)
#       )
#   )
#
#   print("making our forecasts")
#   forecast_values <- model |>
#     #forecasting for one financial year
#     forecast(h = 6) |>
#     mutate(date = yearmonth(date))
#
#   forecasted <- as.data.frame(forecast_values) %>%
#     rename(forecast = .mean)
#
#   actual <- df |>
#     filter(date > max(train$date) & date < yearmonth(paste0(as.numeric(substr(financialYear, 1, 4)) + 1,"-07-01"))) |>
#     select(product_code, item_description, date, adj_consumption_in_packs, program) |>
#     rename(actual = adj_consumption_in_packs)
#
#   # print("this is my forecasted data")
#   # print(head(forecasted))
#   # print("this is my actual data")
#   # print(head(actual))
#
#   accuracy_metrics <- forecasted %>%
#     left_join(actual, by = c("product_code", "date", "program")) %>%
#     ungroup() |>
#     group_by(product_code) %>%
#     summarise(
#       actual = sum(actual, na.rm = T),
#       forecast = sum(forecast)
#     ) |>
#     mutate(forecast = round(as.numeric(forecast), 2)) |>
#     mutate(
#       PE = ((actual - forecast) / (actual)) * 100,
#       PE = round(case_when(
#         PE == Inf | PE == -Inf ~ 100,
#         is.nan(PE) ~ 0,
#         TRUE ~ PE
#       ),1),
#       APE = abs(PE)
#     )
#
#   group_accuracies <- accuracy_metrics %>%
#     summarise(
#       APE_less_than_10 = mean(APE < 10) * 100,
#       APE_less_than_20 = mean(APE < 20) * 100,
#       APE_less_than_30 = mean(APE < 30) * 100
#     )
#
#
#   mean_accuracy_metrics <- accuracy_metrics |>
#     summarise(MAPE = mean(APE),
#               MPE = mean(PE))
#
#   forecast_demand <- forecast_values |>
#     as_tibble() |>
#     group_by(product_code) |>
#     summarise(demand = round(sum(.mean)))
#
#   if (nrow(actual) < 1) {
#     mean_accuracy_metrics <- mean_accuracy_metrics * NA
#     group_accuracies <- group_accuracies * NA
#     accuracy_metrics <- accuracy_metrics |>
#       mutate(actual = NA,
#              PE = NA,
#              APE = NA)
#     # accuracy_metrics <- accuracy_metrics * NA
#   }
#
#   print("function complete")
#   print(execution_time)
#   print(model)
#   print(forecast_demand)
#   print(forecast_values)
#   print(accuracy_metrics)
#   print(group_accuracies)
#   print(mean_accuracy_metrics)
#
#
#
#   # # Use forecastData for the table part of your Shiny app
#   session$output$forecastTable <- renderDT({
#
#     # print("these are my forecasts depending on my choice of method")
#     # print(class(forecast_object()$forecast))
#
#     forecast_values |>
#       as_tibble() |>
#       mutate(.mean = round(.mean, 1)) |>
#       pivot_wider(id_cols = c(program, product_code), names_from = date, values_from = .mean) |>
#       mutate(demand = round(rowSums(across(where(is.numeric))),1)) |>
#       custom_datatable(selection = list(mode = 'multiple', target = 'row'),
#                        datatable_options = list(scrollY = "400px")) |>
#       formatStyle("product_code", backgroundColor = "#C2ECED",
#                   fontWeight = "bold")
#   })
#
#
#
#
#   # execution_time <-  reactive({execution_time})
#   #     model <-  reactive({model})
#   # forecast_demand <-  reactive({forecast_demand})
#   # forecast_values <-  reactive({forecast_values})
#   # accuracy_metrics <-  reactive({accuracy_metrics})
#   # group_accuracies <-  reactive({group_accuracies})
#   # mean_accuracy_metrics <-  reactive({mean_accuracy_metrics})
#
#     return(list(
#       time = execution_time,
#       model = model,
#       demand = forecast_demand,
#       forecast = forecast_values,
#       accuracy_table = accuracy_metrics,
#       summary = group_accuracies,
#       mean_accuracy = mean_accuracy_metrics
#   ))
#
#   })
#
#
# }


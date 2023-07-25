# Module for forecasting the facility products from the cleaned dataframe

# Author: Peter Boshe
# Version: 2023-06-09

# Packages
library(tidyverse)

# Parameters

# ============================================================================

# Code


forecastingUI <- function(id) {
ns = NS(id)
  tagList(
    shinyjs::useShinyjs(),
    grid_container(
      layout = c(
        "forecastingInput forecastingOutput"
      ),
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "295px",
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "forecastingInput",
        card_header("Period to Forecast"),
        card_body(
          radioButtons(
            inputId = ns("forecastingMethod"),
            label = "Select Forecast Method",
            choiceNames = list("Financial Year", "Multiple Years"),
            choiceValues = list(1,2)
          ),
          selectInput(
            inputId = ns("fiscalYear"),
            label = "Enter fiscal year to forecast for",
            choices = NULL
          ),
            sliderInput(
              inputId = ns("forecastingSteps"),
              label = "Select number of years to forecast",
              min = 0,
              max = 3,
              value = 1,
              width = "100%"
            ),
          actionButton(
            inputId = ns("forecastButton"),
            label = "Forecast",
            icon = icon(name = "bullseye")
          ),
          tags$hr(),
          tags$strong(tags$em("Model Performance")),
          verbatimTextOutput(outputId = ns("modelAccuracyPercent")),
          verbatimTextOutput(outputId = ns("modelAccuracySummary")),
          textOutput(outputId = ns("modelSpeed"))
        )
      ),
      grid_card(
        area = "forecastingOutput",
        card_body(
          shinyjs::hidden(
            tags$div(id = ns("spinnerContainer"),
                     style = "display: none;",  # Initially hide the spinner
                     withSpinner(DTOutput(outputId = ns("forecastTable")))
            )
          )


        ),

          tabsetPanel(
            tabPanel(
              title = "Forecast Chart",
              card_body(echarts4rOutput(outputId = ns("forecastChart")))
            ),
            tabPanel(
              title = "Forecast Accuracy Table",
              card_body(DTOutput(outputId = ns("accuracyTable")))
            ),
            tabPanel(
              title = "BUQ forecast comparison",
              card_body(DTOutput(outputId = ns("buqAccuracyTable")))
            )
          )

        # )
      )
  ))
}


forecastingServer <- function(id, cleanedData, filteredData, buqData){
  moduleServer(
    id,
    function(input, output, session){
      shinyjs::useShinyjs()

      # Define a reactive expression that uses values$cleanedData
      forecastData <- reactive({
        cleanedData <- if (!is.null(cleanedData())) cleanedData() else filteredData()

        cleanedData
      })



# update the financial years option by the forecast data ------------------

      observeEvent(session$input$applyAdjustments, {
        req(forecastData())

        financial_years <- forecastData() %...>%
          pull(financial_year) %...>%
          unique() %...>%
          sort(decreasing = TRUE) %...>%
          {
            max_year <- max(.)
            additional_year <- paste0(as.integer(substr(max_year, 1, 4)) + 1, "-", as.integer(substr(max_year, 6, 9)) + 1)
            c(additional_year, .)
          }

        financial_years |> then( ~ {
          data <- .
          updateSelectInput(
            inputId = "fiscalYear",
            choices = data
          )
        })




      })




      # define the forecast object ----------------------------------------------


      observeEvent(session$input$forecastButton, {

        # Show the spinner when the forecast is initiated
        shinyjs::show("spinnerContainer", anim = TRUE)

        if (input$forecastingMethod == 1 && !is.null(input$fiscalYear)) {
          # forecast_object <- forecast_facility_fiscal_arima(forecastData(), input$fiscalYear, session)
          forecast_facility_fiscal_arima(forecastData(), input$fiscalYear, session)
        } else if (input$forecastingMethod == 2 && !is.null(input$forecastingSteps)) {
          # forecast_object <- forecast_facility_arima(forecastData(), input$forecastingSteps)
          forecast_facility_arima(forecastData(), input$forecastingSteps, session)
        }


        # Reset the spinner container to its initial state
        shinyjs::reset("spinnerContainer")

      })


    }
  )
}


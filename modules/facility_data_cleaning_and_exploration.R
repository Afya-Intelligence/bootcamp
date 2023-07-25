# Script for data exploration module for insupply shiny app

# Author: Peter Boshe
# Version: 2023-06-07

# Packages
library(tidyverse)
library(tsibble)
# Parameters
source("R/utils.R")

# ============================================================================

cleaningUI <- function(id) {
ns = NS(id)
  tagList(
    grid_container(
      layout = c(
        "cleaningInput visualOutput"
      ),
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "260px",
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "cleaningInput",
        card_header("Adjustments"),
        card_body(
          # checkboxInput(
          #   inputId = ns("pivotWider"),
          #   label = "Expand data",
          #   value = TRUE
          # ),
          checkboxInput(
            inputId = ns("missingWithMedians"),
            label = "Fill missing with median",
            value = TRUE
          ),
          checkboxInput(
            inputId = ns("outliersWithMedians"),
            label = "Replace outliers with median",
            value = FALSE
          ),
          actionButton(
            inputId = ns("applyAdjustments"),
            label = "Apply adjustments",
            icon = icon("broom")
          )
        )
      ),
      grid_card(
        area = "visualOutput",
        card_body(
          DTOutput(outputId = ns("explorationTable"))
        ),
        card_body(
          echarts4rOutput(outputId = ns("explorationChart"))
        )
      )
    )

  )
}

cleaningServer <- function(id, output, session, filteredData) {


  # Create reactiveValues object to hold the cleaned data
  appValues <- reactiveValues(cleanedData = NULL)

  # Event observer for fetchButton click
  observeEvent(session$input$fetchData, {
    # Reset cleanedData to NULL
    appValues$cleanedData <- NULL
  })

  observeEvent(session$input$applyAdjustments, {
    missingWithMedians <- session$input$missingWithMedians
    outliersWithMedians <- session$input$outliersWithMedians

    if (missingWithMedians && outliersWithMedians) {
      cleaned <- filteredData() %...>%
        mutate(quarter = yearquarter(date, fiscal_start = 7),
               fiscal_year = fiscal_year(quarter),
               financial_year = paste0(fiscal_year-1, "-", fiscal_year)) %...>%
        group_by(financial_year, product_code) %...>%
        mutate(median = median(adj_consumption_in_packs, na.rm = T)) %...>%
        mutate(adj_consumption_in_packs = ifelse(outlier == TRUE, median, adj_consumption_in_packs)) %...>%
        mutate(date = yearmonth(date)) %...>%
        as_tsibble(index = date, key = c(program, facility_code, product_code, facility_type, facility_name, district, region, item_description, product_group, sub_group)) %...>%
        group_by_key() %...>%
        fill_gaps(.full = TRUE) %...>%
        as_tibble() %...>%
        mutate(quarter = yearquarter(date, fiscal_start = 7),
               fiscal_year = fiscal_year(quarter),
               financial_year = paste0(fiscal_year-1, "-", fiscal_year)) %...>%
        group_by(financial_year, product_code) %...>%
        mutate(adj_consumption_in_packs = ifelse(is.na(adj_consumption_in_packs),
                                                 median(adj_consumption_in_packs, na.rm = TRUE),
                                                 adj_consumption_in_packs),
               outlier = ifelse(is.na(outlier), FALSE, outlier),
               adj_consumption_in_packs = ifelse(is.na(adj_consumption_in_packs), 0, adj_consumption_in_packs)) %...>%
        ungroup() %...>%
        fill(reporting_group, .direction = "updown") %...>%
        #as_tibble() |>
        arrange(product_code, date)


      appValues$cleanedData <- cleaned
    } else if (missingWithMedians) {
      cleaned <- filteredData() %...>%
        mutate(date = yearmonth(date)) %...>%
        as_tsibble(index = date, key = c(program, facility_code, product_code, facility_type, facility_name, district, region, item_description, product_group, sub_group)) %...>%
        group_by_key() %...>%
        fill_gaps(.full = TRUE) %...>%
        as_tibble() %...>%
        mutate(quarter = yearquarter(date, fiscal_start = 7),
               fiscal_year = fiscal_year(quarter),
               financial_year = paste0(fiscal_year-1, "-", fiscal_year)) %...>%
        group_by(financial_year, product_code) %...>%
        mutate(adj_consumption_in_packs = ifelse(is.na(adj_consumption_in_packs),
                                                 median(adj_consumption_in_packs, na.rm = TRUE),
                                                 adj_consumption_in_packs),
               outlier = ifelse(is.na(outlier), FALSE, outlier),
               adj_consumption_in_packs = ifelse(is.na(adj_consumption_in_packs), 0, adj_consumption_in_packs)) %...>%
        ungroup() %...>%
        fill(reporting_group, .direction = "updown") %...>%
        #as_tibble() |>
        arrange(product_code, date)


      appValues$cleanedData <- cleaned
    } else {
      # Uncheck all checkboxes and revert to filteredData()
      updateCheckboxInput(session, "missingWithMedians", value = FALSE)
      updateCheckboxInput(session, "outliersWithMedians", value = FALSE)
      appValues$cleanedData <- NULL
    }


  })



# Section for pivoting wider our dataframe --------------------------------

  observeEvent(c(session$input$fetchData, session$input$applyAdjustments), {
    output$explorationTable <- renderDT(server = TRUE, {


  data <- if (!is.null(appValues$cleanedData)) appValues$cleanedData else filteredData() %...>% arrange(product_code, date)


      data %...>%
        mutate(date = yearmonth(date)) %...>%
        mutate(date = as.character(date)) %...>%
        pivot_wider(id_cols = c(program, product_code, item_description),names_from = date, values_from = adj_consumption_in_packs) %...>%
        # mutate_if(is.numeric, ~ replace_na(., 0)) |>
        datatable(
          extensions = list('Buttons' = NULL, 'FixedColumns' = NULL),
          class = 'cell-border hover compact',
          # extensions = "Buttons",
          selection = list(mode = 'multiple', target = 'row'),
          editable = list(target = 'cell', disable = list(columns = c(1, 2, 3))),
          options = list(
            # ordering = FALSE,
            columnDefs = list(list(width = '350px', targets = 1)),
            scrollX = "8000px",
            scrollY = "400px",
            fixedColumns = list(leftColumns = 4),
            # pageLength = 50,
            paging = FALSE,
            dom = "Blfrtip",
            buttons =
              list("copy", list(
                extend = "collection",
                buttons = c("csv", "excel", "pdf"),
                text = "Download"
              ) ), # end of buttons customization
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#DF8B0D', 'color': '#000'});",
              "}")
          )
        ) %...>%
        formatStyle("product_code", backgroundColor = "#C2ECED",
                    fontWeight = "bold") %...>%
        formatStyle("item_description", backgroundColor = "#C2ECED",
                    fontWeight = "bold")



    })
  })

  # be able to edit each numeric cell--------------------------------------------

  # proxyexplorationTable <- dataTableProxy('explorationTable')

    observeEvent(session$input$explorationTable_cell_edit, {
      # explorationTable <- if (!is.null(appValues$cleanedData)) appValues$cleanedData else filteredData()

      data <- if (!is.null(appValues$cleanedData)) appValues$cleanedData else filteredData() %...>% arrange(product_code, date)

      # print("this is the dataset to be edited")


      explorationTable <- data %...>%
        mutate(date = as.Date(paste0(date, " 01"), format = "%Y %b %d")) %...>%
        mutate(date = yearmonth(date)) %...>%
        mutate(date = as.character(date)) %...>%
        pivot_wider(id_cols = c(program, product_code, item_description),names_from = date, values_from = adj_consumption_in_packs)

      # print("lets see if we get the explorationTable data")
      newExplorationTable <- explorationTable |>
        then(~ {
          explorationTable <- .
          # print(test_table)
          info <- session$input$explorationTable_cell_edit

          # Extract the modified value and row/column indices
          new_value <- info$value
          row_index <- info$row
          col_index <- info$col

          # Update the corresponding cell in the explorationTable
          explorationTable[row_index, col_index] <- as.numeric(new_value)


          explorationTable <- explorationTable |>
            pivot_longer(cols = where(is.numeric),
                         names_to = "date",
                         values_to = "adj_consumption_in_packs")

          return(explorationTable)


        })

      updatedTable <- eventReactive(isolate(session$input$explorationTable_cell_edit), {
        future_promise({
          filteredData() |> then(~ {
            df <- .
            df |>
              mutate(date = as.character(yearmonth(date))) |>
              select(program, product_code, item_description, date, outlier)
          })
        }) %...>%
          left_join(newExplorationTable, by = c("program", "product_code", "item_description", "date")) %...>%
          mutate(date = yearmonth(date))
      })


      # Update the original dataset with the changes
      reactive({
        if (!is.null(appValues$cleanedData)) {
          appValues$cleanedData <- updatedTable()
        } else {
          # Update filteredData()
          filteredData() <- updatedTable()
        }
      })



    })



# section to visualize the selected rows ----------------------------------

  observeEvent(c(session$input$explorationTable_rows_selected, session$input$applyAdjustments), {
    req(session$input$explorationTable_rows_selected)
    # print(session$input$explorationTable_rows_selected)

    data <- if (!is.null(appValues$cleanedData)) appValues$cleanedData else filteredData() %...>% arrange(product_code, date)




    output$explorationChart <- renderEcharts4r({
      products_trendline_chart(data, session)
    })
  })
  cleanedData <- reactive({appValues$cleanedData})
  # print(cleanedData)
  return(
    list(
      cleaned = cleanedData
    )
  )


}




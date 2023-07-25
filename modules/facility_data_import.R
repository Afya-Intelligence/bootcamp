# Module for the data extraction given the options

# Author: Peter Boshe
# Version: 2023-06-01

# Packages
library(tidyverse)
library(fst)


# Parameters
source("R/utils.R")

# data_path <- here::here("app-data/20230604_facility_consumption_compressed.fst")
data_path <- here::here("app-data/20230622_all_programs_final_joined.fst")

# ============================================================================

importUI <- function(id) {
ns = NS(id)
  tagList(
        grid_container(
          layout = c(
            "filter_input summary_output"
          ),
          row_sizes = c(
            "1fr"
          ),
          col_sizes = c(
            "260px",
            "1fr"
          ),
          gap_size = "10px",
          # fluidPage(
            grid_card(
              area = "filter_input",
              card_header("Data Import"),
              card_body(
                selectInput(
                  inputId = ns("programInput"),
                  label = "Select Program Category",
                  choices = NULL
                ),
                selectInput(
                  inputId = ns("regionInput"),
                  label = "Select Region",
                  choices = NULL
                ),
                selectInput(
                  inputId = ns("districtInput"),
                  label = "Select District",
                  choices = NULL
                ),
                selectInput(
                  inputId = ns("facilityType"),
                  label = "Select Facility Type",
                  choices = NULL
                ),
                selectInput(
                  inputId = ns("facilityInput"),
                  label = "Select Facility",
                  choices = NULL
                ),
                tags$hr(),
                actionButton(
                  inputId = ns("showAdvancedFilters"),
                  label = "Show Advanced Filters"
                ),
                shinyjs::hidden(
                  div(
                    id = ns("advancedCategories"),
                    # tags$button("Advanced filter options"),
                    # tags$br(),
                    selectInput(
                      inputId = ns("productGroup"),
                      label = "Select Product Group",
                      choices = NULL
                    ),
                    selectInput(
                      inputId = ns("productSubGroup"),
                      label = "Select Product Sub-group",
                      choices = NULL
                    )
                    # selectInput(
                    #   inputId = ns("programInput"),
                    #   label = "Select A Data Program Category",
                    #   choices = NULL
                    # )
                  )
                ),
                tags$hr(),
                actionButton(
                  inputId = ns("fetchData"),
                  label = "Fetch Data"
                ),
                tags$br(),
                textOutput(outputId = ns("facilityTimeline"))

              )
            )
          # )
          ,
          grid_nested(
            area = "summary_output",
            layout = c(
              "summaryTablelo"
            ),
            # title = "Data Summary",

            grid_card(
              area = "summaryTablelo",
              card_body(
                tabsetPanel(
                  tabPanel(
                    title = "Missing Data",
                    card_body(echarts4rOutput(outputId = ns("ordersPerReport"))),
                    tags$hr(),
                    card_body(DTOutput(outputId = ns("leastReportedProductsTable")), min_height = "1000px")
                  ),
                  tabPanel(
                    title = "Outliers",
                    card_body(echarts4rOutput(outputId = ns("topOutlierProductsChart"))),
                    tags$hr(),
                    card_body(DTOutput(outputId = ns("topOutlierProducts")), min_height = "1000px")
                  ),
                  tabPanel(
                    title = "Consumption Summary",
                    card_body(echarts4rOutput(outputId = ns("mostConsumedProducts"))),
                    tags$hr(),
                    card_body(DTOutput(outputId = ns("financialYearSummary")))
                  )
                )
              )
            )

           )
          )
        )

  # )
}

importServer <- function(id, credentials) {
moduleServer(
  id,
  function(input, output, session){
    # # reactive to load data when the app starts ---------------------------------------

    # loaded_data <- reactive({
    #   # use req to only render results when credentials()$user_auth is TRUE
    #   req(credentials()$user_auth)
    #   read_fst(data_path)
    # })
    #
    # buq_data <- reactive({
    #   req(credentials()$user_auth)
    #   read_fst("app-data/buq_2023_2024.fst")
    # })
    #
    loaded_data <- eventReactive(input$importData, {

      req(credentials()$user_auth)
      showNotification("your data is being imported.. please wait..")


      # to filter depending on the choice made
      if (session$input$programInput != "ALL"){
        df <- future(read_fst(data_path),seed = TRUE)  %...>% {
          data <- .
          data |>
            filter(program == session$input$programInput)
        }
      } else {
        df <- future(read_fst(data_path), seed = TRUE)
      }

      buq_df <- reactive({
        req(credentials()$user_auth)
        read_fst("app-data/buq_2023_2024.fst")
      })

      return(
        list(
          data = df,
          buq_data = buq_df
        )
      )
      # df

    })



    # initialize filter by program --------------------------------------------

    observe({
        # req(loaded_data())  # Ensure loaded_data() has completed executing
        data <- loaded_data$data()
        # print("this is my observe data")
        # print(head(data))
        # print("this is my program list")
        # print(unique(data$program))

        # Get program list
        # program_choices <- unique(data$program)

        # Update the choices in selectInput for region
        updateSelectInput(
          inputId = "programInput",
          choices = c("ALL",unique(data$program)),
          selected = "ALL"
        )

    })


    # Update choices of regions depending on program selected -----------------

    observeEvent(session$input$programInput, {
      data <- loaded_data$data()  # Access the loaded data
      # print("This is the received program input")
      # print(session$input$programInput)

      program_filter <- TRUE

      # print("this is the data to be filtered")
      # print(head(data))

      if (session$input$programInput != "ALL") {
        program_filter <- program == input$programInput
      }
      # print(program_filter)




      # Filter the data based on the selected program
      filtered_data <- loaded_data$data()  %...>% {
        data <- .
        data |> dplyr::filter(program_filter)
      }


      # Update the choices in selectInput for district
      updateSelectInput(
        inputId = "regionInput",
        choices = unique(filtered_data$region)
      )
    })



    observeEvent(session$input$regionInput, {
      data <- loaded_data$data()  # Access the loaded data

      # Filter the data based on the selected province
      filtered_data <- data %...>% {
        data <- .
        data |> filter(region == session$input$regionInput)
      }


      # Update the choices in selectInput for district
      updateSelectInput(
        inputId = "districtInput",
        choices = unique(filtered_data$district)
      )
    })

    observeEvent(session$input$districtInput, {
      data <- loaded_data$data()  # Access the loaded data

      # Filter the data based on the selected province and district
      filtered_data <- data %...>% {
        data <- .
        data |>
          filter(region == session$input$regionInput,
                 district == session$input$districtInput)
      }


      # Update the choices in selectInput for facility type
      updateSelectInput(
        inputId = "facilityType",
        choices = unique(filtered_data$facility_type)
      )
    })

    observeEvent(session$input$facilityType, {
      data <- loaded_data$data()  # Access the loaded data

      # Filter the data based on the selected province and district and type
      filtered_data <- data %...>% {
        data <- .
        data |>
        filter(region == session$input$regionInput,
               district == session$input$districtInput,
               facility_type == session$input$facilityType)
      }


      # Update the choices in selectInput for facility type
      updateSelectInput(
        inputId = "facilityInput",
        choices = unique(filtered_data$facility_name)
      )

      updateSelectInput(
        inputId = "productGroup",
        choices = c("ALL", unique(filtered_data$product_group)),
        selected = "ALL"
      )

      updateSelectInput(
        inputId = "productSubGroup",
        choices = c("ALL", unique(filtered_data$sub_group)),
        selected = "ALL"
      )

      # updateSelectInput(
      #   inputId = "programInput",
      #   choices = c("NONE", unique(filtered_data$program)),
      #   # choices = c("NONE" = ""),
      #   selected = "NONE"
      # )


    })


    # Option 2: Using a reactive expression

    filteredData <- eventReactive(session$input$fetchData, {
      data <- loaded_data$data()  # Access the loaded data
      # print(head(data))



      # Create empty filters for group, sub-group, and ven
      group_filter <- TRUE
      sub_group_filter <- TRUE
      # ven_filter <- TRUE
      program_filter <- TRUE


      # Check if groupInput is not empty
      if (!is.null(session$input$groupInput) && session$input$groupInput != "ALL") {
        group_filter <- product_group == session$input$groupInput
      }

      # Check if subGroupInput is not empty
      if (!is.null(session$input$subGroupInput) && session$input$subGroupInput != "ALL") {
        sub_group_filter <- sub_group == session$input$subGroupInput
      }

      # # Check if venInput is not empty
      # if (!is.null(session$input$venInput) && session$input$venInput != "NONE") {
      #   ven_filter <- ven == session$input$venInput
      # }
      if (!is.null(session$input$programInput) && session$input$programInput != "ALL") {
        program_filter <- program == session$input$programInput
      }



      # Filter based on selected inputs
      filtered <- data %...>% {
        data <- .
        data |>
          filter(program_filter) |>
          filter(
            region == session$input$regionInput,
            district == session$input$districtInput,
            facility_type == session$input$facilityType,
            facility_name == session$input$facilityInput
            # group_filter,  # Apply group filter
            # sub_group_filter  # Apply sub-group filter
            # ven_filter
          ) |>
          filter(
            group_filter,
            sub_group_filter) |>  # Apply ven filter
          group_by(product_code) |>
          mutate(outlier = !isnt_out_tukey(adj_consumption_in_packs)) |>
          ungroup()
      }


      filtered  # Return the filtered data
    })


    # get the facility timeline output ----------------------------------------

    observeEvent(session$input$fetchData, {
      # data <- loaded_data()  # Access the loaded data
      #
      # # Filter based on selected inputs
      # filtered <- data %>%
      #   filter(
      #     province == session$input$regionInput,
      #     district == session$input$districtInput,
      #     facility_name == session$input$facilityInput
      #   )

      range <- filteredData() |>
        select(-outlier) |>
        summarise(min_date = min(date), max_date = max(date))

      output$facilityTimeline <- renderText({
        paste0("This dataset includes records from ", range$min_date, " to ", range$max_date)
      })
    })


    # to show/hide advanced features ------------------------------------------

    # Add an observeEvent to toggle the visibility of advancedCategories
    observeEvent(session$input$showAdvancedFilters, {
      shinyjs::toggle(id = "advancedCategories", anim = TRUE)
    })

    # filter the data once choices are made -----------------------------------

    observeEvent(session$input$fetchData, {


      output$ordersPerReport <- renderEcharts4r({

        echartsColors <- c("#0c7c8c", "#5ac2e8", "#DF8B0D", "#768948", "#548C2F")
        filteredData() %>%
          group_by(month = lubridate::month(date), year = lubridate::year(date)) %>%
          summarise(no_of_items = max(row_number())) %>%
          arrange(year, month) %>%
          mutate(month = lubridate::month(month, label = TRUE)) %>%
          unite(col = "month_year", c(month, year), sep = "-") %>%
          e_charts(month_year) %>%
          e_line(serie = no_of_items) |>
          e_tooltip(trigger = "axis") |>
          e_color(echartsColors) |>
          e_title(text = "Number of Products Reported in each R&R",
                  subtext = "calculated by total line items each report",
                  left = "center", top = 10) |>
          e_legend(left = "right", top = 40) |>
          e_toolbox_feature("dataZoom") |>
          e_toolbox_feature("reset") |>  # feature = "reset"
          e_toolbox_feature("dataView") |>
          e_toolbox_feature("saveAsImage")
      })


      # show table for least reported products
      output$leastReportedProductsTable <- renderDT({
        expected_reports <- filteredData() |>
          # select(-outlier) |>
          distinct(month = lubridate::month(date), year = lubridate::year(date)) |>
          nrow()
        filteredData() |>
          # select(-outlier) |>
          group_by(product_code, item_description) %>%
          summarise(reported = max(row_number())) |>
          mutate(missing = expected_reports - reported, expected = expected_reports) |>
          arrange(desc(missing)) |>
          custom_datatable(datatable_options = list(
            caption = "Product occurence in R&R reports",
            scrollY = "450px"))
        # DT::datatable(options = list(scrollX = TRUE, scrollY = "700px", pageLength = 50),
        #               caption = "Product occurance in R&R reports")
      })
      # show chart for most consumed products
      output$mostConsumedProducts <- renderEcharts4r({
        echartsColors <- c("#0c7c8c", "#5ac2e8", "#DF8B0D", "#768948", "#548C2F")
        filteredData() |>
          group_by(item_description) |>
          # group_by(product_code) |>
          summarise(consumption = sum(adj_consumption_in_packs)) |>
          slice_max(order_by = consumption, n = 20) |>
          e_charts(item_description) |>
          e_bar(consumption) |>
          e_flip_coords() |>
          e_y_axis(inverse = TRUE) |>
          e_tooltip(trigger = "axis") |>
          e_color(echartsColors) |>
          e_title(text = "Top Consumed Products",
                  subtext = "calculated by total adjusted consumption in packs",
                  left = "center", top = 10) |>
          e_legend(left = "right", top = 40) |>
          e_toolbox_feature("dataZoom") |>
          e_toolbox_feature("reset") |>  # feature = "reset"
          e_toolbox_feature("dataView") |>
          e_toolbox_feature("saveAsImage")|>
          e_x_axis(
            axisLabel = list(
              interval = 0
            )
          ) %>%
          e_grid(left = "20%")

      })


      # # deal with outliers by marking and displaying ----------------------------
      # isnt_out_tukey <- function(x, k = 1.5, na.rm = TRUE) {
      #   quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
      #   iqr <- diff(quar)
      #
      #   (quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr)
      # }

      output$topOutlierProducts <- renderDT({
        outlier_df <- filteredData() |>
          group_by(product_code, item_description) |>
          mutate(outlier = !isnt_out_tukey(adj_consumption_in_packs))
        outlier_df |>
          summarise(no_of_outliers = sum(outlier)) |>
          arrange(desc(no_of_outliers)) |>
          filter(no_of_outliers > 0) |>
          custom_datatable(datatable_options = list(
            caption = "Products with Outliers in R&R reports",
            scrollY = "500px"))
        # datatable(options = list(scrollX = TRUE, scrollY = "700px", pageLength = 50),
        #           caption = "Products with Outliers in R&R reports")
      })

      output$topOutlierProductsChart <- renderEcharts4r({
        echartsColors <- c("#0c7c8c", "#DB7472", "#DF8B0D", "#768948", "#548C2F")
        outlier_df <- filteredData() |>
          group_by(product_code) |>
          mutate(outlier = !isnt_out_tukey(adj_consumption_in_packs))
        top_products_with_outliers <- outlier_df |>
          summarise(no_of_outliers = sum(outlier)) |>
          arrange(desc(no_of_outliers)) |>
          filter(no_of_outliers > 0) |>
          head(10) |>
          pull(product_code)
        outlier_df |>
          select(-outlier) |>
          filter(product_code %in% top_products_with_outliers) |>
          # e_charts(product_code) |>
          e_charts(item_description) |>
          e_boxplot(adj_consumption_in_packs,
                    itemStyle = list(
                      opacity = 0.8
                    )) |>
          e_flip_coords() |>
          e_y_axis(inverse = TRUE) |>
          e_tooltip(trigger = "axis",
                    formatter = htmlwidgets::JS("
            function (param) {
              param = param[0];
              return [
                'Product Name/Code: ' + param.name + '<hr size=1 style=\"margin: 3px 0\">',
                'max: ' + param.data[1] + '<br/>',
                'Q3: ' + param.data[2] + '<br/>',
                'median: ' + param.data[3] + '<br/>',
                'Q2: ' + param.data[4] + '<br/>',
                'min: ' + param.data[5] + '<br/>'
              ].join('');
            }
            ")
          ) |>
          e_color(echartsColors) |>
          e_title(text = "Products with Most Outliers",
                  subtext = "calculated with Tukey's fences (using IQR range)",
                  left = "center", top = 10) |>
          e_legend(left = "right", top = 40) |>
          e_toolbox_feature("dataZoom") |>
          e_toolbox_feature("reset") |>  # feature = "reset"
          e_toolbox_feature("dataView") |>
          e_toolbox_feature("saveAsImage") |>
          e_grid(left = "40%")


      })

      output$financialYearSummary <- renderDT({
        filteredData() |>
          mutate(quarter = yearquarter(date, fiscal_start = 7),
                 fiscal_year = fiscal_year(quarter),
                 financial_year = paste0(fiscal_year-1, "-", fiscal_year)) |>
          group_by(product_code, item_description, financial_year) |>
          summarise(total = sum(adj_consumption_in_packs),
                    median = median(adj_consumption_in_packs, na.rm = T)) |>
          arrange(financial_year) |>
          pivot_wider(id_cols = c(product_code, item_description),
                      names_from = financial_year,
                      values_from = c(total, median),
                      names_glue = "{.value} of {financial_year}") |>
          mutate_if(is.numeric, ~ replace_na(., 0)) |>
          datatable(
            extensions = list('Buttons' = NULL, 'FixedColumns' = NULL),
            class = 'cell-border hover compact',
            options = list(
              # ordering = FALSE,
              columnDefs = list(list(width = '350px', targets = 1)),
              scrollX = "8000px",
              scrollY = "400px",
              fixedColumns = list(leftColumns = 3),
              # pageLength = 50,
              paging = FALSE,
              dom = "Blfrtip",
              buttons =
                list("copy", list(
                  extend = "collection",
                  buttons = c("csv", "excel", "pdf"),
                  text = "Download"
                ) ) # end of buttons customization
            )
          ) |>
          formatStyle("product_code", backgroundColor = "#C2ECED",
                      fontWeight = "bold") |>
          formatStyle("item_description", backgroundColor = "#C2ECED",
                      fontWeight = "bold")
      })

    })


    return(
      list(
        filtered = filteredData,
        buq = buq_data()
      )
    )

  }
)


}



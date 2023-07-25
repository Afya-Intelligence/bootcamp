# Module for revised data extraction and filtering

# Author: Peter Boshe
# Version: 2023-06-28

# Packages
library(tidyverse)



# Parameters
source("R/utils.R")



data_path <- here::here("app-data/20230622_all_programs_final_joined.fst")
metadata_path <- here::here("app-data/metadata.fst")

# ============================================================================

# Code
importRevisedUI <- function(id) {
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
      grid_card(
        area = "filter_input",
        card_header("Data Import"),
        card_body(
          selectInput(
            inputId = ns("programInput"),
            label = "Select Program Category",
            choices = c("ALL", "ILS", "LAB", "TB")
          ),
          # actionButton(
          #   inputId = ns("importData"),
          #   label = "Import Data"
          # ),
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
            label = "Show Advanced Filters",
            icon = icon(name = "filter")
          ),
          shinyjs::hidden(
            div(
              id = ns("advancedCategories"),
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
            )
          ),
          tags$hr(),
          actionButton(
            inputId = ns("fetchData"),
            label = "Load Data",
            icon = icon(name = "download")
          ),
          tags$br(),
          textOutput(outputId = ns("facilityTimeline"))

        )
      )
      ,
      grid_nested(
        area = "summary_output",
        layout = c(
          "summaryTablelo"
        ),

        grid_card(
          area = "summaryTablelo",
          # card_body(
          tabsetPanel(
            tabPanel(
              title = "Report Summary",
              card_body(echarts4rOutput(outputId = ns("ordersPerReport"))),
              tags$hr(),
              card_body(DTOutput(outputId = ns("leastReportedProductsTable")), min_height = "1000px")
            ),
            tabPanel(
              title = "Outliers Summary",
              card_body(plotlyOutput(outputId = ns("topOutlierProductsChart"))),
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
          # ) # extra card body
        )

      )
    )
  )

}


importRevisedServer <- function(id, credentials) {
  moduleServer(
    id,
    function(input, output, session) {


# parameters --------------------------------------------------------------
observe({
  regionInput <- as.character(session$input$regionInput)
  districtInput <- as.character(session$input$districtInput)
  facilityType <- as.character(session$input$facilityType)
  facilityInput <- as.character(session$input$facilityInput)
  productGroup <- as.character(session$input$productGroup)
  productSubGroup <- as.character(session$input$productSubGroup)
})


      # add an event reactive to load in the metadata -------------------------------


      # metadata <- eventReactive(credentials()$user_auth, {
      #   credentials <- credentials()$user_auth  # Call the function to get the object
      # 
      #   req(credentials)
      #   # showNotification("your data is being imported.. please wait..")
      #   show_modal_spinner() # show the modal window
      # 
      # 
      #   # to filter depending on the choice of program made
      #     df <- read_fst(metadata_path)
      # 
      #     remove_modal_spinner()
      # 
      # 
      #    return(df)
      # 
      # 
      # })
      
      metadata <- reactive({

        # showNotification("your data is being imported.. please wait..")
        show_modal_spinner() # show the modal window
        
        
        # to filter depending on the choice of program made
        df <- read_fst(metadata_path)
        
        remove_modal_spinner()
        
        
        return(df)
        
        
      })


      # add functions to pull dropdown menus ------------------------------------


      observe({

        # filter the data according to the program selected

        if (session$input$programInput != "ALL") {

          filtered <- metadata() |>
          filter(program == session$input$programInput)

        } else {
          filtered <- metadata()
        }


        updateSelectInput(
          inputId = "regionInput",
          choices = unique(filtered$region)
        )
      })



      # function to bring out district values

      observeEvent(session$input$regionInput, {

        # filter based on the region selected

        if (session$input$programInput != "ALL") {

        filtered <- metadata() |>
          filter(
            program == session$input$programInput,
            region == session$input$regionInput
            )
        } else {
          filtered <- metadata() |>
            filter(
              region == session$input$regionInput
            )
        }

        # Update the choices in selectInput for district
        updateSelectInput(
          inputId = "districtInput",
          choices = unique(filtered$district)
        )
      })

      # to pull facility type choices
      observeEvent(session$input$districtInput, {

        # filter depending on the choice of district
        if (session$input$programInput != "ALL") {

          filtered <- metadata() |>
            filter(program == session$input$programInput,
                   region == session$input$regionInput,
                   district == session$input$districtInput
                   )
        } else {
          filtered <- metadata() |>
            filter(
                   region == session$input$regionInput,
                   district == session$input$districtInput
            )
        }

        # Update the choices in selectInput for facility type
        updateSelectInput(
          inputId = "facilityType",
          choices = unique(filtered$facility_type)
        )
      })

      observeEvent(session$input$facilityType, {

        if (session$input$programInput != "ALL") {


            filtered <- metadata() |>
              filter(program == session$input$programInput,
                     region == session$input$regionInput,
                     district == session$input$districtInput,
                     facility_type == session$input$facilityType)
        } else {
          filtered <- metadata() |>
            filter(
                   region == session$input$regionInput,
                   district == session$input$districtInput,
                   facility_type == session$input$facilityType)
        }



        # Update the choices in selectInput for facility type
        updateSelectInput(
          inputId = "facilityInput",
          choices = unique(filtered$facility_name)
        )

        updateSelectInput(
          inputId = "productGroup",
          choices = c("ALL", unique(filtered$product_group)),
          selected = "ALL"
        )
      })

      observeEvent(session$input$productGroup, {

        if (session$input$programInput != "ALL") {

          filtered <- metadata() |>
            filter(program == session$input$programInput,
                   region == session$input$regionInput,
                   district == session$input$districtInput,
                   facility_type == session$input$facilityType,
                   facility_name == session$input$facilityInput,
                   product_group == session$input$productGroup)
        } else {
          filtered <- metadata() |>
            filter(
                   region == session$input$regionInput,
                   district == session$input$districtInput,
                   facility_type == session$input$facilityType,
                   facility_name == session$input$facilityInput,
                   product_group == session$input$productGroup)
        }


        # Update the choices in selectInput for district
        updateSelectInput(
          inputId = "productSubGroup",
          choices = c("ALL", unique(filtered$sub_group))
        )
      })

      # to show/hide advanced features ------------------------------------------

      # Add an observeEvent to toggle the visibility of advancedCategories
      observeEvent(session$input$showAdvancedFilters, {
        shinyjs::toggle(id = "advancedCategories", anim = TRUE)
      })

      # Filtering the data for the rest of the app -----------------------------
      filteredData <- eventReactive(session$input$fetchData, {

        show_modal_spinner() # show the modal window 
        
        regionInput <<- as.character(session$input$regionInput)
        districtInput <<- as.character(session$input$districtInput)
        facilityType <<- as.character(session$input$facilityType)
        facilityInput <<- as.character(session$input$facilityInput)
        productGroup <<- as.character(session$input$productGroup)
        productSubGroup <<- as.character(session$input$productSubGroup)
        
        # filter for the facility code in the metadata
        
        facilityCodeChoice <<- metadata() |> 
          filter(
            region == regionInput,
            district == districtInput,
            facility_type == facilityType,
            facility_name == facilityInput
          ) |> 
          distinct(facility_code) |> 
          pull()
        
        
        
        data_parse <- parse_fst(data_path)  # Access the loaded data




        print(regionInput )
        print(districtInput )
        print(facilityType)
        print(facilityInput)
        print(productGroup)
        print(productSubGroup)
        print(facilityCodeChoice)


        # Create empty filters for group, sub-group, and ven
        # group_filter <- TRUE
        # sub_group_filter <- TRUE

        if (session$input$productGroup != "ALL" &&
            !is.null(session$input$productSubGroup) &&
            session$input$productSubGroup != "ALL") {
          filtered <- data_parse %>%
            filter_fst(
              facility_code == facilityCodeChoice,
              product_group == productGroup,
              sub_group == productSubGroup
            )
          filtered <- filtered |>
            group_by(product_code) |>
            mutate(outlier = !isnt_out_tukey(adj_consumption_in_packs)) |>
            ungroup()

        } else if (session$input$productGroup != "ALL" &&
                   # is.null(session$input$productSubGroup) &&
                   session$input$productSubGroup == "ALL") {
          filtered <- data_parse %>%
            filter_fst(
              facility_code == facilityCodeChoice,
              product_group == productGroup
            )
          filtered <- filtered |>
            group_by(product_code) |>
            mutate(outlier = !isnt_out_tukey(adj_consumption_in_packs)) |>
            ungroup()

        } else {
          filtered <- data_parse |> 
            filter_fst(facility_code == facilityCodeChoice)
          filtered <- filtered |>
            group_by(product_code) |>
            mutate(outlier = !isnt_out_tukey(adj_consumption_in_packs)) |>
            ungroup()
        }
        print("this is our filtered data")
        print(dim(filtered))

        #unlink the file
        # unlink(data_path)

        

          #finish the loader
          remove_modal_spinner()


        future_promise({filtered}, seed = TRUE)  # Return the future filtered data
          
      })


      # get the facility timeline output ----------------------------------------

      observe({

          

        output$facilityTimeline <- renderText({

          filteredData() %...>%
            summarise(min_date = min(date), max_date = max(date)) %...>%
            mutate(message = paste0("This dataset includes records from ", min_date, " to ", max_date)) %...>%
            pull(message)

        })




        output$ordersPerReport <- renderEcharts4r({

          echartsColors <- c("#0c7c8c", "#5ac2e8", "#DF8B0D", "#768948", "#548C2F")
          filteredData() %...>%
            group_by(month = lubridate::month(date), year = lubridate::year(date)) %...>%
            summarise(no_of_items = max(row_number())) %...>%
            arrange(year, month) %...>%
            mutate(month = lubridate::month(month, label = TRUE)) %...>%
            unite(col = "month_year", c(month, year), sep = "-") %...>%
            e_charts(month_year) %...>%
            e_line(serie = no_of_items) %...>%
            e_tooltip(trigger = "axis") %...>%
            e_color(echartsColors) %...>%
            e_title(text = "Number of Products Reported in each R&R",
                    subtext = "calculated by total line items each report",
                    left = "center", top = 10) %...>%
            e_legend(left = "right", top = 40) %...>%
            e_toolbox_feature("dataZoom") %...>%
            e_toolbox_feature("reset") %...>%  # feature = "reset"
            e_toolbox_feature("dataView") %...>%
            e_toolbox_feature("saveAsImage")
          
        })

        # show table for least reported products
        output$leastReportedProductsTable <- renderDT({
          # print("here is the data passing to the table")
          # print(filteredData() %...>% print())
          #
          # expected_reports <- filteredData() %...>%
          #   distinct(month = lubridate::month(date), year = lubridate::year(date)) %...>%
          #   nrow()
          # # print("expected reports are")
          # print(expected_reports)

          # Sys.sleep(2)  # Delay of 1 second

          filteredData() %...>%
            nest_by(month = lubridate::month(date), year = lubridate::year(date)) %...>%
            mutate(number = length(n)) %...>%
            ungroup() %...>%
            mutate(expected_reports = sum(number)) %...>%
            select(-number) %...>%
            unnest(cols = c(data)) %...>%
            # select(-outlier) |>
            group_by(product_code, item_description, expected_reports) %...>%
            summarise(reported = max(row_number())) %...>%
            dplyr::mutate(missing = expected_reports - reported, expected = expected_reports) %...>%
            arrange(desc(missing)) %...>%
            select(-expected_reports) %...>%
            custom_datatable(datatable_options = list(
              caption = "Product occurence in R&R reports",
              scrollY = "450px"))
          # DT::datatable(options = list(scrollX = TRUE, scrollY = "700px", pageLength = 50),
          #               caption = "Product occurance in R&R reports")
        })
        # show chart for most consumed products
        output$mostConsumedProducts <- renderEcharts4r({
          echartsColors <- c("#0c7c8c", "#5ac2e8", "#DF8B0D", "#768948", "#548C2F")
          filteredData() %...>%
            group_by(item_description) %...>%
            # group_by(product_code) |>
            summarise(consumption = sum(adj_consumption_in_packs)) %...>%
            slice_max(order_by = consumption, n = 10) %...>%
            e_charts(item_description) %...>%
            e_bar(consumption) %...>%
            e_flip_coords() %...>%
            e_y_axis(inverse = TRUE) %...>%
            e_tooltip(trigger = "axis") %...>%
            e_color(echartsColors) %...>%
            e_title(text = "Top Consumed Products",
                    subtext = "calculated by total adjusted consumption in packs",
                    left = "center", top = 10) %...>%
            e_legend(left = "right", top = 40) %...>%
            e_toolbox_feature("dataZoom") %...>%
            e_toolbox_feature("reset") %...>%  # feature = "reset"
            e_toolbox_feature("dataView") %...>%
            e_toolbox_feature("saveAsImage")
        })



        output$topOutlierProducts <- renderDT({
          outlier_df <- filteredData() %...>%
            group_by(product_code, item_description) %...>%
            mutate(outlier = !isnt_out_tukey(adj_consumption_in_packs))
          outlier_df %...>%
            summarise(no_of_outliers = sum(outlier)) %...>%
            arrange(desc(no_of_outliers)) %...>%
            filter(no_of_outliers > 0) %...>%
            custom_datatable(datatable_options = list(
              caption = "Products with Outliers in R&R reports",
              scrollY = "500px"))
          # datatable(options = list(scrollX = TRUE, scrollY = "700px", pageLength = 50),
          #           caption = "Products with Outliers in R&R reports")
        })

        output$topOutlierProductsChart <- renderPlotly({
        
          
          filteredData() %...>% 
            group_by(product_code) %...>%
            summarise(no_of_outliers = sum(outlier)) %...>%
            arrange(desc(no_of_outliers)) %...>%
            filter(no_of_outliers > 0) %...>% 
            head(10) %...>%
            pull(product_code) %...>% {
              top_outliers <- .
              filteredData() %...>% 
                filter(product_code %in% top_outliers) %...>% {
                  data <- .
                  plot_ly(data, x = ~adj_consumption_in_packs, y = ~product_code,
                          color = I("#0c7c8c"), marker = list(color = "#DB7472")) |>  
                    add_boxplot(hoverinfo = "x") |> 
                                # mode = "boxpoints") |>  
                    # add_trace(type = "scatter", 
                    #           hoverinfo = "x",
                    #           name = "outliers") |> 
                    layout(
                      title = list(text = "Products with the Most Outliers",
                                   x = 0.5,
                                   font = list(size = 17),
                                   pad = list(t = 10)),
                      yaxis = list(title = ""),
                      xaxis = list(title = "Adjusted Consumption in Packs"))
                }
            } 
          
          


        })

        output$financialYearSummary <- renderDT({
          filteredData() %...>%
            mutate(quarter = yearquarter(date, fiscal_start = 7),
                   fiscal_year = fiscal_year(quarter),
                   financial_year = paste0(fiscal_year-1, "-", fiscal_year)) %...>%
            group_by(product_code, item_description, financial_year) %...>%
            summarise(total = sum(adj_consumption_in_packs),
                      median = median(adj_consumption_in_packs, na.rm = T)) %...>%
            arrange(financial_year) %...>%
            pivot_wider(id_cols = c(product_code, item_description),
                        names_from = financial_year,
                        values_from = c(total, median),
                        names_glue = "{.value} of {financial_year}") %...>%
            mutate_if(is.numeric, ~ replace_na(., 0)) %...>%
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

      return(
        list(
          filtered = filteredData
        )
      )

    }
  )
}


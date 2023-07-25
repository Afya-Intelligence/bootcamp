library(shiny)
# library(shinyauthr)
library(tidyverse)
library(fpp3)
if (!requireNamespace("echarts4r", quietly = TRUE)) {
  install.packages("echarts4r", dependencies = c("Depends", "Imports"), type = "source", repos = "http://cran.r-project.org")
}
library(echarts4r)
library(urca)
library(gridlayout)
library(bslib)
library(shinyjs)
library(shinycssloaders)
library(shinybusy)
library(lubridate)
library(DT)
library(htmlwidgets)
library(DBI)
library(RSQLite)
library(fst)
library(tidyfst)
library(data.table)
library(plotly)
#libraries for asynchronous programming
library(future)
library(future.apply)
plan(multisession, workers = 6)
library(promises)
options(shiny.maxRequestSize = 1024^3)
source("modules/facility_data_import.R")
source("modules/facility_data_cleaning_and_exploration.R")
source("modules/facility_data_forecasting.R")
source("modules/facility_import_revised.R")
source("R/forecast_facility_fiscal_arima_script.R")
source("R/forecast_facility_arima_script.R")
# source("modules/authentication.R")

# parameters



# ============================================================================


# Define UI
ui <- fluidPage(

  shinyjs::useShinyjs(),
  # authorizationUI("1"),
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    `enable-gradients` = TRUE,
    `enable-shadows` = TRUE,
    primary = "#0c7c8c",
    success = "#DF8B0D",
    font_scale = 0.9
  ),
  # shinyauthr::loginUI(id = "login",
  #                     title = em(h4("eLMIS - Predictive Analytics Module")),
  #                     #7 days for cookie to expire
  #                     cookie_expiry = 7),
  # add logout button UI
  # div(
  # id = "show-page-content",
  # style = "display: none;",
  navbarPage(
    title = strong("eLMIS - Predictive Analytics Module"),
    tabPanel(
      title = "Data Extraction",
      importRevisedUI("1"),
      icon = icon(name = "download")
    ),

    tabPanel(
      title = "Data Cleaning",
      cleaningUI("1"),
      icon = icon(name = "broom")
    ),
    tabPanel(
      title = "Forecasting",
      forecastingUI("1"),
      icon = icon(name = "bullseye")
    ),
    # tabPanel(div(class = "pull-right", style = "margin-left: auto;", shinyauthr::logoutUI(id = "logout")))
  )

  # ) # div
)

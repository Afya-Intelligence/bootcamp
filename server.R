
# Parameters

user_base <- readRDS("user_base.rds")

# setting up cookie based authentication
#
# connect to, or setup and connect to local SQLite db
if (file.exists("session_info_db")) {
  db <- dbConnect(SQLite(), "session_info_db")
} else {
  db <- dbConnect(SQLite(), "session_info_db")
  dbCreateTable(db, "sessionids", c(user = "TEXT", sessionid = "TEXT", login_time = "TEXT"))
}


# a user who has not visited the app for this many days
# will be asked to login with user name and password again
# cookie_expiry <<- 7 # Days until session expires

# This function must accept two parameters: user and sessionid. It will be called whenever the user
# successfully logs in with a password.  This function saves to your database.

add_sessionid_to_db <- function(user, sessionid, conn = db) {
  tibble(user = user, sessionid = sessionid, login_time = as.character(now())) %>%
    dbWriteTable(conn, "sessionids", ., append = TRUE)
}

# This function must return a data.frame with columns user and sessionid  Other columns are also okay
# and will be made available to the app after log in as columns in credentials()$user_auth

get_sessionids_from_db <- function(conn = db) {
  dbReadTable(conn, "sessionids") %>%
    mutate(login_time = ymd_hms(login_time)) %>%
    as_tibble() %>%
    #set days until session expiry -> 7
    filter(login_time > now() - days(7))
}



# ============================================================================



# Define server function
server <- function(input, output, session) {
  # for debugging
  options(shiny.debug = TRUE)
  shinyjs::useShinyjs()

  # # call the logout module with reactive trigger to hide/show
  # logout_init <- shinyauthr::logoutServer(
  #   id = "logout",
  #   active = reactive(credentials()$user_auth)
  # )


  # # call login module supplying data frame,
  # # user and password cols and reactive trigger
  # credentials <- shinyauthr::loginServer(
  #   id = "login",
  #   data = user_base,
  #   user_col = user,
  #   pwd_col = password,
  #   cookie_logins = TRUE,
  #   sessionid_col = sessionid,
  #   cookie_getter = get_sessionids_from_db,
  #   cookie_setter = add_sessionid_to_db,
  #   log_out = reactive(logout_init())
  # )
  # 
  # shiny::observe({
  #   shiny::req(credentials()$user_auth)
  #   shinyjs::show("show-page-content")
  # })
  # 
  # shiny::observe({
  #   shiny::req(logout_init())
  #   shinyjs::hide("show-page-content")
  # })
 # using new import module
  # importedData <- importRevisedServer("1", credentials = credentials)
  importedData <- importRevisedServer("1")
  filteredData <- importedData$filtered
  buqData <- importedData$buq



  # Call the module to clean the data and pass the filteredData reactive expression
  processedData <- callModule(cleaningServer, "1", filteredData = filteredData)
  cleanedData <- processedData$cleaned

  # Call the module to forecast for facilities
  # Create a reactive values object
  forecastingServer("1", cleanedData = cleanedData, filteredData = filteredData, buqData = buqData)

}

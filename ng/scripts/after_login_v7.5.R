
# options(shiny.error = browser())

library(shiny)
library(shinyauthr)
library(shinyjs)
library(shinyhelper)
library(shinydashboard)
library(sodium)         # Hashing Passwords with 'sodium'
library(jsonlite)
library(rjson)          # Define json data format for data storage, querying
library(RJSONIO)
library(DT)
library(DBI)
library(RSQLite)
library(RMySQL)
library(tidyverse)
library(reshape2)


source("../R/update_status.R")
source("../R/CRUD_functions.R")
source("../R/dates_in_next_wk.R")
source("../R/tableShown.R")
source("../R/room_confirm_msg.R")
source("../R/create_candidate_table.R")
source("../R/time_lup.R")
source("../R/helper_fns.R")

# modules
source("../R/searchAvailRoom.R")
source("../R/updateMyRoomStatus.R")
source("../R/cancelBooking.R")
source("../R/help_tab.R")


# database source ----

## free server
# options(edwinyu = list(
#   "host" = "db4free.net",
#   "port" = 3306,
#   "user" = "edwinyu",
#   "password" = "Edwinyrl2019"
# ))
# drv <<- MySQL()
# database <- "room_avail"

# local
options(edwinyu = list(
  "host" = NULL,
  "port" = 0,
  "user" = NULL,
  "password" = NULL
))

drv <<- RSQLite::SQLite()
database <<- "../sql/room_avail.db"

# reset database
# file.copy(from = "schema_room_avail.db", to = database, overwrite = TRUE)


body <- dashboardBody(
  shinyjs::useShinyjs(),
  tags$head(tags$style(".table{margin: 0 auto;}"),
            tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                        type = "text/javascript"),
            includeScript("returnClick.js")
  ),
  shinyauthr::loginUI("login"),
  tabItems(
    tabItem(tabName = "Room_status_update", updateMyRoomStatusUI("room_status")),
    tabItem(tabName = "Room_booking", searchAvailRoomUI("room_booking")),
    tabItem(tabName = "Room_booked", cancelBookingUI("cancel")),
    tabItem(tabName = "Help", helpUI("help"))
  ),
  
  HTML('<div data-iframe-height></div>')
)

# -------------------------------------------------------------------------

sidebar <- dashboardSidebar(
  collapsed = TRUE,
  sidebarMenu(width = 70,
              menuItem(text = 'Room Status Update',
                       tabName = 'Room_status_update',
                       icon = icon('table')),
              menuItem(text = 'Room booking',
                       tabName = 'Room_booking',
                       icon = icon('table')), 
              menuItem(text = "My Booked Room",
                       tabName = 'Room_booked',
                       icon = icon('table')),
              menuItem(text = "Help",
                       tabName = 'Help',
                       icon = icon('info'))
  )
)

# -------------------------------------------------------------------------

header <-  dashboardHeader(
  title = "Welcome to Polished Fribble (Beta)",
  titleWidth = 450,
  tags$li(class = "dropdown",
          style = "padding: 8px;",
          shinyauthr::logoutUI("logout")),
  tags$li(class = "dropdown",
          tags$a(icon("github"), 
                 href = "https://github.com/paulc91/shinyauthr",
                 title = "See the code on GitHub")))

# -------------------------------------------------------------------------

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "purple"
)

# -------------------------------------------------------------------------

server <- shinyServer(function(input, output, session) {
  
  observe_helpers(help_dir = "helpfiles", withMathJax = FALSE)
  
  indiv_table <- loadData(database, 'individual_information')
  
  logout_init <- callModule(module = shinyauthr::logout, 
                            id = "logout", 
                            active = reactive(credentials()$user_auth))
  login_info <-
    data.frame(
      ID = indiv_table$UserName,
      Password = indiv_table$Password,
      Password_Hash = sapply(indiv_table$Password,
                             sodium::password_store),
      Permissions = indiv_table$Permissions)
  
  credentials <- callModule(shinyauthr::login,
                            id = "login", 
                            data = login_info,
                            user_col = ID,
                            pwd_col = Password,
                            sodium_hashed = FALSE,
                            log_out = reactive(logout_init()))
  
  observe({
    if (credentials()$user_auth) {
      shinyjs::removeClass(selector = "body",
                           class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body",
                        class = "sidebar-collapse")
    }
  })
  
  user_data <- reactive({credentials()$info})
  time <- Sys.time()
  next_wk <- dates_in_next_wk()
  
  output$personal <-
    DT::renderDataTable({
      req(credentials()$user_auth)
      room_table <- loadData(database, 'new_room_status')
      
      my_room_no <-
        indiv_table$RoomNumber[indiv_table$UserName == user_data()$ID]
      room_table[room_table$Room_no == my_room_no & 
                   !is_past(room_table$Date), ]},
      options = list(scrollX = TRUE))
  
  output$cancel <-
    DT::renderDataTable({
      booked_table <- loadData(database, "room_booked")
      booked_table$day <- date_to_weekday(booked_table$date)
      booked_table[booked_table$booker == user_data()$ID &
                     !is_past(booked_table$date), ]
    })
  
  updateMyRoomStatusServer("room_status", credentials, user_data)
  searchAvailRoomServer("room_booking", credentials, user_data)
  cancelBookingServer("cancel", credentials, user_data)
  helpServer("help", credentials)
})

# run application 
shinyApp(ui = ui, server = server)
# runApp("after_login_v7.5.R", display.mode = "showcase")


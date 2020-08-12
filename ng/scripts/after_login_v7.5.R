
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
    tabItem(tabName = "Room_status_update", uiOutput("room_status")),
    tabItem(tabName = "Room_booking", searchAvailRoomUI("room_booking")),
    tabItem(tabName = "Room_booked", uiOutput("room_booked"))
  ),
  
  # actionButton("change_schedule", "Click Me to Adjust Schedule")
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
                       icon = icon('table'))
  )
)

# -------------------------------------------------------------------------

header <-  dashboardHeader(
  title = "Welcome to Polished Fribble",
  titleWidth = 450,
  tags$li(class = "dropdown",
          style = "padding: 8px;",
          shinyauthr::logoutUI("logout")),
  tags$li(class = "dropdown",
          tags$a(icon("github"), 
                 href = "https://github.com/paulc91/shinyauthr",
                 title = "See the code on github")))

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
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- callModule(module = shinyauthr::logout, 
                            id = "logout", 
                            active = reactive(credentials()$user_auth))
  ##TODO: can we replace above?
  # logout_init <- moduleServer(id = "logout",
  #                             module = function(input, output, session) {
  #                               active <- reactive(credentials()$user_auth)
  #                               shinyauthr::logout(input, output, session, active)
  #                             })
  
  # call login module supplying data frame, user and password cols
  # and reactive trigger
  login_info <-
    data.frame(
      ID = indiv_table$UserName,
      Password = indiv_table$Password,
      Password_Hash = sapply(indiv_table$Password, sodium::password_store),
      Permissions = indiv_table$Permissions)
  
  credentials <- callModule(shinyauthr::login,
                            id = "login", 
                            data = login_info,
                            user_col = ID,
                            pwd_col = Password,
                            sodium_hashed = FALSE,
                            log_out = reactive(logout_init()))
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  user_info <- reactive({credentials()$info})
  
  ##TODO: why is this duplicated?...
  # pull out user information from login module
  user_data <- reactive({credentials()$info})
  
  time <- Sys.time()
  
  next_wk <- dates_in_next_wk() # previously 'a'
  
  # diagram displaying current status of personal room information of user  
  output$personal <- DT::renderDataTable({
    req(credentials()$user_auth)
    room_table <- loadData(database, 'new_room_status')
    
    my_room_no <- indiv_table$RoomNumber[indiv_table$UserName == user_data()$ID]  ##TODO: remove duplication of this line
    room_table[room_table$Room_no == my_room_no & 
                 !is_past(room_table$Date), ]},
    options = list(scrollX = TRUE)
  )
  
  # diagram displaying all existing booking of user
  output$cancel <- DT::renderDataTable({
    booked_table <- loadData(database, "room_booked")
    booked_table$day <- date_to_weekday(booked_table$date)
    booked_table[booked_table$booker == user_data()$ID &
                   !is_past(booked_table$date), ]
  })
  
  ## 1. save Updated Room information   
  
  observeEvent(input$save_room, {   
    
    date_update <- input$date_update
    
    lup_time <- c(am = "am", pm = "pm", ampm = "both", "neither")
    
    available_time1 <- paste(input$time1, collapse = "")
    am <- unname(lup_time[available_time1])
    
    room_table <- loadData(database, 'new_room_status')
    
    my_room_no <- indiv_table$RoomNumber[indiv_table$UserName == user_data()$ID]
    
    # If a room has been booked by some else (after user first updating their information)
    # The user cannot change the room status again,
    # and a notification is shown for asking him to contact admin
    
    # examine if the row user wants to update exists in the table
    # if exists, checks if any time has been booked and then decide to update
    # if doesn't exist, row is updated directly without checking
    
    rows_existing <- room_table$Room_no == my_room_no & room_table$Date == date_update
    is_existing_record <-  any(rows_existing)
    is_already_booked <- any(as.matrix(room_table[rows_existing, ] == "Booked"))
    
    ##TODO: simplify these ifs...duplication
    
    if (!is_existing_record) {
      
      update_status(use = "write",
                    room_no = my_room_no,
                    date = date_update,
                    am = am,
                    database = database,
                    table = 'new_room_status')
    } else {
      if (is_already_booked) {
        
        showNotification("Someone has booked your room already. Please contact admin.",
                         type = "error",
                         duration = 30,
                         closeButton = TRUE)
      } else {
        update_status(use = "write",
                      room_no = my_room_no,
                      date = date_update,
                      am = am,
                      database = database,
                      table = 'new_room_status')}
    }
    
    output$personal <- DT::renderDataTable(
      {
        req(credentials()$user_auth)
        room_table = loadData(database, 'new_room_status')
        room_table = room_table[room_table$Room_no == my_room_no &
                                  !is_past(room_table$Date), ]
        room_table},
      options = list(scrollX = TRUE)
    )
  })
    
  ## 4. cancel the room booking
  observeEvent(input$cancel, {
    
    booked_table <- loadData(database, "room_booked")
    room_table <- loadData(database, 'new_room_status')
    
    date_update <- input$date_update
    
    if (input$booking_no %in% booked_table$booking_no) {
      
      delete_info <- booked_table[booked_table$booking_no == input$booking_no, ]
      
      avail <- room_table %>% 
        filter(Date == delete_info$date,
               Room_no == delete_info$room_no)
      
      avail[1, delete_info$time] <- "Available"
      
      my_room_no <- indiv_table$RoomNumber[indiv_table$UserName == user_data()$ID]
      
      delete_booking(input$booking_no,
                     database = database,
                     table = "room_booked")
      
      update_status(use = "booking",
                    room_no = my_room_no,
                    date = date_update,
                    avail = avail,
                    database = database,
                    table = "new_room_status" )
      
      output$cancel <- DT::renderDataTable({
        req(credentials()$user_auth)
        booked_table <- loadData(database, "room_booked")
        booked_table$day <- date_to_weekday(booked_table$date)
        booked_table[booked_table$booker == user_data()$ID, ]
      })
      
      current <- weekdays(as.POSIXct(Sys.Date()), abbreviate = FALSE)
      
      cancel_confirm_msg <- sprintf("You have successfully canceled the booked room %s \n (%s,%s,",
                                    paste(input$time4, collapse = ' and '), ")",
                                    input$room_book_no2, current, input$day4)
      
      showNotification(cancel_confirm_msg,
                       type = "message",
                       duration = 30,
                       closeButton = TRUE)
    } else {
      showNotification(paste("Your booking reference number does not exist, recheck it please"),
                       type = "message",
                       duration = 30,
                       closeButton = TRUE)
    }
  })
  
  
  # -------------------------------------------------------------------------
  
  output$room_status <- renderUI({
    req(credentials()$user_auth)
    fluidPage(box(width = 3,
                  h4("Tell others when your room will be available for booking now"),
                  wellPanel(
                    dateInput('date_update',
                              label = 'Date',
                              value = Sys.Date()) %>%
                      helper(colour = "mediumpurple1",
                             type = "inline",
                             size = "m",
                             title = "Guidance:",
                             content = c(
                               "- Input the date on which your rooms will be available for booking",
                               "- Then specify the exact period of the day: am, pm, both of them.",
                               "- After saving your chosen time slots, it would be shown on the righthand side.",
                               "- You may always modify your available time slots, so long as it isn't booked by someone else",
                               "- If you input nothing in this table, your room would not appear in any search result (unavailable by default)"))),
                  
                  checkboxGroupInput("time1",
                                     "Time",
                                     choices = c("am","pm"),
                                     selected = ""),
                  actionButton("save_room",
                               "Save",
                               width = "25%")),
              box(width = 9,
                  dataTableOutput(outputId = 'personal')) %>%
                helper(
                  colour = "mediumpurple1",
                  type = "inline",
                  size = "m",
                  content = c("Explaination about cells: ",
                              "  ",
                              "Available  : Your room can be booked for this time slot",
                              "Unavailable: Your room cannot be booked for this time slot",
                              "Booked     : Others have booked your room for this time slot, you can no longer make changes on this day",
                              " ",
                              "To check for the availability of others' rooms and make bookings, please go to Room Booking")
                ),
              tags$head(
                tags$style(
                  HTML(".shiny-notification {position:fixed;top: calc(50%);left: calc(50%);}")
                )
              ))
  })
  
  searchAvailRoomServer("room_booking", credentials, user_data)
  
  output$room_booked <- renderUI({
    req(credentials()$user_auth)
    fluidPage(box(width = 3,
                  h4("Cancel the booked room"),
                  textInput("booking_no","Input booking number here:", value = "") %>%
                    helper(
                      colour = "mediumpurple1",
                      type = "inline",
                      size = "m",
                      title = 'Guidance:',
                      content = c("Inseroom_table the booking number to the room that you no longer need")),
                  actionButton("cancel",
                               "Cancel Booking",
                               width = "50%"),
                  tags$head(
                    tags$style(
                      HTML(".shiny-notification {position:fixed;top: calc(50%);left: calc(50%);}"
                      )
                    )
                  )
    ),
    box(width = 9,
        dataTableOutput(outputId = 'cancel'))
    )
  })
})

# Run the application 
shinyApp(ui = ui, server = server)
# runApp("after_login_v7.5.R", display.mode = "showcase")


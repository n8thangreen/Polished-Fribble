
library(shiny)
library(shinyauthr)
library(shinyjs)
library(tidyverse)
library(sodium)         # Hashing Passwords with 'sodium'
library(shinydashboard)
library(jsonlite)
library(rjson)          # Define json data format for data storage, querying
library(RSQLite)
library(RJSONIO)
library(DBI)
library(RMySQL)
library(DT)
library(shinyhelper)


source("update_status.R")
source("CRUD_functions.R")
source("dates_in_next_wk.R")


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
database <<- "room_avail.db"


is_past <- function(date) as.Date(date) < Sys.Date()

abbr <- function(x){substr(x, 1, 3)}

date_to_weekday <<- function(date) {format(as.Date(date), format = "%a")}


body <- dashboardBody(
  shinyjs::useShinyjs(),
  tags$head(tags$style(".table{margin: 0 auto;}"),
            tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                        type="text/javascript"),
            includeScript("returnClick.js")
  ),
  shinyauthr::loginUI("login"),
  tabItems(
    tabItem(tabName = "Room_status_update", uiOutput("room_status")),
    tabItem(tabName = "Room_booking", uiOutput("room_booking")),
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
  title = "Welcome to Polished Fribble!",
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
  
  #Currently read from the database I created remotely(based on a free server)
  #It's safer and maybe run faster if changing for a better server
  #Note that the host,username and password below is for the server
  #NOT FOR THE INDIVIDUAL USERS OF THE LOGIN SYSTEM
  
  observe_helpers(help_dir = "helpfiles", withMathJax = FALSE)
  
  individual <- loadData(database, 'individual_information')
  individual_rb <- loadData(database, 'new_room_status')
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- callModule(shinyauthr::logout, 
                            id = "logout", 
                            active = reactive(credentials()$user_auth))
  
  # call login module supplying data frame, user and password cols
  # and reactive trigger
  login_info <-
    data.frame(
      ID = individual$UserName,
      Password = individual$Password,
      Password_Hash = sapply(individual$Password, sodium::password_store),
      Permissions = individual$Permissions)
  
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
  
  # pull out user information from login module
  user_data <- reactive({credentials()$info})
  time <- Sys.time()
  
  next_wk <- dates_in_next_wk() #previously a
  
  
  # diagram displaying current status of personal room information of user  
  output$personal <- DT::renderDataTable({
    req(credentials()$user_auth)
    updated_room <- loadData(database, 'new_room_status')
    
    my_room_no <- individual$RoomNumber[individual$UserName == user_data()$ID]  ##TODO: remove duplication of this line
    updated_room[updated_room$Room_no == my_room_no & 
                   !is_past(updated_room$Date), ]},
    options = list(scrollX = TRUE)
  )
  
  # diagram displaying all existing booking of user
  output$cancel <- DT::renderDataTable({
    room_booked <- loadData(database, "room_booked")
    room_booked$day <- date_to_weekday(room_booked$date)
    room_booked[room_booked$booker == user_data()$ID &
                  !is_past(room_booked$date), ]
  })
  
  ## 1. Save Updated Room information   
  
  observeEvent(input$save_room, {   
    
    date_update <- input$date1
    
    lup_time <- c(am = "am", pm = "pm", ampm = "both", "neither")
    
    available_time1 <- paste(input$time1, collapse = "")
    am <- unname(lup_time[available_time1])
    
    rt <- loadData(database, 'new_room_status')
    
    my_room_no <- individual$RoomNumber[individual$UserName == user_data()$ID]
    
    #If a room has been booked by some else(after user first updating their information)
    #The user cannot change the room status again, and a notification is shown for asking him to contact admin
    
    #first examine if the row that user wants to update has existed in the table
    #if it exists, it first checks if any time slot has been booked and then decide whether to update
    #if it doesn't exist, the row is updated directly without checking
    
    rows_existing <- rt$Room_no == my_room_no & rt$Date == date_update
    is_existing_record <-  any(rows_existing)
    
    ##TODO: simplify these ifs...duplication
    
    if (!is_existing_record) {
      
      update_status(use = "write",
                    room_no = my_room_no,
                    date = date_update,
                    am = am,
                    database = database,
                    table = 'new_room_status')
    } else {
      if (any(as.matrix(rt[rows_existing, ] == "Booked"))) {
        
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
        updated_room = loadData(database, 'new_room_status')
        updated_room = updated_room[updated_room$Room_no == my_room_no &
                                      !is_past(updated_room$Date), ]
        updated_room},
      options = list(scrollX = TRUE)
    )
  })
  
  ## 2. search for available room information
  
  observeEvent(input$search, {
    
    req(input$date2)
    room_table <- loadData(database, 'new_room_status')
    n_rooms <- nrow(room_table)
    
    avail_am <- NULL
    avail_pm <- NULL
    avail_both <- NULL
    avail_neither <- rep(TRUE, n_rooms)
    
    am_times <- c('9am_10am','10am_11am','11am_12pm') ##TODO: remove duplication
    pm_times <- c('12pm_1pm','1pm_2pm','2pm_3pm', '3pm_4pm','4pm_5pm')
    
    for (i in seq_len(n_rooms)) {
      avail_am[i]   <- any(room_table[i, am_times] == "Available") 
      avail_pm[i]   <- any(room_table[i, pm_times] == "Available")
      avail_both[i] <- all(avail_am[i], avail_pm[i]) 
    }
    
    lup_time <- list(am = avail_am,
                     pm = avail_pm,
                     ampm = avail_both,
                     avail_neither)
    
    search_ampm <- paste(input$cb_ampm, collapse = "")
    avail <- unname(lup_time[search_ampm]) %>% unlist()
    
    my_room_no <- individual$RoomNumber[individual$UserName == user_data()$ID]
    
    selected_rt <- room_table[avail & room_table$Date %in% as.character(input$date2), ]
    table_shown <- selected_rt[selected_rt$Room_no != my_room_no, ]
    
    output$all <- DT::renderDataTable({
      req(credentials()$user_auth)
      table_shown},
      selection = list(mode = "multiple",
                       target = "cell"),
      options = list(scrollX = TRUE))
    
    output$cand_bookings <- renderPrint({
      req(input$all_cells_selected) 
      
      if (nrow(input$all_cells_selected) == 0) {
        cat('Please select the rooms')
      } else { 
        row_id <- input$all_cells_selected[, 1]
        col_id <- input$all_cells_selected[, 2]
        booking_candidate <- data.frame(date = table_shown[row_id, "Date"],
                                        day = table_shown[row_id, "Weekday"],
                                        room_no = table_shown[row_id, "Room_no"],
                                        time = colnames(table_shown)[col_id])
        booking_candidate
      }
    })
  })
  
  ## 3. book the room
  observeEvent(input$book, {
    rt <- loadData(database, 'new_room_status')
    room_table <- loadData(database, 'new_room_status')
    n_rooms <- nrow(room_table)
    
    am_times <- c('9am_10am','10am_11am','11am_12pm')
    pm_times <- c('12pm_1pm','1pm_2pm','2pm_3pm', '3pm_4pm','4pm_5pm')
    
    search_time <- paste(input$cb_ampm, collapse = "")
    
    # which days have morning or afternoon available?
    
    avail <- 
      room_table %>%
      as_tibble() %>%
      melt(id.vars = c("Date", "Weekday", "Room_no"),
           variable.name = "time") %>% 
      mutate(am = ifelse(search_time == "ampm",
                         "ampm",
                         ifelse(time %in% am_times, "am", "pm"))) %>% 
      group_by(Date, Room_no, am) %>% 
      summarise(free = any(value == "Available")) %>% 
      filter(am == search_time) %>%
      ungroup() %>% 
      select(free)
    
##TODO: test this...
    
    my_room_no <- individual$RoomNumber[individual$UserName == user_data()$ID]
    
    selected_rt <- room_table[avail & room_table$Date %in% as.character(input$date2), ]
    table_shown <- selected_rt[selected_rt$Room_no != my_room_no, ]
    
    index  <- input$all_cells_selected
    row_id <- input$all_cells_selected[, 1]
    col_id <- input$all_cells_selected[, 2]
    
    booking_candidate <-
      tibble(
        date = table_shown[row_id, "Date"],
        day  = table_shown[row_id, "Weekday"],
        room_no = table_shown[row_id, "Room_no"],
        time = colnames(table_shown)[col_id])
    
    unique_row_no <- as.vector(unique(row_id))
    booking_cand <- list()
    
    for(j in seq_along(unique_row_no)){
      row_idx <- unique_row_no[j]
      
      booking_cand[[j]] <-
        list(Date = table_shown[row_idx, "Date"],
             Weekday = table_shown[row_idx, "Weekday"],
             Room_no = table_shown[row_idx, "Room_no"],
             time_slots = list(index[row_id == row_idx, 2]))
    }
    
    time_slots <- map_dfc(booking_cand, "time_slots")
    
    ## all times available for each date?
    interval_avail <- NULL
    
    for (i in seq_along(unique_row_no)) {
      
      t <- time_slots[[i]]
      
      dat <- apply(as.matrix(rt[, t] == "Available"),
                   MARGIN = 1,
                   FUN = all)
      
      interval_avail <- rbind(interval_avail, dat)
    }
    
    # booking supports only booking one room at a time
    # User chooses day and (multiple) time slots to make the booking
    # If no single room satisfying all requirements
    # a notification is shown to let users select fewer time slots at a time
    
    candidate <- NULL
    
    for (i in seq_along(unique_row_no)) {
      candidate <- rbind(candidate,
                         rt[interval_avail[i, ] &
                              rt$Date == as.character(booking_cand[[i]]$Date) &
                              rt$Room_no != my_room_no &
                              rt$Room_no == booking_cand[[i]]$Room_no, ])
    }
    
    if (nrow(candidate) == 0) {
      if (length(time_slots) > 1) {
        showNotification("No room is available for all time slots of your choice. Try selecting fewer time slots at a time.",
                         type = "warning",
                         closeButton = TRUE,
                         duration = 30)
        
      }else if (length(time_slots) == 1) {
        
        showNotification("No room is available for this time slot so far.",
                         type = "warning",
                         closeButton = TRUE,
                         duration = 30)}
    } else {
      room_no <- candidate$Room_no
      
      room_status <- candidate[, c("9am_10am", "10am_11am", "11am_12pm", "12pm_1pm", "1pm_2pm", "2pm_3pm", "3pm_4pm", "4pm_5pm")]
      
      # change status to booked
      for (i in 1:nrow(room_status)) {
        room_status[i, ][time_slots[[i]] - 3] <- "Booked"
      }
      
      # change from Available -> Booked
      update_status(use = "booking",
                    room_no = room_no,
                    date = as.character(booking_cand[[1]]$Date), 
                    avail = room_status,
                    database = database,
                    table = 'new_room_status')
      
      
      update_booking(room_no = room_no,
                     booker = user_data()$ID,
                     date = as.character(booking_cand[[1]]$Date),
                     time = time_slots,
                     booking_no = NA,
                     database = database,
                     table = "room_booked")
      
      room_confirm <- NULL
      
      for (i in seq_along(unique_row_no)) {
        room_confirm <- paste(room_confirm,
                              'room', room_no[i], "\n",
                              "(", booking_cand[[i]]$Date, ",",
                              date_to_weekday(as.character(booking_cand[[i]]$Date)), ",",
                              paste(colnames(rt)[time_slots[[i]]], collapse = ' and '),
                              ")", sep = " ")
      }
      
      room_confirm <- paste("You have successfully booked", room_confirm)
      showNotification(room_confirm,
                       type = "message",
                       duration = 30,
                       closeButton = TRUE)
      
      # update the booking information displayed
      output$cancel <- DT::renderDataTable({
        req(credentials()$user_auth)
        room_booked <- loadData(database, "room_booked")
        room_booked$day <- date_to_weekday(room_booked$date)
        room_booked[room_booked$booker == user_data()$ID & !is_past(room_booked$date), ]
      })
    }
  })
  
  ## 4.cancel the room booking
  observeEvent(input$cancel, {
    
    room_booked <- loadData(database, "room_booked")
    rt <- loadData(database, 'new_room_status')
    
    date_update <- input$date1
    
    if (input$booking_no %in% room_booked$booking_no) {
      
      delete_Info <- room_booked[room_booked$booking_no == input$booking_no, ]
      
      avail <- rt %>% 
        filter(Date == delete_Info$date,
               Room_no == delete_Info$room_no)
      
      avail[1, delete_Info$time] <- "Available"
      
      my_room_no <- individual$RoomNumber[individual$UserName == user_data()$ID]
      
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
        room_booked <- loadData(database, "room_booked")
        room_booked$day <- date_to_weekday(room_booked$date)
        room_booked[room_booked$booker == user_data()$ID, ]
      })
      
      current <- weekdays(as.POSIXct(Sys.Date()), abbreviate = FALSE)
      
      cancel_confirm <- sprintf("You have successfully canceled the booked room %s \n (%s,%s,",
                                paste(input$time4, collapse = ' and '), ")",
                                input$room_book_no2, current, input$day4)
      
      showNotification(cancel_confirm,
                       type = "message",
                       duration = 30,
                       closeButton = TRUE)
    }else{
      booking_no_error <- paste("Your booking reference number does not exist, recheck it please!")
      showNotification(booking_no_error,type = "message",
                       duration = 30,
                       closeButton = TRUE)
    }
  })
  
  output$room_status <- renderUI({
    req(credentials()$user_auth)
    fluidPage(box(width=3,
                  h4("Tell others when your room will be available for booking now!"),
                  wellPanel(
                    dateInput('date1',
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
                  
                  checkboxGroupInput("time1","Time",
                                     choices = c("am","pm"),
                                     selected = ""),
                  actionButton("save_room", "Save", width = "25%")),
              box(width = 9,
                  dataTableOutput(outputId = 'personal')) %>%
                helper(
                  colour = "mediumpurple1",
                  type = "inline",
                  size = "m",
                  content = c("Explaination about cells: ","  ",
                              "Available  : Your room can be booked for this time slot",
                              "Unavailable: Your room cannot be booked for this time slot",
                              "Booked     : Others have booked your room for this time slot, you can no longer make changes on this day",
                              " ",
                              "To check for the availability of others' rooms and make bookings, please go to Room Booking:) ")
                ),
              tags$head(
                tags$style(
                  HTML(".shiny-notification {position:fixed;top: calc(50%);left: calc(50%);}")
                )
              ))
  })
  
  output$room_booking <- renderUI({
    
    individual <- loadData(database, 'individual_information')
    
    req(credentials()$user_auth)
    fluidRow(box(width=3,
                 h4("Find out which room can be booked : "),
                 wellPanel(
                   dateInput('date2',
                             label = 'Date',
                             value = Sys.Date()) %>%
                     helper(colour = "mediumpurple1",
                            type = "inline",
                            size = "m",
                            title = 'Guidance:',
                            content = c("- You can search for others' room availability here, and make your booking in the next section.",
                                        "- Please select the date and the approximate time you wish to use other's office, and then press 'Search'.",
                                        "- If any room satisfys your requirements, it will be shown on the right.")
                     )),
                 checkboxGroupInput("cb_ampm", "Time",
                                    choices = c("am","pm"),
                                    selected = ""),
                 actionButton("search", "Search", width = "25%"),
                 h4("Confirm rooms of your choice:"),
                 verbatimTextOutput('cand_bookings'),
                 verbatimTextOutput('test'),
                 actionButton("book", "Book", width = "25%") %>%
                   helper(
                     colour = "mediumpurple1",
                     type = "inline",
                     size = "m",
                     title = '"How to select the time slot?',
                     content = c(
                       "- Please click on the cells within the data table on the right.",
                       "- Corresponding room information would appear in the box below.",
                       "- You may then confirm your booking, but notice that 'Booked' or 'Unavailable' rooms cannot be booked"
                     )
                   ),
                 tags$head(
                   tags$style(
                     HTML(".shiny-notification {position:fixed;top: calc(50%);left: calc(50%);}"
                     )
                   )
                 )
    ),
    box(width = 9,
        dataTableOutput(outputId = 'all'))
    )
  })
  
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
                      content = c("Insert the booking number to the room that you no longer need")),
                  actionButton("cancel", "Cancel Booking", width = "50%"),
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


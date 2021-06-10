
#
updateMyRoomStatusUI <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("room_status"))
}


# If a room has been booked by some else (after user first updating their information)
# The user cannot change the room status again,
# and a notification is shown for asking him to contact admin
#
# examine if the row user wants to update exists in the table
# if exists, checks if any time has been booked and then decide to update
# if doesn't exist, row is updated directly without checking
#
updateMyRoomStatusServer <- function(id, credentials) {
  
  counter_save <<- 0
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$room_status <- renderUI({
        
        ns <- session$ns
        
        fluidPage(
          box(width = 3,
              h4("Record when you are in the department.
                 Table shows available desks in your room. (Refresh if no table shown.)"),
              wellPanel(
                dateInput(ns('date_update'),
                          label = 'Date',
                          value = Sys.Date()) %>%
                  helper(colour = "mediumpurple1",
                         type = "inline",
                         size = "m",
                         title = "Guidance:",
                         content = c(
                           "- Input the date on which you will be in the department",
                           "- Then specify the exact period of the day: am, pm, both of them.",
                           "- After saving your chosen time slots,
                           it would be shown on the righthand side.",
                           "- You may always modify your In time slots,
                           so long as it isn't booked by someone else",
                           "- If you input nothing in this table,
                           your room will not appear in any search result
                           (desk not available by default)"))),
              
              checkboxGroupInput(ns("time"),
                                 "Time",
                                 choices = c("am","pm"),
                                 selected = ""),
              actionButton(ns("save_room"),
                           "Save",
                           width = "25%"),
              actionButton(ns("refresh"),
                           "Refresh",
                           width = "25%")),
          box(width = 9,
              DT::dataTableOutput(outputId = ns('personal_table'))) %>%
            helper(
              colour = "mediumpurple1",
              type = "inline",
              size = "m",
              content = c("Explanation about cells:",
                          "",
                          "Out :", "Your room can be booked for this time slot.",
                          "",
                          "In  :", "Your room cannot be booked for this time slot.",
                          "",
                          "Booked      :", "Others have booked your room for this time slot.",
                          "You can no longer make changes on this day.",
                          "",
                          "To check for the availability of others' rooms and make bookings",
                          "please go to Room Booking.")),
          tags$head(
            tags$style(
              HTML(".shiny-notification {position:fixed;top: calc(50%);left: calc(50%);}")
            )
          ))
      })
      
      return(
        observeEvent(input$save_room | input$refresh, {
          
          notif_duration <- 10
          
          if (length(input$save_room > 0)) {
            
            user_data <- credentials$info
            date_update <- input$date_update
            avail_time <- paste(input$time, collapse = "")
            
            ampm <- 
              switch(avail_time,
                     am = "am",
                     pm = "pm",
                     ampm = "both",
                     "neither")
            
            room_table <- loadData(database, 'new_room_status')
            indiv_table <- loadData(database, 'individual_information')
            booked_table <- loadData(database, "room_booked")
            
            my_room_no <-
              indiv_table$RoomNumber[indiv_table$UserName == user_data[['ID']]]
            
            rows_existing <-
              (room_table$Room_no == my_room_no) &
              (room_table$Date == date_update)
            
            is_existing_record <- any(rows_existing)
            
            rooms_existing <- room_table[rows_existing, , drop = FALSE]
            
            if (nrow(rooms_existing) == 0) {
              is_room_booked <- FALSE
            } else {
              # is_room_booked <- rooms_existing == "Booked"
              is_room_booked <-
                (rooms_existing == 0 && my_room_no %in% booked_table$room_no)
            }
            
            is_already_booked <- any(as.matrix(is_room_booked))
            
            if (is_existing_record && is_already_booked) {
              
              showNotification("Your room has reached capacity.
                               Please contact admin.",
                               type = "error",
                               duration = 30,
                               closeButton = TRUE)
            } else {
              update_status(use = "write",
                            room_no = my_room_no,
                            date = date_update,
                            am = ampm,
                            database = database,
                            table = 'new_room_status')
            }
            
            personal_table <-
              room_table[room_table$Room_no == my_room_no &
                           !is_past(room_table$Date), ] %>% 
              arrange(desc(Date))
            
            output$personal_table <-
              DT::renderDataTable({
                personal_table},
                options = list(scrollX = TRUE,
                               # pageLength = 10
                               paging = FALSE))
            
            if (input$save_room == counter_save + 1) {
              showNotification(my_room_message(date_update, ampm),
                               type = "message",
                               closeButton = TRUE,
                               duration = notif_duration)
            }
            
            counter_save <<- input$save_room
          }
        })
      )
    }
  )
}


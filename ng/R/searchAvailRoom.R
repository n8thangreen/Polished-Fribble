
#
searchAvailRoomUI <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("room_booking"))
}


#
searchAvailRoomServer <- function(id, credentials) {
  
  counter_search <<- 0
  counter_refresh <<- 0
  counter_book <<- 0
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$room_booking <- renderUI({
        
        ns <- session$ns
        
        indiv_table <- loadData(database, 'individual_information')
        
        fluidRow(box(width = 3,
                     h4("Search for space in the department.
                        You cannot book your own room so it does not show in the list.
                        Use the 'Are you in or out?' tab instead.
                        (Refresh if no table shown): "),
                     wellPanel(
                       dateInput(ns("date_search"),
                                 label = 'Date',
                                 value = Sys.Date()) %>%
                         helper(colour = "mediumpurple1",
                                type = "inline",
                                size = "m",
                                title = 'Guidance:',
                                content = c(
                                  "- You can search for room availability here,
                                and record somewhere to work in the next section.",
                                  "- Please select the date and the approximate time you
                                wish to be in the department, and then press 'Search'.",
                                  "- If any rooms satisfy your requirements, they will be
                                shown on the right.")
                         )),
                     checkboxGroupInput(ns("checkbox_ampm"),
                                        "Time",
                                        choices = c("am","pm"),
                                        selected = ""),
                     actionButton(ns("search"),
                                  "Search",
                                  width = "50%"),
                     actionButton(ns("refresh"),
                                  "Refresh",
                                  width = "50%"),
                     h4("Confirm rooms of your choice:"),
                     
                     verbatimTextOutput(ns("cand_bookings")),
                     
                     actionButton(ns("book"),
                                  "Book",
                                  width = "25%") %>%
                       helper(
                         colour = "mediumpurple1",
                         type = "inline",
                         size = "m",
                         title = '"How to select the time slot?',
                         content = c(
                           "- Please click on the cells within the data table on the right.",
                           "- Do not select the non-time cells e.g. the whole line.",
                           "- Corresponding room information will appear in the box below.",
                           "- You may then confirm your selection, but notice that 0 available
                           desk rooms cannot be reserved")
                       ),
                     tags$head(
                       tags$style(
                         HTML(".shiny-notification {position:fixed;top: calc(50%);left: calc(50%);}")
                       )
                     )
        ),
        box(width = 9,
            dataTableOutput(outputId = ns("all_table")))
        )
      })
      
      return(
        observeEvent(input$search | input$book | input$refresh, {
          
          user_data <- credentials$info
          
          if (length(input$search) > 0 &&
              (input$search == counter_search + 1 ||
               input$refresh == counter_refresh + 1)) {
            
            counter_search <<- input$search
            counter_refresh <<- input$refresh
            
            req(input$date_search)
            
            appendAllAvailable(input$date_search)
            
            table_shown <- tableShown(input$checkbox_ampm,
                                      input$date_search,
                                      user_data[['ID']])
            output$all_table <-
              DT::renderDataTable(table_shown,
                                  selection = list(mode = "multiple",  # pick date-time cells
                                                   target = "cell"),
                                  options = list(scrollX = TRUE))
            
            output$cand_bookings <-
              renderPrint({
                req(input$all_table_cells_selected)
                
                if (nrow(input$all_table_cells_selected) == 0) {
                  cat('Please select a time')
                } else if (any(input$all_table_cells_selected[, 2] %in% c(0,1,2,3))) {
                  cat('Please select time cells only')
                } else {
                  row_id <- input$all_table_cells_selected[, 1]
                  col_id <- input$all_table_cells_selected[, 2]
                  
                  data.frame(date = table_shown[row_id, "Date"],
                             day = table_shown[row_id, "Weekday"],
                             room_no = table_shown[row_id, "Room_no"],
                             time = colnames(table_shown)[col_id])
                }
              })
          } else if (length(input$book) > 0 &&
                     input$book == counter_book + 1) {
            
            counter_book <<- input$book
            candidate <- create_candidate_table(input, user_data[['ID']])
            notif_duration <- 10
            
            if (is.null(candidate)) {
              showNotification("Please make a selection",
                               type = "warning",
                               closeButton = TRUE,
                               duration = notif_duration)
              
            } else if (nrow(candidate) == 0) {
              if (nrow(input$all_table_cells_selected) > 1) {
                showNotification(
                  "No room is available for all time slots of your choice.
                         Try selecting fewer time slots at a time.",
                  type = "warning",
                  closeButton = TRUE,
                  duration = notif_duration)
                
              } else if (nrow(input$all_table_cells_selected) == 1) {
                showNotification("No room is available for this time slot.",
                                 type = "warning",
                                 closeButton = TRUE,
                                 duration = notif_duration)}
              
            } else if (any(is.na(candidate$Room_no)) || any(is.na(candidate$Date))) {
              showNotification("No room is available for this time slot.",
                               type = "warning",
                               closeButton = TRUE,
                               duration = notif_duration)
            } else {
              room_no_to_book <- candidate$Room_no
              dates_to_book <- candidate$Date
              times_to_book <- time_lup(input$all_table_cells_selected)
              
              candidate <- select(candidate, -Room_no, -Date, -Weekday)
              
              # change to booked
              update_status(use = "booking",
                            room_no = room_no_to_book,
                            date = dates_to_book, 
                            avail = candidate,
                            database = database,
                            table = 'new_room_status')
              
              booking_no <-
                new_booking_no(booking_no = NA,
                               db = database,
                               table = "room_booked",
                               date = dates_to_book)
              
              update_booking(room_no = room_no_to_book,
                             booker = user_data[['ID']],
                             date = dates_to_book,
                             time_idx = input$all_table_cells_selected,
                             # booking_no = NA,
                             booking_no = booking_no,
                             database = database,
                             table = "room_booked")
              
              showNotification(room_confirm_msg(room_no_to_book,
                                                dates_to_book,
                                                times_to_book,
                                                booking_no),
                               type = "message",
                               closeButton = TRUE,
                               duration = notif_duration)
            }
          }
        })
      )
    }
  )
}


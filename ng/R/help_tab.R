
#
helpUI <- function(id) {
  
  ns <- NS(id)
  uiOutput(ns("help"))
}


#
helpServer <- function(id, credentials) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$help <- renderUI({
        
        ns <- session$ns
        
        # req(credentials$user_auth)
        
        tagList(
          h1("What is Polished Fribble?"),
          p("An R Shiny app for UCL Statistical Sciences department room booking.
            This app came out of the idea that it would be helpful for everyone to have
            access to information about which rooms are available at any given time.
            This is both a room 'unbooker' as well as a room booker app, since it provides
            information on which rooms are free and defaults to that they are not.
            That is, a room owner must chnage their room status to Available first before a someone
            can book it."),
          p("So there are 2 main types of user and related functions:
          i) A room owner can edit their rooms upcoming availability; 
          ii) A room user can query available rooms and make a reservation."),
          br(),
          h1("How to use"),
          h2("Changing your personal room status"),
          p("Using the Room status tab select a date in the dropdown calendar and either morning (am) and/or afternoon (pm). Then press Save. You can update a previous room status."),
          h2("Booking a room"),
          p("Using the Room booking tab select a date in the dropdown calendar and either morning (am) and/or afternoon (pm). Then press Search/Update. Highlight time slots that you would like to book by clicking on the text 'Available'. This will highlight the cell. Confirm the room booking by pressing Book. Press the Search/Update button to see the updated room booking table."),
          h2("Cancelling your room booking"),
          p("Using the My Booked Room tab press the Cancel/Update button to see an updated table of all your bookings. Enter the booking number from the booking_no column in the 'Input booking number here:' box and press the Cancel Booking. Press the button again to update the table, without the previous booking.")
        )
      }) 
    }
    
  )
}

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
          p("An R Shiny app for UCL Statistical Sciences department.
            You can record whether you will be in or out of the department on a given day.
            This app came out of the idea that it would be helpful for everyone to have
            access to information about which rooms are not available at any time."),
          p("So there are 2 main types of user and related functions:
          i) A room owner can edit  whether they will be in the department or not; 
          ii) A room user can query available rooms and make use of available space."),
          br(),
          h1("How to use"),
          h2("Changing your personal status"),
          p("Using the 'Are you in or out?' tab select a date in the dropdown calendar and either morning (am) and/or afternoon (pm). Then press Save. You can update a previous room status."),
          h2("Search and reserve available space"),
          p("Using the 'Find somewhere to work' tab select a date in the dropdown calendar and either morning (am) and/or afternoon (pm). Then press Search/Update. Highlight time slots that you would like to book by clicking on the text 'Out'. This will highlight the cell. Confirm the room booking by pressing Book. Press the Search/Update button to see the updated room booking table."),
          h2("Cancelling your reservation"),
          p("Using the 'Amend my record' tab press the Cancel/Update button to see an updated table of all your bookings. Enter the booking number from the booking_no column in the 'Input booking number here:' box and press the Cancel Booking. Press the button again to update the table, without the previous booking.")
        )
      }) 
    }
    
  )
}

library(testthat)
library(shiny)

source("../R/cancelBooking.R")

credentials <- list(user_auth = TRUE,
                    info = data.frame(
                      ID = "Simon",
                      Password = "Password1",
                      Password_Hash = "$7$C6..../....9w4R0ATbf4zrF9YWFgSqsb4jCfGZ4hJkBRaTHdgjC43$Zjl3B3ohoW9q7Zoc9sMUe1kHwVzyiRgSFD6fGfthID2",
                      Permissions = "standard"))

user_data <- credentials$info


testServer(
  cancelBookingServer,
  args = list(credentials = reactive({credentials}),
              user_data = reactive({user_data})), {
                
                session$setInputs(booking_no = "",
                                  cancel = 1)
                
                print(output$cancel_table)
                # expect_equal()
})


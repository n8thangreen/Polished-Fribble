# notification messages ---------------------------------------------------


#
room_confirm_msg <- function(room_no_to_book,
                             dates_to_book,
                             time_slots,
                             booking_no) {
  room_confirm <- NULL
  
  for (i in seq_along(dates_to_book)) {
    room_confirm <- paste(room_confirm,
                          'room', room_no_to_book[i], "\n",
                          "(", dates_to_book[i], ",",
                          date_to_weekday(dates_to_book[i]), ",",
                          paste(time_slots[[i]], collapse = ' and '),
                          ")\n
                          Booking number", booking_no)
  }
  
  paste("You have successfully booked", room_confirm)
}

# room_confirm <-
#   glue::glue(
#     "{room_confirm} room {room_no_to_book[i]} \n
#     ({dates_to_book[i]}, {date_to_weekday(dates_to_book[i])},
#     {paste(time_slots[[i]], collapse = ' and ')})\n
#     Booking number {booking_no}")



#
my_room_message <- function(date_update, ampm) {
  
  my_room <- paste(date_update, ".\n",
                   "In the department", ampm)
  
  paste("You have updated your attendance:", my_room)
}

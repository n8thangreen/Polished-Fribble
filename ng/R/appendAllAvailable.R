
#' if a room hasn't been updated assume it 
#' available i.e. Out from 9am - 5pm
#'
appendAllAvailable <- function(date_search) {
  
  indiv_table <- loadData(database, 'individual_information')
  room_table <- loadData(database, 'new_room_status')
  
  # which rooms not in room_table?
  all_rooms <- unique(indiv_table$RoomNumber)
  new_rooms <- sort(all_rooms[!(all_rooms %in% room_table$Room_no)])
  
  update_status(new_rooms,
                date_search,
                use = "write",
                am = "neither",
                database = database)
  return()
}


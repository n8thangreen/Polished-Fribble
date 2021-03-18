
#' if a room hasn't been updated assume it 
#' available i.e. Out from 9am - 5pm
#'
appendAllAvailable <- function(date_search) {
  
  indiv_table <- loadData(database, 'individual_information')
  room_table <- loadData(database, 'new_room_status')
  capacity_table <- loadData(database, 'room_capacity')
  
  # which rooms not in room_table?
  all_rooms <- unique(indiv_table$RoomNumber)
  logged_rooms <- room_table$Room_no[room_table$Date == date_search]
  new_rooms <- sort(all_rooms[!(all_rooms %in% logged_rooms)])
  new_rooms <- new_rooms[new_rooms != ""]
  
  if (length(new_rooms) == 0) return()
  
  update_status(new_rooms,
                date_search,
                use = "write",
                am = "neither",
                database = database)
  return()
}


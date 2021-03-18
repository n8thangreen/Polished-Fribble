
#' @examples
#' occupancy_table()
#'
occupancy_table <- function() {
  
  room_table <- loadData(database, 'new_room_status')
  
  n_rooms <- nrow(room_table)
  todays_date <- "2021-03-08"  ##TODO:

  room_table %>% 
    filter(Date == todays_date) %>% 
    summarise(across(matches("am|pm"),
                     ~ceiling(sum(.x == "In")/n()*100)))
  
  
}


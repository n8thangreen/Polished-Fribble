
#' filtered room status for each date
#' to display for search
#' and working out which date-times clicked
#'
tableShown <- function(checkbox,
                       date_search,
                       username) {
  
  indiv_table <- loadData(database, 'individual_information')
  room_table <- loadData(database, 'new_room_status')
  
  n_rooms <- nrow(room_table)
  
  am_times <- c('9am_10am', '10am_11am', '11am_12pm')
  
  search_time <- paste(checkbox, collapse = "")
  
  # which days have morning or afternoon available?
  
  avail_ampm <- 
    room_table %>%
    as_tibble() %>%
    melt(id.vars = c("Date", "Weekday", "Room_no"),
         variable.name = "time") %>% 
    mutate(search_time = search_time,
           time_in_am = time %in% am_times,
           ampm = if_else(time_in_am, "am", "pm"),
           ampm = if_else(search_time == "ampm", "ampm", ampm)) %>% 
    group_by(Date, Room_no, ampm) %>% 
    summarise(free = any(value == "Out")) %>%  # at least one free slot?
    filter(ampm == search_time) %>%
    ungroup() %>% 
    select(free)
  
  my_room_no <- indiv_table$RoomNumber[indiv_table$UserName == username]
  
  if (nrow(avail_ampm) == 0) avail_ampm <- rep(FALSE, nrow(room_table))
  
  table_shown <-
    room_table[avail_ampm &
                 room_table$Date %in% as.character(date_search) &
                 room_table$Room_no != my_room_no, ] 
  
  table_shown
}


# filtered room status for each date
# to display for search
# and working out which date-times clicked
#
tableShown <- function(input, username) {
  
  indiv_table <- loadData(database, 'individual_information')
  room_table <- loadData(database, 'new_room_status')
  
  n_rooms <- nrow(room_table)
  
  am_times <- c('9am_10am','10am_11am','11am_12pm')
  
  search_time <- paste(input$cb_ampm, collapse = "")
  
  # which days have morning or afternoon available?
  
  avail_ampm <- 
    room_table %>%
    as_tibble() %>%
    melt(id.vars = c("Date", "Weekday", "Room_no"),
         variable.name = "time") %>% 
    mutate(am = ifelse(search_time == "ampm",       # group by time of day
                       "ampm",
                       ifelse(time %in% am_times, "am", "pm"))) %>% 
    group_by(Date, Room_no, am) %>% 
    summarise(free = any(value == "Available")) %>%  # at least one free slot?
    filter(am == search_time) %>%
    ungroup() %>% 
    select(free)
  
  my_room_no <- indiv_table$RoomNumber[indiv_table$UserName == username]
  
  table_shown <- room_table[avail_ampm &
                              room_table$Date %in% as.character(input$date2) &
                              room_table$Room_no != my_room_no, ]
  table_shown
}
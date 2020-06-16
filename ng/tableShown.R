
# filtered room status for each date
#
tableShown <- function(input) {
 
  room_table <- loadData(database, 'new_room_status')
  n_rooms <- nrow(room_table)
  
  am_times <- c('9am_10am','10am_11am','11am_12pm')
  pm_times <- c('12pm_1pm','1pm_2pm','2pm_3pm', '3pm_4pm','4pm_5pm')
  
  search_time <- paste(input$cb_ampm, collapse = "")
  
  # which days have morning or afternoon available?
  
  room_table_long <-
    room_table %>%
    as_tibble() %>%
    melt(id.vars = c("Date", "Weekday", "Room_no"),
         variable.name = "time") 
  
  avail <- 
    room_table_long %>% 
    mutate(am = ifelse(search_time == "ampm",       # group by time of day
                       "ampm",
                       ifelse(time %in% am_times, "am", "pm"))) %>% 
    group_by(Date, Room_no, am) %>% 
    summarise(free = any(value == "Available")) %>%  # at least one free slot?
    filter(am == search_time) %>%
    ungroup() %>% 
    select(free)
  
  my_room_no <- individual$RoomNumber[individual$UserName == user_data()$ID]
  
  table_shown <- room_table[avail &
                              room_table$Date %in% as.character(input$date2) &
                              room_table$Room_no != my_room_no, ]
  table_shown
}
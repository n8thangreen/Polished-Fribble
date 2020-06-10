
# UCL room booker
# functions
# using SQLite local database


##TODO:for multiple times

# modify existing booking time
update <- function(booking_id,
                   date = NA,
                   time = NA,
                   room_no = NA) {
  
  db <- dbConnect(SQLite(), 'room_booker.sqlite')
  on.exit(dbDisconnect(db))
  
  if (is.na(date)) {
    date <-
      dbGetQuery(db,
                 "SELECT date
                 FROM calendar_table
                 WHERE booking_no = booking_id")}
  if (is.na(time)) {
    time <- 
      dbGetQuery(db,
                 "SELECT time
                 FROM calendar_table
                 WHERE booking_no = booking_id")}
  if (is.na(room_no)) {
    room_no <- 
      dbGetQuery(db,
                 "SELECT room_no
                 FROM calendar_table
                 WHERE booking_no = booking_id")}
  
  query <- paste0("UPDATE calendar_table",
                  "SET ",
                  "time = '", time, "', ",
                  "date = '", date, "', ",
                  "room_no = '", room_no, "', ",
                  "WHERE booking_no = ", booking_id, "'")
  
  dbSendQuery(db, query)
}


# new booking
create <- function(name,
                   date,
                   time,
                   room_no) {
  
  db <- dbConnect(SQLite(), 'room_booker.sqlite')
  on.exit(dbDisconnect(db))
  
  qbooking <- "SELECT MAX(booking_no)
               FROM booking_table"
  booking_id <- unlist(dbGetQuery(db, qbooking)) + seq_along(name)
  
  com <- paste0("(",
                booking_id , ", '",
                name,
                "')")
  query <- paste0("INSERT INTO booking_table
                  VALUES ", paste(com, collapse = ", "))
  
  dbExecute(db, query)
  
  com <- paste0("(",
                booking_id, ", '",
                date, "', '",
                time, "', '",
                room_no,
                "')")
  query <- paste0("INSERT INTO calendar_table
                  VALUES ", paste(com, collapse = ", "))
  dbExecute(db, query)
}


# Load data currently saved in sqlite
loadData <- function(table = 'calendar_table') {
  
  db <- dbConnect(SQLite(), "room_booker.sqlite")
  on.exit(dbDisconnect(db))
  
  query <- sprintf("SELECT * FROM %s", table)
  dbGetQuery(db, query)
}

# remove booking
##TODO: dont know why ';' doesnt work..

delete <- function(booking_id) {
  
  db <- dbConnect(SQLite(), "room_booker.sqlite")
  on.exit(dbDisconnect(db))
  
  query <- paste0('DELETE FROM booking_table',
                  " WHERE booking_no = ", booking_id)
  dbSendQuery(db, query)
  
  query <- paste0('DELETE FROM calendar_table',
                  " WHERE booking_no = ", booking_id)
  dbSendQuery(db, query)
}



##TODO:.why this function?...
#
# automatically update empty table of next month
create_next_month <- function(booking_id){
  
  date_to_cha <- function(date) format(date, format = "%Y-%m-%d")
  
  # database of next month bookings
  calendar_data <- loadData()
  
  for (i in 0:30) {
    # loop over each day in a month
    
    is_wkend <- weekday_to_cha(Sys.Date() + i) %in% c('Sat','Sun')
    is_new_id <- calendar_data$booking_id != booking_id 
    is_newDate <- calendar_data$Date != date_to_cha(Sys.Date() + i)
    
    if (is_new_id & is_newDate & !is_wkend){
      
      create(
        name = calendar_data$name,
        date = date_to_cha(Sys.Date() + i),
        time = calendar_data$time,
        room_no = calendar_data$time)
    }
  }
}


# ALTER TABLE person_table
# ADD CONSTRAINT check_room
# CHECK (room_no IN ('1', '2', '3'));
# 
# ALTER TABLE person_table
# ADD CHECK (room_no IN ('1', '2', '3'));

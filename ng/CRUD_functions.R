
#load the table from the given database   
loadData <- function(database, table) {
  db <- dbConnect(drv,
                  dbname = database,
                  host = options()$edwinyu$host, 
                  port = options()$edwinyu$port,
                  user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  on.exit(dbDisconnect(db), add = TRUE)
  
  query <- sprintf("SELECT * FROM %s", table)
  dbGetQuery(db, query)
}


#For deleting rows in table "new_room_status" from db "room_avail"    
delete <- function(room_no,
                   date_1,
                   date_2 = date_1,
                   database,
                   table) {
  
  db <- dbConnect(drv,
                  dbname = database,
                  host = options()$edwinyu$host, 
                  port = options()$edwinyu$port,
                  user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  on.exit(dbDisconnect(db), add = TRUE)
  
  query <- paste0('DELETE FROM ', table,
                  " WHERE Room_no = '", room_no
                  ,"' AND ",
                  "Date >= '", date_1,"' AND <='", date_2,"'")
  
  print(query) #for debug
  dbSendQuery(db, query)
}


# updating booking(for table room_booked)
# room_booked is a table I created in MySQL with PRIMARY booking_no   
update_booking <- function(room_no,
                           booker,
                           date,
                           time,
                           booking_no,
                           database,
                           table){
  db <- dbConnect(drv,
                  dbname = database,
                  host = options()$edwinyu$host, 
                  port = options()$edwinyu$port,
                  user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  on.exit(dbDisconnect(db), add = TRUE)
  
  value_names <- c("booking_no", "date", "time", "room_no", "booker")
  values <- c(booking_no, date, time, room_no, booker)
  
  query <- paste(
    sprintf(
    "INSERT INTO %s VALUES ('%s')",
    table, 
    paste(values, collapse = "', '")),
    "ON DUPLICATE KEY UPDATE",
    paste(sprintf("%1$s = VALUES(%1$s)", value_names), collapse = ", ")
  )
  
  print(query)
  dbGetQuery(db, query)
}


# deleting booking from room_booked  
delete_booking <- function(booking_no,
                           database,
                           table) {
  db <- dbConnect(drv,
                  dbname = database,
                  host = options()$edwinyu$host, 
                  port = options()$edwinyu$port,
                  user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  on.exit(dbDisconnect(db), add = TRUE)
  
  query <- sprintf("DELETE FROM %s WHERE booking_no = '%s'", table, booking_no)
  
  print(query) #for debug
  dbSendQuery(db, query)
}

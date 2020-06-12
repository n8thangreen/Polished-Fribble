
#load the table from the given database   
loadData <- function(database, table) {
  db <- dbConnect(drv,
                  dbname = database,
                  host = options()$edwinyu$host, 
                  port = options()$edwinyu$port,
                  user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  query <- sprintf("SELECT * FROM %s", table)
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}


#For deleting rows in table "new_room_status" from db "room_avail"    
delete <- function(room_no,
                   date_1,
                   date_2=NULL,
                   database,
                   table) {
  db <- dbConnect(drv,
                  dbname = database,
                  host = options()$edwinyu$host, 
                  port = options()$edwinyu$port,
                  user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  if (is.null(date_2)){
    query <- paste0('DELETE FROM ',table," WHERE Room_no = '", room_no,"' AND ",
                    "Date = '", date_1,"'")
  }else{
    query <- paste0('DELETE FROM ',table," WHERE Room_no = '", room_no,"' AND ",
                    "Date BETWEEN '", date_1,"' AND '",date_2,"'")
  }
  
  print(query)#for debug
  dbSendQuery(db, query)
  dbDisconnect(db)
}


#functions for updating booking(for table room_booked)
#room_booked is a table I created in MySQL with PRIMARY booking_no   
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
  query <- paste0("INSERT INTO ",table,
                  " VALUES ('",booking_no,"','",date,"','",time,"','",room_no,"','",booker,"')
                    ON DUPLICATE KEY UPDATE room_no=VALUES(room_no),booker=VALUES(booker),date=VALUES(date),time=VALUES(time)")
  print(query)
  dbGetQuery(db, query)
  dbDisconnect(db)
}

#function for deleting booking from room_booked  
delete_booking <- function(booking_no,
                           database,
                           table) {
  db <- dbConnect(drv,
                  dbname = database,
                  host = options()$edwinyu$host, 
                  port = options()$edwinyu$port,
                  user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  
  query <- paste0('DELETE FROM ',table," WHERE booking_no = '",booking_no,"'")
  
  print(query)#for debug
  dbSendQuery(db, query)
  dbDisconnect(db)
}

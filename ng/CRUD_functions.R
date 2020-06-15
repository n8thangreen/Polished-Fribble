
# load the table from the given database   
#
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


# delete rows in table "new_room_status" from db "room_avail"
#
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
                  " WHERE Room_no = '", room_no,
                  "' AND ",
                  "Date >= '", date_1,"' AND <='", date_2,"'")
  
  print(query) #for debug
  dbSendQuery(db, query)
}


# update booking for table room_booked
#
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
  
  # create unique booking ref
  # ...
  
  value_names <- c("booking_no", "date", "time", "room_no", "booker")
  
  # matrix each row time slot
  time <- unlist(time)
  values <- matrix(c(booking_no, date, room_no, booker), nrow = 1)
  dat <- values[rep(1, length(time)), ]

  time_slots <-
    c("9am_10am", "10am_11am", "11am_12pm", "12pm_1pm", "1pm_2pm", "2pm_3pm", "3pm_4pm", "4pm_5pm") %>%
    setNames(4:11)
  
  q <- cbind(dat, time = time_slots[time])
  
  # parse to query
  q <- 
    as_tibble(q) %>%
    set_names(nm = c("booking_no", "date", "room_no", "booker", "time")) %>% 
    select(booking_no, date, time, room_no, booker) %>% # put in correct order
    mutate_at(vars(2:4), funs(paste0("'", ., "'"))) %>% 
    mutate_at(vars(1), funs(paste0(., "'"))) %>%        # don't quote first and last
    mutate_at(vars(5), funs(paste0("'", .))) %>%
    tidyr::unite("q", sep = ",") %>% 
    select(q) %>% 
    unlist()
  
  ## MySQL  
  # query <- paste(
  #   sprintf(
  #   "INSERT INTO %s VALUES ('%s')",
  #   table, 
  #   paste(values, collapse = "', '")),
  #   "ON DUPLICATE KEY UPDATE",
  #   paste(sprintf("%1$s = VALUES(%1$s)", value_names), collapse = ", ")
  # )
  
  ## SQLite
  query <- paste(
    paste0(
      sprintf(
        "INSERT INTO %s VALUES ('%s')", table, q),
      " ON CONFLICT (date, time, room_no) DO UPDATE SET ",
      paste(sprintf("'%1$s' = excluded.'%1$s'", value_names), collapse = ", "), ";"),
    collapse = " ")
    
  dbGetQuery(db, query)
}


# deleting booking from room_booked 
#
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
  
  dbSendQuery(db, query)
}

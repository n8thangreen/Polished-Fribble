
# For updating rows in table "new_room_status" from db "room_avail"
## when use="write", the function inputs am="am"/"pm"/"both"/"neither", used for writing personal room status to the table
# when use="booking", the function inputs avail
# avail is an n by 8 matrix, each row corresponds to 8 periods for certain room on a specific day

# new_room_status is a table I created in MySQL with Room_no and Date as PRIMARY,
# so that when updating rows with the same Room_no and Date existing in the table
# it modifies the exisitng row rather than adding a new row
#
update_status <- function(room_no,             # single integer?
                          date,                # vector?
                          use = "write",
                          am = NA,             # vector?
                          avail = NA,
                          database,
                          table) {
  
  date_to_weekday <- function(date) {format(as.Date(date), format = "%a")}
  
  if(use == "write"){
    
    A <- data_frame(room_no = room_no,
                    date = date,
                    am = am,
                    avail = NA_character_)
    
    A[A$am == "am", "avail"]      <- paste0(paste0(rep("'Available'",3),  collapse = ","),
                                            ",",
                                            paste0(rep("'Unavailable'",5), collapse = ","))
    
    A[A$am == "pm", "avail"]      <- paste0(paste0(rep("'Unavailable'",3), collapse = ","),
                                            ",",
                                            paste0(rep("'Available'",5), collapse = ","))
    
    A[A$am == "both", "avail"]    <- paste0(paste0(rep("'Available'",3), collapse = ","),
                                            ",",
                                            paste0(rep("'Available'",5), collapse = ","))
    
    A[A$am == "neither", "avail"] <- paste0(paste0(rep("'Unavailable'",3), collapse = ","),
                                            ",",
                                            paste0(rep("'Unavailable'",5), collapse = ","))
    
    A['weekday'] <- date_to_weekday(A$date)
    
    
    # check correct column order
    query_cols <- c("date", "weekday", "room_no", "avail")
    
    # split
    AM   <- A[A$am == "am", query_cols]
    PM   <- A[A$am == "pm", query_cols]
    Both <- A[A$am == "both", query_cols]
    Neither <- A[A$am == "neither", query_cols]
    
    ##TODO: these seems to so the same thing.
    ## why not just do directly on A?
    
    parse_query <- function(...) {
      paste0(paste0("('",
                    paste(..., sep = "','"),
                    "')"),
             collapse = ",") 
    }
    
    query_am <- 
      if ("am" %in% am){
        parse_query(AM)
      } else {NULL}
    
    query_pm <- 
      if ("pm" %in% am) {
        parse_query(PM)
      } else {NULL}
    
    query_both <- 
      if ("both" %in% am) {
        parse_query(Both)
      } else {NULL}
    
    query_neither <- 
      if ("neither" %in% am) {
        parse_query(Neither)
      } else {NULL}
    
    l <- c(query_am, query_pm, query_both, query_neither)
    
    q <- paste(l, collapse = ",")
    
  } else if (use == "booking") {
    
    availability <- paste(paste0("'", avail[, 1]), avail[, 2], avail[, 3], avail[, 4],
                          avail[, 5], avail[, 6],  avail[, 7], paste0(avail[, 8], "'"), sep = "','")
    
    A <- data_frame(date = date,
                    room_no = room_no,
                    avail = availability)
    
    A['weekday'] <- date_to_weekday(A$date)
    
    A <- A[, query_cols]
    
    q <- parse_query(A)
    
  }
  
  ##TODO: dont need to update duplicate key? ie date, time, room_no
  #
  query <- paste0("INSERT INTO ", table,
                  " VALUES ", q,
                  " ON DUPLICATE KEY UPDATE Date=VALUES(Date),",
                  "Weekday=VALUES(Weekday), Room_no=VALUES(Room_no),",
                  "9am_10am=VALUES(9am_10am), 10am_11am=VALUES(10am_11am), 11am_12pm=VALUES(11am_12pm),",
                  "12pm_1pm=VALUES(12pm_1pm), 1pm_2pm=VALUES(1pm_2pm),     2pm_3pm=VALUES(2pm_3pm),",
                  "3pm_4pm=VALUES(3pm_4pm),   4pm_5pm=VALUES(4pm_5pm)")
  
  print(query) #for debug
  db <- dbConnect(MySQL(),
                  dbname = database,
                  host = options()$edwinyu$host, 
                  port = options()$edwinyu$port,
                  user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  dbGetQuery(db, query)
  dbDisconnect(db) 
}


# test

# date <- c("2020-06-12", "2020-06-13")
# use <- "write"
# room_no <- 1
# am <- c("am", "both")
# table <- 'new_room_status'
# database <- 'room_avail'
# 
# avail <- setNames(data.frame('Unavailable','Unavailable','Unavailable',
#                              'Available','Available','Available','Available','Available'), NULL)


# update_status(use = use,
#               room_no = room_no,
#               date = date,
#               am = am,
#               database = 'room_avail',
#               table = 'new_room_status')

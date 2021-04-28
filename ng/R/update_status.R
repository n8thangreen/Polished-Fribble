
#' Update room status
#'
#' Update rows in table "new_room_status" from database "room_avail".
#'
#' - "write", function inputs am="am"/"pm"/"both"/"neither",
#'      used for writing personal (own) room status to the table
#' - "booking", the function inputs avail. used for someone else's room
#'
#' avail is an n by 8 matrix, each row corresponds to 8 periods for certain room
#' on a specific day.
#'
#' new_room_status is a table created in MySQL with Room_no and Date as PRIMARY,
#' so that when updating rows with the same Room_no and Date existing in the table
#' it modifies the existing row rather than adding a new row
#'
#' @param room_no One or more room IDs
#' @param date Format "2020-06-13"
#' @param use 'write' or 'booking'
#' @param am am, pm, both, neither
#' @param avail Room availability. From 9am to 5pm either In or Out. Matrix.
#' @param database 
#' @param table 
#'
#' @return
#' @export
#'
update_status <- function(room_no,
                          date,
                          use = "write",
                          am = NA,
                          avail = NA,
                          database,
                          table = 'new_room_status') {
  
  # contain in speech marks and paste together
  ##TODO: dbQuoteLiteral() use instead?
  parse_query <- function(dat) {
    
    dat %>% 
      mutate(
        date = paste0(date, "'"),
        weekday = paste0("'", weekday, "'"),
        room_no = paste0("'", room_no, "'"),
        avail = paste0("'", map(avail, paste, collapse = "','"))) %>%
      tidyr::unite("q", date:avail, sep = ",") %>% 
      select(q) %>% 
      unlist()
  }
  
  db <- dbConnect(drv,
                  dbname = database,
                  host = options()$edwinyu$host, 
                  port = options()$edwinyu$port,
                  user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  on.exit(dbDisconnect(db), add = TRUE)
  
  booking_info <- list(room_no = room_no, date = date, am = am)
  A <- make_room_status_entry(use, booking_info, avail)
  q <- parse_query(A)
  
  time_slots <- c("9am_10am", "10am_11am", "11am_12pm", "12pm_1pm",
                  "1pm_2pm",  "2pm_3pm",   "3pm_4pm",   "4pm_5pm")
  
  table_headings <- c("Weekday", "Room_no", time_slots)
  
  # slightly different syntax
  if (class(drv) == "MySQLDriver") {
    
    query <- paste(
      sprintf(
        "INSERT INTO %s VALUES ('%s')", table, q),
      "ON DUPLICATE KEY UPDATE",
      paste(sprintf("%1$s = VALUES(%1$s)", table_headings), collapse = ", "),
      collapse = "; ")
    
  } else if (class(drv) == "SQLiteDriver") {
    
    query <- paste(
      sprintf(
        "INSERT INTO %s VALUES ('%s')", table, q),
      "ON CONFLICT (Date, Room_no) DO UPDATE SET ",
      paste(sprintf("'%1$s' = excluded.'%1$s'", table_headings), collapse = ", "))
  }
  
  ##TODO: hack in case of single string to list
  query <- as.list(strsplit(query, ";")[[1]])
  print(paste("query:", query))
  
  sapply(query, FUN = function(x) dbGetQuery(conn = db, x))
  # sapply(query, FUN = function(x) dbExecute(conn = db, x)) ##TODO: is this better?
}


# # In/Out bookings
# # 
# make_room_status_entry <- function(use, info, avail) {
#   
#   if (use == "write") {
#     A <-
#       tibble(date = info$date,
#              weekday = date_to_weekday(date),
#              room_no = info$room_no,
#              am = info$am,
#              avail = case_when(
#                am == "am" ~ list(c(rep("In",3), rep("Out",5))),
#                am == "pm" ~ list(c(rep("Out",3), rep("In",5))),
#                am == "both" ~ list(c(rep("In",3), rep("In",5))),
#                am == "neither" ~ list(c(rep("Out",3), rep("Out",5))))
#       ) %>% 
#       select(-am)
#     
#   } else if (use == "booking") {
#     A <-
#       tibble(date = info$date,
#              weekday = date_to_weekday(date),
#              room_no = info$room_no) %>% 
#       cbind.data.frame(
#         as_tibble(info$avail, .name_repair = "minimal")) %>% 
#       group_by(date, weekday, room_no) %>%
#       nest() %>% 
#       rename(avail = data)
#   } else {
#     stop("use not recognised.")
#   }
#   
#   return(A)
# }


# room occupancy numbers
#
# info <- list(room_no = "R.103", date = "2021-03-18", am = "am")
# info <- list(room_no = list("R.103", "R.114"), date = "2021-03-18", am = "neither")
#
make_room_status_entry <- function(use, info, avail) {
  
  capacity_table <- loadData(database, "room_capacity")
  
  ##TODO: if someone else has already booked it then need to use latest occupancy number
  if (use == "write") {
    
    capacity <-
      merge(data.frame(RoomNumber = info$room_no), capacity_table) %>% 
      select(limit) %>%      # 25%
      # select(capacity) %>% # all desks 
      unlist() %>%
      unname()
    
    print(length(capacity))
    print(info$am)
    
    A <-
      tibble(date = info$date,
             weekday = date_to_weekday(date),
             room_no = info$room_no,
             am = info$am) %>% 
      mutate(avail = case_when(
        am == "am" ~ map(capacity, ~c(rep(.x - 1, 3), rep(.x, 5))),
        am == "pm" ~ map(capacity, ~c(rep(.x, 3), rep(.x - 1, 5))),
        am == "both" ~ map(capacity, ~c(rep(.x - 1, 3), rep(.x - 1, 5))),
        am == "neither" ~ map(capacity, ~c(rep(.x, 3), rep(.x, 5))))
      ) %>% 
      select(-am)
    
    print(unnest(A, cols = c(avail)))
    
  } else if (use == "booking") {
    ##TODO:...
    A <-
      tibble(date = info$date,
             weekday = date_to_weekday(date),
             room_no = info$room_no) %>% 
      cbind.data.frame(
        as_tibble(avail, .name_repair = "minimal")) %>% 
      group_by(date, weekday, room_no) %>%
      nest() %>% 
      rename(avail = data)
    
  } else {
    stop("use not recognised.")
  }
  
  return(A)
}


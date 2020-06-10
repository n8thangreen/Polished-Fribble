
# UCL room booker
# functions and example
# using SQLite local database


##TODO:for multiple times

# modify existing booking time
update <- function(booking_id,
                   date,
                   time,
                   room_no) {
  
  db <- dbConnect(SQLite(), 'room_booker.sqlite')
  on.exit(dbDisconnect(db))
  
  query <- paste0("UPDATE calendar_table SET ",
                  "time = '", time, "', ",
                  "WHERE ID = ", booking_id, "'")

  dbSendQuery(db, query)
}


# new booking
create <- function(booking_id,
                   date,
                   time,
                   room_no) {
  
  db <- dbConnect(SQLite(), 'room_booker.sqlite')
  on.exit(dbDisconnect(db))
  
  com <- paste0("(",
                booking_id , ", '",
                name,
                "')")
  query <- paste0("INSERT INTO booking_table VALUES ", com)
  dbExecute(db, query)
  
  com <- paste0("(",
                booking_id, ", '",
                date, "', '",
                time, "', '",
                room_no,
                "')")
  query <- paste0("INSERT INTO calendar_table VALUES ", com)
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
delete <- function(booking_id) {
  
  db <- dbConnect(SQLite(), "room_booker.sqlite")
  on.exit(dbDisconnect(db))
  
  query <- paste0('DELETE FROM booking_table',
                  " WHERE booking_no = '", booking_id, "'; ",
                  'DELETE FROM calendar_table',
                  " WHERE booking_no = '", booking_id, "'")
  dbSendQuery(db, query)
}



##TODO:...
# automatically update empty table of next month
create_next_month <- function(booking_id){
  
  date_to_cha <- function(date) format(date, format = "%Y-%m-%d")
  
  room_data <- loadData()
  
  for(i in 0:30){
    if(sum(room_data$ID == id &
           room_data$Date == date_to_cha(Sys.Date() + i) ) == 0 &
       !(weekday_to_cha(Sys.Date() + i) %in% c('Sat','Sun'))){
      
      create(
        booking_id = id,
        date = date_to_cha(Sys.Date() + i),
        time = ,
        room_no = )
    }
  }
}


# -------------------------------------------------------------------------

#DEMO

library(RSQLite)
library(DBI)
library(odbc)


data <- read.csv('individual room status.csv', stringsAsFactors = FALSE)

db <- dbConnect(SQLite(), 'room_booker.sqlite')

dbListTables(db)
dbReadTable(db, "booking_table")
dbReadTable(db, "calendar_table")
dbReadTable(db, "person_table")





dbWriteTable(db, "room_table", room, overwrite = TRUE)
# dbReadTable(db, 'room_table') 

update(2, '2019-06-21', 'Fri', 'am', 'Long live Walking Dead')  # Only available_period and notes are updatable
update(1, '2019-06-19', 'Wed', 'both', 'Bow down to the KeyNG') # ID and date is for identifying which row to update,
# 'Wed' doesn't do anything here actually


#Add new data
#create functions support passing multiple rows of data at a time
create(booking_id = c(3,4),
       date = c('2019-07-19','2019-10-01'),
       time))

#Deletion
delete(id = 4, date = '2019-10-01')


#Add the room information table in the next 30 days(counting from today)
#The weekends aren't added
#The dates existing in the table aren't added
create_next_month(booking_id = 4, room_table = 'room_table')


#Note that create function cannot be used for changing existing data
#It instead added new rows

updated_room <- loadData()
print(updated_room)




# UCL room booker
# functions and example
# using SQLite local database


library(RSQLite)
library(DBI)


# modify existing booking
update <- function(id,
                   date,
                   day_of_the_week,
                   available_period,
                   notes,
                   room_table = 'room_table') {
  
  db <- dbConnect(SQLite(), 'room_store.sqlite')
  on.exit(dbDisconnect(db))
  
  query <- paste0("UPDATE ", room_table," SET ",
                  "Available_period = '", available_period , "', ",
                  "Notes = '", notes, "' ",
                  "WHERE ID = ", id," AND ",
                  "Date = '", date,"'")
  print(query) #for debug
  dbSendQuery(db, query)
}


# new booking
create <- function(id,
                   date,
                   day_of_the_week,
                   available_period,
                   notes,
                   room_table = 'room_table') {
  
  db <- dbConnect(SQLite(), 'room_store.sqlite')
  on.exit(dbDisconnect(db))
  
  table <- dbReadTable(db, 'room_table')
  com <- paste0("(",
                id , ", '",
                date, "','",
                day_of_the_week, "','",
                available_period,"','",
                notes,"')")
  query <- paste0("INSERT INTO ",room_table,
                  " VALUES ",paste(com, collapse = ", "))
  print(query) #for debug
  dbGetQuery(db, query)
}


# Load data currently saved in sqlite
loadData <- function(room_table = 'room_table') {
  
  db <- dbConnect(SQLite(), "room_store.sqlite")
  on.exit(dbDisconnect(db))
  
  query <- sprintf("SELECT * FROM %s", room_table)
  dbGetQuery(db, query)
}

# remove booking
delete <- function(id,
                   date,
                   day_of_the_week,
                   room_table = 'room_table') {
  
  db <- dbConnect(SQLite(), "room_store.sqlite")
  on.exit(dbDisconnect(db))
  
  query <- paste0('DELETE FROM ',room_table,
                  ' WHERE ID = ', id," AND ",
                  "Date = '", date,"'")
  print(query) #for debug
  dbSendQuery(db, query)
}


# automatically update the empty table of next month
create_next_month <- function(id,
                              room_table = 'room_table'){
  
  date_to_cha <- function(date) format(date, format = "%Y-%m-%d")
  
  weekday_to_cha <- function(date) format(date, format = "%a")
  
  room_data <- loadData()
  
  for(i in 0:30){
    if(sum(room_data$ID == id &
           room_data$Date == date_to_cha(Sys.Date() + i) ) == 0 &
       !(weekday_to_cha(Sys.Date() + i) %in% c('Sat','Sun'))){
      
      create(
        id = id,
        date = date_to_cha(Sys.Date() + i),
        day_of_the_week = weekday_to_cha(Sys.Date() + i),
        available_period = NA,
        notes = NA,
        room_table = 'room_table')
    }
  }
}


# -------------------------------------------------------------------------

#DEMO

room <- read.csv('individual room status.csv', stringsAsFactors = FALSE)

db <- dbConnect(SQLite(), 'room_store.sqlite')

dbWriteTable(db, "room_table", room, overwrite = TRUE)
# dbReadTable(db, 'room_table') 

update(2, '2019-06-21', 'Fri', 'am', 'Long live Walking Dead')  # Only available_period and notes are updatable
update(1, '2019-06-19', 'Wed', 'both', 'Bow down to the KeyNG') # ID and date is for identifying which row to update,
                                                                # 'Wed' doesn't do anything here actually


#Add new data
#create functions support passing multiple rows of data at a time
create(id = c(3,4),
       date = c('2019-07-19','2019-10-01'),
       day_of_the_week = c('Thu','Thu'),
       available_period = c('am','am'),
       notes = c('Young OG','Shout out to Chris Wu'))

#Deletion
delete(id = 4, date = '2019-10-01')


#Add the room information table in the next 30 days(counting from today)
#The weekends aren't added
#The dates existing in the table aren't added
create_next_month(id = 4, room_table = 'room_table')


#Note that create function cannot be used for changing existing data
#It instead added new rows

updated_room <- loadData()
print(updated_room)



#So far didn't make any changes to the local file 'individual room status.csv'
#We can then output updated_room by write.csv(updated_room, file = "......csv") to output the file
#Or make use of the new dataframe directly


##########################################################################################
#update and delete doesn't support modifying multiple rows at a time so far
#Such operations are available in SQL or MySQL
#But the package I used here(DBI) doesn't support many commands in SQL or MySQL
#It might be useful to look at the link below for more packages doing SQL in R(if you think it's necessary)
#http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/sql.html


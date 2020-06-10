
# UCL room booker
# functions and example
# using SQLite local database


library(RSQLite)
library(DBI)
library(odbc)


source("R/booker_functions.R")

# data <- read.csv('individual room status.csv', stringsAsFactors = FALSE)

db <- dbConnect(SQLite(), 'room_booker.sqlite')

dbListTables(db)
dbReadTable(db, "booking_table")
dbReadTable(db, "calendar_table")
dbReadTable(db, "person_table")



#Add new data
#create functions support passing multiple rows of data at a time
# all separate bookings

create(name = c("Nathan", "Bob"),
       date = c('2019-07-19','2019-10-01'),
       time = c("11am-12pm", "11am-12pm"),
       room_no = c(5,100))


##TODO: what do I actually want this to do?...

update(booking_id = 2,
       date = '2019-06-21',
       time = "1pm-2pm",
       room_no = "20")


dbReadTable(db, "booking_table")
dbReadTable(db, "calendar_table")
delete(booking_id = 3)


# create_next_month(booking_id = 4)


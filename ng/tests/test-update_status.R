
database <<- "room_avail_test.db"
room_table <- loadData(database, 'new_room_status')


# single whole day

date <- "2020-06-13"
use <- "write"
room_no <- 1
am <- "both"
table <- 'new_room_status'

avail <- matrix(c('Unavailable','Unavailable','Unavailable',
                             'Available','Available','Available','Available','Available'), nrow = 1)

update_status(room_no,
              date,
              use,
              am,
              avail,
              database)

dbReadTable(db, "new_room_status")


# multiple days

date <- c("2020-03-12", "2020-03-13")
am <- c("am", "both")

update_status(room_no,
              date,
              use,
              am,
              avail,
              database)

dbReadTable(db, "new_room_status", check.names = FALSE)

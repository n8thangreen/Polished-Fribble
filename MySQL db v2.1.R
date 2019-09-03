loadData <- function(database,table) {
  db <- dbConnect(MySQL(), dbname = database, host = options()$edwinyu$host, 
                  port = options()$edwinyu$port, user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  query <- sprintf("SELECT * FROM %s", table)
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}


#when use="write", the function inputs am="am"/"pm"/"both"/"neither", used for writing personal room status to the table
#when use="booking", the function inputs avail
#avail is an n by 8 matrix, each row corresponds to 8 periods for certain room on a specific day
#See examples for more details
update_status <- function(room_no,date,use="write",am=NA,avail=NA,database,table) {
  date_to_weekday<-function(date){format(as.Date(date), format="%a")}
  if(use=="write"){
    A=data_frame(room_no=room_no,date=date,am=am,avail=NA)
    
    A[A$am=="am",4]=paste0(paste(rep("Available",3),collapse = "','"),"','",paste(rep("Unavailable",5),collapse = "','"))
    A[A$am=="pm",4]=paste0(paste(rep("Unavailable",3),collapse = "','"),"','",paste(rep("Available",5),collapse = "','"))
    A[A$am=="both",4]=paste0(paste(rep("Available",3),collapse = "','"),"','",paste(rep("Available",5),collapse = "','"))
    A[A$am=="neither",4]=paste0(paste(rep("Unavailable",3),collapse = "','"),"','",paste(rep("Unavailable",5),collapse = "','"))
    A['weekday']=date_to_weekday(A$date)
    
    AM=A[A$am=="am",]
    PM=A[A$am=="pm",]
    Both=A[A$am=="both",]
    Neither=A[A$am=="neither",]
    
    if("am" %in% am){
      query_am= paste0(paste0("('",paste(AM$room_no,AM$avail,AM$date,AM$weekday,sep="','"),"')"),collapse = ",") 
    }
    else{ query_am=NULL    }
    if("pm" %in% am){query_pm=paste0(paste0("('",paste(PM$room_no,PM$avail,PM$date,PM$weekday,sep="','"),"')"),collapse = ",") } else{query_pm=NULL}
    if("both" %in% am){query_both=paste0(paste0("('",paste(Both$room_no,Both$avail,Both$date,Both$weekday,sep="','"),"')"),collapse = ",") } else{query_both=NULL}
    if("neither" %in% am){query_neither=paste0(paste0("('",paste(Neither$room_no,Neither$avail,Neither$date,Neither$weekday,sep="','"),"')"),collapse = ",") } else{query_neither=NULL}
    
    l=c(query_am,query_pm,query_both,query_neither)
    
    q=paste(l,collapse=",")
    
    
  }else if(use=="booking"){
    availability=paste(avail[,1],avail[,2],avail[,3],avail[,4],avail[,5],avail[,6],avail[,7],avail[,8],sep="','")
    A=data_frame(room_no=room_no,date=date,avail=availability)
    A['weekday']=date_to_weekday(A$date)
    q=paste0(paste0("('",paste(A$room_no,A$avail,A$date,A$weekday,sep="','"),"')"),collapse = ",") 
    
  }
  
  
  query=paste0("INSERT INTO ",table," VALUES ",q," ON DUPLICATE KEY UPDATE Room_no=VALUES(Room_no),
9am_10am=VALUES(9am_10am),10am_11am=VALUES(10am_11am),11am_12am=VALUES(11am_12am),12pm_1pm=VALUES(12pm_1pm),1pm_2pm=VALUES(1pm_2pm),
2pm_3pm=VALUES(2pm_3pm),3pm_4pm=VALUES(3pm_4pm),4pm_5pm=VALUES(4pm_5pm),Date=VALUES(Date),Weekday=VALUES(Weekday)")
  print(query)#for debug
  db <- dbConnect(MySQL(), dbname = database, host = options()$edwinyu$host, 
                  port = options()$edwinyu$port, user = options()$edwinyu$user, 
                  password = options()$edwinyu$password)
  dbGetQuery(db, query)
  dbDisconnect(db)
  
  
  
}




delete <- function(room_no,date_1,date_2=NULL,database,table) {
  db <- dbConnect(MySQL(), dbname = database, host = options()$edwinyu$host, 
                  port = options()$edwinyu$port, user = options()$edwinyu$user, 
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


create_next_week <- function(room_no,database,table){
  
  date_to_cha<-function(date){format(date, format="%Y-%m-%d")}
  
  weekday_to_cha<-function(date){format(date, format="%a")}
  
  room_data=loadData(database,table)
  
  room_vec=c()
  date_vec=c()

  for(i in 0:7){
    if(    sum(room_data$Room_no==room_no & room_data$Date==date_to_cha(Sys.Date()+i) )==0 &  !(weekday_to_cha(Sys.Date()+i) %in% c('Sat','Sun'))     ){
      room_vec=append(room_vec,room_no)
      date_vec=append(date_vec,date_to_cha(Sys.Date()+i))
      
      
    }
  }
  
  update_status(use="booking",room_no=room_vec,date=date_vec,avail=matrix(nrow=length(date_vec),ncol=8),database=database,table=table)

}



#Example
loadData("room_avail","new_room_status")


update_status(use="write",room_no=c(1,1,1,1),date=c("2019-08-01","2019-08-02","2019-08-03","2019-08-04"),am=c("am","pm","both","neither"),database = "room_avail",table = "new_room_status")


update_status(use="write",room_no=2,date="2019-08-01",am="am" ,database = "room_avail",table = "new_room_status")

update_status(use="booking",room_no =c(1,1),date=c("2019-08-30","2019-08-31"),avail=matrix(data=rep(c('Unavailable','Available','Available','Unavailable','Unavailable','Unavailable','Unavailable','Unavailable'),2),nrow=2),database = "room_avail",table = "new_room_status")

create_next_week(room_no = 1,database = "room_avail",table = "new_room_status")

delete(room_no =1, "2019-08-01","2019-08-31",database = "room_avail",table = "new_room_status")



#if any of these commands fail to run, it might not successfully disconnect from the database
#it causes trouble if too many connections remain
#use the line below to disconnect all existing connections
for(con in dbListConnections(MySQL()) ) dbDisconnect(con)




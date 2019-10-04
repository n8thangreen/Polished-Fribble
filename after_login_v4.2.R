# devtools::install_github("paulc91/shinyauthr")

library(shiny)
library(shinyauthr)
library(shinyjs)
# Querying dataset
library(tidyverse)
# Hashing Passwords with 'sodium'
library(sodium)
library(shinydashboard)
library(jsonlite)
# Define a json data format for data storage and querying
library(rjson)
library(RSQLite)
library(RJSONIO)
library(RSQLite)
library(DBI)
library(RMySQL)
library(DT)
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "Login",
                fluidPage(box(
                    # Login
                    ##################################################################################
                    shinyjs::useShinyjs(),
                    tags$head(tags$style(".table{margin: 0 auto;}"),
                              tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                                          type="text/javascript"),
                              includeScript("returnClick.js")
                    ),
                    shinyauthr::loginUI("login"),

                    # actionButton("change_schedule", "Click Me to Adjust Schedule")
                    HTML('<div data-iframe-height></div>')
                    ##################################################################################
                ))),
        tabItem(tabName = "Room_Status_Update",
                # header
                fluidPage(box(width=3,
                             h4("UCL Statistical Science Department Room Update System"),
                             selectInput("day1",label = "Day",
                                        choices = c("Monday" = "Monday","Tuesday" = "Tuesday","Wednesday" = "Wednesday",
                                                    "Thursday" = "Thursday", "Friday" = "Friday"),selected = ""),
                             checkboxGroupInput("time1","Time",choices = c("am","pm"),selected = ""),
                             actionButton("save1","Save",width = "25%")
                                 ),
                         box(width = 9,
                             dataTableOutput(outputId = 'personal')),
                         tags$head(
                             tags$style(
                                 HTML(".shiny-notification {position:fixed;top: calc(50%);left: calc(50%);}"
                                 )
                             )
                         )
                          )),
        tabItem(tabName = "Room_booking",
                # header
                fluidRow(box(width=3,
                             h4("Search for the available room information"),
                             selectInput("day2",label = "Day",
                                         choices = c("Monday" = "Monday","Tuesday" = "Tuesday","Wednesday" = "Wednesday",
                                                     "Thursday" = "Thursday", "Friday" = "Friday","All"="All"),selected = "All"),
                             checkboxGroupInput("time2","Time",choices = c("am","pm"),selected = ""),
                             actionButton("search","Search",width = "25%"),
                             
                             h4("Book your room here:"),
                             selectInput("day3",label = "Day",
                                         choices = c("Monday" = "Monday","Tuesday" = "Tuesday","Wednesday" = "Wednesday",
                                                     "Thursday" = "Thursday", "Friday" = "Friday"),selected = ""),
                             checkboxGroupInput("time3","Time",choices = c("9am_10am","10am_11am","11am_12pm","12pm_1pm","1pm_2pm",
                                                                           "2pm_3pm","3pm_4pm","4pm_5pm"),selected = ""),
                             selectInput("room_book_no","Room_Number",choices = c("1"="1","2"="2"),selected = ""),
                             actionButton("book","Book",width = "25%"),
                             tags$head(
                                 tags$style(
                                     HTML(".shiny-notification {position:fixed;top: calc(50%);left: calc(50%);}"
                                     )
                                 )
                             )
                             
                             ),
                        box(width = 9,
                            dataTableOutput(outputId = 'all'))
                )),
        tabItem(tabName = "Room_booked",
                #header
                fluidPage(box(width = 3,
                              h4("Cancel the booked room"),
                              textInput("booking_no","Input your booking number here:",value = ""),
                              actionButton("cancel","Cancel",width = "25%"),
                              tags$head(
                                  tags$style(
                                      HTML(".shiny-notification {position:fixed;top: calc(50%);left: calc(50%);}"
                                      )
                                  )
                              )
                ),
                box(width = 9,
                    dataTableOutput(outputId = 'cancel'))
                )
        )
            )# End of tabItems
)# End of body

#Side bar
sidebar<-dashboardSidebar(
    collapsed = TRUE,
    div(textOutput('Welcome'), style = 'padding: 20px'),
    sidebarMenu(width=70,
                ############## Add a new tab for login page #############################
                menuItem(text='Login', tabName='Login', icon=icon('users')),
                menuItem(text='Room Status Update', tabName='Room_Status_Update',icon=icon('table')),
                menuItem(text='Room booking', tabName='Room_booking',icon=icon('table')), 
                menuItem(text= "My Booked Room", tabName='Room_booked',icon=icon('table'))))

# Define UI for application that draws a histogram
ui <- dashboardPage(
    header = dashboardHeader(tags$li(class = "dropdown", style = "padding: 8px;",
                                     shinyauthr::logoutUI("logout")),
                             tags$li(class = "dropdown", 
                                     tags$a(icon("github"), 
                                            href = "https://github.com/paulc91/shinyauthr",
                                            title = "See the code on github"))),
    sidebar = sidebar,
    body = body
)


# Define server logic required to draw a histogram
server <- shinyServer(function(input,output,session){
    ############################# Database setting ############################
    library(RMySQL)
    
    
    #You can now read from the database I created remotely(based on a free server)
    
    options(edwinyu = list(
        "host" = "db4free.net",
        "port" = 3306,
        "user" = "edwinyu",
        "password" = "Edwinyrl2019"
    ))
    
    
    loadData <- function(database,table) {
        db <- dbConnect(MySQL(), dbname = database, host = options()$edwinyu$host, 
                        port = options()$edwinyu$port, user = options()$edwinyu$user, 
                        password = options()$edwinyu$password)
        query <- sprintf("SELECT * FROM %s", table)
        data <- dbGetQuery(db, query)
        dbDisconnect(db)
        data
    }
    
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
9am_10am=VALUES(9am_10am),10am_11am=VALUES(10am_11am),11am_12pm=VALUES(11am_12pm),12pm_1pm=VALUES(12pm_1pm),1pm_2pm=VALUES(1pm_2pm),
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
    

    
    update_booking <- function(room_no,booker,date,time,booking_no,database,table){
        db <- dbConnect(MySQL(), dbname = database, host = options()$edwinyu$host, 
                        port = options()$edwinyu$port, user = options()$edwinyu$user, 
                        password = options()$edwinyu$password)
        query=paste0("INSERT INTO ",table," VALUES ('",booking_no,"','",room_no,"','",booker,"','",date,"','",time,"') ON DUPLICATE KEY UPDATE room_no=VALUES(room_no),booker=VALUES(booker),date=VALUES(date),time=VALUES(time)")
        print(query)
        dbGetQuery(db, query)
        dbDisconnect(db)
        
    }
    
    delete_booking <- function(booking_no,database,table) {
        db <- dbConnect(MySQL(), dbname = database, host = options()$edwinyu$host, 
                        port = options()$edwinyu$port, user = options()$edwinyu$user, 
                        password = options()$edwinyu$password)
        
        query <- paste0('DELETE FROM ',table," WHERE booking_no = '",booking_no,"'")
        
        
        print(query)#for debug
        dbSendQuery(db, query)
        dbDisconnect(db)
    }
    
    individual <- loadData('room_avail','individual_information')
    individual_rb <- loadData('room_avail','new_room_status')
    #####################################################################################
    # call the logout module with reactive trigger to hide/show
    logout_init <- callModule(shinyauthr::logout, 
                              id = "logout", 
                              active = reactive(credentials()$user_auth))
    
    # call login module supplying data frame, user and password cols
    # and reactive trigger
    login_info <- data.frame(ID=individual$UserName,Password=individual$Password)
    
    credentials <- callModule(shinyauthr::login, 
                              id = "login", 
                              data = login_info,
                              user_col = ID,
                              pwd_col = Password,
                              sodium_hashed = FALSE,
                              log_out = reactive(logout_init()))
    
    ####################################################################################
    # pulls out the user information returned from login module
    user_data <- reactive({credentials()$info})
    time <- Sys.time()
    
    a <- data.frame(date=rep(NA,5),day = c("Mon","Tue","Wed","Thu", "Fri"))
    date_to_weekday<-function(date){format(as.Date(date), format="%a")}
    if(date_to_weekday(Sys.Date()) %in% c("Sun","Sat") ){
        d=Sys.Date()
        while( date_to_weekday(d) %in% c("Sun","Sat") ){
            d=d+1
        }
        while( !(date_to_weekday(d) %in% c("Sun","Sat")) ){
            a[a$day==date_to_weekday(d),]$date = as.character(d)
            d=d+1
        }
        
    }else{
        
        d=Sys.Date()+7
        while( !(date_to_weekday(d) %in% c("Sun","Sat")) ){
            d=d-1
        }
        d=d+1
        while( !(date_to_weekday(d) %in% c("Sun","Sat")) ){
            a[a$day==date_to_weekday(d),]$date = as.character(d)
            d=d+1
        }
        
    }   
    
    output$personal <- DT::renderDataTable({
        updated_room=loadData('room_avail','new_room_status')
        updated_room[updated_room$Room_no==individual$RoomNumber[individual$UserName==user_data()$ID],]
    },
    options = list(scrollX = TRUE)
    )
    
    output$cancel <- DT::renderDataTable({
        room_booked=loadData("room_avail","room_booked")
        room_booked$day <- date_to_weekday(room_booked$date)
        room_booked[room_booked$booker==user_data()$ID,]
        
    })
    
    ## 1. Save Updated Room information   
    
    observeEvent(input$save1, 
                 {   
                     abbr=function(x){substr(x, 1, 3)}
                     key=abbr(input$day1)
                     date_update=a[a$day==key,]$date
                     
                     available_time1 <- ""
                     for(j in 1:length(input$time1)){
                         available_time1 <- paste(available_time1,input$time1[j],sep = "")
                     }
                     
                     if(available_time1=="am"){
                         am = "am"
                     }else if(available_time1=="pm"){
                         am = "pm"
                     }else if(available_time1=="ampm"){
                         am = "both"
                     }else{
                         am = "neither"
                     }
                     rt=loadData('room_avail','new_room_status')
                     r_no=individual$RoomNumber[user_data()$ID==individual$UserName]
                     
                     if(nrow(rt[rt$Room_no==r_no & rt$Date==date_update, ])==0){
                         
                         update_status(use="write",room_no = r_no,
                                       date = date_update,
                                       am = am,database='room_avail',table = 'new_room_status')
                         
                         
                         
                         
                     }else{
                         if(any(as.matrix(rt[rt$Room_no==r_no & rt$Date==date_update, ]=="Booked"))){
                             
                             showNotification("Someone has booked your room already. Please contact admin.",type="error",duration=30,closeButton = TRUE)
                             
                             
                         }else{
                             update_status(use="write",room_no = r_no,
                                           date = date_update,
                                           am = am,database='room_avail',table = 'new_room_status') 
                         }
                         
                     }
                     
                     
                     output$personal <- DT::renderDataTable({
                         updated_room=loadData('room_avail','new_room_status')
                         updated_room = updated_room[updated_room$Room_no==individual$RoomNumber[individual$UserName==user_data()$ID],]
                         updated_room},
                         options = list(scrollX = TRUE)
                     )
                 })    

    
    ## 2. search for available room information
    
    observeEvent(input$search,
                 {
                     
                     req(input$day2)
                     room_table=loadData('room_avail','new_room_status')
                     abbr=function(x){substr(x, 1, 3)}
                     avail_am=c()
                     avail_pm=c()
                     avail_both=c()
                     for(i in 1:nrow(room_table)){
                         avail_am[i]=any(room_table[i,2:4]=="Available")
                         avail_pm[i]=any(room_table[i,5:9]=="Available")
                         avail_both[i]=any(room_table[i,2:9]=="Available")
                     }
                    
                     search_time=paste(input$time2,collapse ="")
                     
                     if(search_time=="ampm"){
                         avail=avail_both
                     }else if(search_time=="am"){
                         avail=avail_am
                     }else if(search_time=="pm"){
                         avail=avail_pm
                     }else{
                         avail=rep(TRUE,nrow(room_table))
                             }
                     
                     
                     if(input$day2=="All"){
                         key=abbr( c("Monday","Tuesday","Wednesday","Thursday", "Friday"))
                         selected_rt=room_table[room_table$Weekday %in% key & avail & room_table$Date %in% a$date, ]
                     }else{
                         key=abbr(input$day2)
                         selected_rt=room_table[room_table$Weekday==key & avail &  room_table$Date %in% a$date, ]
                     }
                     
                     output$all <- DT::renderDataTable({
                         selected_rt[selected_rt$Room_no!=individual$RoomNumber[individual$UserName==user_data()$ID],]
                         },
                         options = list(scrollX = TRUE))
                     
                     
                
                 })
    
    ## 3. book the room

    
    
    observeEvent(input$book,
                 {
                     rt=loadData('room_avail','new_room_status')
                     
                     
                     List=strsplit(input$time3,"-")
                     for(i in 1:length(List) ){
                             List[[i]]=paste(List[[i]],collapse = "_")
                          }
                     
                     intervals=unlist(List)
                     
                     
                     
                     
                     #The mechanism of booking supports only booking one room at a time
                     #User chooses day and (multiple) time slots to make the booking
                     #If no single room satisfying all requirements, a notification is shown to let users select fewer time slots at a time
                     candidate=rt[ apply( as.matrix(rt[,intervals]=="Available"), 1, all) & rt$Date==a[a$day==substr(input$day3, 1, 3),]$date & rt$Room_no!=individual$RoomNumber[individual$UserName==user_data()$ID] & rt$Room_no == input$room_book_no,]
                     if(nrow(candidate)==0){
                         if(length(intervals)>1){
                             showNotification("No room is available for all time slots of your choice. Try selecting fewer time slots at a time.",type="warning",closeButton = TRUE,duration=30)
                             
                         }else if(length(intervals)==1){
                             
                             showNotification("No room is available for this time slot so far.",type="warning",closeButton = TRUE,duration=30)
                
                         }
                         
                     }else{
                         req(input$day3)
                         req(input$time3)
                         room_no=candidate[1,]$Room_no
                         day=substr(input$day3, 1, 3)
                         room_status=candidate[1,2:9]
                         
                         
                         for(i in intervals){
                             room_status[[i]]="Booked"
                         }
                         update_status(use="booking",room_no = room_no,
                                       date = a[a$day==day,]$date, 
                                       avail=matrix(unname(unlist(room_status)),nrow=1),
                                       database='room_avail',table = 'new_room_status')
                         room_confirm=paste("You have successfully booked room",room_no,"\n","(",a[a$day==day,]$date,",",day,",",paste(input$time3,collapse=' and '),")",sep="")
                         showNotification(room_confirm,type="message",duration=30,closeButton = TRUE)
                         
                         update_booking(room_no=input$room_book_no,booker=user_data()$ID,date = a[a$day==day,]$date,time=input$time3,booking_no = as.integer(Sys.time()),"room_avail","room_booked")
                     }
                         output$cancel <- DT::renderDataTable({
                             room_booked=loadData("room_avail","room_booked")
                             room_booked$day <- date_to_weekday(room_booked$date)
                             room_booked[room_booked$booker==user_data()$ID,]
                     
                 })
                         
        })
    
    ## 4.cancel the room booking
    observeEvent(input$cancel,
                 {
                     room_booked <- loadData("room_avail","room_booked")
                     rt <- loadData('room_avail','new_room_status')
                     
                     abbr=function(x){substr(x, 1, 3)}
                     key=abbr(input$day1)
                     date_update=a[a$day==key,]$date
                     
                     if(input$booking_no %in% room_booked$booking_no){
                     delete_Info <- room_booked[room_booked$booking_no==input$booking_no,]
                     
                     delete_booking(input$booking_no,"room_avail","room_booked")
                     avail <- rt[rt$Date==delete_Info$date,]
                     avail <- avail[avail$Room_no==delete_Info$room_no,]
                     avail[1,delete_Info$time] <- "Available"
                     update_status(use = "booking",room_no = individual$RoomNumber[individual$UserName==user_data()$ID], date = date_update,avail = avail,database = "room_avail",table = "new_room_status" )
                     
                     output$cancel <- DT::renderDataTable({
                         room_booked=loadData("room_avail","room_booked")
                         room_booked$day <- date_to_weekday(room_booked$date)
                         room_booked[room_booked$booker==user_data()$ID,]
                         
                     })
                     
                     current <- weekdays(as.POSIXct(Sys.Date()), abbreviate = F)
                     cancel_confirm=paste("You have successfully canceled the booked room",input$room_book_no2,"\n","(",current,",",input$day4,",",paste(input$time4,collapse=' and '),")",sep="")
                     showNotification(cancel_confirm,type="message",duration=30,closeButton = TRUE)
                     }else{
                         booking_no_error <- paste("Your booking reference number does not exist, recheck it please!")
                         showNotification(booking_no_error,type="message",duration=30,closeButton = TRUE)
                     }
                     
                 })
    
}) 

# Run the application 
shinyApp(ui = ui, server = server)





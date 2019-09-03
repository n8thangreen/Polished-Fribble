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
                fluidRow(box(width=3,height = 10,
                             h4("UCL Statistical Science Department Room Update System"),
                             selectInput("day1",label = "Day",
                                        choices = c("Monday" = "Monday","Tuesday" = "Tuesday","Wednesday" = "Wednesday",
                                                    "Thursday" = "Thursday", "Friday" = "Friday"),selected = ""),
                             checkboxGroupInput("time1","Time",choices = c("AM","PM"),selected = ""),
                             actionButton("save1","Save",width = "25%")
                                 ),
                         box(width = 9,height = 15,
                             dataTableOutput(outputId = 'personal'))
                          )),
        tabItem(tabName = "Room_booking",
                # header
                fluidRow(box(width=3,height = 10,
                             h4("Search for the available room information"),
                             selectInput("day2",label = "Day",
                                         choices = c("Monday" = "Monday","Tuesday" = "Tuesday","Wednesday" = "Wednesday",
                                                     "Thursday" = "Thursday", "Friday" = "Friday","All"="All"),selected = "All"),
                             checkboxGroupInput("time2","Time",choices = c("AM","PM"),selected = ""),
                             actionButton("search","Search",width = "25%"),
                             
                             h4("Book your room here:"),
                             selectInput("day3",label = "Day",
                                         choices = c("Monday" = "Monday","Tuesday" = "Tuesday","Wednesday" = "Wednesday",
                                                     "Thursday" = "Thursday", "Friday" = "Friday","All"="All"),selected = "All"),
                             checkboxGroupInput("time3","Time",choices = c("9am-10am","10am-11am","11am-12am","12am-1pm","1pm-2pm",
                                                                           "2pm-3pm","3pm-4pm","4pm-5pm"),selected = ""),
                             actionButton("book","Book",width = "25%")
                             ),
                        box(width = 9,height = 15,
                            dataTableOutput(outputId = 'all'))
                ))
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
                menuItem(text='Room booking', tabName='Room_booking',icon=icon('table'))))  

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
    
    ## 1. Save Updated Room information
    observeEvent(input$save1, 
                 {   
                     
                     current <- weekdays(as.POSIXct(Sys.Date()), abbreviate = F)
                     a <- data.frame(number=1:5,day = c("Monday","Tuesday","Wednesday","Thursday", "Friday"))
                     if(current=="Monday"){date_update <- Sys.Date()+(a$number[a$day==input$day1]-1)+7}
                     else if(current=="Tuesday"){date_update <- Sys.Date()+(a$number[a$day==input$day1]-2)+7}
                     else if(current=="Wednesday"){date_update <- Sys.Date()+(a$number[a$day==input$day1]-3)+7}
                     else if(current=="Thursday"){date_update <- Sys.Date()+(a$number[a$day==input$day1]-4)+7}
                     else if(current=="Friday"){date_update <- Sys.Date()+(a$number[a$day==input$day1]-5)+7}
                     
                     available_time1 <- ""
                     for(j in 1:length(input$time1)){
                         available_time1 <- paste(available_time1,input$time1[j],sep = "")
                     }
                     
                     if(available_time1=="AM"){
                         am = "am"
                     }else if(available_time1=="PM"){
                         am = "pm"
                     }else if(available_time1==" AMPM"){
                         am = "both"
                     }else{
                         am = "neither"
                     }
                     
                     update_status(use="write",room_no = individual$RoomNumber[user_data()$ID==individual$UserName],
                            date = date_update,
                            am = am,database='room_avail',table = 'new_room_status')
                     
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
                
                 })
    
    ## 3. book the room
    observeEvent(input$book,
                 {
                     
                 })
    
}) 

# Run the application 
shinyApp(ui = ui, server = server)


# 1. updates on current week data
# 2. historical data records
# 3. overview of other people's data
# 4. Realize the above by db









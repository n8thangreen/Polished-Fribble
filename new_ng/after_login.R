
# rewrite of the 2019 version
# N Green
# June 2020


library(shiny)
library(shinyauthr)     # login authentication
library(shinyjs)
library(tidyverse)      # Querying dataset
library(sodium)         # Hashing Passwords with 'sodium'
library(shinydashboard)
library(jsonlite)
library(rjson)          # Define a json data format for data storage and querying
library(RSQLite)
library(RJSONIO)
library(RSQLite)
library(DBI)
library(lubridate) 


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
                    
                    tabPanel("My Room Status", DT::dataTableOutput("user_table")),
                    # actionButton("change_schedule", "Click Me to Adjust Schedule")
                    HTML('<div data-iframe-height></div>')
                    ##################################################################################
                ))),
        tabItem(tabName = "my_room",
                # header
                fluidRow(box(width=3,height = 15,
                             h4("Import your room status"), 
                             selectInput("choice", label = "Choice",
                                         choices = c("Room Status Update" = "rsu",
                                                     "Room Booking" = "rb"),
                                         selected = "rsu"),
                             conditionalPanel(
                                 condition = "input.choice == 'rsu'",
                                 selectInput("day1",label = "Day",
                                             choices = c("Monday"    = "Monday",
                                                         "Tuesday"   = "Tuesday",
                                                         "Wednesday" = "Wednesday",
                                                         "Thursday"  = "Thursday",
                                                         "Friday"    = "Friday"),
                                             selected = ""),
                                 checkboxGroupInput("time1","Time",
                                                    choices = c("AM","PM"),
                                                    selected = ""),
                                 textInput("notes1", label = "Notes", value = ""),
                                 actionButton("save1", "Save", width = "20%")
                             ),
                             conditionalPanel(
                                 condition = "input.choice == 'rb'",
                                 selectInput("day2",label = "Day",
                                             choices = c("Monday"    = "Monday",
                                                         "Tuesday"   = "Tuesday",
                                                         "Wednesday" = "Wednesday",
                                                         "Thursday"  = "Thursday",
                                                         "Friday"    = "Friday",
                                                         "All" = "All"),
                                             selected = "All"),
                                 selectInput("room",
                                             label = "The room you want to check:",
                                             choices = c("1","2","all"),
                                             selected = "all"),
                                 checkboxGroupInput("time2","Time",
                                                    choices = c("AM","PM"),
                                                    selected = ""),
                                 actionButton("search", "Search", width = "20%")
                                 
                             )),
                         box(width = 6, height = 10,
                             conditionalPanel(condition = "input.choice == 'rsu'",
                                              dataTableOutput(outputId = 'personal')),
                             conditionalPanel(condition = "input.choice == 'rb'",
                                              dataTableOutput(outputId = 'all'))
                         )))
    )# End of tabItems
)# End of body

sidebar <- dashboardSidebar(
    collapsed = TRUE,
    div(textOutput('Welcome'), style = 'padding: 20px'),
    sidebarMenu(width = 70,
                ############## Add a new tab for login page #############################
                menuItem(text = 'Login',
                         tabName = 'Login',
                         icon = icon('users')),
                menuItem(text = 'Stats Department Room Booking System',
                         tabName = 'my_room',
                         icon = icon('table'))))  

# Define UI for application that draws a histogram
ui <- dashboardPage(
    header = dashboardHeader(tags$li(class = "dropdown",
                                     style = "padding: 8px;",
                                     shinyauthr::logoutUI("logout")),
                             tags$li(class = "dropdown", 
                                     tags$a(icon("github"), 
                                            href = "https://github.com/paulc91/shinyauthr",
                                            title = "See the code on github"))),
    sidebar = sidebar,
    body = body
)


# Define server logic required to draw a histogram
server <- shinyServer(
    function(input,
             output,
             session) {
        
        ## load functions
        source("R/booker_functions.R")
        
        db <- dbConnect(SQLite(), 'room_booker.sqlite')
        
        dbWriteTable(db, "room_table", individual_rs, overwrite = TRUE)
        dbReadTable(db,'room_table')
        
        #####################################################################################
        # call the logout module with reactive trigger to hide/show
        logout_init <- callModule(shinyauthr::logout, 
                                  id = "logout", 
                                  active = reactive(credentials()$user_auth))
        
        # call login module supplying data frame, user and password cols
        # and reactive trigger
        login_info <- data.frame(ID = individual$UserName,
                                 Password = individual$Password)
        
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
        
        output$user_table <- renderTable({
            # use req to only render results when credentials()$user_auth is TRUE
            req(credentials()$user_auth)
            # print(credentials$user_auth)
            # print(user_data()$ID)
            timetable_data[individual_rs$ID == user_data()$ID, ]
        })
        
        ## Save Updated Room information
        observeEvent(input$save1, {
            
            current_day <- weekdays(as.POSIXct(Sys.Date()), abbreviate = FALSE)
            
            dayoftheweek <- setNames(1:5, c("Monday","Tuesday","Wednesday","Thursday","Friday"))
            new_day_num <- unname(dayoftheweek[input$day1])
            today_num <- unname(dayoftheweek[current_day])
            
            # changed this so its within 7 days
            # rather than always next week
            date_update <- 
                switch(new_day_num,
                       "Monday"    = today() + ifelse(today_num > 1, 8 - today_num,  1 - today_num),
                       "Tuesday"   = today() + ifelse(today_num > 2, 9 - today_num,  2 - today_num),
                       "Wednesday" = today() + ifelse(today_num > 3, 10 - today_num, 3 - today_num),
                       "Thursday"  = today() + ifelse(today_num > 4, 11 - today_num, 4 - today_num),
                       "Friday"    = today() + 5 - today_num)
        
            
            available_time1 <- ""
            for(j in 1:length(input$time1)){
                available_time1 <- paste(available_time1,input$time1[j], sep = "\n")
            }
            
            #Update or Create?
            if(date_update%in%individual_rs[individual_rs$ID == individual$RoomNumber[individual$UserName == user_data()$ID], ]$Date){
                update(individual$RoomNumber[user_data()$ID == individual$UserName],
                       date_update,weekdays(as.POSIXct(date_update), abbreviate = FALSE),
                       available_time1,input$notes1)
            }else{
                delete(individual$RoomNumber[user_data()$ID == individual$UserName],
                       date_update)
                create(individual$RoomNumber[user_data()$ID == individual$UserName],
                       date_update,weekdays(as.POSIXct(date_update), abbreviate = FALSE),
                       available_time1,input$notes1)
            }
            
            output$personal <- renderDataTable({
                updated_room = loadData()
                updated_room = updated_room[updated_room$ID == individual$RoomNumber[individual$UserName == user_data()$ID], ]
                updated_room
            })
        })
        
        observeEvent(input$search, {   
            available_time2 <- ""
            for (j in 1:length(input$time2)) {
                available_time2 <- paste(available_time2,input$time2[j], sep = "\n")}
            
            output$all <- renderDataTable({
                all_room <- loadData()
                if (input$day2 == "All") {
                    if (input$room == "All") { 
                        all_room1 <- all_room
                    } else {
                        all_room1 <- all_room[all_room$ID == input$room, ]}
                    
                    all_room2 <- all_room1[all_room1$Available_period == available_time2, ]
                    return(all_room2)
                }
                else {
                    if (input$room=="All") { 
                        all_room1 <- all_room
                    } else {
                        all_room1 <- all_room[all_room$ID == input$room, ]}
                    
                    all_room2 <- all_room1[all_room1$Available_period == available_time2, ]
                    all_room3 <- all_room2[all_room2$Day == input$day2, ]
                    return(all_room3)
                }
            })
        })
    }) 

# Run the application 
shinyApp(ui = ui,
         server = server)


# 1. updates on current_day week data
# 2. historical data records
# 3. overview of other people's data
# 4. Realize the above by db


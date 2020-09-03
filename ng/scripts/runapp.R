library(shiny)

Sys.setenv(CREDENTIALS_INFO_ID = "Simon")
Sys.setenv(CREDENTIALS_AUTH = TRUE)
# Sys.setenv(SHINYPROXY_USERGROUPS = "shinyroom")

shiny::runApp("after_login_v7.5.R")
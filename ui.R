#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(rworldmap)
library(data.table)
library(countrycode)
library(stringr)
library(dplyr)

# Let's set up the ui

ui <- 
  dashboardPage(skin = "black",
    dashboardHeader(title = "World terrorism: 1970 - 2015 terrorist attacks", titleWidth = 480,
                    tags$li(a(href = 'https://github.com/colladete/most-profitable-movies',
                              "Get the code",
                              icon("download"),
                              style = "padding-top:10px; padding-bottom:10px;"),
                            class = "dropdown"),
                    tags$li(a(href = 'http://www.justminingdata.com',
                      img(src = 'http://justminingdata.com/wp-content/uploads/2017/04/just-mining.png',
                          title = "Company Home", height = "15px"),
                      style = "padding-top:10px; padding-bottom:10px;"),class = "dropdown")),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      fluidRow(
        column(width = 9,
               box(width = NULL, title = "It might take some seconds to load the map", solidHeader = TRUE, status = "warning",
                   leafletOutput("worlmap", height = 500))),
        column(width = 3,
               box(width = NULL, status = "warning", tags$h5("I have filtered to display only the year 2015. If you want to extend the years without filtering the other options, it might take some seconds (even minutes) to load."),
                   sliderInput("year",
                               "Filter by years:",
                               min = 1970,
                               max = 2015,
                               value = c(2015, 2015)),
                   selectInput("typeattack", label = "Type of attack", 
                               choices = c("All", "Bombing/Explosion", 
                                           "Armed Assault",
                                           "Assassination",
                                           "Unknown",
                                           "Hostage Taking (Kidnapping)",
                                           "Hostage Taking (Barricade Incident)",
                                           "Facility/Infrastructure Attack",
                                           "Hijacking", "Unarmed Assault"), selected = "All"),
                   selectInput("numdead", label = "Number of dead people", 
                               choices = c("All" = "All", "0" = "0", 
                                           "1" = "1",
                                           "Between 2 and 10"  = "2 and 10",
                                           "Between 11 and 100" = "11 and 100",
                                           "Between 101 and 500" = "101 and 500",
                                           "More than 500" = "more 500"), selected = "All"),
                   textInput("orgname",
                             "Organization name", value = "")
                   
                   
                   
               )
        )
        )
      )
)
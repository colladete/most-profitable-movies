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
library(stringr)
library(purrr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(dplyr)

# Define UI for application that draws a histogram
ui <- 
  dashboardPage(
    dashboardHeader(title = "Movie profit"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(4,
           box(width = NULL, solidHeader = TRUE,
           textInput("movname",
                     "Enter a movie title to find out its profit", value = ""),
           tableOutput("movprofit")
    )),
    
    # Show a plot of the generated distribution
    column(8,
           box(width = NULL, solidHeader = TRUE,
           sliderInput("year",
                       "Filter by years:",
                       min = 1920,
                       max = 2016,
                       value = c(1920, 2016), width = 600),
           tableOutput("table")
    )),
    
    column(12, box(width = NULL, title = "Profit by genre", status = "primary", solidHeader = TRUE, plotOutput("plot"))
  )
)
)
)

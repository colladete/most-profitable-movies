#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(purrr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(dplyr)

#First of all, let's create the dataframe
movie_metadata <- read.csv("movie_metadata.csv", stringsAsFactors = F)
movie_metadata <- as_data_frame(movie_metadata)

#How does it look like? 
glimpse(movie_metadata)

movie_metadata2 <- movie_metadata %>%
  mutate(movie_title = str_trim(movie_title)) %>%
  filter(!is.na(budget) & !is.na(gross))

glimpse(movie_metadata2)

movie_metadata3 <- movie_metadata2 %>%
  separate(genres, into = c("genre1", "genre2", "genre3", "genre4", "genre5", "genre6", "genre7", "genre8"), 
           sep = "[|]", fill = "right")

movie_metadata4 <- movie_metadata3[!duplicated(movie_metadata3$movie_title),]

movie_metadata4 <- movie_metadata4 %>%
  filter(!is.na(budget) & !is.na(gross))

calculate.movie.profit <- function(movie) {
  mov_profit <- (movie_metadata4$gross[movie_metadata4$movie_title == movie]/
                   movie_metadata4$budget[movie_metadata4$movie_title == movie])
  return(mov_profit)
}

movie_profit_data <- data_frame(movie_title = movie_metadata4$movie_title, 
                                profit = map_dbl(movie_metadata4$movie_title, calculate.movie.profit),
                                year = movie_metadata4$title_year, genre1 = movie_metadata4$genre1, 
                                genre2 = movie_metadata4$genre2, genre3 = movie_metadata4$genre3, 
                                genre4 = movie_metadata4$genre4, genre5 = movie_metadata4$genre5,
                                genre6 = movie_metadata4$genre6, genre7 = movie_metadata4$genre7,
                                genre8 = movie_metadata4$genre8, budget = movie_metadata4$budget, 
                                gross = movie_metadata4$gross)

genrelist <- unique(unlist(movie_profit_data[4:11]))

genrelist <- genrelist[-c(10,18,24)]



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("What are the most profitable movies?"),
  br(),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(4,
           textInput("movname",
                     "Enter a movie title to find out its profit", value = ""),
           tableOutput("movprofit")
    ),
    
    # Show a plot of the generated distribution
    column(8,
           sliderInput("year",
                       "Filter by years:",
                       min = 1920,
                       max = 2016,
                       value = c(1920, 2016), width = 600),
           tableOutput("table")
    ),
    
    column(12, plotOutput("plot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  films_sel <-  reactive({ movie_profit_data[grep(tolower(input$movname), tolower(movie_profit_data$movie_title)),] %>%
  mutate('Movie title' = movie_title, 'Profit' = profit) %>%
  select(`Movie title`, Profit)
    
  })
  
  output$movprofit <- renderTable({
    
    if (input$movname == "") {
      head(films_sel(), n = 0)
      
    } else {
      
      head(films_sel(), n = 5)
    }
      
  })
  
  most.prof <- reactive({
    
    movie_profit_data %>%
      filter(year >= input$year[1] & year <= input$year[2]) %>%
      mutate(Genre = ifelse(is.na(genre2), genre1, paste(genre1, genre2, sep = ",")),
             'Movie title' = movie_title, Profit = profit, Year = year, 'Budget in $' = as.integer(budget),
             'Gross in $' = gross) %>%
      select(`Movie title`, Profit, Year, Genre, `Budget in $`, `Gross in $`) %>%
      arrange(desc(Profit))
    
  }) 
  
  output$table <- renderTable({head(most.prof(), n = 10)})
  
  
  output$plot <- renderPlot({
    
    movie_profit_data2 <- movie_profit_data %>%
      filter(year >= input$year[1] & year <= input$year[2])
    
    genreplot = data.frame()
    
    for (i in seq_along(genrelist)) {
      
      genreplot[i,1] = genrelist[i]
      genreplot[i,2] = mean(movie_profit_data2$profit[which(movie_profit_data2$genre1 == genrelist[i] | movie_profit_data2$genre2 == genrelist[i] |
                                                              movie_profit_data2$genre3 == genrelist[i] | movie_profit_data2$genre4 == genrelist[i] |
                                                              movie_profit_data2$genre5 == genrelist[i] | movie_profit_data2$genre6 == genrelist[i] |
                                                              movie_profit_data2$genre7 == genrelist[i] | movie_profit_data2$genre8 == genrelist[i])])
      
    }
    
    names(genreplot) = c("Genre", "Average profit")
    
   ggplot(genreplot, aes(x = Genre, y = `Average profit`)) +
                    geom_bar(stat = "identity") +
     theme_economist()
  
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


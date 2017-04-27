#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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


server <- function(input, output) { 
  
  # We read and save the dataset and the map. 
  
  terrorism <- fread("globalterrorismdb_0616dist.csv")
  
  terrorism <- as_data_frame(terrorism)
  
  newmap <-getMap()
  
  # Now I am going to make sure that both the map and the dataset have the same codes for the countries
  # I am also including some that were missing
  
  newmap$countrycode <- as.character(newmap$ISO_A3)
  terrorism$countrycode <- countrycode(terrorism$country_txt, "country.name", "iso3c")
  terrorism$countrycode[terrorism$country_txt == "Kosovo"] <- "KOS"
  terrorism$countrycode[terrorism$country_txt == "Serbia-Montenegro"] <- "SRB" 
  terrorism$countrycode[terrorism$country_txt == "North Yemen" | terrorism$country_txt == "International"] <- "YEM"
  
  # I am going to keep the following columns
  
  terrorism2 <- terrorism %>%
    select(iyear, country_txt, countrycode, city, latitude, longitude, attacktype1, 
           attacktype1_txt, targtype1, targtype1_txt, natlty1_txt, gname, weaptype1,
           weaptype1_txt, nkill, nwound)  
  
  
  output$worlmap <- renderLeaflet({
    
    terrorism3 <- terrorism2 %>%
      filter(iyear >= input$year[1] & iyear <= input$year[2])
    
    if(input$typeattack != "All") {
      terrorism3 <- terrorism3[terrorism3$attacktype1_txt == input$typeattack,]}
    
    if (input$numdead == "0") {
      terrorism3 <- terrorism3[terrorism3$nkill == 0,]
    } else if(input$numdead == "1") {
      terrorism3 <- terrorism3[terrorism3$nkill == 1,]
    } else if(input$numdead == "2 and 10") {
      terrorism3 <- terrorism3[terrorism3$nkill > 1 & terrorism3$nkill <= 10,]
    } else if(input$numdead == "11 and 100") {
      terrorism3 <- terrorism3[terrorism3$nkill > 10 & terrorism3$nkill <= 100,]
    }  else if(input$numdead == "101 and 500") {
      terrorism3 <- terrorism3[terrorism3$nkill > 100 & terrorism3$nkill <= 500,]
    } else if(input$numdead == "more 500") {
      terrorism3 <- terrorism3[terrorism3$nkill > 500,]
    } else {
      terrorism3 <- terrorism3[terrorism3$nkill >= 0,]
    }  
    
    if (!input$orgname == "") {
      
      terrorism3 <- terrorism3[grep(input$orgname, terrorism3$gname, ignore.case = T),]
      
    }
    
    
    terrorism3$size <- ifelse(terrorism3$nkill == 0, 1,
                              ifelse(terrorism3$nkill == 1, 2,
                                     ifelse(terrorism3$nkill > 1 & terrorism3$nkill <= 10, 3,
                                            ifelse(terrorism3$nkill > 10 & terrorism3$nkill <= 100, 4, 
                                                   ifelse(terrorism3$nkill > 100 & terrorism3$nkill <= 500, 5, 6)))))
    
    
    
    for (i in seq_along(terrorism3$size)) {
      
      terrorism3$popupyear[i] <- paste(sep = ": ",
                                       "<b>Year</b>", terrorism3$iyear[i])
      terrorism3$popupcountry[i] <- paste(sep = ": ",
                                          "<b>Country</b>", terrorism3$country_txt[i])
      terrorism3$popupcity[i] <- paste(sep = ": ",
                                       "<b>City</b>", terrorism3$city[i])
      terrorism3$popupname[i] <- paste(sep = ": ",
                                       "<b>Name of organization</b>", terrorism3$gname[i])
      terrorism3$popuptype[i] <- paste(sep = ": ",
                                       "<b>Type of attack</b>", terrorism3$attacktype1_txt[i])
      terrorism3$popupnkill[i] <- paste(sep = ": ",
                                        "<b>Number of people killed</b>", terrorism3$nkill[i])
      
      terrorism3$popup[i] <- paste(sep = "<br/>", terrorism3$popupyear[i], terrorism3$popupcountry[i], terrorism3$popupcity[i],
                                   terrorism3$popupname[i], terrorism3$popuptype[i], terrorism3$popupnkill[i])
      
    }
    
    terrorism4 <- terrorism3 %>%
      group_by(countrycode) %>%
      summarise(nattacs = n())
    
    numattacs <- double()
    
    for(i in seq_along(newmap$ScaleRank)){
      
      numattacs[i] <- ifelse(as.character(newmap$ISO_A3[i]) %in% terrorism4$countrycode, 
                             terrorism4$nattacs[terrorism4$countrycode == as.character(newmap$ISO_A3[i])], 0)
      
    }
    
    newmap@data$nattacs <- numattacs
    
    my_colors <- palette(c("#458b00", "#b4eeb4", "#f0fff0", "#fa8072", "#ff3030", "#b22222"))
    pal <- colorNumeric(
      palette = my_colors,
      domain = newmap$nattacs)
    
    leaflet(newmap) %>% 
      addProviderTiles(providers$Thunderforest.Transport) %>%
      setView(-5.273437, 43.068888, zoom = 2) %>%
      addPolygons(stroke = FALSE, fillOpacity = 0.5, 
                  smoothFactor = 0.5, color = ~pal(nattacs)) %>%
      addCircleMarkers(~terrorism3$longitude, ~terrorism3$latitude, 
                       popup = ~terrorism3$popup, 
                       label = ~as.character(terrorism3$iyear), radius = (~terrorism3$size)) %>%
      addLegend(pal = pal, values = ~newmap$nattacs, title = "Number of attacks")
    
  })       
  
}  


library(shiny)
library(dplyr)
library(ggplot2)
library(mapview)
library(sf)
library(leaflet)
library(shinydashboard)
library(hrbrthemes)
library(forcats)
library(sp)
library(rgdal)




teste <- read_csv("data/teste.csv")
teste.cartao <- read_csv("data/teste1.csv")

linhas <- st_read("data/linhas/linhas.shp", crs = 4326) 
linhas.paradas <- st_read("data/linhas_paradas/linhas_paradas1.shp", crs = 4326)

teste1 <- teste %>%
  group_by(linha) %>%
  summarise(total_valid = sum(n)) %>%
  arrange(desc(total_valid)) %>%
  mutate(rank = 1:n())




# Define a server for the Shiny app
function(input, output) {
  
  # Fill in the spot we created for a plot
  output$Volume <- renderPlot({
    
    media.total <- teste %>%
      group_by(hora) %>%
      summarise(n = sum(n)) %>%
      mutate(total = sum(n)) %>%
      mutate(prop = n/total)
    
    label.hora <- function(x) {
      paste(x, "h", sep = "")
    }
    
    # Render 
    teste %>%
      filter(linha == input$linha) %>%
      #mutate(HORA1 = hms::as.hms(HORA)) %>%
      ggplot(aes(x = hora, y =prop ))+
      labs(x = "Hora", y = "Frequência", subtitle = "A linha representa a média horária de todas as linhas")+
      geom_col(fill = "grey85", color = "black")+
      geom_line(data = media.total, aes(x = hora, y = prop))+
      geom_point(data = media.total, aes(x = hora, y = prop))+
      scale_x_continuous(breaks = c(0:23), labels = paste0(0:23, "h"))+
      scale_y_percent()+
      theme_ipsum()
      #scale_x_time(breaks = '1 hour', minor_breaks = '5 min')+
      
    
    
  })
  
  # Mapa
  
  icons <- awesomeIcons(
    icon = "bus",
    library = "fa")
  
  output$Mapa <- renderLeaflet({
    leaflet() %>%
      #addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(data = filter(linhas, linha == input$linha)) %>%
      addAwesomeMarkers(data = filter(linhas.paradas, linha == input$linha), group = "Paradas", icon = icons) %>%
      addLayersControl(overlayGroups = "Paradas",
                       options = layersControlOptions(collapsed = FALSE)) %>%
      addMiniMap()
  })
  
  #infobox


  output$progressBox <- renderInfoBox({
    infoBox(
      title = "Validações", 
      value = paste("Total de ", teste1[which(teste1$linha == input$linha),2]$total_valid[1], " passageiros por dia", sep = ""), 
      icon = icon("address-book"),
      color = "purple"
    )
  })
  
  output$rank <- renderInfoBox({
    infoBox(
      title = "Rank", 
      value = paste("É a ", teste1[which(teste1$linha == input$linha),3]$rank[1], "ª linha mais utilizada", sep = ""),
      icon = icon("trophy"),
      color = "purple"
    )
  })
  
  output$cartao <- renderPlot({
    
    # Render 
    teste.cartao %>%
      filter(linha == input$linha) %>%
      ggplot(aes(x = fct_reorder(tipo_cartao, n), y =n ))+
      labs(x = "Tipo de Pagamento", y = "Frequência")+
      geom_col(fill = "grey85", color = "black")+
      coord_flip()+
      theme_ipsum()
    #scale_x_time(breaks = '1 hour', minor_breaks = '5 min')+
    
    
    
  })
}




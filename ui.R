

library(shiny)
# library(mapview)
library(sf)
library(shinydashboard)
#library(semantic.dashboard)
library(readr)
library(leaflet)
library(dplyr)

linhas <- st_read("data/linhas_v2/linhas_v2.shp", crs = 4326) %>%
  mutate(sentido = stringr::str_sub(lnh_snt, -1, -1)) %>%
  mutate(linha_vai = as.character(linha))



teste <- read_csv("data/teste.csv")




header <- dashboardHeader(
  title = div(icon("bus"), " Linhas de Transporte Público de Fortaleza"),
  #title = "Linhas de Transporte Público de Fortaleza",
  titleWidth = 500
)

body <- dashboardBody(
  fluidRow(
    column(width = 6,
           box(width = NULL, 
               solidHeader = TRUE,
               title = "Itinerário da Linha",
               leafletOutput("Mapa", height = 700)
           ),
           infoBoxOutput("progressBox"),
           infoBoxOutput("rank")

    ),
    column(width = 6,
           box(width = NULL, 
               status = "warning",
               selectInput(inputId = "linha", 
                           label = "Escolha a linha:", 
                           choices = unique(linhas$linha), 
                           selected = "200 - ANTONIO BEZERRA/CENT"),
               checkboxGroupInput(inputId = "teste",
                           label = "Selecione o sentido:",
                           choices = c("Ida" = "I", 
                                       "Volta" = "V"),
                           selected = "Ida")
                           ),
           box(width = NULL, 
               status = "warning",
               title = "Distribuição das viagens ao longo do dia:",
               plotOutput("Volume")),
           box(width = NULL,
               status = "warning",
               title = "Validações por tipo de pagamento",
               plotOutput("cartao")))
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
)


dashboardPage(
  title = "Linhas de Transporte Público de Fortaleza",
  header,
  dashboardSidebar(disable = T),
  body,
  skin = "black"
)



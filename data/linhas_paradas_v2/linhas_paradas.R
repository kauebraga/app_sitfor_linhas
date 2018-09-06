
library(dplyr)
library(sf) #pra importar os dados espaciais e tal
library(purrr)
library(data.table)
library(readr)
library(tidyr)
library(ggplot2)
library(forcats)
library(hrbrthemes)
library(mapview)
library(RColorBrewer)
library(ggthemes)
library(stringr)
library(ggthemes)
library(lubridate)

library(rgdal)
library(spgwr)
library(dfrtopics)

library(patchwork)

library(RANN)

# abrir paradas

stops <- read_delim("/home/kaue/Documents/transporte_publico/data/gtfs/stops.txt", delim=",") %>% 
  select(stop_id, stop_name, lon = stop_lon, lat = stop_lat)

# abrir stop times

stop_times <- read_delim("/home/kaue/Documents/transporte_publico/data/gtfs/stop_times.txt", delim = ",") %>%
  select(trip_id, stop_id, arrival_time, departure_time, stop_sequence)

# abrir trips

trips <- read_delim("/home/kaue/Documents/transporte_publico/data/gtfs/trips.txt", delim = ",") %>%
  select(trip_id, shape_id)


# Fazer integração das bases:


stops_linhas <- stop_times %>%
  left_join(trips, by = "trip_id") %>%
  filter(!is.na(arrival_time)) %>% #tirar as paradas que não tem hora para chegada ou saída
  distinct(shape_id, stop_id) %>%
  mutate(linhaa = as.integer(str_sub(shape_id, 6, 8))) %>%
  left_join(opaa, by = c("linhaa" = "linha_exc")) %>%
  mutate(linha_sentido = paste(linhaa, stringr::str_sub(shape_id, 9, 19), sep = "")) %>%
  left_join(stops, by = "stop_id") 

# salvar

stops_linhas %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_write("data/linhas_paradas_v2/linhas_paradas_v2.shp")



# tratar linhas -----------------------------------------------------------

opaa <- st_read("data/linhas/linhas.shp") %>%
  as_tibble() %>%
  select(-geometry) %>%
  na.omit()

st_read("/home/kaue/Documents/transporte_publico/data/linhas/linhas.shp", crs = 4326) %>%
  mutate(linhaa = as.integer(str_sub(shape_id, 6, 8))) %>%
  left_join(opaa, by = c("linhaa" = "linha_exc")) %>%
  mutate(linha_sentido = paste(linhaa, stringr::str_sub(shape_id, 9, 19), sep = "")) %>%
  na.omit() %>%
  st_write("data/linhas_v2/linhas_v2.shp")


eca <-   
st_read("/home/kaue/Documents/transporte_publico/data/linhas/linhas.shp", crs = 4326) %>%
  mutate(linhaa = as.integer(str_sub(shape_id, 6, 8))) %>%
  left_join(opaa, by = c("linhaa" = "linha_exc")) %>%
  mutate(linha_sentido = paste(linhaa, stringr::str_sub(shape_id, 9, 19), sep = "")) %>%
  na.omit()

sfc_as_cols <- function(x, names = c("x","y")) {
  x <- st_cast(x, "POINT")
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}


oia <- sfc_as_cols(eca)

oia %>%
  group_by(linha_sentido) %>%
  mutate(x1 = lead(x),
         y1 = lead(y)) %>%
  slice(-n()) %>%
  slice(seq(1, n(), 10)) %>%
  as_tibble() %>%
  write_csv("arrows.csv")



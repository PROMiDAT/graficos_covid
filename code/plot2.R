library(tidyverse)
library(sf)
library(ggtext)
library(glue)


# Carga datos necesario para el mapa --------------------------------------
mapa <- read_sf("datos/geo_data/cantones/cantones.shp")
mapa

# Calcular centros de cada canton -----------------------------------------
centros <- st_centroid(mapa) 
mapa <- cbind(mapa, st_coordinates(st_centroid(centros$geometry)))


# Registro de casos confirmados  ------------------------------------------

casos <- read_csv("datos/casos_cantones.csv")
casos


# Unir mapa y casos confirmados -------------------------------------------

mapa <- left_join(mapa, casos, by = c("cant_nom2" = "canton"))
mapa



# Calcular el poligono de la cantidad de infectados por canton ------------
crear_triangulos <- function(data) {
  value <- data$confirmados * 1500
  d = data.frame(
    x = c(data$X-1500, data$X+1500, data$X),
    y = c(data$Y, data$Y, data$Y+value),
    id = rep(data$cant_nom2,3))
  return(d)
}

infectados <- mapa %>% 
  filter(confirmados > 0) %>% 
  transpose() %>% 
  map_df(.f = ~crear_triangulos(.))


agregar_canton <- function(canton, altura = 1500, hjust = 0, size = 3.5) {
  geom_richtext(data = subset(mapa, cant_nom2 == canton), 
                mapping = aes(x = X, y = Y + confirmados*altura,
                              label = glue("{cant_nom2}<br><span style='color:#dd2c00'>{confirmados} casos</span>")),
                fill = NA, label.color = NA, hjust = hjust, size = size) 
}


ggplot(data = mapa) +
  geom_sf(fill = "#f7f5f0", color = "gray50", size = .2) +
  geom_polygon(data = infectados,
               mapping = aes(x = x, y = y, group = id),
               color = "#222831",
               fill = "#dd2c00",
               size = 0.07) +
  agregar_canton("San José") +
  agregar_canton("Desamparados", size = 2.5, altura = 1400) +
  agregar_canton("Alajuela", hjust = 1) +
  agregar_canton("San Carlos",altura = 1200, hjust = 1, size = 2.5) +
  agregar_canton("Santa Ana", hjust = 1, size = 2.5) +
  agregar_canton("Garabito", altura = 1000, hjust = 1, size = 2.5) +
  agregar_canton("Osa", size = 2.5) +
  agregar_canton("Limón", size = 2.5) +
  agregar_canton("Curridabat", size = 2.5) +
  agregar_canton("Coto Brus", size = 2.5) +
  agregar_canton("Pérez Zeledón", size = 2.5) +
  agregar_canton("San Ramón", size = 2.5, hjust = 1, altura = 1200) +
  agregar_canton("Santa Cruz", size = 2.5) +
  agregar_canton("Nicoya", size = 2.5) +
  agregar_canton("Liberia", size = 2.5) +
  labs(title = "Casos confirmados de Covid-19 por cantón, Costa Rica",
       subtitle = "Datos actualizados al 22 de abril del 2020",
       caption = "Fuente de datos: geovision.uned.ac.cr",
       fill = "Casos Confirmados") +
  theme_void() +
  theme(
    plot.subtitle = element_text(size= 8)
  )

ggsave("mapa2.png", width = 8, dpi = 300, device = "png")

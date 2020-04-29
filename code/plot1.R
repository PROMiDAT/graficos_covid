
# Dependencias ------------------------------------------------------------
library(tidyverse)
library(sf)
library(leaflet)

casos_canton <- read_csv("data/casos_por_canton.csv")

mapa_cantones <- read_sf("data/geo_data/cantones/cantones.shp")
mapa_cantones

mapa_cantones <- left_join(mapa_cantones, casos_canton, by = c("cant_nom2" = "canton"))
mapa_cantones

mapa_cantones <- mapa_cantones %>% 
  mutate(
    confirmados = if_else(is.na(confirmados), 0, confirmados),
    grupo = case_when(
      confirmados == 0 ~ "Sin Casos",
      confirmados > 0 & confirmados <= 5 ~ "5",
      confirmados > 5 & confirmados <= 10 ~ "10",
      confirmados > 10 & confirmados <= 30 ~ "30",
      confirmados > 30 & confirmados <= 70 ~ "70",
      confirmados > 70  ~ "+70"),
    grupo = factor(x = grupo,
                   levels = c("Sin Casos","5","10","30","70","+70"),
                   ordered = T)
    )

mis_colores <- c("#e7f0fa",'#86b277', '#babf70', '#d2ab58', '#ce7732', '#a90000')

ggplot(data = mapa_cantones, mapping = aes(fill = grupo)) +
  geom_sf(color = "white", size = .2) +
  theme_void() +
  labs(title = "Casos confirmados de Covid-19 por cantón, Costa Rica",
       subtitle = "Datos actualizados al 22 de abril del 2020",
       caption = "Fuente de datos: geovision.uned.ac.cr",
       fill = "Casos Confirmados") +
  scale_fill_manual(values = mis_colores,
                    guide = guide_legend(
                      direction = "horizontal",
                      keyheight = unit(3, units = "mm"),
                      keywidth = unit(10, units = "mm"),
                      title.position = 'top',
                      title.theme = element_text(size = 8),
                      title.hjust = 0.5,
                      label.hjust = 1,
                      nrow = 1,
                      label.position = "bottom")
  ) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 15, hjust = 0),
        plot.subtitle = element_text(size = 12),
        text = element_text(size = 12),
        plot.caption = element_text(margin = margin(b = 10, t = 10)))

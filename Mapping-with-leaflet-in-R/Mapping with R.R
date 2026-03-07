##################################################
######     Mapping with R                   ######
######    Author: César Quezada             ######
##################################################

# limpiamos el workspace
rm(list = ls())
# Limpiamos la consola
cat("\014")
# fijamos a UTF-8
options(encoding = "utf-8")

# Cambiamos el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# (1) instalando paquetería necesia
# ---------------------------------

# install.packages("leaflet")
# install.packages("dplyr")
library(leaflet)
library(dplyr)


# (1) Creando el Mapa
# --------------------

# -12.062189, -77.148771


m <- leaflet() %>%
  # agregamos mosaicos
  addTiles() %>%  
  # Establecemos las coordenadas y el nivel de zoom
  setView(lng=-77.148771, lat=-12.062189, zoom = 16) %>%
  # agreguamos un marcador con una ventana emergente, 
  addMarkers(lng=-77.148771, lat=-12.062189, popup="<b>Hello</b><br><a href='https://www.realfelipe.com/'>-Me</a>")
m


# (2) Varias ubicaciones desde un csv
# -----------------------------------
library(readr)
incidentes <- read_csv("datos/incidentes_viales_2019.csv")


# resumen por meses
incidentes %>% select(SINIESTRO, AÑO, MES) %>%
  group_by(AÑO,MES) %>% summarise(n = n())

# resumentes portipo de vehiculo
incidentes %>% filter(MES == 'ENERO') %>%
  select(SINIESTRO, MES,`TIPO VEHICULO`) %>% 
  group_by(MES,`TIPO VEHICULO`) %>% summarise(n = n())


# obtenemos un resumen de los puntos donde hubo un incidente de motocicleta 
# en el mes de enero 2019
incidentes %>% 
  filter(MES == 'ENERO' & `TIPO VEHICULO` == 'Motocicleta') %>% 
  summarise(n = n())

# obtenemos la base filtradas de accidentes
df_mtcy <- incidentes %>% 
  filter(MES == 'ENERO' & `TIPO VEHICULO` == 'Motocicleta')

str(df_mtcy$LATITUD)

# Eliminamos valores NAs
df_mtcy$LATITUD <- as.numeric(df_mtcy$LATITUD)
df_mtcy$LONGITUD <- as.numeric(df_mtcy$LONGITUD)

df_mtcy$LATITUD[df_mtcy$LATITUD == 0] = NA
df_mtcy$LONGITUD[df_mtcy$LONGITUD == 0] = NA

# names(df_mtcy)
df_mtcy <- df_mtcy %>% select(SINIESTRO, LATITUD,LONGITUD)
df_mtcy <- na.omit(df_mtcy)

median(df_mtcy$LATITUD)
median(df_mtcy$LONGITUD)


# Generando el mapa accidentes en motos
m <- leaflet(df_mtcy) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  setView(-99.39516, 20.57261, zoom = 7) %>% 
  addCircles(~LONGITUD, ~LATITUD, weight = 3, radius=40, 
             color="#ffa500", stroke = TRUE, fillOpacity = 0.8) 
m


# Generando el mapa accidentes en motos por segmentos
# ---------------------------------------------------

# resumentes portipo de vehiculo
incidentes %>% filter(MES == 'ENERO') %>%
  select(SINIESTRO, MES,`TIPO VEHICULO`) %>% 
  group_by(MES,`TIPO VEHICULO`) %>% summarise(n = n())


# obtenemos un resumen de los puntos donde hubo un incidente de motocicleta 
# en el mes de enero 2019
incidentes %>% 
  filter(MES == 'ENERO' & (`TIPO VEHICULO` == 'Motocicleta'|`TIPO VEHICULO` == 'Cami?n')) %>% 
  summarise(n = n())

# obtenemos la base filtradas de accidentes
df_accidentes <- incidentes %>% 
  filter(MES == 'ENERO' & (`TIPO VEHICULO` == 'Motocicleta'|`TIPO VEHICULO` == 'Cami?n'))

str(df_accidentes$LATITUD)

# Eliminamos valores NAs
df_accidentes$LATITUD <- as.numeric(df_accidentes$LATITUD)
df_accidentes$LONGITUD <- as.numeric(df_accidentes$LONGITUD)

df_accidentes$LATITUD[df_accidentes$LATITUD == 0] = NA
df_accidentes$LONGITUD[df_accidentes$LONGITUD == 0] = NA

# names(df_accidentes)
df_accidentes <- df_accidentes %>% select(SINIESTRO, LATITUD,LONGITUD, `TIPO VEHICULO`)
df_accidentes <- na.omit(df_accidentes)

median(df_accidentes$LATITUD)
median(df_accidentes$LONGITUD)


# creamos una paleta de colores
cof <- colorFactor(c("#13ED3F","#ffa500"),domain=c("Motocicleta", "Cami?n"))
# mapping based on type
m <- leaflet(df_accidentes) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  setView(-99.58197, 19.67011, zoom = 7) %>% 
  addCircleMarkers(~LONGITUD, ~LATITUD, popup=df_accidentes$`TIPO VEHICULO`, weight = 3, radius=4, 
                   color=~cof(`TIPO VEHICULO`), stroke = F, fillOpacity = 0.8) 
m


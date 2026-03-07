
# gganimate
# ---------

# Este artículo describe cómo crear animaciones en R usando el paquete gganimate R.
# gganimate es una extensión del paquete ggplot2 para crear ggplots animados. 
# Proporciona una gama de nuevas funciones que se pueden agregar al objeto de trazado 
# para personalizar cómo debe cambiar con el tiempo.

# Características clave de gganimate:

# transiciones : desea que sus datos cambien
# vistas : quieres que tu punto de vista cambie
# sombras : quieres que la animación tenga memoria


# https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/


# clean the workspace
rm(list = ls())
cat("\014")
# fijamos a UTF-8
options(encoding = "utf-8")



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


# (1) CARGANDO LIBRERÍAS NECESARIAS
# ### #############################
library(ggplot2) # paquete gráfico
library(gganimate) # paquete integrado de animaciones
library(plotly) # paquete gráfico
library(dplyr) # paquete para tratamiento de datos
library(gapminder) # marco de datos gapminder o "tibble" para análisis estadístico
library(babynames) # marco de datos de nacimiento de niños.
library(hrbrthemes) # compilación de temas, escalas y utilidades adicionales de ggplot2.
#devtools::install_github('thomasp85/transformr') # complementos de gganimate


# (2) GRAFICO DE CAJAS
# ### ################
# ### ################

# Analizando BBDD mtcars:
# Una de las bases de datos que proporciona el software R. 
# Los datos se extrajeron de la revista Motor Trend de EE. UU. De 1974,
# y comprenden el consumo de combustible y 10 aspectos del diseño y 
# rendimiento del automóvil para 32 automóviles (modelos 1973-74).
# https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/mtcars

View(mtcars)
str(mtcars)

# Vamos a medir la relación del nro de millas logradas por 
# el número de cilindros que posee el carro.

# Grafico Estático
# ----------------
p <- ggplot(mtcars, aes(factor(cyl), mpg)) + 
  geom_boxplot(aes(fill = cyl)) 
p

# p <- plot_ly(data = gapminder, x = ~ lifeExp, color = ~ continent,
#         type="box")

# Grafico Dinámico
# ----------------

# transition_time(). La duración de la transición entre los estados 
# se establecerá para que se corresponda con la diferencia de tiempo real 
# entre ellos.
p <- ggplot(mtcars, aes(factor(cyl), mpg)) + 
  geom_boxplot(aes(fill = cyl)) 
p + transition_time(gear)


# (3) GRAFICO DE PUNTOS
# ### #################
# ### #################

# Analizando BBDD gapminder:

# La fundación Gapminder7 es una organización sin fines de lucro con sede en Suecia
# que promueve el desarrollo global mediante el uso de estadísticas que pueden 
# ayudar a reducir mitos comunes e historias sensacionalistas sobre la salud y la 
# economía mundial. Una selección importante de datos ya está cargada en la librería
# gapminder
# https://cran.r-project.org/web/packages/gapminder/README.html

View(gapminder)

# Vamos a cruzar las variables: esperanza de vida al nacer con el PBI percapita
# por cada país

# Grafico Estático
# ----------------
p <- ggplot(gapminder %>% filter(year == 1952), 
            aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(title = 'Year: 1952',x = "GDP per capita", y = "Life expectancy")
p

# Grafico Dinámico
# ----------------

# frame_time. Da el tiempo al que corresponde el fotograma actual.
p <- ggplot(gapminder, 
            aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p + transition_time(year) + labs(title = "Year: {frame_time}")



# Creando facets por continente:
# -----------------------------

# GRÁFICO ESTÁTICO
# ----------------
p <- ggplot(gapminder %>% filter(year == 1952),
            aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  labs(title = 'Year: 1952', x = 'GDP per capita', y = 'life expectancy')
p


# GRÁFICO DINÁMICO
# ----------------  
p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy')
p + transition_time(year) +
  ease_aes('linear')



# (4) GRAFICO DE LINEAS 
# ### #################
# ### #################

# Analizando BBDD airquality:
View(airquality)
# Base de datos que contiene información de las medidas de calidad del aire de Nueva York.
# Proporciona mediciones diarias de la calidad del aire en Nueva York, de mayo a septiembre de 1973.
# https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/airquality


# GRAFICO ESTÁTICO
# -----------------
# Analizamos la temperatura comparando los dias a lo largo de los meses

p <- ggplot(airquality,aes(Day, Temp, group = Month, color = factor(Month))) +
  geom_line() + scale_color_viridis_d() +
  labs(x = "Day of Month", y = "Temperature") +
  theme(legend.position = "top")
p


# GRAFICO DINÁMICO
# ----------------

# transition_reveal. Revelar datos a lo largo de una dimensión determinada
p <- ggplot(airquality,aes(Day, Temp, group = Month, color = factor(Month))) +
  geom_line() + scale_color_viridis_d() +
  labs(x = "Day of Month", y = "Temperature") +
  theme(legend.position = "top")
p + geom_point() + transition_reveal(Day)



# (5) GRAFICO DE LINEAS 
# ### #################
# ### #################

#  Analizando BBDD babynames:
# Este paquete contiene un conjunto de datos del mismo nombre que contiene 
# el número de niños y niñas nacidos cada año desde 1880 con cada nombre.

View(babynames)

# Seleccionamos 3 nombres
don <- babynames %>% 
  filter(name %in% c("Ashley", "Patricia", "Helen")) %>%
  filter(sex=="F")
don

# GRAFICO ESTÁTICO
# -----------------
p <- ggplot(don, aes(x=year, y=n, group=name, color=name)) + 
  geom_line() + geom_point() +
  ggtitle("Popularidad de nombres estadounidenses en los 30 años anteriores") +
  theme_ipsum() +
  ylab("Numero de bebes nacidas")
p


# GRAFICO DINÁMICO
# ----------------
p <- ggplot(don, aes(x=year, y=n, group=name, color=name)) + 
  geom_line() + geom_point() +
  ggtitle("Popularidad de nombres estadounidenses en los 30 años anteriores") +
  theme_ipsum() +
  ylab("Numero de bebes nacidas")
p + transition_reveal(year)



# (6) GRAFICO DE BARRAS
# ### ##################
# ### ##################

# Vamos a crear un diagrama de barras de la temperatura media por mes:

mean.temp <- airquality %>%
  group_by(Month) %>%
  summarise(Temp = mean(Temp))
mean.temp

# GRAFICO ESTÁTICO
# -----------------
p <- ggplot(mean.temp, aes(Month, Temp, fill = Temp)) +
  geom_col() +
  scale_fill_distiller(palette = "YIGnBu", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE)
p

# GRAFICO DINAMICO
# -----------------

# transition_states. Transición entre varias etapas distintas de los datos

p <- ggplot(mean.temp, aes(Month, Temp, fill = Temp)) +
  geom_col() +
  scale_fill_distiller(palette = "YIGnBu", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE)
p + transition_states(Month, wrap = FALSE) +
  shadow_mark()


# (7) GRAFICO DE DENSIDADES
# ### #####################
# ### #####################

table(gapminder$year)

# Vamos a crear un gráfico de densidad de la esperanza de vida a través
# de los años:


# GRAFICO ESTÁTICO
# -----------------

# Utilizamos filtros para mostrar el gráfico para un año en específico.

p <- gapminder %>% filter(year == 1952)  %>% 
  ggplot(aes(lifeExp, fill = factor(year))) + 
  geom_density(alpha= 0.2) + 
  xlab("Life Expectancy") + 
  ylab("Kernal density") +
  guides(fill = guide_legend(title = "Year"))
p


# GRAFICO DINÁMICO
# ----------------
p <- gapminder %>% 
  ggplot(aes(lifeExp, fill = factor(year))) + 
  geom_density(alpha= 0.2) + 
  xlab("Life Expectancy") + 
  ylab("Kernal density") +
  guides(fill = guide_legend(title = "Year"))
p + transition_time(year) + labs(title = "Year: {frame_time}")



# (8) GUARDAR ANIMACIÓN
# ### -----------------

# Si necesita guardar la animación para su uso posterior, puede utilizar la función anim_save().
# Funciona de forma muy similar ggsave() de ggplot2 y captura automáticamente la última animación 
# renderizada si no especifica una directamente.

anim_save("figures/graf_densidades.gif", animation = last_animation(), path = NULL)






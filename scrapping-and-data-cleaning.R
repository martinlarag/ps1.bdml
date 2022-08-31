
# PS 1 BD&ML --------------------------------------------------------------

## Paula Alarcón
## Martín Lara
## Nicolás

### Punto 1, scrapping y limpieza de datos


# Scrapping de datos GEIH -------------------------------------------------

#se cargan los paquetes necesarios
require(pacman)
p_load(tidyverse, rvest)

#Se crea un vector con los links de las páginas dónde están los datos
url_base <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html")
url_base #visualización del vector

#Iterando en cada uno de los urls de la página se descargan los datos  y se juntan en un data frame
dt <- data.frame()
for (url in url_base) {
  print(url)
  temp <- read_html(url) %>% 
    html_table() 
  temp <- as.data.frame(temp[[1]])
  dt <- rbind(dt, temp)
}
dt #el data frame resultante tiene 178 variables y 32177 observaciones

# Limpieza de datos -------------------------------------------------------

dt_mayores <- dt %>% subset(age>=18) #depuramos a todos los menores de edad
sum(apply(dt_mayores, 1, anyNA))#contamos las filas que contienen NAs



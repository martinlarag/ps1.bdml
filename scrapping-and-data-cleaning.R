<<<<<<< Updated upstream

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


=======
rm(list=ls())
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

write.csv(dt, file = "data_ps1.csv")

# Limpieza de datos -------------------------------------------------------

dt_mayores <- dt %>% subset(age>=18) #depuramos a todos los menores de edad
sum(apply(dt_mayores, 1, anyNA))#contamos las filas que contienen NAs

na_count <-sapply(dt_mayores, function(y) sum(length(which(is.na(y)))))#creo una función para saber cuantos NAs hay por columna 
na_cols <- as.data.frame(cbind(colnames(dt_mayores), na_count))#creamos un dataframe con el número de NAs por columna y sus nombres

sum(dt_mayores$iof2, dt_mayores$iof2es)

dt_sin_jubilados <- dt_mayores %>% subset(iof2==0)#depuramos a las personas qque han recibido ingresos por pensión para mantener solamente personas que reciben sus ingresos por trabajo o subsidios y ayudas



>>>>>>> Stashed changes

rm(list=ls())
# PS 1 BD&ML --------------------------------------------------------------

## Paula Alarcón
## Martín Lara
## Nicolás Gonzalez

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

dt_emp <- dt %>% subset(age>=18) %>% 
  subset(dsi==0)#depuramos a todos los menores de edad

sum(apply(dt_emp, 1, anyNA))#contamos las filas que contienen NAs

na_count <-sapply(dt_emp, function(y) sum(length(which(is.na(y)))))#creo una función para saber cuantos NAs hay por columna 
na_cols <- as.data.frame(cbind(colnames(dt_emp), na_count))#creamos un dataframe con el número de NAs por columna y sus nombres

na_cols <- na_cols %>% mutate(na_count/length(dt_emp))

p_load(heatmaply)
heatmaply_na(
  dt_emp[1:30, ],
  showticklabels = c(TRUE, FALSE)
)



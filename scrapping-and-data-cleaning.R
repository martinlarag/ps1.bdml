#############################################################################
#
#              Big Data & Machine Learning for Applied Economics                   
#                               Problem Set 1
#                                   Grupo: 
#                      Paula Alarcón    Código: 201812516
#                      Martín Lara      Código: 20171...
#                      Nicolás González Código: 201813698
#
#############################################################################
#
#                   Punto 1: scrapping y limpieza de datos
#
#############################################################################

# Scrapping de datos GEIH ---------------------------------------------------

# Limpio mi lugar de trabajo
rm(list=ls())

# Asigno un directorio donde me guarde los resultados: 
setwd(choose.dir())

#se cargan los paquetes necesarios
require(pacman)
p_load(tidyverse, rvest, heatmaply, plyr, dplyr, stargazer)

#Se crea un vector con los links de las páginas dónde están los datos
url_base <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html")
url_base #visualización del vector

#Iterando en cada uno de las urls de la página se descargan los datos y se juntan en un data frame
dt <- data.frame()
for (url in url_base) {
  print(url)
  temp <- read_html(url) %>% 
    html_table() 
  temp <- as.data.frame(temp[[1]])
  dt <- rbind(dt, temp)
}
View(dt) #el data frame resultante tiene 178 variables y 32177 observaciones

write.csv(dt, file = "data_ps1.csv")

# Limpieza de datos ------------------------------------------------------------

dt_emp <- dt %>% subset(age>=18) %>% 
  subset(dsi==0)#depuramos a todos los menores de edad

#Sacamos el porcentaje de missing values por variable
na_percentage <-sapply(dt, function(y) sum(length(which(is.na(y))))/length(dt$directorio))#creo una función para saber cuantos NAs hay por columna 
data_x <- as.data.frame(na_percentage)

# En un priomer proceso de eliminación de variables, eliminamos aquellas con un 
#alto porcentaje de missing values
var <- cbind(Var_name = rownames(data_x), data_x)
rownames(var) <- 1:nrow(var)
var_for_drop <- var[var$na_percentage>=0.45,]
var_for_keep <- var[var$na_percentage<0.45,]
count(var) # Contamos cuantas variables tenemos en total (=178)
count(var_for_keep) # Contamos cuantas variables tienen % missing menor o igual a 45% (=48)
count(var_for_drop) # Contamos cuantas variables tienen % missing mayor a 45% (=130)

# Elimino de mi base de datos aquellas variables con alto porcentaje de NAS
var_drop <- list(var_for_drop$Var_name)
# Acá hice una lista con mis nombre de variables quer quiero dropear pero no me deja HELP
unlist(var_drop, recursive = TRUE, use.names = TRUE)
dt_new <- dt %>% select(- one_of(c(as.character(var_drop))))

# Estadisticas descriptivas ------------------------------------------------------------
stargazer(dt, type='text')

dt_income <- dt %>% select(directorio,estrato1,sex,age,ingtot,college,ingtot,ingtotes,ingtotob,cuentaPropia,totalHoursWorked)


# carga de library VIM para obtener función kNN ----------------------------------------
library(VIM)
newdata <-  kNN(airquality, variable = c("Ozone", "Solar.R"), k = 6)
summary(newdata)

#############################################################################
#
#              Big Data & Machine Learning for Applied Economics                   
#                               Problem Set 1
#                                   Grupo: 
#                      Paula Alarcón    Código: 201812516
#                      Martín Lara      Código: 20171...
#                      Nicolás González Código: 201813698
#
#############################################################################
#
#                      Punto 2: Age-Earnings Profile
#
#############################################################################

# 1) Variables relevantes ---------------------------------------------------
dt_earnings <- dt %>% select(directorio,estrato1,sex,age,ingtot,college,ingtot,ingtotes,ingtotob,cuentaPropia,totalHoursWorked)

# Creo una nueva variable de edad al cuadrado:

dt_earnings <- dt_earnings %>%
  mutate(
    age2 = age**2
  )
#



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
p_load(tidyverse, rvest, heatmaply, plyr, dplyr, stargazer, VIM, boot)

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
#Creamos una base con las variables de interés

dt_total <- dt %>% select(college, cotPension, cuentaPropia, totalHoursWorked, ingtotob, ingtotes, ingtot, directorio, p6580s1, p6630s6, p6760, p6750, p7500s1a1, p7500s2a1, p7510s5a1, maxEducLevel, oficio, p6426 , age, sex, dsi, fex_c, iof1es, iof2es, iof6es, hoursWorkUsual, estrato1)

#Filtramos a los menores de edad y a los desempleados

dt_interes <- dt_total %>% subset(age>=18) %>% 
  subset(dsi==0)#depuramos a todos los menores de edad

#Sacamos el porcentaje de missing values por variable
na_percentage <-sapply(dt_interes, function(y) sum(length(which(is.na(y))))/length(dt_interes$directorio))#creo una función para saber cuantos NAs hay por columna 
data_x <- as.data.frame(na_percentage)
View(na_percentage)


# En un primer proceso de eliminación de variables, eliminamos aquellas con un 
#alto porcentaje de missing values
var <- cbind(Var_name = rownames(data_x), data_x)
rownames(var) <- 1:nrow(var)
var_for_drop <- var[var$na_percentage>=0.45,]
var_for_keep <- var[var$na_percentage<0.45,]
count(var) # Contamos cuantas variables tenemos en total (=27)
count(var_for_keep) # Contamos cuantas variables tienen % missing menor o igual a 45% (=19)
count(var_for_drop) # Contamos cuantas variables tienen % missing mayor a 45% (=8)

#Seleccionamos las variables que cumplen con el requisito y sacamos estadísticas descriptivas
dt_final <- dt_interes %>% select(age, college, cotPension, cuentaPropia, directorio, dsi, estrato1, fex_c, hoursWorkUsual, ingtot, ingtotob, maxEducLevel, oficio, p6426, p7500s1a1, p7500s2a1, p7510s5a1, sex, totalHoursWorked)
View(dt_final)
stargazer(dt_final, type='latex')

#Imputamos Missing Values y comparamos estadísticas descriptivas
dt_imputado <-  kNN(dt_final, variable = c("cotPension", "hoursWorkUsual", "p6426", "totalHoursWorked"), k = 6)
summary(dt_imputado)
stargazer(dt_imputado, type='latex')
                       
####### PENDIENTE HACE GRÁFICOS #######                       


                       
                       
                       

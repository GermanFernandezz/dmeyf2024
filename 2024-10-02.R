

library(ggplot2)
library(tidyr)
library(dplyr)



#Generar dataset a partir de datos de subidas a kaggle
semillas <- as.data.frame(list("semilla" <- c("s111667","s245083","s517717","s552749","s571433"),
                               "e8000" <- c(91.814,89.271,88.781,89.434,91.371),
                               "e8500" <- c(92.794,90.857,91.371,88.407,94.101),
                               "e9000" <- c(94.754,94.474,93.097,90.834,93.354),
                               "e9500" <- c(96.754,94.871,96.994,93.401,95.197),
                               "e10000" <- c(97.647,96.107,95.291,93.844,95.384),
                               "e10500" <- c(99.420,93.984,98.137,93.634,94.427),
                               "e11000" <- c(98.106,92.514,99.140,93.074,92.631),
                               "e11500" <- c(99.630,94.451,98.417,91.581,92.071),
                               "e12000" <- c(97.764,93.727,99.397,94.451,91.324),
                               "e12500" <- c(101.824,95.057,96.737,97.600,94.427),
                               "e13000" <- c(99.934,96.504,96.947,95.361,94.637)
                               )
                          )

#Por las dudas, para que el nombre de las columnas estén bien
colnames(semillas) <- c("semilla", "e8000", "e8500", "e9000", "e9500", "e10000", "e10500", "e11000", "e11500", "e12000", "e12500", "e13000")


#Saco vector con el nombre de las columnas menos la primera que es la semilla. La voy a usar para hacer el promedio de cada envio y poder graficar la curva promedio
columnas <- colnames(semillas[,-c(1)]) 

#Para ir juntando los valores de los proemdios de cada envio
medias <- list()

#HAgo los promedios de cada envio y lo agrego a medias
for (col in columnas) {
  medias[[col]] <- mean(semillas[[col]], na.rm = TRUE)
}

# Imprimir resultados
medias

#Hago vector con las medias por envioa
promedios <- c("medias", medias$e8000, medias$e8500, medias$e9000, medias$e9500, medias$e10000, medias$e10500, medias$e11000, medias$e11500, medias$e12000, medias$e12500, medias$e13000)

# Agrego la media al data frame con todos los resultados
semillas <- rbind(semillas, promedios)

# TODO LO QUE VIENE AHORA ES PARA TENER LOS DATOS EN UN FORMATO QUE LE SIRVA A GGPLOT

# Paso las semillas a id de cada fila
rownames(semillas) <- semillas$semilla
semillas <- semillas[,-c(1)]

# Transpongo el data frame y agrego nueva columna con la cantidad de envios
semillas_t <- as.data.frame(t(semillas))
semillas_t$envios <- c(8000,8500,9000,9500,10000,10500,11000,11500,12000,12500,13000)


# Convertir la matriz de formato ancho a largo
matriz_larga <- pivot_longer(
  data = semillas_t,
  cols = colnames(semillas_t[-c(7)]),  # Selecciona todas las columnas que empiezan con "serie_"
  names_to = "semilla",            # Nombre de la columna que almacenará los nombres de las series
  values_to = "ganancia"                # Nombre de la columna que almacenará los valores
)

# Ver la matriz en formato largo
print(matriz_larga)

matriz_larga$ganancia <- as.numeric(matriz_larga$ganancia)



# Crear el gráfico
ggplot(matriz_larga, aes(x = envios, y = ganancia, color = semilla, group = semilla)) +
  geom_point() +  # Agrega los puntos
  geom_line() +   # Conecta los puntos con líneas
  labs(title = "Ganancia en Kaggle con diferentes semillas", ylab ="Ganancia")

str(matriz_larga)










  

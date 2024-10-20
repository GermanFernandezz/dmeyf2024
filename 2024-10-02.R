

library(ggplot2)
library(tidyr)
library(dplyr)



#Generar dataset a partir de datos de subidas a kaggle
semillas <- as.data.frame(list("semilla" <- c("s218551","s228913","s247759","s338761","s370207","s397849","s548837"),
                               "e8000" <- c(82.084, 90.414,87.357,92.351,87.614,87.334,90.834),
                               "e8500" <- c(85.677, 92.211,89.901,95.407,92.141,90.344,94.381),
                               "e9000" <- c(91.277, 98.790,91.114,97.880,97.764,87.637,98.977),
                               "e9500" <- c(94.917, 100.004,91.371,99.000,96.084,87.031,97.297),
                               "e10000" <- c(95.477,99.980,92.071,97.554,96.131,91.464,95.851),
                               "e10500" <- c(96.340,99.770,92.281,97.530,96.807,93.097,96.924),
                               "e11000" <- c(100.937,98.884,91.931,94.101,96.994,98.604,96.014),
                               "e11500" <- c(101.450,95.734,92.864,92.561,95.431,98.510,96.900),
                               "e12000" <- c(103.760,99.234,94.031,91.324,95.734,99.397,97.320),
                               "e12500" <- c(102.057,99.210,94.451,90.064,96.061,100.797,97.367),
                               "e13000" <- c(102.897,96.667,95.897,88.197,99.887,99.234,96.784)
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
  cols = colnames(semillas_t[-c(dim(semillas_t)[2])]),  # Selecciona todas las columnas que empiezan con "serie_"
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










  

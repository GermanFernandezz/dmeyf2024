# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rpart")
require("parallel")
require("primes")
require("ggplot2")

# para que se detenga ante el primer error
# y muestre el stack de funciones invocadas
options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})

# Inicializamos variables
PARAM <- list()
PARAM$experimento_data1 <- "PP7230"
PARAM$experimento_data2 <- "PP7230"
PARAM$experimento_bayesiana <- "HT7240"

PARAM$experimento <- "KA7260"

#------------------------------------------------------------------------------
# limita el uso de memoria RAM a  Total_hardware - GB_min

action_limitar_memoria <- function( GB_min = 4 ) {
  
  MemTotal <- as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern=TRUE))
  MemTotal <- as.integer( MemTotal/ 1024 - GB_min*1024 )
  if( MemTotal < 0 )  action_abortar( " No hay suficiente RAM para trabajar (min 4GB ) " )
  ulimit::memory_limit( MemTotal )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui empieza el programa

# Limito la memoria, para que ningun alumno debe sufrir que el R 
#  aborte sin avisar si no hay suficiente memoria
#  la salud mental de los alumnos es el bien mas preciado 
action_limitar_memoria( 4 )

# Aqui empieza el programa
setwd("~/buckets/b1/exp/")

# cargo el resultado de la Bayesian Optimization
#Se debe especificar bien la ruta de este archivo. Uno es mi modelo base y el otro es el nuevo
tb_BO_log1 <- fread(paste0(PARAM$experimento_bayesiana,"/BO_log.txt"))
tb_BO_log2 <- fread(paste0(PARAM$experimento_bayesiana,"/BO_log.txt"))


# cargo el dataset donde voy a entrenar el modelo
#Debo especificar bien los dataset si es que hice algún datadrifting o cambio de variables
dataset1 <- fread(paste0(PARAM$experimento_data,"/dataset.csv.gz"))
dataset2 <- fread(paste0(PARAM$experimento_data,"/dataset.csv.gz"))


# creo la carpeta donde va el experimento
dir.create(paste0(PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./", PARAM$experimento, "/"))


# paso la clase a binaria que tome valores {0,1}  enteros
dataset1[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)]
dataset2[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)]

PARAM$p_valor_limite <- 0.05  # Umbral de p-valor

ganancias_1_total <- c()  # Para guardar las ganancias del modelo 1
ganancias_2_total <- c()  # Para guardar las ganancias del modelo 2
p_valores <- c()  # Para guardar los p-valores obtenidos
num_seeds <- 0  # Contador de semillas
max_seeds <- 200  # Máximo de semillas permitidas
semilla_primigenia <- 111667 # Semilla general para reproducibilidad
#Genero las semillas de números primos
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(semilla_primigenia) 
semillas <- sample(primos, max_seeds )




# los campos que se van a utilizar
campos_buenos <- setdiff(
  colnames(dataset),
  c("clase_ternaria", "clase01",
    "part_training", "part_validation", "part_testing",
    "part_final_train", "part_future")
)


# Función para entrenar y predecir con una semilla
entrenar_y_predecir <- function(seed) {
  # Reemplazamos la semilla en la lista de parámetros
  param_completo1$seed <- seed
  param_completo2$seed <- seed
  
  # Convertimos los datos a formato lgb.Dataset
  dtrain1 <- lgb.Dataset(
    data = data.matrix(dataset1[part_training == 1L, campos_buenos, with = FALSE]),
    label = dataset1[part_training == 1L, clase01],
    free_raw_data = FALSE
  )
  dtrain2 <- lgb.Dataset(
    data = data.matrix(dataset2[part_training == 1L, campos_buenos, with = FALSE]),
    label = dataset2[part_training == 1L, clase01],
    free_raw_data = FALSE
  )
  dvalid1 <- lgb.Dataset(
    data = data.matrix(dataset1[part_validation == 1L, campos_buenos, with = FALSE]),
    label = dataset1[part_validation == 1L, clase01],
    free_raw_data = FALSE
  )
  dvalid2 <- lgb.Dataset(
    data = data.matrix(dataset2[part_validation == 1L, campos_buenos, with = FALSE]),
    label = dataset2[part_validation == 1L, clase01],
    free_raw_data = FALSE
  )
  
  # Entrenamos ambos modelos con los mismos parámetros, pero con la semilla diferente
  model_1 <- lightgbm(
    data = dtrain1,
    params = param_completo1,
    verbose = -100
  )
  model_2 <- lightgbm(
    data = dtrain2,
    params = param_completo2,
    verbose = -100
  )
  # Realizamos las predicciones sobre el set de validación
  predicciones_1 <- predict(
      modelo1,
      data.matrix(dvalid1[, campos_buenos, with = FALSE])
    )
  predicciones_2 <- predict(
    modelo2,
    data.matrix(dvalid2[, campos_buenos, with = FALSE])
  )
  return(list(predicciones_1 = predicciones_1, predicciones_2 = predicciones_2))
}


# Función para ordenar, seleccionar y calcular la ganancia
ordenar_y_seleccionar <- function(predicciones, envio_inicial = 8000, paso = 500, envio_max = 13000) {
  tbl <- data.table("prob" = predicciones, 
                    "clase01" = dataset1[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)])
  
  # Ordenamos por la probabilidad predicha en orden decreciente
  setorder(tbl, -prob)
  
  # Inicializamos listas para almacenar las ganancias y sus tamaños
  ganancias <- c()
  envios <- c()
  
  # Seleccionamos los subsets según los pasos especificados
  for (envio in seq(envio_inicial, envio_max, paso)) {
    subset <- tbl[1:envio, ]
    
    # Calculamos la ganancia para este subset
    ganancia <- sum(ifelse(subset$clase01 == 1, 273000, -7000))
    ganancias <- c(ganancias, ganancia)
    envios <- c(envios, envio)  # Guardamos el tamaño correspondiente
  }
  
  return(list(ganancias = ganancias, envios = envios))
}

# Abrimos el archivo en modo de escritura
archivo_resultados <- file("resultados.txt", open = "wt")

# Iteramos sobre las semillas
for (seed in semillas) {
  # Aumentamos el contador de semillas
  num_seeds <- num_seeds + 1
  
  # Entrenamos los modelos y obtenemos las predicciones
  predicciones <- entrenar_y_predecir(seed)
  
  # Calculamos las ganancias para los envios
  ganancias_1 <- ordenar_y_seleccionar(predicciones$predicciones_1)
  ganancias_2 <- ordenar_y_seleccionar(predicciones$predicciones_2)
  
  # Calculamos las ganancias para ambos modelos en cada tamaño
  for (envio in seq(8000, 13000, 500)) {
    envio_str <- as.character(envio)
    
    # Inicializamos ganancias_total para este tamaño si no existe
    if (is.null(ganancias_total[[envio_str]])) {
      ganancias_total[[envio_str]] <- list(ganancias_1 = c(), ganancias_2 = c())
    }
    
    # Inicializamos p_valores_totales para este tamaño si no existe
    if (is.null(p_valores_totales[[envio_str]])) {
      p_valores_totales[[envio_str]] <- c()
    }
    
    # Obtenemos las ganancias directamente de las listas ganancias_1 y ganancias_2
    ganancia_modelo_1 <- ganancias_1[[envio_str]]
    ganancia_modelo_2 <- ganancias_2[[envio_str]]
    
    # Guardamos las ganancias de la semilla actual
    ganancias_total[[envio_str]]$ganancias_1 <- c(ganancias_total[[envio_str]]$ganancias_1, ganancia_modelo_1)
    ganancias_total[[envio_str]]$ganancias_2 <- c(ganancias_total[[envio_str]]$ganancias_2, ganancia_modelo_2)
    
    # Aplicamos el test de Wilcoxon y guardamos el p-valor
    if (length(ganancias_total[[envio_str]]$ganancias_1) > 0 && length(ganancias_total[envio_str]$ganancias_2) > 0) {
      resultado_wilcox <- wilcox.test(ganancias_total[[envio_str]]$ganancias_1, ganancias_total[[envio_str]]$ganancias_2, paired = TRUE)
      p_valores_totales[[envio_str]] <- c(p_valores_totales[[envio_str]], resultado_wilcox$p.value)
    }
    
    # Escribimos los resultados en el archivo
    cat(num_seeds, 
        envio, 
        ganancia_modelo_1, 
        ganancia_modelo_2, 
        seed, 
        tail(p_valores_totales[[envio_str]], 1), 
        "\n", 
        file = archivo_resultados, 
        append = TRUE)
    
    # Verificamos si el p-valor es menor a 0.05 para cada tamaño
    if (!is.na(tail(p_valores_totales[[envio_str]], 1)) && tail(p_valores_totales[[envio_str]], 1) < PARAM$p_valor_limite) {
      cat("El proceso se detiene en la semilla:", num_seeds, "para el tamaño:", envio_str, "\n")
      cat("P-valor:", tail(p_valores_totales[[envio_str]], 1), "\n")
      break
    }
  
  # Verificamos si nos detenemos antes de llegar al final
  if (tail(p_valores_totales[[envio_str]], 1) < PARAM$p_valor_limite) {
    break
  }
}

# Cerramos el archivo una vez que terminamos de escribir
close(archivo_resultados)  
  





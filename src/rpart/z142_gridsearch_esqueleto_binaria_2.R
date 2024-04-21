# esqueleto de grid search ajustado

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
# reemplazar por las propias semillas
PARAM$semillas <- c(100019, 100043, 100049, 100057, 100069)

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_binaria, seed=semilla)
#   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  
  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))
  
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
       by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset  
  particionar(dataset, division = c(7, 3), agrupa = "clase_binaria", seed = semilla)
  
  # genero el modelo
  # quiero predecir clase_binaria a partir del resto
  modelo <- rpart("clase_binaria ~ .",
                  data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
                  xval = 0,
                  control = param_basicos
  ) # aqui van los parametros del arbol
  
  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
                        dataset[fold == 2], # fold==2  es testing, el 30% de los datos
                        type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad para las clases binarias
  
  # prediccion es una matriz con DOS columnas,
  #  llamadas "pos" y  "neg"
  # cada columna es el vector de probabilidades
  
  # calculo la ganancia en testing  que es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "pos"] > 0.025,
               ifelse(clase_binaria == "pos", 117000, -3000),
               0
    ))
  ]
  
  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3
  
  return(ganancia_test_normalizada)
}

ArbolesMontecarlo <- function(semillas, param_basicos) {
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
  #  tantas veces como valores tenga el vector  PARAM$semillas  
  ganancias <- mcmapply(ArbolEstimarGanancia,
                        semillas, # paso el vector de semillas
                        MoreArgs = list(param_basicos), # aqui paso el segundo parametro
                        SIMPLIFY = FALSE,
                        mc.cores = 5 # en Windows este valor debe ser 1
  )
  
  ganancia_promedio <- mean(unlist(ganancias))
  
  return(ganancia_promedio)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local

#setwd("~/Documentos/2024/universidad austral/laboratorio1")# Establezco el Working Directory
setwd("~/buckets/b1/")

# cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]

# genero la clase binaria
dataset[, clase_binaria := ifelse(clase_ternaria == "BAJA+2", "pos", "neg")]

#guardar el dataset modificado para explorarlo
fwrite(dataset, "./datasets/dataset_pequeno_binario.csv")

# quito la clase ternaria
dataset[, clase_ternaria := NULL]


# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearch_binario2.txt"

# genero la data.table donde van los resultados del Grid Search
tb_grid_search <- data.table(
  cp = numeric(),
  max_depth = integer(),
  min_split = integer(),
  min_bucket = integer(),
  ganancia_promedio = numeric()
)

# itero por los loops anidados para cada hiperparametro

# Definir los rangos de parámetros como variables
rangos_cp <- c(-1) #seq(-1, 1, by = 0.25) #después de varios experimentos me doy cuenta que no es necesario variar cp en negativo, con -1 basta
rangos_min_split <- c(1000, 800, 600, 400, 200, 100, 50, 20, 10)
rangos_min_bucket <- c(5, 10, 15, 20, 25)
rangos_max_depth <- c(4, 6, 8, 10, 12, 14)


# Inicializa el contador de combinaciones válidas
combinaciones_validas <- 0

# Genera todas las combinaciones posibles y cuenta solo las válidas
for (vmin_split in rangos_min_split) {
  for (vmin_bucket in rangos_min_bucket) {
    if (vmin_split >= 2 * vmin_bucket) {
      combinaciones_validas <- combinaciones_validas + 1
    }
  }
}

# Ahora calcula iteraciones_total considerando solo combinaciones válidas
iteraciones_total <- length(rangos_cp) * combinaciones_validas * length(rangos_max_depth)

# Inicializar contadores de tiempo y progreso
inicio <- Sys.time()
iteraciones_completadas <- 0

# Bucle de grid search usando las variables definidas
for (vcp in rangos_cp) {
  for (vmin_split in rangos_min_split) {
    for (vmin_bucket in rangos_min_bucket) {
      if (vmin_split >= 2 * vmin_bucket) {
        for (vmax_depth in rangos_max_depth) {
          iteracion_inicio <- Sys.time()
          
          # vminsplit  minima cantidad de registros en un nodo para hacer el split
          param_basicos <- list(
            cp = vcp, # complejidad minima
            minsplit = vmin_split,
            minbucket = vmin_bucket, # minima cantidad de registros en una hoja
            maxdepth = vmax_depth  # profundidad máxima del arbol
          )
          
          # Un solo llamado, con las 5 semillas
          ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
          
          # agrego a la tabla
          tb_grid_search <- rbindlist(
            list(tb_grid_search, 
                 list(cp = vcp, max_depth = vmax_depth, min_split = vmin_split, min_bucket = vmin_bucket, ganancia_promedio = ganancia_promedio))
          )
          
          # Actualizar el contador de progreso y calcular el tiempo
          iteraciones_completadas <- iteraciones_completadas + 1
          tiempo_iteracion <- Sys.time() - iteracion_inicio
          tiempo_total <- Sys.time() - inicio
          tiempo_promedio_por_iteracion <- tiempo_total / iteraciones_completadas
          iteraciones_restantes <- iteraciones_total - iteraciones_completadas
          tiempo_restante_estimado <- tiempo_promedio_por_iteracion * iteraciones_restantes
          
          # Mostrar mensaje de progreso con el tiempo formateado de manera más legible
          cat(sprintf("Progreso: %d de %d iteraciones completadas. Tiempo restante estimado: %.2f minutos\n", iteraciones_completadas, iteraciones_total, tiempo_restante_estimado / 60))
        }
      }
    }
  }
  # escribo la tabla a disco en cada vuelta del loop mas externo
  Sys.sleep(2) # espero un par de segundos
  fwrite(tb_grid_search,
         file = archivo_salida,
         sep = "\t")
}

print("fin de la ejecución")
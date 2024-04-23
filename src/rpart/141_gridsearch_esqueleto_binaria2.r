# esqueleto de grid search
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")

require("rpart")
require("parallel")

PARAM <- list()
#141223 
# semilla nerio 141223, 258113, 270131, 686087, 832969 , otra mayor-> 300089, 320057, 320027, 320009, 320039
# reemplazar por las propias semillas
PARAM$semillas <- c(101197, 102103, 103159, 104681, 999983) #141223, 270131, 320027, 103159, 999983) #c(101197, 102103, 103159, 104681, 999983)

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
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
  particionar(dataset2, division = c(7, 3), agrupa = "clase_binaria1", seed = semilla)

  # genero el modelo
  # quiero predecir clase_binaria1 a partir del resto
  modelo <- rpart("clase_binaria1 ~ .",
    data = dataset2[fold == 1], # fold==1  es training,  el 70% de los datos
    xval = 0,
    control = param_basicos
  ) # aqui van los parametros del arbol

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
    dataset2[fold == 2], # fold==2  es testing, el 30% de los datos
    type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad

  # prediccion es una matriz con 2 columnas,
  #  llamadas "suma"y "resta" por la modificacion de la clase clase_binaria1 
  # cada columna es el vector de probabilidades


  # calculo la ganancia en testing  qu es fold==2
  ganancia_test <- dataset2[
    fold == 2,
    sum(ifelse(prediccion[, "SUMA"] > 0.025,
      ifelse(clase_binaria1 == "SUMA", 117000, -3000),
      0
    ))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3

  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------

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

crear_clase_binaria <- function(dt) {
  dataset2 <- copy(dt)
  setDT(dataset2)
  
  # Crear la nueva columna basada en condiciones
  dataset2[, clase_binaria1 := fcase(
    clase_ternaria == "BAJA+1", "RESTA",
    clase_ternaria == "BAJA+2", "SUMA",
    clase_ternaria == "CONTINUA", "RESTA",
    default = NA_character_  # Esto deja la fila vacía si ninguna condición se cumple
  )]
  
  return(dataset2)

} 

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
#setwd("~/buckets/b1/") # Establezco el Working Directory

setwd("/Users/Agu/Documents/git/labo1/")
# cargo los datos

# cargo los datos
#dataset <- fread("./datasets/dataset_pequeno.csv")

###llamo la funcion para crear la nueva clase ternia 
dataset2 <- crear_clase_binaria(fread("./datasets/dataset_pequeno.csv"))


# trabajo solo con los datos con clase, es decir 202107 y del nuevo dataset con clase_binaria1
dataset2 <- dataset2[clase_binaria1 != ""]
#dataset[clase_ternaria != "", .N] 
#dataset2[clase_binaria1 != "", .N]


# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearch_binaria1.txt"

# genero la data.table donde van los resultados del Grid Search
tb_grid_search <- data.table( 
                              vcp = integer(),
                              max_depth = integer(),
                              min_bucket = integer(),
                              min_split = integer(),
                              ganancia_promedio = numeric() )

#2 <= 2*minbucket <= minsplit <= #dataset (cantidad de registros) 
#2 <= max_depth <=30
#-1 <= cp <=0.1  

# itero por los loops anidados para cada hiperparametro
### -0.99, 
for(vcp in c(-0.95,-.87,-.43)){
  for (vmax_depth in c(4,7,12))  #7,
    for (vmin_split in c(34,220,888,889,878) ) # 912?, 12,23,133))
      for (vmin_bucket in c(15,279,284,431))# min_split/4)) ##270))
      {
    # notar como se agrega

    # vminsplit  minima cantidad de registros en un nodo para hacer el split
    param_basicos <- list(
      "cp" = vcp, #-0.5, # complejidad minima
      "minsplit" = vmin_split,
      "minbucket" = vmin_bucket, #5, # minima cantidad de registros en una hoja
      "maxdepth" = vmax_depth
    ) # profundidad máxima del arbol

    # Un solo llamado, con la semilla 17
    ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)

    # agrego a la tabla
    tb_grid_search <- rbindlist( 
      list( tb_grid_search, 
            list( vcp,vmax_depth, vmin_bucket, vmin_split, ganancia_promedio) ) )

  }

  # escribo la tabla a disco en cada vuelta del loop mas externo
  Sys.sleep(2)  # espero un par de segundos

  fwrite( tb_grid_search,
          file = archivo_salida,
          sep = "\t" )
}

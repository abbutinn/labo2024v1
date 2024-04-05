rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")


PARAM <- list()
# reemplazar por las propias semillas
PARAM$semillas <- c(101197, 102103, 103159, 104681, 999983)

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

setwd("/Users/Agu/Documents/git/labo1/")

dataset <- fread("./datasets/dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]


colSums(is.na(dataset))

# Correlación entre características
# Esto requiere que primero conviertas las variables categóricas a numéricas o las excluyas del análisis de correlación
# Puedes usar cor() si tus datos son todos numéricos, o bien, usar algún paquete como 'Hmisc' para correlaciones con categóricas
correlationMatrix <- cor(dataset[, sapply(dataset, is.numeric)])
corrplot::corrplot(correlationMatrix, method = "circle")


# # genero la data.table donde van los resultados del Grid Search
# tb_grid_search <- data.table( 
#   vcp = integer(),
#   max_depth = integer(),
#   min_bucket = integer(),
#   min_split = integer(),
#   ganancia_promedio = numeric() )
# 
# 
# # itero por los loops anidados para cada hiperparametro
# for(vcp in c(-0.95, -0.7, 0.3)){
#   for (vmax_depth in c(3, 4, 110,300)) 
#     for (vmin_split in c(4, 8,14,44 ) ) #, 400, 150, 100, 50, 20, 10))
#       for (vmin_bucket in c(18,40,140)) ##,8,16,32, min_split/4 ))
#       {
#       
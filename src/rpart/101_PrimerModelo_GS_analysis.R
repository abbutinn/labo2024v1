# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot
rm(list = ls())
# cargo las librerias que necesito
require("data.table")
library("data.table")
require("rpart")
library("rpart")
require("rpart.plot")
library("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("/Users/Agu/Documents/git/labo1/") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
#inicial cp =-0.3, minsplit = 0, minbucket =1, maxdepth =3 
#modelo_base <- rpart(
#        formula = "clase_ternaria ~ .",
#        data = dtrain, # los datos donde voy a entrenar
#        xval = 0,
#        cp = -0.3, # esto significa no limitar la complejidad de los splits
#        minsplit = 0, # minima cantidad de registros para que se haga el split
#        minbucket = 1, # tamaño minimo de una hoja
#        maxdepth = 3
#) # profundidad maxima del arbol

##con cp =-0.3, minsplit = 166, minbucket =324, maxdepth =6 -> 11117 
modelo1 <- rpart(
  formula = "clase_ternaria ~ .",
  data = dtrain, # los datos donde voy a entrenar
  xval = 0,
  cp = -0.9, # esto significa no limitar la complejidad de los splits
  minsplit = 800, # minima cantidad de registros para que se haga el split
  minbucket = 240, # tamaño minimo de una hoja
  maxdepth = 14
) # profundidad maxima del arbol


# grafico el arbol
prp(modelo1,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)


# aplico el modelo a los datos nuevos
#prediccion_base <- predict(
#        object = modelo_base,
#        newdata = dapply,
#        type = "prob"
#)
#############agu
# aplico el modelo a los datos nuevos
prediccion_1 <- predict(
  object = modelo1,
  newdata = dapply,
  type = "prob"
)
#####agu 
# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
#dapply[, prob_baja2 := prediccion_base[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
#dapply[, Predicted_base := as.numeric(prob_baja2 > 1 / 40)]

#####modelo1
# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2_mod1 := prediccion_1[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted:= as.numeric(prob_baja2_mod1 > 1 / 40)]

#####


# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2001/K101_001_rank100.csv",
        sep = ","
)


dapply[, sum(Predicted == 1)] 


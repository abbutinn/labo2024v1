rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection


require("data.table")
require("rpart")
require("parallel")
library("data.table")
library("rpart")
require("plotly")
library("plotly")
# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")
library("DiceKriging")
library("mlrMBO")

options(repr.plot.width = 20, repr.plot.height = 20)




##### funcion para medir tiempo de corridas.. 

medir_tiempo <- function(func,comentario, ...) {
  t0 <- Sys.time() #devuelve fecha en formato POSIXct, util para trabajar con series temporales de forma coherente. 
  operacion <- func(...)
  t1 <- Sys.time()
  delta <- as.numeric(t1 - t0, units = "secs") ## tiempo ejecucion.
  cat("Tiempo de ejecución:", delta, "segs\n")
  cat(comentario, operacion, "\n")
  #return(operacion)
  
}

###ej con read. 
setwd("/Users/Agu/Documents/git/labo1/")
#df_csv <- medir_tiempo(read.csv,"./datasets/dataset_pequeno.csv") #45s la primera, 31s la 2da,
dataset <- medir_tiempo(fread, "./datasets/dataset_pequeno.csv") # fread clava 2.81s jojo, la 2da 3.59s
### si lo quiero bajar de la nube.. dataset <- fread("https://storage.googleapis.com/open-courses/austral2024-fc72/dataset_pequeno.csv")
 
nrow(dataset)
ncol(dataset)
## otra forma de contar las filas. 
dataset[, .N]

colnames(dataset)

dataset[, .N, foto_mes]

#exploracion clase ternaria 
dataset[, .N, list(foto_mes, clase_ternaria)]
#1) exploracion de datos.. 

##contar baja+2
medir_tiempo(function() nrow(dataset[dataset$clase_ternaria == "BAJA+2", ]))
medir_tiempo(function() dataset[clase_ternaria == "BAJA+2", .N] ) # mas rapido ?
medir_tiempo(function() dataset[, sum(clase_ternaria == "BAJA+2")])

### conteo de proporcion de baja+2 
medir_tiempo(function() dataset[foto_mes == 202107, sum(clase_ternaria == "BAJA+2") / .N],"proporcion de baja+2:")

a1 <- dataset[foto_mes == 202107 & ctrx_quarter < 20, sum(clase_ternaria == "BAJA+2") / .N]
medir_tiempo(function() a1,"Conteo de la proporcion de BAJA+2 en un predicado:")

a2 <- dataset[foto_mes == 202107 & ctrx_quarter < 20, sum(clase_ternaria == "BAJA+2") / .N] / dataset[foto_mes == 202107, sum(clase_ternaria == "BAJA+2") / .N]
medir_tiempo(function() a2,"Lift del predicado ctrx_quarter vs el universo, calculado de forma brutal:")




#### agregando columnas con :=
##Ganancia del dataset. 
#primero asigno a Todos los registro el valor de -3000

dataset[foto_mes == 202107, ganancia := -3000 ]
## y le asigno a los baja+2 una ganancia de 117000
dataset[foto_mes == 202107 & clase_ternaria == "BAJA+2", ganancia := 117000]

### calculo la ganancia que tendria una campana de marketin en donde se envia estimulo a TODOS los clientes.
dataset[foto_mes == 202107, sum(ganancia)] # <- se perderian 334 millones.

##ganancia de un predicado simple (ctrx_quarter) < 20 
# en el contexto de la programacion un predicado es generalmente una funcion que devuelve un valor booleano, basado en una logica de desicion.

#ctrx_quarter <20
dataset[foto_mes == 202107 & ctrx_quarter < 20, sum(ganancia)] ## 41874000
## ctrx_quarter <4
dataset[foto_mes == 202107 & ctrx_quarter < 4, sum(ganancia)] # 26175000

### de forma brutal e ineficiente, busco donde esta el mejor corte de ctrx_quarter. 

for (transacciones in 0:50)
{
  cat(transacciones, dataset[foto_mes == 202107 & ctrx_quarter < transacciones, sum(ganancia)], "\n")
} ## la transaccion 25  dio 42735000  de ganancia. 


#### Ganancia de predicado complejo

dataset[foto_mes == 202107 & ctrx_quarter < 18 & mpasivos_margen < 29.8, sum(ganancia)]

#Graficos de densidades. 
library("ggplot2")
campo <- "cliente_antiguedad"
ggplot(dataset[foto_mes == 202107], aes_string(x = campo)) +
  geom_density(trim = TRUE, na.rm = TRUE) +
  facet_grid("clase_ternaria~ .")



#####construyendo un arbol.
## "clase_ternaria ~ ." significa predecir clase_ternaria usando la totalidad de las variables del dataset


modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = dataset[foto_mes == 202107]
)

library("rpart.plot")

rpart.plot::prp(modelo)

## para imprimir el modelo graficamente. 
prp(modelo, extra = 101, digits = -5, branch = 1, type = 4, varlen = 0, faclen = 0, tweak = 1.3)


# genero el modelo
modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = dataset[foto_mes == 202107],
  xval = 0,
  cp = 0.0
) ## si no limito... sale un arbol enorme
# imprimo el modelo graficamente
prp(modelo, extra = 101, digits = -5, branch = 1, type = 4, varlen = 0, faclen = 0, tweak = 1.3)


# genero el modelo limitando el maxdepth
modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = dataset[foto_mes == 202107],
  xval = 0,
  cp = 0.0,
  maxdepth = 2
) ##3 el cp no hace que el arbol se abra... 

# imprimo el modelo graficamente
prp(modelo, extra = 101, digits = -5, branch = 1, type = 4, varlen = 0, faclen = 0, tweak = 1.3, cex = 1.2)

# genero el modelo
modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = dataset[foto_mes == 202107],
  xval = 0,
  cp = -1,
  maxdepth = 3
)

# imprimo el modelo graficamente
#options(repr.plot.width = 40, repr.plot.height = 20)
prp(modelo, extra = 101, digits = -5, branch = 1, type = 4, varlen = 0, faclen = 0, tweak = 1.1, cex = 1.2)

######
# genero el modelo
dataset2 <- copy(dataset)
julio <- copy(dataset[foto_mes == 202107])
dataset2[foto_mes == 202109, clase_ternaria := "Z01"]
julio[, clase_ternaria := "Z01"]
dataset2 <- rbind(dataset2, julio)
dataset2[, foto_mes := NULL]
setorder(dataset2, clase_ternaria)

modelo <- rpart(
  formula = "clase_ternaria ~ . -Master_fultimo_cierre -Visa_fultimo_cierre -mcomisiones_mantenimiento",
  data = dataset2,
  xval = 0,
  cp = -1,
  maxdepth = 3
)

# imprimo el modelo graficamente
options(repr.plot.width = 40, repr.plot.height = 20)
prp(modelo, extra = 101, digits = -5, branch = 1, type = 4, varlen = 0, faclen = 0, tweak = 1.1, cex = 1.2)

modelo$splits

######

modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = dataset[foto_mes == 202107],
  xval = 0,
  cp = -1,
  maxdepth = 2
)

# imprimo el modelo graficamente
prp(modelo, extra = 101, digits = -5, branch = 1, type = 4, varlen = 0, faclen = 0, tweak = 1.1, cex = 1.2)

##en teoria la variable mas importante del dataset es ctrx_quarter...

boxplot(dataset[foto_mes == 202107, ctrx_quarter])
hist(dataset[foto_mes == 202107, ctrx_quarter])
plot(density(dataset[foto_mes == 202107, ctrx_quarter]))
## normalizo ctrx_quarter 
dataset[foto_mes == 202107, ctrx_quarter_normalizado := scale(ctrx_quarter)]
#grafico para confirmar 
plot(density(dataset[foto_mes == 202107, ctrx_quarter_normalizado]))

# genero el modelo
modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = dataset[foto_mes == 202107],
  xval = 0,
  cp = -1,
  maxdepth = 2
)

# imprimo el modelo graficamente
prp(modelo, extra = 101, digits = -5, branch = 1, type = 4, varlen = 0, faclen = 0, tweak = 1.1, cex = 1.2)

###el arbol es inmune a la normalizacion de variables. 


###### creando data.table a partir de columnas.. 
rm(list = ls() )
gc()

library("data.table") # cargo la libreria  data.table
#dataset <- medir_tiempo(fread, "./datasets/dataset_pequeno.csv") # fread clava 2.81s jojo, la 2da 3.59s

##ej vector id y vector_enviar estimulo
vector_ids <- c(107,228,351,468,579)
vector_ids
vector_enviar <- c(0,1,1,0,1) 

###creo un dataset a partir de las 2 columnas. a la primera la llamo n_cliente y a la 2da predicted
#en R, un dataframe o data.table es una lista de columnas.
#es una lista y no un vector. porque los vectores necesitan que todos los valores sean del mismo tipo de datos.
tabla_final <- as.data.table(list("numero_de_cliente" = vector_ids, "Predicted" = vector_enviar))

tabla_final








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

#### bayesiano
func_volcano <- function(x) {
  z <- volcano[x$b, x$a]
}
  
#grafico el volcan 
p <- plot_ly(z= volcano, type = "surface")
p ## la funcion tiene maximos locales. 
 
configureMlr(show.learner.output = FALSE)


obj.fun <- makeSingleObjectiveFunction(
  fn = func_volcano, ##nombre de la funcion
  minimize = FALSE, # estoy Maximizando la ganancia (por defecto es true, le ponemos false porque queremos maximizar la ganancia)
  has.simple.signature = FALSE, # porque tengo DOS dimensiones
  par.set = makeParamSet(
    makeIntegerParam("a", lower = 1, upper = 61),
    makeIntegerParam("b", lower = 1, upper = 87)
  ),
)

## definimos la funcion proxy, que se construye internamente intentando emular la realidad. 
## cl es la clase de learner, `reg.km` indica el metodo de kriging `regression kriging method`
##kriging es un metodo de interpolacion avanzada  https://www.youtube.com/watch?v=ZB7GvJrNKq0
##predict.type me devuelve la forma de la prediccion, se, me devuelve media y standar error.
#covtype es la funcion de covarianza que va a utilizar, , cual es la covarianza de dos mediciones como fucion de la distancia entre los puntos donde fueron tomadas las mediciones, fue inventada por Bertil Matérn

fproxy <- makeLearner(
  cl = "regr.km",
  predict.type = "se",
  covtype = "matern3_2"
)


##ultima definicion, especificar la optimizacion bayesiana

#crit indica el criterio con el que se completan los valores iniciales "no inteligentes"
#iters indica la cantidad de iteraciones inteligentes que hará la Optimizacion Bayesiana, las que son adicionales a las primeras cuatro de este caso.* iters indica la cantidad de iteraciones inteligentes que hará la Optimizacion Bayesiana, las que son adicionales a las primeras cuatro de este caso.

ctrl <- makeMBOControl()
ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
ctrl <- setMBOControlTermination(ctrl, iters = 1000)

#finalmente , lanzo la Optimizacion Bayesiana

#fun es la especificacion de la funcion que deseo optimizar, si maximizo o minimizo, cuales son las variables de la misma
#learner especifica cual es la función proxy interna que va a utilizar la Optimziación Bayesiana
#control indica la la forma en que se harán las iteraciones

run <- mbo(
  fun = obj.fun,
  learner = fproxy,
  control = ctrl
)

tb_resultados <- as.data.table(run$opt.path)

tb_resultados


tb_resultados[which.max(tb_resultados$y)]






## Taller 2

#clear all
rm(list = ls())

# paquetes
install.packages("here")
install.packages("caret")
require("caret")
require("dplyr")
require("here")
require("here")
require("tidyverse")
#cargar bases
test_hogares <- readRDS("C:/Users/linit/Documents/semestre 8/Big Data/Taller 2/data/data/test_hogares.Rds")
test_personas <- readRDS("C:/Users/linit/Documents/semestre 8/Big Data/Taller 2/data/data/test_personas.Rds")
train_hogares <- readRDS("C:/Users/linit/Documents/semestre 8/Big Data/Taller 2/data/data/train_hogares.Rds")
train_personas <- readRDS("C:/Users/linit/Documents/semestre 8/Big Data/Taller 2/data/data/train_personas.Rds")

########### TRAIN
#revisión de variables, luego id del hogar y orden identifación de persona
colnames(train_hogares)
colnames(train_personas)[1:2]

#crear una variable que sea la suma de los ingresos de los individuos en el hogar a partir de la base de persona
sum_ingresos<-train_personas %>% group_by(id) %>% summarize(Ingtot_hogar=sum(Ingtot,na.rm = TRUE)) 
summary(sum_ingresos)

#unirla la nueva variable a la base de hogares
train_hogares<-left_join(train_hogares,sum_ingresos)
colnames(train_hogares)
head(train_hogares[c("id","Ingtotug","Ingtot_hogar")])

#Calculo de pobreza 
#Se observa en la base train        33.024 hogares pobres y 131.936 hogares no pobres
table(train_hogares$Pobre)
#Calculo de pobreza con pobre_hand  33.024 hogares pobres y 131.936 hogares no pobres
train_hogares<- train_hogares %>% mutate(Pobre_hand=ifelse(Ingpcug<Lp,1,0))
table(train_hogares$Pobre_hand) 
#Analizando "Pobre" y "Pobre_hand para comprobar si lo anterior es cierto comparamos "Pobre" con una variable "Pobre_hand" crada por nosotros
#vemos ue coinciden 
table(train_hogares$Pobre,train_hogares$Pobre_hand)
train_hogares$Indigente

##CREAR jefe hombre del hogar
train_personas <- mutate(train_personas, jefehogar = ifelse(Orden == 1,1,0))
train_personas <- mutate(train_personas, jefehombre = ifelse(jefehogar == 1 & P6020 ==1,1,0))
train_personas$jefehombre

#crear una variable xsl hogar a partir de la base de persona
jefehombrehogar<-train_personas %>% group_by(id) %>% summarize(jefehombre=sum(jefehombre,na.rm = TRUE))  
summary(jefehombrehogar)

#unirla la nueva variable a la base de hogares
train_hogares<-left_join(train_hogares,jefehombrehogar)
colnames(train_hogares)
head(train_hogares[c("Ingtotug","jefehombre")])
glimpse(train_hogares)
#eliminar NA en la variable de interes
train_hogares <- train_hogares[!is.na(train_hogares$Ingpcug),]

#Variable informalidad usando p6920
train_personas <- mutate(train_personas, formal = ifelse(P6920 == 1
          | P6920==3,1,0))
train_personas$formal
train_personas <- mutate(train_personas, jefeformal = ifelse(jefehogar == 1 & formal ==1,1,0))
train_personas$jefeformal
  #Mezclamos Jefe con formal
jefeformal<-train_personas %>% group_by(id) %>% summarize(jefeformal=sum(jefeformal,na.rm = TRUE))  
summary(jefehombrehogar)
  #unirla la nueva variable a la base de hogares
train_hogares<-left_join(train_hogares,jefeformal)
colnames(train_hogares)

#Ratio personas-cuartos
train_hogares <- mutate(train_hogares, persxcuarto = Nper/P5010)
train_hogares$persxcuarto


####Emparejar Bases####


####train_hogares####

#Estructura
glimpse(train_hogares)
glimpse(test_hogares)

#Vamos a equilibrar train y test para no usar variables de train que no
#est?n en test.
#Pobre y Ingpcug se quedan en train porque son nuestras variables de inter?s
#Se quitan Ingtotug, Ingtotugarr, Indigente, Npobres, Nindigentes
train_hogares <- select(train_hogares, -c(Ingtotug, Ingtotugarr, Indigente, Npobres, Nindigentes))
colnames(train_hogares)
colnames(test_hogares)

#NAs: P5100, p5130, p5140 tienen NAs
sapply(train_hogares, function(x) sum(is.na(x)))
#Estas variables parecen no ser relevantes para predecir el ingreso
#ya que hablan de los pagos por amortizaci?n, el valor estimado del arriendo
#de sus viviendas y el pago mensual por arriendo.
#podr?an dropearse
#Tambi?n dropeamos Dominio, ya que para controlar por la ubicaci?n se tiene
#Depto y Cabecera
train_hogares <- select(train_hogares, -c(Dominio, P5100, P5130, P5140, Fex_c, Fex_dpto))

# Arreglamos la variable Clase (dic?toma) para que sean 1 y 0
#tabulaci?n de Clase
train_hogares %>%
  group_by(Clase) %>%
  summarise(n = n()) %>%
  mutate(
    totalN = (cumsum(n)),
    percent = round((n / sum(n)), 3),
    cumuPer = round(cumsum(freq = n / sum(n)), 3)) 
#Convertimos en double
train_hogares[, "Clase"] <- as.double(train_hogares[, "Clase", drop = T])
#Convertimos en dummy
variables_dicotomas <- c("Clase")

train_hogares[,variables_dicotomas] = train_hogares[,variables_dicotomas] - 1

#Definimos variables categoricas
variables_categoricas <- c("P5090", "Depto")
for (v in variables_categoricas) {
  train_hogares[, v] <- as.factor(train_hogares[, v, drop = T])
}

#Dumificamos la base

#convertimos id en string

tr_hog_d <- model.matrix(~ Clase + P5000 + P5010 + P5090 + Nper + 
                           Npersug + Ingpcug + Li + 
                           Lp + Pobre + Depto, train_hogares) %>%
  as.data.frame()

colnames(tr_hog_d)

glimpse(tr_hog_d)



####test_hogares####

#Estructura
glimpse(train_hogares)
glimpse(test_hogares)

#NAs: P5100, p5130, p5140 tienen NAs
sapply(test_hogares, function(x) sum(is.na(x)))
#Estas variables parecen no ser relevantes para predecir el ingreso
#ya que hablan de los pagos por amortizaci?n, el valor estimado del arriendo
#de sus viviendas y el pago mensual por arriendo.
#podr?an dropearse
test_hogares <- select(test_hogares, -c(Dominio, P5100, P5130, P5140, Fex_c, Fex_dpto))

# Arreglamos la variable Clase (dic?toma) para que sean 1 y 0
#tabulaci?n de Clase
test_hogares %>%
  group_by(Clase) %>%
  summarise(n = n()) %>%
  mutate(
    totalN = (cumsum(n)),
    percent = round((n / sum(n)), 3),
    cumuPer = round(cumsum(freq = n / sum(n)), 3)) 
#Convertimos en double
test_hogares[, "Clase"] <- as.double(test_hogares[, "Clase", drop = T])
#Convertimos en dummy
variables_dicotomas <- c("Clase")

test_hogares[,variables_dicotomas] = test_hogares[,variables_dicotomas] - 1

#Definimos variables categoricas
variables_categoricas <- c("P5090", "Depto")
for (v in variables_categoricas) {
  test_hogares[, v] <- as.factor(test_hogares[, v, drop = T])
}

#Dumificamos la base

te_hog_d <- model.matrix(~ Clase + P5000 + P5010 + P5090 + Nper + 
                           Npersug + Li + Lp + Depto, train_hogares) %>%
  as.data.frame()

colnames(te_hog_d)
colnames(tr_hog_d)
glimpse(te_hog_d)
glimpse(tr_hog_d)

####ahora usamos metodos de clasificacion de variable

#FORWARD
train_hogares <- select(train_hogares, -id)
forward<- train(Ingpcug ~ ., data = train_hogares,
                method = "leapForward",
                trControl = trainControl(method = "cv", number = 10))
forward
summary(forward$finalModel)

#OLS 
ols <- train(Ingpcug ~ ., 
             data = train_hogares,
             trControl = trainControl(method = "cv", number = 10),
             method = "lm")
ols

#RIDGE
ridge <- train(
  Ingpcug ~., data = train_hogares, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0,lambda=lambda), preProcess = c("center", "scale")
)
ridge




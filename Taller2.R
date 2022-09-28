## Taller 2

# paquetes
install.packages("here")
require("here")
require("here")
require("tidyverse")
#cargar bases
test_hogares <- readRDS("C:/Users/linit/Documents/semestre 8/Big Data/Taller 2/data/data/test_hogares.Rds")
test_personas <- readRDS("C:/Users/linit/Documents/semestre 8/Big Data/Taller 2/data/data/test_personas.Rds")
train_hogares <- readRDS("C:/Users/linit/Documents/semestre 8/Big Data/Taller 2/data/data/train_hogares.Rds")
train_personas <- readRDS("C:/Users/linit/Documents/semestre 8/Big Data/Taller 2/data/data/train_personas.Rds")

########### TRAIN
#revisiÃ³n de variables, luego id del hogar y orden identifaciÃ³n de persona
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

#####Limpieza Bases#####

# paquetes
require("here")
require("tidyverse")
#cargar bases
test_hogares <- readRDS("C:/Users/Luz Myriam Fonseca/Documentos/Julian/9 Semestre/BDMLAE/Taller 2/data/test_hogares.Rds")
test_personas <- readRDS("C:/Users/Luz Myriam Fonseca/Documentos/Julian/9 Semestre/BDMLAE/Taller 2/data/test_personas.Rds")
train_hogares <- readRDS("C:/Users/Luz Myriam Fonseca/Documentos/Julian/9 Semestre/BDMLAE/Taller 2/data/train_hogares.Rds")
train_personas <- readRDS("C:/Users/Luz Myriam Fonseca/Documentos/Julian/9 Semestre/BDMLAE/Taller 2/data/train_personas.Rds")

####train_hogares####

#Estructura
glimpse(train_hogares)
glimpse(test_hogares)

#NAs: P5100, p5130, p5140 tienen NAs
sapply(train_hogares, function(x) sum(is.na(x)))
#Estas variables parecen no ser relevantes para predecir el ingreso
#ya que hablan de los pagos por amortización, el valor estimado del arriendo
#de sus viviendas y el pago mensual por arriendo.
#podrían dropearse
train_hogares <- select(train_hogares, -c(P5100, P5130, P5140, Fex_c, Fex_dpto))

# Arreglamos la variable Clase (dicótoma) para que sean 1 y 0
#tabulación de Clase
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
variables_categoricas <- c("Dominio","P5090", "Depto")
for (v in variables_categoricas) {
  train_hogares[, v] <- as.factor(train_hogares[, v, drop = T])
}

#Dumificamos la base

#convertimos id en string

tr_hog_d <- model.matrix(~ Clase + Dominio +
                           P5000 + P5010 + P5090 + Nper + 
                           Npersug + Ingtotug + 
                           Ingtotugarr + Ingpcug + Li + 
                           Lp + Pobre + Indigente + Npobres
                         + Nindigentes + Depto, train_hogares) %>%
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
#ya que hablan de los pagos por amortización, el valor estimado del arriendo
#de sus viviendas y el pago mensual por arriendo.
#podrían dropearse
test_hogares <- select(test_hogares, -c(P5100, P5130, P5140, Fex_c, Fex_dpto))

# Arreglamos la variable Clase (dicótoma) para que sean 1 y 0
#tabulación de Clase
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
variables_categoricas <- c("Dominio","P5090", "Depto")
for (v in variables_categoricas) {
  test_hogares[, v] <- as.factor(test_hogares[, v, drop = T])
}

#Dumificamos la base

te_hog_d <- model.matrix(~ Clase + Dominio +
                           P5000 + P5010 + P5090 + Nper + 
                           Npersug + Li + 
                           Lp + Depto, train_hogares) %>%
  as.data.frame()

colnames(te_hog_d)

glimpse(te_hog_d)

####Lasso####
library(pacman)
p_load(fastDummies, caret, glmnet, MLmetrics)

# Convertimos la y en log (y1->Pobre , y2->Lp)
#y1 -> modelo con dummy
y1_train <- tr_hog_d[,"Pobre"]
#y2 -> modelo con linea de pobreza
y2_train <- log(tr_hog_d[,"Ingpcug"])
#Vector de X
X_train <- select(tr_hog_d, -c(Pobre, Ingpcug))
#no tengo variable "pobre" en test
#y1_test <- te_hog_d[,","Pobre"]
#y2 -> modelo con ingreso per capita, no la tengo en test
#y2_test <- log(tr_hog_d[,"Ingpcug"])
#Vector  de X test
X_test <- te_hog_d

# Estandarizamos la base de datos. 
# Necesitamos estandarizar
#####Estandarización#####
mu <- mean(X_train$P5000)
sigma <- sd(X_train$P5000)
X_train$P5000 <- (X_train$P5000 - mu)/sigma
X_test$P5000 <- (X_test$P5000 - mu)/sigma

mu <- mean(X_train$P5010)
sigma <- sd(X_train$P5010)
X_train$P5010 <- (X_train$P5010 - mu)/sigma
X_test$P5010 <- (X_test$P5010 - mu)/sigma

mu <- mean(X_train$Nper)
sigma <- sd(X_train$Nper)
X_train$Nper <- (X_train$Nper - mu)/sigma
X_test$Nper <- (X_test$Nper - mu)/sigma

mu <- mean(X_train$Npersug)
sigma <- sd(X_train$Npersug)
X_train$Npersug <- (X_train$Npersug - mu)/sigma
X_test$Npersug <- (X_test$Npersug - mu)/sigma

mu <- mean(X_train$Ingtotug)
sigma <- sd(X_train$Ingtotug)
X_train$Ingtotug <- (X_train$Ingtotug - mu)/sigma

mu <- mean(X_train$Ingtotugarr)
sigma <- sd(X_train$Ingtotugarr)
X_train$Ingtotugarr <- (X_train$Ingtotugarr - mu)/sigma

mu <- mean(X_train$Li)
sigma <- sd(X_train$Li)
X_train$Li <- (X_train$Li - mu)/sigma
X_test$Li <- (X_test$Li - mu)/sigma

mu <- mean(X_train$Npobres)
sigma <- sd(X_train$Npobres)
X_train$Npobres <- (X_train$Npobres - mu)/sigma

mu <- mean(X_train$Nindigentes)
sigma <- sd(X_train$Nindigentes)
X_train$Nindigentes <- (X_train$Nindigentes - mu)/sigma

glimpse(X_train)
glimpse(X_test)



## Taller 2

#clear all
rm(list = ls())

# paquetes
install.packages("here")
install.packages("caret")
install.packages("leaps")
require("leaps")
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
summary(jefeformal)
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
summary(forward)
str<-str(forward)

## out
data(forward)
sumtable(forward)
vartable <- vtable(forward,out='return')


#OLS
install.packages("apaTables")
library(apaTables)

reg_ols<-lm(Ingpcug ~ jefehombre+jefeformal+persxcuarto, data = train_hogares)
reg_ols
summary(reg_ols)$coefficient
apa.reg.table(reg_ols, filename= "regresion_ingreso.doc", table.number= 1)


#RIDGE
#ridge <- train(
#  Ingpcug ~., data = train_hogares, method = "glmnet",
#  trControl = trainControl("cv", number = 10),
#  tuneGrid = expand.grid(alpha = 0,lambda=lambda), preProcess = c("center", "scale")
#)
#ridge
install.packages("glmnet")
require("glmnet")
modelo_ridge <- glmnet(
  x = train_hogares,
  y = train_hogares$Ingpcug,
  alpha = 0,
  nlambda = 300,
  standardize = FALSE
)
modelo_ridge

####División train y estandarización####
library(pacman)
p_load(fastDummies, caret, glmnet, MLmetrics)

#Dividimos train_hogares en dos
set.seed(666)
id_train <- sample(1:nrow(train_hogares), size = 0.7*nrow(train_hogares), 
                   replace = FALSE)

train <- tr_hog_d[id_train, ]
test  <- tr_hog_d[-id_train, ]

# y_train y x_train
#y_train -> modelo predicción ingreso
y_train <- train[,"Ingpcug"]
#Vector de X
X_train <- select(train, -c(Pobre, Ingpcug))
#y_test -> modelo con ingreso per capita
y_test <- test[,"Ingpcug"]
#Vector  de X test
X_test <- select(test,-c(Pobre, Ingpcug)) 

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

mu <- mean(X_train$Li)
sigma <- sd(X_train$Li)
X_train$Li <- (X_train$Li - mu)/sigma
X_test$Li <- (X_test$Li - mu)/sigma

glimpse(X_train)
glimpse(X_test)

####Lasso####
# Para obtener un ajuste con regularización Lasso se indica argumento alpha = 1.
# Si no se especifica valor de lambda, se selecciona un rango automático.
modelo_lasso <- glmnet(
  x = X_train,
  y = y_train,
  alpha = 1,
  nlambda = 300,
  standardize = FALSE,
  na.rm = TRUE
)

# Analicemos cómo cambian los coeficientes para diferentes lambdas
regularizacion <- modelo_lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo_lasso$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10",
                                  scales::math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización (Lasso)", x = "Lambda", y = "Coeficientes") +
  theme_bw() +
  theme(legend.position="bottom")
# ¿Cómo escoger el mejor lambda? 
# Veamos cuál es el mejor prediciendo (fuera de muestra)
# En este caso vamos a crear la predicción para cada uno de los
# 300 lambdas seleccionados
predicciones_lasso <- predict(modelo_lasso, 
                              newx = as.matrix(X_test))
lambdas_lasso <- modelo_lasso$lambda

# Cada predicción se va a evaluar
resultados_lasso <- data.frame()
for (i in 1:length(lambdas_lasso)) {
  l <- lambdas_lasso[i]
  y_hat_out2 <- predicciones_lasso[, i]
  r22 <- R2_Score(y_pred = y_hat_out2, y_true = y_test)
  rmse2 <- RMSE(y_pred = y_hat_out2, y_true = y_test)
  resultado <- data.frame(Modelo = "Lasso",
                          Muestra = "Fuera",
                          Lambda = l,
                          R2_Score = r22, 
                          RMSE = rmse2)
  resultados_lasso <- bind_rows(resultados_lasso, resultado)
}
#RMSE
ggplot(resultados_lasso, aes(x = Lambda, y = RMSE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)
#R2
ggplot(resultados_lasso, aes(x = Lambda, y = R2_Score)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

filtro <- resultados_lasso$RMSE == min(resultados_lasso$RMSE)
mejor_lambda_lasso <- resultados_lasso[filtro, "Lambda"]

# Guardamos el mejor Lasso
y_hat_in2 <- predict.glmnet(modelo_lasso,
                            newx = as.matrix(X_train),
                            s = mejor_lambda_lasso)
y_hat_out2 <- predict.glmnet(modelo_lasso,
                             newx = as.matrix(X_test),
                             s = mejor_lambda_lasso)

# Métricas dentro y fuera de muestra. Paquete MLmetrics
r2_in2 <- R2_Score(y_pred = exp(y_hat_in2), y_true = exp(y_train))
rmse_in2 <- RMSE(y_pred = exp(y_hat_in2), y_true = exp(y_train))

r2_out2 <- R2_Score(y_pred = exp(y_hat_out2), y_true = exp(y_test))
rmse_out2 <- RMSE(y_pred = exp(y_hat_out2), y_true = exp(y_test))

# Guardamos el desempeño
resultados2 <- data.frame(Modelo = "Lasso", 
                          Muestra = "Dentro",
                          R2_Score = r2_in2, RMSE = rmse_in2) %>%
  rbind(data.frame(Modelo = "Lasso", 
                   Muestra = "Fuera",
                   R2_Score = r2_out2, RMSE = rmse_out2))

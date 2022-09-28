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



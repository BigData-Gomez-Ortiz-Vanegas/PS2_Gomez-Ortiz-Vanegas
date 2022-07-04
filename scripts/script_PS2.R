setwd("C:/Users/Sofia/Documents/2022-2/BigData") 

require(pacman)
require("here")
require("tidyverse")
p_load(tidyverse, rvest)
library(textreadr)
library(table1)
library(sjPlot)
library(stargazer)
library(gmodels)
library(caret)

## 0. Limpieza de datos

#Importar las bases de la instrucción

hog_test <- readRDS("C:/Users/Sofia/Documents/2022-2/BigData/test_hogares.Rds")
per_test <- readRDS("C:/Users/Sofia/Documents/2022-2/BigData/test_personas.Rds")
hog_train <- readRDS("C:/Users/Sofia/Documents/2022-2/BigData/train_hogares.Rds")
per_train <- readRDS("C:/Users/Sofia/Documents/2022-2/BigData/train_personas.Rds")

#La base de train contiene más variables que la base de test - se recurre a
#modelos para completar las variables relevantes

#Renombrar las variables que se van a utilizar

per_test <- rename(per_test,
                   cab_municipal = Clase,
                   antiguedad_industria = P6426,
                   edad = P6040,
                   educacion = P6210,
                   r_jefe_hogar = P6050,
                   sexo = P6020
  )

per_train <- rename(per_train,
                    cab_municipal = Clase,
                    antiguedad_industria = P6426,
                    edad = P6040,
                    educacion = P6210,
                    r_jefe_hogar = P6050,
                    sexo = P6020
)

# a. Ingreso total (Ingtot)

per_train$Ingtot <- as.numeric(gsub("\\.", "", per_train$Ingtot)) # pasar variable a formato numérico
per_train$Oficio <- as.numeric(gsub("\\.", "", per_train$Oficio)) #pasar variable a formato numérico

mod1<-lm(Ingtot ~ Oficio + Dominio + cab_municipal + edad + educacion +antiguedad_industria, per_train) #Se corre el modelo en la base de train (variables explicativas: oficico, municipio, cabecera municipal o resto, edad, max nivel educativo, antiguedad en la empresa)
ing_predicho<-predict(mod1,newdata=per_test) #se predice con los datos de la base de test
ing_predicho <- replace(ing_predicho, ing_predicho<0,0) #reemplazar valores negativos por 0
per_test$Ingtot <- ing_predicho #se agrega a la base de per_test


# Pasar las variables relevantes en la base de personas a nivel de hogar

#Ingtot

ingtot_sum_test <- aggregate(Ingtot ~ id, data=per_test, FUN=sum) #sumar el ingreso total a nivel de hogar
ingtot_sum_train <- aggregate(Ingtot ~ id, data=per_train, FUN=sum) #sumar el ingreso total a nivel de hogar
hog_test <- merge(hog_test,ingtot_sum_test, by = "id") # agregar a base de hogares test
hog_train <- merge(hog_train, ingtot_sum_train, by = "id") # agregar a base de hogares train

#Sexo de jefe/a del hogar

jefe_sexo_test <- select(per_test, c(id, r_jefe_hogar, sexo)) # crear nueva tabla con las variables de id, jefe de hogar y sexo
jefe_sexo_test <- jefe_sexo_test[jefe_sexo_test$r_jefe_hogar == 1, ] # solo conservar las observaciones de los jefe de hogar
hog_test <- merge(hog_test, jefe_sexo_test, by = "id") # juntar con base de hogares test


jefe_sexo_train <- select(per_train, c(id, r_jefe_hogar, sexo)) # crear nueva tabla con las variables de id, jefe de hogar y sexo
jefe_sexo_train <- jefe_sexo_train[jefe_sexo_train$r_jefe_hogar == 1, ] # solo conservar las observaciones de los jefe de hogar
hog_train <- merge(hog_train, jefe_sexo_train, by = "id") # juntar con base de hogares test

hog_test <- rename(hog_test, sexo_jefe_hogar = sexo) # cambiar nombre de la variable "sexo"
hog_train <- rename(hog_train, sexo_jefe_hogar = sexo)

hog_test$r_jefe_hogar <- NULL # eliminar variable que indicaba que si era o no jefe del hogar (toda la columna era =1)
hog_train$r_jefe_hogar <- NULL

write.csv(hog_test ,"hogares_test.csv") #exportar
write.csv(hog_train ,"hogares_train.csv") #exportar

## 1. Estadísticas descriptivas

#gráficos

ggplot(combined,aes(x= hog_train$Ingtot, y=hog_test$Ingtot))

#tablas
summary(hog_test$Ingtot) #ingreso total
summary(hog_test$sexo_jefe_hogar)#sexo del jefe del hogar
summary(hog_test$P5000)#cantidad de cuartos en el hogar
summary(hog_test$P5010)#cantidad de cuartos en los que duermen las personas del hogar
summary(hog_test$P5100)#cuota amortización
summary(hog_test$P5130)#si tuviera que pagar arriendo cuánto sería
summary(hog_test$P5140)#pago por arriendo
summary(hog_test$Nper)#número de personas en el hogar
summary(hog_test$Npersug)#número de personas en la unidad de gasto
summary(hog_test$Li)#línea de indigencia
summary(hog_test$Lp)#línea de pobreza

summary(hog_train$Ingtot)
summary(hog_train$sexo_jefe_hogar)
summary(hog_train$P5000)#cantidad de cuartos en el hogar
summary(hog_train$P5010)#cantidad de cuartos en los que duermen las personas del hogar
summary(hog_train$P5100)#cuota amortización
summary(hog_train$P5130)#si tuviera que pagar arriendo cuánto sería
summary(hog_train$P5140)#pago por arriendo
summary(hog_train$Nper)#número de personas en el hogar
summary(hog_train$Npersug)#número de personas en la unidad de gasto
summary(hog_train$Ingtotug)#Ingreso total de la unidad de gasto antes de imputación de arriendo a propietarios y usufructuarios 
summary(hog_train$Ingtotugarr)#Ingreso total de la unidad de gasto con imputación de arriendo a propietarios y usufructuarios
summary(hog_train$Ingpcug)#Ingreso percápita de la unidad de gasto con imputación de arriendo a propietarios y usufructuarios 
summary(hog_train$Li)#línea de indigencia
summary(hog_train$Lp)#línea de pobreza
summary(hog_train$Pobre)#Pobre =1, 0 d.l.c.
summary(hog_train$Indigente)#Indigente =1, 0 d.l.c.
summary(hog_train$Npobres)#número de pobres
summary(hog_train$Nindigentes)#número de indigentes
setwd("~/Desktop/Problem Set 2")


rm(list=ls())
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
library(doParallel)

n_cores <- detectCores()
print(paste("Mi PC tiene", n_cores, "nucleos"))

## 0. Limpieza de datos
#Importar las bases de la instruccion
hog_test <- read.csv("test_hogares.csv",sep=";")
per_test <- read.csv("test_personas.csv",sep=";")
hog_train <- read.csv("train_hogares.csv",sep=";")
per_train <- read.csv("train_personas.csv",sep=";")
# Limpiar bases a nivel persona
per_test <- per_test %>% mutate(base = 'TEST')
per_train <- per_train %>% mutate(base = 'TRAIN')
per_4_cleaning <- union_all(per_test, per_train)
#Renombrar las variables que se van a utilizar
per_4_cleaning <- rename(per_4_cleaning,
                         cab_municipal = Clase,
                         antiguedad_industria = P6426,
                         edad = P6040,
                         educacion = P6210,
                         r_jefe_hogar = P6050,
                         sexo = P6020,
                         cotiza_salud = P6090,
                         horas_trabajo = P6800,
                         cotiza_pension = P6920,
                         quiere_mas_trabajo = P7090,
                         subsidios = P7505
)
per_4_cleaning <- per_4_cleaning %>% select(id, cab_municipal, Dominio, sexo,
                                            edad, r_jefe_hogar, cotiza_salud, cotiza_pension,
                                            educacion, Oficio, antiguedad_industria,
                                            horas_trabajo, quiere_mas_trabajo, subsidios, base)
per_4_cleaning <- per_4_cleaning %>% mutate(
  Oficio = case_when(
    Oficio ==  1 ~ "Químicos",
    Oficio ==  2 ~ "Arquitectos",
    Oficio ==  3 ~ "Agrimensores",
    Oficio ==  4 ~ "Pilotos",
    Oficio ==  5 ~ "Biólogos",
    Oficio ==  6 ~ "Médicos",
    Oficio ==  7 ~ "Enfermeros",
    Oficio ==  8 ~ "Estadistas",
    Oficio ==  9 ~ "Economistas",
    Oficio ==  11 ~ "Contadores",
    Oficio ==  12 ~ "Abogados",
    Oficio ==  13 ~ "Docentes",
    Oficio ==  14 ~ "Sacerdotes",
    Oficio ==  15 ~ "Escritores",
    Oficio ==  16 ~ "Escultores",
    Oficio ==  17 ~ "Compositores",
    Oficio ==  18 ~ "Atletas",
    Oficio ==  19 ~ "Bibliotecarios",
    Oficio ==  20 ~ "Cuerpos legislativos",
    Oficio ==  21 ~ "Directores",
    Oficio ==  30 ~ "Jefes de empleados de oficinas",
    Oficio ==  31 ~ "Agentes administrativos",
    Oficio ==  32 ~ "Secretarias",
    Oficio ==  33 ~ "Auxiliar de contabilidad",
    Oficio ==  34 ~ "Operadores de máquinas",
    Oficio ==  35 ~ "Jefes de estaciones de ferrocarril",
    Oficio ==  36 ~ "Jefes de tren",
    Oficio ==  37 ~ "Carteros y mensajeros",
    Oficio ==  38 ~ "Telefonistas",
    Oficio ==  39 ~ "Almacenistas",
    Oficio ==  40 ~ "Director",
    Oficio ==  41 ~ "Comerciante",
    Oficio ==  42 ~ "Jefes de ventas",
    Oficio ==  43 ~ "Agente de ventas",
    Oficio ==  44 ~ "Agente de seguros",
    Oficio ==  45 ~ "Vendedores",
    Oficio ==  49 ~ "Prestamista",
    Oficio ==  50 ~ "Directores de hotel",
    Oficio ==  51 ~ "Gerente - propietario de hotel",
    Oficio ==  52 ~ "Mayordomo",
    Oficio ==  53 ~ "Cocineros",
    Oficio ==  54 ~ "Empleada doméstica",
    Oficio ==  55 ~ "Guardián de edificio",
    Oficio ==  56 ~ "Lavanderos",
    Oficio ==  57 ~ "Peluqueros",
    Oficio ==  58 ~ "Bomberos",
    Oficio ==  59 ~ "Guías de turismo",
    Oficio ==  60 ~ "Administrador de explotación agropecuaria",
    Oficio ==  61 ~ "Agricultores y ganaderos",
    Oficio ==  62 ~ "Trabajadores agropecuarios en general",
    Oficio ==  63 ~ "Talador de árboles",
    Oficio ==  64 ~ "Pescadores en río y mar",
    Oficio ==  70 ~ "Supervisor de fabricación",
    Oficio ==  71 ~ "Minero",
    Oficio ==  72 ~ "Fundidor",
    Oficio ==  73 ~ "Trabajadores de tratamiento de la madera",
    Oficio ==  74 ~ "Operarios de los tratamientos químicos",
    Oficio ==  75 ~ "Clasificador de fibras",
    Oficio ==  76 ~ "Curtidor",
    Oficio ==  77 ~ "Trabajadores de la preparación de alimentos y bebidas – panaderos",
    Oficio ==  78 ~ "Trabajadores en el procesamiento del tabaco",
    Oficio ==  79 ~ "Sastres",
    Oficio ==  80 ~ "Zapateros",
    Oficio ==  81 ~ "Carpinteros",
    Oficio ==  82 ~ "Labrantes y adornistas",
    Oficio ==  83 ~ "Herreros",
    Oficio ==  84 ~ "Ajustadores",
    Oficio ==  85 ~ "Electricistas de vivienda y automotriz",
    Oficio ==  86 ~ "Operadores de estaciones de radio y TV",
    Oficio ==  87 ~ "Fontaneros",
    Oficio ==  88 ~ "Joyeros y plateros",
    Oficio ==  89 ~ "Vidrieros",
    Oficio ==  90 ~ "Trabajadores de la fabricación de productos de caucho y plástico",
    Oficio ==  91 ~ "Confeccionadores de cajas",
    Oficio ==  92 ~ "Trabajadores de las artes gráficas",
    Oficio ==  93 ~ "Pintor de edificaciones",
    Oficio ==  94 ~ "Trabajadores manufactureros",
    Oficio ==  95 ~ "Albañiles",
    Oficio ==  96 ~ "Operador de instalaciones de producción de energía eléctrica",
    Oficio ==  97 ~ "Manipulación de mercancía y movimiento de tierras",
    Oficio ==  98 ~ "Conductores de vehículos de transporte",
    Oficio ==  99 ~ "Peones no clasificados",
    TRUE ~ 'Sin Oficio'
  ),
  cotiza_salud = case_when(
    cotiza_salud == 1 ~ 'Si cotiza',
    cotiza_salud == 2 ~ 'No cotiza',
    TRUE ~ 'No informa'
  ),
  cotiza_pension = case_when(
    cotiza_pension == 1 ~ 'Si cotiza',
    cotiza_pension == 2 ~ 'No cotiza',
    cotiza_pension == 3 ~ 'Ya es pensionado',
    TRUE ~ 'No informa'
  ),
  quiere_mas_trabajo = case_when(
    quiere_mas_trabajo == 1 ~ 'Si',
    quiere_mas_trabajo == 2 ~ 'No',
    TRUE ~ 'No sabe/Notrabaja'
  )
  ,
  sexo = case_when(
    sexo == 1 ~ 'Hombre',
    sexo == 2 ~ 'Mujer',
    TRUE ~ 'No binario'
  ),
  subsidios = case_when(
    subsidios == 1 ~ 'si',
    TRUE ~ 'No'
  ),
  horas_trabajo = ifelse(is.na(horas_trabajo), 
                         0, horas_trabajo)
  ,
  antiguedad_industria = ifelse(is.na(antiguedad_industria), 
                                0, antiguedad_industria))
per_4_cleaning <- per_4_cleaning %>% filter(Orden == 1)
per_test <- per_4_cleaning %>% filter(base == 'TEST')
per_train <- per_4_cleaning %>% filter(base == 'TRAIN')
per_test <- per_test %>% select(-base)
per_train <- per_train %>% select(-base)
rm("per_4_cleaning") # Borrar bases que no necesitamos más
# Limpiar bases a nivel hogar
#Renombrar las variables que se van a utilizar
hog_test <- rename(hog_test,
                   cab_municipal = Clase,
                   cuartos = P5000,
                   cuartos_dormir = P5010,
                   estado_propiedad = P5090,
                   valor_arriendo_hipotetico = P5130,
                   valor_arriendo_real = P5140
)
hog_test <- hog_test %>% select(id, cuartos,
                                cuartos_dormir, estado_propiedad, 
                                valor_arriendo_hipotetico,
                                Nper, Npersug, Lp)
hog_train <- rename(hog_train,
                    cab_municipal = Clase,
                    cuartos = P5000,
                    cuartos_dormir = P5010,
                    estado_propiedad = P5090,
                    valor_arriendo_hipotetico = P5130,
                    valor_arriendo_real = P5140
)
hog_train <- hog_train %>% select(id, cuartos,
                                  cuartos_dormir, estado_propiedad, 
                                  valor_arriendo_hipotetico,
                                  Nper, Npersug, Lp, Ingtotugarr)
hog_train <- merge(hog_train, per_train, by = "id")
hog_test <- merge(hog_test, per_test, by = "id")
rm("per_train", "per_test")
# Hogares bajo LP
hog_train <- hog_train %>% mutate(
  h_pobre = case_when(
    Ingtotugarr <= Lp ~ 'si',
    TRUE ~ 'no'
  ))
hog_train <- hog_train %>% mutate_if(is.character,as.factor) %>% select(-Lp, -id)
hog_test <- hog_test %>% mutate_if(is.character,as.factor) %>% select(-Lp, -id)
# 1.Estadisticas descriptivas
#graficos
#cajas y bigotes
library(ggplot2)
library(gridExtra)
library(gapminder)
library(dplyr)

#Ingreso
boxplot(hog_train$Ingtotugarr, ylab="Ingreso Total")
#Numero de personas en el hogar
boxplot(hog_train$Nper, hog_test$Nper, ylab="Número de personas en el hogar")
# Corrr ingreso vs Sexo jefe de hogar
boxplot(hog_train$Ingtotugarr ~ hog_train$sexo, xlab = "sexo del jefe/a del hogar", ylab = "Ingreso total")
# Corr ingreso vs Nper
boxplot(hog_train$Ingtotugarr/hog_train$Nper ~ hog_train$Nper, xlab = "Número de personas en el hogar", ylab = "Ingreso total")
#tablas
descriptiva_train <- table1(~Ingtotugarr+ h_pobre+ cuartos+
                              cuartos_dormir+ estado_propiedad+ 
                              valor_arriendo_hipotetico+
                              Nper+ Npersug+ sexo+ edad+ cotiza_salud+ 
                              cotiza_pension+ educacion+ horas_trabajo+
                              quiere_mas_trabajo,
                            data=hog_train, overall="Total")
descriptiva_train
# Eliminar missing values, dado que algunos modelos no pueden procesarlos
hog_train <- hog_train %>% drop_na()
(prop.table(table(hog_train$h_pobre)))*100# Solo el 2.7% es pobre

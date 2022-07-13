setwd("C:/Users/USER/OneDrive - Universidad de los Andes/Escritorio/BigData/PS2")

rm(list=ls())

install.packages("doParallel")
install.packages("ROSE")
require(pacman)
require("here")
require("tidyverse")
p_load(tidyverse, rvest,doParallel, rattle, MLmetrics,
       janitor, fastDummies, tidymodels)
library(textreadr)
library(table1)
library(sjPlot)
library(stargazer)
library(gmodels)
library(caret)
library("class")
library(ggplot2)

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

per_4_cleaning <- per_4_cleaning %>% select(id, sexo,
                                            edad,educacion, 
                                            antiguedad_industria,
                                            horas_trabajo, 
                                            quiere_mas_trabajo, 
                                            Orden, base)

per_4_cleaning <- per_4_cleaning %>% mutate(
  cotiza_salud = ifelse(is.na(cotiza_salud),3, cotiza_salud),
  cotiza_pension = ifelse(is.na(cotiza_pension),4, cotiza_pension),
  quiere_mas_trabajo = ifelse(is.na(quiere_mas_trabajo),3, quiere_mas_trabajo),
  subsidios = ifelse(is.na(subsidios),3, subsidios),
  horas_trabajo = ifelse(is.na(horas_trabajo),0, horas_trabajo),
  antiguedad_industria = ifelse(is.na(antiguedad_industria),0, antiguedad_industria))

per_4_cleaning <- per_4_cleaning %>% filter(Orden == 1)

# Categóricas a factores
per_4_cleaning$sexo <- factor(per_4_cleaning$sexo)
per_4_cleaning$quiere_mas_trabajo <- factor(per_4_cleaning$quiere_mas_trabajo)
per_test <- per_4_cleaning %>% filter(base == 'TEST')
per_train <- per_4_cleaning %>% filter(base == 'TRAIN')

per_test <- per_test %>% select(-base, -Orden)
per_train <- per_train %>% select(-base, -Orden)

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

hog_test <- hog_test %>% select(id,estado_propiedad, cuartos,
                                valor_arriendo_hipotetico,
                                Nper, Lp)

hog_train <- rename(hog_train,
                    cab_municipal = Clase,
                    cuartos = P5000,
                    cuartos_dormir = P5010,
                    estado_propiedad = P5090,
                    valor_arriendo_hipotetico = P5130,
                    valor_arriendo_real = P5140
)

hog_train <- hog_train %>% select(id, estado_propiedad, cuartos,valor_arriendo_hipotetico,
                                  Nper, Lp, Ingtotugarr)


hog_train <- merge(hog_train, per_train, by = "id")
hog_test <- merge(hog_test, per_test, by = "id")

# Categóricas a factores
hog_train$estado_propiedad <- factor(hog_train$estado_propiedad)

hog_test$estado_propiedad <- factor(hog_test$estado_propiedad)

rm("per_train", "per_test")

# Hogares bajo LP
hog_train <- hog_train %>% mutate(
  h_pobre = case_when(
    Ingtotugarr <= (Lp*Nper) ~ 'si',
    TRUE ~ 'no'
  ))
prop.table(table(hog_train$h_pobre)) #49% no, 50% si



hog_train <- hog_train %>% mutate_if(is.character,as.factor) %>% select(-id, -Ingtotugarr)
hog_test <- hog_test %>% mutate_if(is.character,as.factor) %>% select(-Lp)

hog_train <- hog_train %>% drop_na()

hog_train_backup <- hog_train
hog_test_backup <- hog_test


split1 <- createDataPartition(hog_train$h_pobre, p = .8)[[1]]
other <- hog_train[-split1,]
training <- hog_train[ split1,]

set.seed(10101)
split2 <- createDataPartition(other$h_pobre, p = 1/3)[[1]]
evaluation <- other[ split2,]
testing <- other[-split2,]

# SMOTE como técnica de rebalanceo
library("ROSE")
smote <- ovun.sample(h_pobre ~ ., training, method='both')
oversampled_data = smote$data
prop.table(table(oversampled_data$h_pobre)) #49% no, 50% si

# Modelo 1: k-NN

# Modelo 1.1: Sin remuestro, K = 3
# Modelo 1.2: Sin remuestro, K = 5
# Modelo 1.3: Sin remuestro, K = 9

# Modelo 1.1.1: SMOTE, K = 3 
# Modelo 1.2.1: SMOTE, K = 5
# Modelo 1.3.1: SMOTE, K = 9

# Modelo 1.1.2: SMOTE, K = 3, cutoff
# Modelo 1.2.2: SMOTE, K = 5, cutoff
# Modelo 1.3.2: SMOTE, K = 9, cutoff

# Revisar cantidad de nucleos:
n_cores <- detectCores()
print(paste("Mi PC tiene", n_cores, "nucleos"))

cl <- makePSOCKcluster(n_cores - 2) 
registerDoParallel(cl)

# Modelos 1.1, 1.2, 1.3:
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)
set.seed(10101)
knn <- train(h_pobre ~ .
             , data = training
             , method = "knn"
             , trControl = ctrl
             , preProcess = c("center","scale"))
# Liberamos nuestros procesadores
stopCluster(cl)
knn

# Modelos 1.1.1, 1.2.1, 1.3.1:
set.seed(10101)

cl <- makePSOCKcluster(n_cores - 2) 
registerDoParallel(cl)
knn_smote <- train(h_pobre ~ .
                   , data = oversampled_data
                   , method = "knn"
                   , trControl = ctrl
                   , preProcess = c("center","scale"))
stopCluster(cl)

knn_smote

# Modelos 1.1.2, 1.2.2, 1.3.2:
set.seed(10101)
cl <- makePSOCKcluster(n_cores - 2) 
registerDoParallel(cl)
knn_roc <- train(h_pobre ~ .
                 , data = oversampled_data
                 , method = "knn"
                 , trControl = ctrl
                 , metric = "ROC"
                 , preProcess = c("center","scale"))
stopCluster(cl)

knn_roc

# Evaluar en muestra de evaluacion para obtener cutoff optimo
set.seed(10101)
evalResults <- data.frame(Default = evaluation$h_pobre)
evalResults$Roc <- predict(knn_roc,
                           newdata = evaluation,
                           type = "prob")[,1]

library(pROC)
set.seed(10101)
cl <- makePSOCKcluster(n_cores - 2) 

rfROC <- roc(evalResults$Default, evalResults$Roc, 
             levels=levels(as.factor(evalResults$Default)),direction = ">")
rfThresh <- coords(rfROC, x = "best", best.method = "closest.topleft")
rfThresh # Cutoff = 0.47
set.seed(10101)
plot(rfROC, print.thres = "best")
stopCluster(cl)


# Comparacion de kNN

testResults <- data.frame(Default = testing$h_pobre)

# Modelos 1.1, 1.2, 1.3:
testResults$knn_test <- predict(knn,newdata = testing,
                                type = "prob")[,1]

# Modelos 1.1.1, 1.2.1, 1.3.1:
testResults$knn_smote_test <- predict(knn_smote,newdata = testing,
                                      type = "prob")[,1]

# Modelos 1.1.2, 1.2.2, 1.3.2:
testResults$knn_roc_test <- predict(knn_roc,newdata = testing,
                                    type = "prob")[,1]

# Matrices de confusion
testResults<-testResults %>%
  mutate(knn_test=ifelse(knn_test>0.5,"si","no"),
         knn_smote_test=ifelse(knn_smote_test>0.5,"si","no"),
         knn_roc_test=ifelse(knn_roc_test>rfThresh$threshold,"si","no"))

# Predicciones dentro de muestra
with(testResults,table(Default,knn_test)) # Mejor modelo
with(testResults,table(Default,knn_smote_test))
with(testResults,table(Default,knn_roc_test))

# Modelo 2) Random Forest

# Dummyficar vars categoricas
split1 <- createDataPartition(hog_train$h_pobre, p = .8)[[1]]
other <- hog_train[-split1,]
training <- hog_train[ split1,]


filtro <- !sapply(training, is.numeric)
categoricas_x <- names(training)[filtro]
categoricas_x <- categoricas_x[categoricas_x != "h_pobre"]

categoricas_eval <- names(training)[filtro]
categoricas_eval <- categoricas_eval[categoricas_eval != "h_pobre"]

categoricas_y <- names(other)[filtro]
categoricas_y <- categoricas_y[categoricas_y != "h_pobre"]

training2 <-  dummy_cols(training, select_columns = categoricas_x, 
                         remove_selected_columns = T)
other2 <-  dummy_cols(other, select_columns = categoricas_y, 
                      remove_selected_columns = T)
oversampled_data2 <- dummy_cols(oversampled_data, select_columns = categoricas_x, 
                                remove_selected_columns = T)

# Modelo 2.1 Básico
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

modelo1 <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

cl <- makePSOCKcluster(n_cores - 2) 
registerDoParallel(cl)

# Importancia de variables:
modelo1_fit <- fit(modelo1, h_pobre ~ ., data = training2)
stopCluster(cl)

importancia <- varImp(modelo1_fit$fit)
importancia <- importancia %>%
  data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Porcentaje = Overall/sum(Overall)) %>%
  filter(Porcentaje > 0) %>%
  arrange(desc(Porcentaje))

ggplot(importancia, aes(x = Porcentaje, 
                        y = reorder(Variable, Porcentaje))) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.8) +
  labs(y = "Variable") +
  scale_x_continuous(labels = scales::percent) +
  theme_classic()


# Modelo 2.1.1 Básico + SMOTE
modelo12 <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

cl <- makePSOCKcluster(n_cores - 2) 
registerDoParallel(cl)

modelo1_fit2 <- fit(modelo12, h_pobre ~ ., data = oversampled_data2)
stopCluster(cl)

# Modelo 2.2 Hallando Hiperparámetros + SMOTE
modelo2 <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

# Creamos grilla
tree_grid <- crossing(
  cost_complexity = c(0.0001),
  min_n = c(4, 8, 12),
  tree_depth = c(2, 8, 16)
)

# Definimos CV
set.seed(10101)
folds <- vfold_cv(oversampled_data2, strata = h_pobre, v = 5)

# Entrenamos el modelo utilizando procesamiento en paralelo
cl <- makePSOCKcluster(n_cores - 2) 
registerDoParallel(cl)

set.seed(10101)
modelo2_cv <- tune_grid(
  modelo2,
  h_pobre ~ .,
  resamples = folds,
  grid = tree_grid,
  metrics = metric_set(f_meas),
  control = control_grid(event_level = 'second')
)
stopCluster(cl)

# Escogemos el mejor modelo
modelo2 <- finalize_model(modelo2, select_best(modelo2_cv))
# Entrenamos el mejor modelo
modelo2_fit_depth <- fit(modelo2, h_pobre ~ ., training2)

# Comparacion de RF
library('caret')
testResults2 <- data.frame(Default = other2$h_pobre)

# Modelos 1.1, 1.2, 1.3:
testResults2$modelo_es <- predict(modelo1_fit,new_data = other2,
                                  type = "prob")[,1]

# Modelos 1.1.1, 1.2.1, 1.3.1:
testResults2$modelo_smote <- predict(modelo1_fit2,new_data = other2,
                                     type = "prob")[,1]

# Modelos 1.1.2, 1.2.2, 1.3.2:
testResults2$modelo_depth <- predict(modelo2_fit_depth,new_data = other2,
                                     type = "prob")[,1]

names(testResults2)[2] <- "modelo1"
names(testResults2)[3] <- "modelo2"
names(testResults2)[4] <- "modelo3"

# Matrices de confusion
testResults2<-testResults2 %>%
  mutate(modelo_es=ifelse(modelo1>0.5,"si","no"),
         modelo_smote=ifelse(modelo2>0.5,"si","no"),
         modelo_depth=ifelse(modelo3>0.5,"si","no"))

# Predicciones dentro de muestra
with(testResults2,table(Default,modelo_es)) # Mejor modelo
with(testResults2,table(Default,modelo_smote))
with(testResults2,table(Default,modelo_depth))

# Mejor modelo Random Forest Sencillo
hog_test <- hog_test %>% drop_na()

submission <- data.frame(id = hog_test$id)
submission$Pobre_classification <- predict(modelo1_fit,
                                           new_data = hog_test,
                                           type = "prob")[,1]

rm("knn", "knn_roc", "knn_smote", "modelo1", "modelo1_fit", "modelo2", 
   "modelo12", "modelo2_cv", "modelo2_fit_depth", "other", "oversampled_data2",
   "rfROC", "rfThresh", "smote", "testing2", "testResults", "testResults2",
   "training2", "tree_grid", "ctrl", "eval2", "folds", "evalResults", "modelo1_fit2")

## Predicción de Ingreso
# Limpiar nuevamente
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

per_4_cleaning <- per_4_cleaning %>% select(id, sexo,
                                            edad,educacion, 
                                            antiguedad_industria,
                                            horas_trabajo, 
                                            quiere_mas_trabajo, 
                                            Orden, base)

per_4_cleaning <- per_4_cleaning %>% mutate(
  Oficio = ifelse(is.na(Oficio),9999, Oficio),
  cotiza_salud = ifelse(is.na(cotiza_salud),3, cotiza_salud),
  cotiza_pension = ifelse(is.na(cotiza_pension),4, cotiza_pension),
  quiere_mas_trabajo = ifelse(is.na(quiere_mas_trabajo),3, quiere_mas_trabajo),
  subsidios = ifelse(is.na(subsidios),3, subsidios),
  horas_trabajo = ifelse(is.na(horas_trabajo),0, horas_trabajo),
  antiguedad_industria = ifelse(is.na(antiguedad_industria),0, antiguedad_industria))

per_4_cleaning <- per_4_cleaning %>% filter(Orden == 1)

# Categóricas a factores
per_4_cleaning$sexo <- factor(per_4_cleaning$sexo)
per_4_cleaning$Oficio <- factor(per_4_cleaning$Oficio)
per_4_cleaning$quiere_mas_trabajo <- factor(per_4_cleaning$quiere_mas_trabajo)
per_test <- per_4_cleaning %>% filter(base == 'TEST')
per_train <- per_4_cleaning %>% filter(base == 'TRAIN')

per_test <- per_test %>% select(-base, -Orden)
per_train <- per_train %>% select(-base, -Orden)

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

hog_test <- hog_test %>% select(id,estado_propiedad, cuartos,
                                valor_arriendo_hipotetico,
                                Nper, Lp)

hog_train <- rename(hog_train,
                    cab_municipal = Clase,
                    cuartos = P5000,
                    cuartos_dormir = P5010,
                    estado_propiedad = P5090,
                    valor_arriendo_hipotetico = P5130,
                    valor_arriendo_real = P5140
)

hog_train <- hog_train %>% select(id, estado_propiedad, cuartos,valor_arriendo_hipotetico,
                                  Nper, Lp, Ingtotugarr)

hog_train <- merge(hog_train, per_train, by = "id")
hog_test <- merge(hog_test, per_test, by = "id")

# Categóricas a factores
hog_train$estado_propiedad <- factor(hog_train$estado_propiedad)
hog_test$estado_propiedad <- factor(hog_test$estado_propiedad)

rm("per_train", "per_test")

# Hogares bajo LP
hog_train <- hog_train %>% mutate(
  h_pobre = case_when(
    Ingtotugarr <= (Lp*Nper) ~ 'si',
    TRUE ~ 'no'
  ))
prop.table(table(hog_train$h_pobre)) #49% no, 50% si

hog_train <- hog_train %>% mutate_if(is.character,as.factor) %>% select(-id, -h_pobre)
hog_test <- hog_test %>% mutate_if(is.character,as.factor)

hog_train <- hog_train %>% drop_na()

# Revisar cantidad de nucleos:
n_cores <- detectCores()
print(paste("Mi PC tiene", n_cores, "nucleos"))

set.seed(10101)
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl<- trainControl(method = "cv",
                    number = 5,
                    #summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

# Modelo 0: benchmark
benchmark <- lm(Ingtotugarr~1,hog_train)

RMSE_benchmark <- with(hog_train,mean((Ingtotugarr-predict(benchmark))^2))
RMSE_benchmark

# Modelo 1: OLS + edad
ols1 <- lm(Ingtotugarr~edad,hog_train)
RMSE_ols1 <- with(hog_train,mean((Ingtotugarr-predict(ols1))^2))
tab_model(ols1)

# Modelo 2: OLS + edad + CV
cl <- makePSOCKcluster(n_cores - 2) 
registerDoParallel(cl)
ols2 <- train(
  Ingtotugarr ~ edad,
  data = hog_train,
  method = "lm",
  trControl = ctrl,
  family = "binomial",
  preProcess = c("center", "scale")
)
stopCluster(cl)
ols2

# Modelo 3: OLS + controles
ols3 <- lm(Ingtotugarr~.,hog_train)
RMSE_ols3 <- with(hog_train,mean((Ingtotugarr-predict(ols3))^2))
tab_model(ols3)
RMSE_ols3

# Modelo 4: OLS + controles + CV
cl <- makePSOCKcluster(n_cores - 2) 
registerDoParallel(cl)
ols4 <- train(
  Ingtotugarr ~ .,
  data = hog_train,
  method = "lm",
  trControl = ctrl,
  family = "binomial",
  preProcess = c("center", "scale")
)
stopCluster(cl)
ols4 # Mejor modelo

# Convertir a binario
hog_train_backup <- hog_train_backup %>% mutate(
  h_pobre = case_when(
    Ingtotugarr <= (Lp*Nper) ~ 'si',
    TRUE ~ 'no'
  ))
testResults3 <- data.frame(Lp = hog_train_backup$Lp)
ols1_r <- predict(ols1)
ols2_r <- predict(ols2)
ols3_r <- predict(ols3)
ols4_r <- predict(ols4)
testResults3$X1 <- ols1_r
testResults3$X2 <- ols2_r
testResults3$X3 <- ols3_r
testResults3$X4 <- ols4_r

testResults3$h_pobre <- hog_train_backup$h_pobre

# Matrices de confusion
testResults3<-testResults3 %>%
  mutate(ols1=ifelse(X1<Lp,"si","no"),
         ols2=ifelse(X2<Lp,"si","no"),
         ols3=ifelse(X3<Lp,"si","no"),
         ols4=ifelse(X4<Lp,"si","no"))

# Resultados
with(testResults3,table(h_pobre,ols1)) # Mejor modelo
with(testResults3,table(h_pobre,ols2))
with(testResults3,table(h_pobre,ols3))
with(testResults3,table(h_pobre,ols4))

# Predecir en el test set
submission$Pobre_income <- 
  predict(ols4,new_data = hog_test)

ing_predicho<-predict(ols4,newdata=hog_test) 
ing_predicho <- replace(ing_predicho, ing_predicho<0,0)
submission$Pobre_income <- ing_predicho

write.csv(submission ,"submission.csv")

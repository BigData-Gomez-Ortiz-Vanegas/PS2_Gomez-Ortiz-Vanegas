###    BIG DATA
##   Problem Set 1

## El objetivo principal de este problem set es construir un modelo predictivo de la renta individual

#Limpiamos el ambiente
rm(list=ls())

#Establecemos directorio de los datos
setwd("~/Desktop/Taller 1 AI")

# A continuación, instalamos las librerias necesarias
require(pacman)
p_load(tidyverse, rvest)
library(textreadr)
library(table1)
library(sjPlot)
library(stargazer)
library(gmodels)

###1)------------ Data Acquisition ----------------
#para ello se hace un scrapping de los datos de la GEIH 2018, ubicados en un sitio web 

## Scrape 10 htmls
for (i in 1:10)
{
  assign(paste("tabla",i, sep = ""), 
         html_table(html_node(assign(
           paste("datos_ignacio_",i, sep=""),
           rvest::read_html(paste(
             "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html", 
             sep=""))),xpath = '/html/body/table')))
}

## Unir bases de datos
tabla_final <- do.call("rbind", list(tabla1, tabla2, tabla3, tabla4, tabla5,
                                     tabla6, tabla7, tabla8, tabla9, tabla10))
# Exportar (Borrar)
write.csv(tabla_final ,"tabla_final_ps1.csv")

## BORRAR:
tabla_final <- read.csv(file = 'tabla_final_ps1.csv')

#2) -------------Data Cleaning ------------------

## En el enunciado nos piden enfocarnos en empleados mayores de 18 años
#Eliminamos los datos de las personas que no se encuentran ocupadas y son menores de 18 años.

tabla_final <- tabla_final %>% filter(dominio == 'BOGOTA' & age >= 18 & ocu == 1)
keeps <- c("directorio",
           "orden",
           "secuencia_p",
           "age",
           "clase",
           "cotPension",
           "cuentaPropia",
           "dsi",
           "estrato1",
           "formal",
           "hoursWorkUsual",
           "inac",
           "ingtot",
           "ingtotob",
           "iof2",
           "iof2es",
           "maxEducLevel",
           "mes",
           "ocu",
           "oficio",
           "p6050",
           "p6426",
           "regSalud",
           "sex",
           "sizeFirm",
           "y_subEducativo_m",
           "y_subFamiliar_m",
           "y_total_m",
           "y_total_m_ha")

## Feature selection
tabla_final <- tabla_final[keeps]
tabla_final$id_unico <- paste(tabla_final$directorio,
                              tabla_final$secuencia_p,tabla_final$orden,sep='-')
tabla_final <- select(tabla_final, 
                      -c('directorio', 'secuencia_p', 'orden', 'mes'))
#table(tabla_final$relab)

## Convertir variables tipo texto a factor
y <- c("cotPension", "oficio", "regSalud", "p6050", "estrato", "sex")
tabla_final[y] <- lapply(tabla_final[y], factor)
## Order features
col_order <- c("id_unico",
               "age",
               "sex",
               "ingtot",
               "ingtotob",
               "estrato1",
               "maxEducLevel",
               "hoursWorkUsual",
               "cotPension",
               "formal",
               "cuentaPropia",
               "dsi",
               "inac",
               "ocu",
               "oficio",
               "regSalud",
               "p6050",
               "p6426",
               "sizeFirm",
               "iof2",
               "iof2es",
               "y_subEducativo_m",
               "y_subFamiliar_m",
               "y_total_m",
               "y_total_m_ha")
tabla_final <- tabla_final[, col_order]

## Imputación de Missing values
tabla_final <- tabla_final %>% 
  mutate(iof2 = case_when(
    !is.na(iof2es) ~ iof2es,
    TRUE ~ iof2
  ))
## Renombrar variables
tabla_final <- tabla_final %>% 
  select(, -iof2es)  %>%
  rename(r_jefe_hogar = p6050) %>%
  rename(antiguedad_industria = p6426) %>%
  rename(ingreso_pensiones = iof2)
#write.csv(tabla_final ,"tabla_final_2.csv")


# ------------ Analisis Descriptivo --------------
## y: ingtotot
## Creación de variable categórica para edad
tabla_final= tabla_final %>% mutate(
  cat_edad = case_when(
    age <= 30~ '18-30',
    age > 30 & age <= 50 ~ '30-50',
    TRUE ~ '>50'
  )
)
## Tabla descriptiva por rango de edad
descriptiva_1 <- table1(~ age + factor(sex) + ingtot + factor(estrato1) +
                          factor(maxEducLevel) + hoursWorkUsual + factor(cotPension) +
                          factor(formal) + antiguedad_industria | cat_edad, 
                        data=tabla_final, overall="Total")
### Regression 1
tabla_final$age2 <- tabla_final$age*tabla_final$age
reg1 <- lm(ingtotob ~ age + age2, data = tabla_final)
tab_reg <- tab_model(reg1)
## Bootstrap
sample_coef_intercept <- NULL
sample_coef_x1 <- NULL
sample_erstd_x1 <- NULL
sample_coef_x2 <- NULL
sample_erstd_x2 <- NULL
for (i in 1:1000) {
  sample_d = tabla_final[sample(1:nrow(tabla_final), 0.3*nrow(tabla_final), replace = TRUE), ]
  
  model_bootstrap <- lm(ingtot ~ age + age2, data = sample_d)
  
  sample_coef_intercept <-
    c(sample_coef_intercept, model_bootstrap$coefficients[1])
  
  sample_coef_x1 <-
    c(sample_coef_x1, model_bootstrap$coefficients[2])
  
  sample_erstd_x1 <-
    c(sample_erstd_x1, coef(summary(model_bootstrap))[2, 2])
  
  sample_coef_x2 <-
    c(sample_coef_x2, model_bootstrap$coefficients[3])
  
  sample_erstd_x2 <-
    c(sample_erstd_x2, coef(summary(model_bootstrap))[3, 2])
}
coefs <- rbind(sample_coef_intercept, sample_coef_x1, sample_erstd_x1, 
               sample_coef_x2, sample_erstd_x2)

## Combinamos los resultados en una tabla
means.boot = c(mean(sample_coef_intercept), mean(sample_coef_x1), 
               mean(sample_coef_x2))
erstd.boot = c(0,mean(sample_erstd_x1),mean(sample_erstd_x2))
knitr::kable(round(
  cbind(
    sample = coef(summary(reg1))[, c(1,2)],
    bootstrap = means.boot,
    erstdBoots = erstd.boot),4), 
  "simple", caption = "Coefficients in different models")

# Construimos el intervalo de confianza 
confint(reg1)
a <-
  cbind(
    quantile(sample_coef_intercept, prob = 0.025),
    quantile(sample_coef_intercept, prob = 0.975))
b <-
  cbind(quantile(sample_coef_x1, prob = 0.025),
        quantile(sample_coef_x1, prob = 0.975))
c <-
  cbind(quantile(sample_coef_x2, prob = 0.025),
        quantile(sample_coef_x2, prob = 0.975))
d <-
  round(cbind(
    sample = confint(reg1),
    boot = rbind(a, b, c)), 4)
colnames(d) <- c("2.5 %", "97.5 %",
                 "2.5 %", "97.5 %")

# 3) ---------- The earnings gap -----------------------

## Se realiza una regresión del modelo con la dummy género 
reg2 <- lm(ingtotob ~ sex, data = tabla_final)
summary(reg2)
tab_reg2 <- tab_model(reg2)

## C) Se realiza el Bootstrap por género
tabla_men <- tabla_final %>% filter(sex == 1)
tabla_fem <- tabla_final %>% filter(sex == 0)

## men:
tabla_men$age2 <- tabla_men$age*tabla_men$age
reg_men <- lm(ingtotob ~ age + age2, data = tabla_men)
sample_coef_intercept <- NULL
sample_coef_x1 <- NULL
sample_erstd_x1 <- NULL
sample_coef_x2<- NULL
sample_erstd_x2 <- NULL
for (i in 1:1000) {
  sample_d = tabla_men[sample(1:nrow(tabla_men), 0.3*nrow(tabla_men), replace = TRUE), ]
  
  model_bootstrap <- lm(ingtot ~ age + age2, data = sample_d)
  
  sample_coef_intercept <-
    c(sample_coef_intercept, model_bootstrap$coefficients[1])
  
  sample_coef_x1 <-
    c(sample_coef_x1, model_bootstrap$coefficients[2])
  
  sample_erstd_x1 <-
    c(sample_erstd_x1, coef(summary(model_bootstrap))[2, 2])
  
  sample_coef_x2 <-
    c(sample_coef_x2, model_bootstrap$coefficients[3])
  
  sample_erstd_x2 <-
    c(sample_erstd_x2, coef(summary(model_bootstrap))[3, 2])
}
coefs <- rbind(sample_coef_intercept, sample_coef_x1, sample_erstd_x1, 
               sample_coef_x2, sample_erstd_x2)

## Combinamos los resultados en una tabla
means.boot = c(mean(sample_coef_intercept), mean(sample_coef_x1), 
               mean(sample_coef_x2))
erstd.boot = c(0,mean(sample_erstd_x1),mean(sample_erstd_x2))
knitr::kable(round(
  cbind(
    sample = coef(summary(reg_men))[, c(1,2)],
    bootstrap = means.boot,
    erstdBoots = erstd.boot),4), 
  "simple", caption = "Coefficients in different models")

# Construimos el intervalo de confianza para el género masculino
confint(reg_men)
a <-
  cbind(
    quantile(sample_coef_intercept, prob = 0.025),
    quantile(sample_coef_intercept, prob = 0.975))
b <-
  cbind(quantile(sample_coef_x1, prob = 0.025),
        quantile(sample_coef_x1, prob = 0.975))
c <-
  cbind(quantile(sample_coef_x2, prob = 0.025),
        quantile(sample_coef_x2, prob = 0.975))
d <-
  round(cbind(
    sample = confint(reg_men),
    boot = rbind(a, b, c)), 4)
colnames(d) <- c("2.5 %", "97.5 %",
                 "2.5 %", "97.5 %")
d


## female:
tabla_fem$age2 <- tabla_fem$age*tabla_fem$age
reg_fem <- lm(ingtotob ~ age + age2, data = tabla_fem)
sample_coef_intercept <- NULL
sample_coef_x1 <- NULL
sample_erstd_x1 <- NULL
sample_coef_x2<- NULL
sample_erstd_x2 <- NULL
for (i in 1:1000) {
  sample_d = tabla_fem[sample(1:nrow(tabla_fem), 0.3*nrow(tabla_fem), replace = TRUE), ]
  
  model_bootstrap <- lm(ingtot ~ age + age2, data = sample_d)
  
  sample_coef_intercept <-
    c(sample_coef_intercept, model_bootstrap$coefficients[1])
  
  sample_coef_x1 <-
    c(sample_coef_x1, model_bootstrap$coefficients[2])
  
  sample_erstd_x1 <-
    c(sample_erstd_x1, coef(summary(model_bootstrap))[2, 2])
  
  sample_coef_x2 <-
    c(sample_coef_x2, model_bootstrap$coefficients[3])
  
  sample_erstd_x2 <-
    c(sample_erstd_x2, coef(summary(model_bootstrap))[3, 2])
}
coefs <- rbind(sample_coef_intercept, sample_coef_x1, sample_erstd_x1, 
               sample_coef_x2, sample_erstd_x2)


## Combinamos los resultados en una tabla
means.boot = c(mean(sample_coef_intercept), mean(sample_coef_x1), 
               mean(sample_coef_x2))
erstd.boot = c(0,mean(sample_erstd_x1),mean(sample_erstd_x2))
knitr::kable(round(
  cbind(
    sample = coef(summary(reg_fem))[, c(1,2)],
    bootstrap = means.boot,
    erstdBoots = erstd.boot),4), 
  "simple", caption = "Coefficients in different models")


# Construimos el intervalo de confianza para el género femenino 
confint(reg_fem)
a <-
  cbind(
    quantile(sample_coef_intercept, prob = 0.025),
    quantile(sample_coef_intercept, prob = 0.975))
b <-
  cbind(quantile(sample_coef_x1, prob = 0.025),
        quantile(sample_coef_x1, prob = 0.975))
c <-
  cbind(quantile(sample_coef_x2, prob = 0.025),
        quantile(sample_coef_x2, prob = 0.975))
d <-
  round(cbind(
    sample = confint(reg_fem),
    boot = rbind(a, b, c)), 4)
colnames(d) <- c("2.5 %", "97.5 %",
                 "2.5 %", "97.5 %")
d

### Predicción por género
# Se esperaría que empleados con características laborales y de puestos similares, no debería existir ninguna diferencia salarial entre hombres y mujeres

## Limpiar nuevamente la base
## Impute zeros
tabla_final <- tabla_final %>% 
  mutate(ingtotob = case_when(
    ingtotob == 0 ~ ingtot,
    TRUE ~ ingtotob
  ))

## Selección de variables
tabla_final$ingtotob[tabla_final$ingtotob == 0] <- NA
tabla_final$log_wage <- log(tabla_final$ingtotob)
tabla_final <- tabla_final %>% select(ingtotob, log_wage,sex,age,estrato1,maxEducLevel,
                                      hoursWorkUsual,formal,cuentaPropia, 
                                      oficio,sizeFirm,antiguedad_industria)
tabla_final <- tabla_final %>% drop_na()

## a) Estimación del salario vs género
reg_gender <- lm(log_wage ~ sex, tabla_final)
tab_model(reg_gender)

## Predict peak age by gender
tabla_final$age2 <- tabla_final$age*tabla_final$age
tabla_men <- tabla_final %>% filter(sex == 1)
tabla_fem <- tabla_final %>% filter(sex == 0)

## Hombres (with age squared)
reg_gap_men <- lm(log_wage ~ age + age2, tabla_men)
pred_ols <- predict(reg_gap_men)
ggplot(data=tabla_men, mapping = aes(x = age, y = pred_ols)) +
  geom_point(col = "cadetblue", size = 0.5) +
  labs(x= "Edad", y = "Ingreso total observado - Hombres")+
  theme_minimal()

## Mujeres (with age squared)
reg_gap_fem <- lm(log_wage ~ age + age2, tabla_fem)
pred_ols <- predict(reg_gap_fem)
ggplot(data=tabla_fem, mapping = aes(x = age, y = pred_ols)) +
  geom_point(col = "cadetblue", size = 0.5) +
  labs(x= "Edad", y = "Ingreso total observado - Mujeres")+
  theme_minimal()

## Hombres (without age squared)
reg_gap_men <- lm(log_wage ~ age, tabla_men)
pred_ols <- predict(reg_gap_men)
ggplot(data=tabla_men, mapping = aes(x = age, y = pred_ols)) +
  geom_point(col = "cadetblue", size = 0.5) +
  labs(x= "Edad", y = "Ingreso total observado - Hombres")+
  theme_minimal()

## Mujeres (without age squared)
reg_gap_fem <- lm(log_wage ~ age, tabla_fem)
pred_ols <- predict(reg_gap_fem)
ggplot(data=tabla_fem, mapping = aes(x = age, y = pred_ols)) +
  geom_point(col = "cadetblue", size = 0.5) +
  labs(x= "Edad", y = "Ingreso total observado - Mujeres")+
  theme_minimal()

## e) Conditional gap
reg_gap <- lm(log_wage ~ factor(sex) + age + factor(estrato1) + maxEducLevel + 
                hoursWorkUsual + factor(formal) + factor(cuentaPropia) + 
                factor(oficio) + sizeFirm + antiguedad_industria, 
              data = tabla_final)
tab_model(reg_gap)
stargazer(reg_gap,type="text")

### d) FWL
tabla_con_resid<-tabla_final %>% mutate(res_y_e=lm(log_wage ~ age + factor(estrato1) + maxEducLevel + 
                                                     hoursWorkUsual + factor(formal) + factor(cuentaPropia) + 
                                                     factor(oficio) + sizeFirm + antiguedad_industria,tabla_final)$residuals,
                                        res_x_e=lm(sex~ age + factor(estrato1) + maxEducLevel + 
                                                     hoursWorkUsual + factor(formal) + factor(cuentaPropia) + 
                                                     factor(oficio) + sizeFirm + antiguedad_industria,tabla_final)$residuals,)
reg3<-lm(res_y_e~res_x_e,tabla_con_resid)
#stargazer(reg3,type="text")
stargazer(reg_gap,reg3,type="text")
tab_model(reg_gap,reg3)


# 5) --------------------Predicting earnings ----------------------------

## Nos solicitan dividir la muestra en dos: una de entrenamiento (70%) y otra de prueba (30%).
## El enunciado nos pide establecer la semilla (10101)

set.seed(10101)
## Procedemos a usar el 70% del conjunto de datos para entrenar el modelo: 11,393 obs.
train_sample <- sample(16276, 11393)
train_set <- tabla_final[train_sample, ]
test_set <- tabla_final[-train_sample, ]


## i) Estimación del modelo que incluya solo la constante, Benchmark
benchmark<-lm(log_wage~1,train_set)
tab_model(benchmark)
test_set$benchmarkModel<-predict(benchmark,newdata = test_set)
with(test_set,mean((log_wage-benchmarkModel)^2))  # 0.7845742

## ii) Estimación de los modelos anteriores
## Modelo 1) reg log wage sex controls
model1 <- lm(log_wage ~ factor(sex) + age + factor(estrato1) + maxEducLevel + 
               hoursWorkUsual + factor(formal) + factor(cuentaPropia) + 
               factor(oficio) + sizeFirm + antiguedad_industria,train_set)
tab_model(model1)

test_set$model_1<-predict(model1,newdata = test_set)
with(test_set,mean((log_wage-model_1)^2))  # 0.3882793

## Modelo 2) reg log wage age age^2
train_set$age2 <- train_set$age*train_set$age

model2 <- lm(log_wage ~ age + age2,train_set)
tab_model(model2)

test_set$model_2<-predict(model2,newdata = test_set)
with(test_set,mean((log_wage-model_2)^2))  #0.7644618

## iii) 5 modelos cada vez más complejos
## Model 2) 
model2 <- lm(log_wage ~ sex + age + age2 + factor(estrato1) 
             + maxEducLevel + hoursWorkUsual + factor(formal) + 
               factor(cuentaPropia) + factor(oficio) + sizeFirm 
             + antiguedad_industria,train_set)
tab_model(model2)

test_set$model_2<-predict(model2,newdata = test_set)
with(test_set,mean((log_wage-model_2)^2))  #0.3852743


## Model 3) 
model3 <- lm(log_wage ~ sex + age + age2 + age*sex + factor(estrato1) 
             + maxEducLevel + hoursWorkUsual + factor(formal) + 
               factor(cuentaPropia) + factor(oficio) + sizeFirm 
             + antiguedad_industria,train_set)
tab_model(model3)

test_set$model_3<-predict(model3,newdata = test_set)
with(test_set,mean((log_wage-model_3)^2))  #0.3849555

## Model 4) 
train_set$age3 <- train_set$age*train_set$age*train_set$age
test_set$age3 <- test_set$age*test_set$age*test_set$age

model4 <- lm(log_wage ~ factor(sex) + age + age2 +age3 + age*sex + factor(estrato1) 
             + maxEducLevel + hoursWorkUsual + factor(formal) + 
               factor(cuentaPropia) + factor(oficio) + sizeFirm 
             + antiguedad_industria,train_set)
tab_model(model4)

test_set$model_4<-predict(model4,newdata = test_set)
with(test_set,mean((log_wage-model_4)^2))  #0.3839591

## Model 5) 
train_set$age3 <- train_set$age*train_set$age*train_set$age
model5 <- lm(log_wage ~ factor(sex) + age + age2 +age3 + age*sex + factor(estrato1) 
             + maxEducLevel*sex + hoursWorkUsual + factor(formal) + 
               factor(cuentaPropia) + factor(oficio) + sizeFirm 
             + antiguedad_industria,train_set)
tab_model(model5)

test_set$model_5<-predict(model5,newdata = test_set)
with(test_set,mean((log_wage-model_5)^2))  #0.3840328

## v) Leverage
alpha <- rep(0, nrow(test_set))

for (i in 1:nrow(test_set)) {
  alpha[i] <- (model4$residual[i])/(1-lm.influence(model4)$hat[i])
}

absolute_val_alpha <- abs(alpha)

hist(absolute_val_alpha)
summary(absolute_val_alpha)

quantile(absolute_val_alpha, .80)
summary(tabla_final$log_wage)

# b) K-fold cross-validation
#Repetimos el paso anterior usando el modelo de K-fold con validación cruzada 
## Modelo 1
model1_cv <- train(log_wage ~ factor(sex) + age + factor(estrato1) + maxEducLevel + 
                     hoursWorkUsual + factor(formal) + factor(cuentaPropia) + 
                     factor(oficio) + sizeFirm + antiguedad_industria
                   ,data=tabla_final
                   ,trControl = trainControl(method = "cv", number=10),
                   method="lm")

model1_cv$results$RMSE^2

## Modelo 2
model2_cv <- train(log_wage ~ sex + age + age2 + factor(estrato1) 
                   + maxEducLevel + hoursWorkUsual + factor(formal) + 
                     factor(cuentaPropia) + factor(oficio) + sizeFirm 
                   + antiguedad_industria
                   ,data=tabla_final
                   ,trControl = trainControl(method = "cv", number=10),
                   method="lm")

model2_cv$results$RMSE^2

## Modelo 3
model3_cv <- train(log_wage ~ sex + age + age2 + age*sex + factor(estrato1) 
                   + maxEducLevel + hoursWorkUsual + factor(formal) + 
                     factor(cuentaPropia) + factor(oficio) + sizeFirm 
                   + antiguedad_industria
                   ,data=tabla_final
                   ,trControl = trainControl(method = "cv", number=10),
                   method="lm")

model3_cv$results$RMSE^2

## Modelo 4
model4_cv <- train(log_wage ~ factor(sex) + age + age2 +age3 + age*sex + factor(estrato1) 
                   + maxEducLevel + hoursWorkUsual + factor(formal) + 
                     factor(cuentaPropia) + factor(oficio) + sizeFirm 
                   + antiguedad_industria
                   ,data=tabla_final
                   ,trControl = trainControl(method = "cv", number=10),
                   method="lm")

model4_cv$results$RMSE^2

## Modelo 5
model5_cv <- train(log_wage ~ factor(sex) + age + age2 +age3 + age*sex + factor(estrato1) 
                   + maxEducLevel*sex + hoursWorkUsual + factor(formal) + 
                     factor(cuentaPropia) + factor(oficio) + sizeFirm 
                   + antiguedad_industria
                   ,data=tabla_final
                   ,trControl = trainControl(method = "cv", number=10),
                   method="lm")

model5_cv$results$RMSE^2
# C) LOOCV
# Crear matriz con 3 columnas: 1) Ing observado, 2) Ing predicho, 3) Indicador LOOCV

loocv <- matrix(rep(0, nrow(tabla_final)), nrow=nrow(tabla_final),ncol=3)
loocv[,1] <- tabla_final$log_wage

tabla_final$age2 <- tabla_final$age*tabla_final$age
tabla_final$age3 <- tabla_final$age*tabla_final$age*tabla_final$age


for (i in 1:nrow(tabla_final)) {
  # Correr modelo 4 sin obs i
  reg <- lm(log_wage ~ factor(sex) + age + age2 +age3 + age*sex + factor(estrato1) 
            + maxEducLevel + hoursWorkUsual + factor(formal) + 
              factor(cuentaPropia) + factor(oficio) + sizeFirm 
            + antiguedad_industria,tabla_final[-i,])
  
  # Guardar el Ing estimados
  loocv[i,2] <- predict(reg,newdata = tabla_final[i,])
  
  # Calcular el error de predicción
  loocv[i,3] <- (loocv[i,1] - loocv[i,2])^2
}

mean(loocv[,3]) # error de predicción

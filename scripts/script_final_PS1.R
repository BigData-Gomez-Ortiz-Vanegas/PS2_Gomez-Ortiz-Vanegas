## PSet 1

# direc Isa: setwd("C:/Users/USER/OneDrive - Universidad de los Andes/Escritorio/BigData/PS1/PS1/Documents")
# rm(list=ls())

# direc Sofi: setwd("C:/Users/Sofia/Documents/2022-2/BigData/PS1_Gomez-Ortiz-Vanegas/stores")
# rm(list=ls())

require(pacman)
p_load(tidyverse, rvest)
library(textreadr)
library(table1)
library(sjPlot)
library(stargazer)
library(gmodels)
library(caret)

## 1) Data Acquisition

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

#Nota- saca este warning:
# Warning message:
#   In read_xml.raw(raw, encoding = encoding, base_url = base_url, as_html = as_html,  :
#                     ID tableHTML_column_1 already defined [513]

## Unir bases de datos
tabla_final <- do.call("rbind", list(tabla1, tabla2, tabla3, tabla4, tabla5,
                                     tabla6, tabla7, tabla8, tabla9, tabla10))

# Exportar (Borrar)
write.csv(tabla_final ,"tabla_final_ps1.csv")

## BORRAR:
tabla_final <- read.csv(file = 'tabla_final_ps1.csv')

## 2) Data Cleaning
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

## Set strings as factors
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

## Missing values imputation
tabla_final <- tabla_final %>% 
  mutate(iof2 = case_when(
    !is.na(iof2es) ~ iof2es,
    TRUE ~ iof2
  ))

## Renaming features
tabla_final <- tabla_final %>% 
  select(, -iof2es)  %>%
  rename(r_jefe_hogar = p6050) %>%
  rename(antiguedad_industria = p6426) %>%
  rename(ingreso_pensiones = iof2)

#write.csv(tabla_final ,"tabla_final_2.csv")

## Descriptive statistics

## y: ingtotot

## Creating a categorical variable for age
tabla_final= tabla_final %>% mutate(
  cat_edad = case_when(
    age <= 30~ '18-30',
    age > 30 & age <= 50 ~ '30-50',
    TRUE ~ '>50'
  )
)

## Descriptives table by age range

descriptiva_1 <- table1(~ age + factor(sex) + ingtot + factor(estrato1) +
         factor(maxEducLevel) + hoursWorkUsual + factor(cotPension) +
           factor(formal) + antiguedad_industria | cat_edad, 
       data=tabla_final, overall="Total")

##3) Age-earnings profile

#b

tabla_final$age2 <- tabla_final$age*tabla_final$age

reg1 <- lm(ingtotob ~ age + age2, data = tabla_final)
tab_reg <- tab_model(reg1)

#c
library(stargazer)
stargazer(ols,type = "text",title = "Fitted Model OLS", out = "ols.doc")

#d
library(ggplot2)

pred_ols <- predict(reg1)

ggplot(data=df, mapping = aes(x = age, y = pred_ols)) +
  geom_point(col = "cadetblue", size = 0.5) +
  labs(x= "Edad", y = "Ingreso total observado")+
  theme_minimal()

#e
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

## Combining the results in a table
means.boot = c(mean(sample_coef_intercept), mean(sample_coef_x1), 
               mean(sample_coef_x2))
erstd.boot = c(0,mean(sample_erstd_x1),mean(sample_erstd_x2))
knitr::kable(round(
  cbind(
    sample = coef(summary(reg1))[, c(1,2)],
    bootstrap = means.boot,
    erstdBoots = erstd.boot),4), 
  "simple", caption = "Coefficients in different models")


# confidence interval; 
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

## 4) The earnings gap

#a

install.packages("magrittr")
library(magrittr)

install.packages("dplyr")
library(dplyr)

#a
# se tuvo que hacer cambios a los datos para lidiar con los 0 en la variable ingtotob (error con log)
## 1) Remplazar ingtotob = ingtot cuando ingtotob = 0
tabla_final <- df %>% 
  mutate(ingtotob = case_when(
    ingtotob == 0 ~ ingtot,
    TRUE ~ ingtotob
  ))

## 2) Remplazar los que definitivamente quedan en 0 por NAs

tabla_final$ingtotob[tabla_final$ingtotob == 0] <- NA

## 3) Calcular el log

tabla_final$log_wage <- log(tabla_final$ingtotob)

## 4) correr regresión

reg2 <- lm(formula = log_wage ~ sex, data = tabla_final)
library(stargazer)
stargazer(ols_gender,type = "text",title = "Gender Model OLS", out = "gender.doc")

#b
## 5) Quedarnos solo con las variables que vamos a usar y borrar las filas que tengan missings (perdemos como 200)

tabla_final <- tabla_final %>% select(log_wage,sex,age,estrato1,maxEducLevel,
                                      hoursWorkUsual,formal,cuentaPropia, 
                                      oficio,sizeFirm,antiguedad_industria)
tabla_final <- tabla_final %>% drop_na()

## 6) Correr la reg de edad
tabla_final$age2 <- tabla_final$age*tabla_final$age
reg_gap1 <- lm(log_wage ~ age + age2, tabla_final)
pred_ols <- predict(reg_gap1)

## 7) graficar por género
ggplot(data=tabla_final, mapping = aes(x = age, y = pred_ols)) +
  geom_point(col = "cadetblue", size = 0.5) +
  labs(x= "Edad", y = "Ingreso total observado")+
  theme_minimal() + facet_wrap(~sex)

#c
## Bootstrap by gender

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

## Combining the results in a table
means.boot = c(mean(sample_coef_intercept), mean(sample_coef_x1), 
               mean(sample_coef_x2))
erstd.boot = c(0,mean(sample_erstd_x1),mean(sample_erstd_x2))
knitr::kable(round(
  cbind(
    sample = coef(summary(reg_men))[, c(1,2)],
    bootstrap = means.boot,
    erstdBoots = erstd.boot),4), 
  "simple", caption = "Coefficients in different models")


# confidence interval; 
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

## Combining the results in a table
means.boot = c(mean(sample_coef_intercept), mean(sample_coef_x1), 
               mean(sample_coef_x2))
erstd.boot = c(0,mean(sample_erstd_x1),mean(sample_erstd_x2))
knitr::kable(round(
  cbind(
    sample = coef(summary(reg_fem))[, c(1,2)],
    bootstrap = means.boot,
    erstdBoots = erstd.boot),4), 
  "simple", caption = "Coefficients in different models")


# confidence interval; 
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

### e) predicting by gender

## Clean db again

## Impute zeros
tabla_final <- tabla_final %>% 
  mutate(ingtotob = case_when(
    ingtotob == 0 ~ ingtot,
    TRUE ~ ingtotob
  ))

## Select features
tabla_final$ingtotob[tabla_final$ingtotob == 0] <- NA
tabla_final$log_wage <- log(tabla_final$ingtotob)

tabla_final <- tabla_final %>% select(ingtotob, log_wage,sex,age,estrato1,maxEducLevel,
                                      hoursWorkUsual,formal,cuentaPropia, 
                                      oficio,sizeFirm,antiguedad_industria)
tabla_final <- tabla_final %>% drop_na()
## a) Estimage wage vs gender
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

### 5) Predicting earnings
set.seed(10101)

## We use 70% of the dataset to train: 11,393 obs.
train_sample <- sample(16276, 11393)

train_set <- tabla_final[train_sample, ]
test_set <- tabla_final[-train_sample, ]

## i) Benchmark

benchmark<-lm(log_wage~1,train_set)
tab_model(benchmark)

test_set$benchmarkModel<-predict(benchmark,newdata = test_set)
with(test_set,mean((log_wage-benchmarkModel)^2))  # 0.7845742

## ii) Estimating previous models again

  ## Model 1) reg log wage sex controls
  model1 <- lm(log_wage ~ factor(sex) + age + factor(estrato1) + maxEducLevel + 
                 hoursWorkUsual + factor(formal) + factor(cuentaPropia) + 
                 factor(oficio) + sizeFirm + antiguedad_industria,train_set)
  tab_model(model1)
  
  test_set$model_1<-predict(model1,newdata = test_set)
  with(test_set,mean((log_wage-model_1)^2))  # 0.3882793
  
  ## Model 2) reg log wage age age^2
  train_set$age2 <- train_set$age*train_set$age
  
  model2 <- lm(log_wage ~ age + age2,train_set)
  tab_model(model2)
  
  test_set$model_2<-predict(model2,newdata = test_set)
  with(test_set,mean((log_wage-model_2)^2))  #0.7644618
  

## iii) 5 models increasing in complexity
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
  
  # Calcular el error de predicci?n
  loocv[i,3] <- (loocv[i,1] - loocv[i,2])^2
}

mean(loocv[,3]) # error de predicci?n

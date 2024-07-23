## ----Configuracion General---------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
# indica desde dónde instalar paquetes
options(repos = c(CRAN = "http://cran.rstudio.com")) 
require("knitr")
# Seteo de directorio de trabajo
setwd("C:/Users/mconde/Documents/AID/TP2")
# Muestra directorio de trabajo
getwd()


## ----Configuracion General2, message=FALSE, warning=FALSE--------------------------------------------------------
#librerías
library(plotly)
library(MASS)
library(car)
library(tidyverse)
library(dplyr)
library(reshape2)
library(kableExtra)
library(readxl)
library(stats)
library(BSDA)
library(ggplot2)
library(fastmap)
library(latex2exp)
library(vip) #importancia de las variables al modelos
library(mvShapiroTest) #hacer shapiro en analisis de discriminante
library(tidymodels)
library(devtools)
library(klaR)
library(ggord)
library(mlr)
library(GGally)
library(heplots) #varianza multivariada


## ----Lectura de datos--------------------------------------------------------------------------------------------
#Leer datos
entrenamiento <-read_excel("datos_tp2.xlsx")
barrio<-read_excel("barrios.xlsx") #barrios portal datos abiertos CABA


## ----Filtro Datos------------------------------------------------------------------------------------------------
#filtro de datos
entrenamiento_sin_columnas <- entrenamiento %>%select(-start_date,-end_date,-created_on, -title,-description)

datos <- subset(entrenamiento_sin_columnas, l2 == "Capital Federal" & property_type  == "PH" & operation_type == "Venta" & !is.na(l3))



## ----ASIGNAR BARRIOS Y ZONA--------------------------------------------------------------------------------------
barrio$comuna1<- barrio$COMUNA/100000000000
barrio$zona <- NA  # Inicializamos la columna zona con NA
# Asignamos los valores correspondientes según las condiciones
barrio$zona[barrio$comuna1 %in% c(12, 13, 14, 15)] <- "Norte A"
barrio$zona[barrio$comuna1 %in% c(1, 2, 3)] <- "Este B"
barrio$zona[barrio$comuna1 %in% c(4, 8, 9)] <- "Sur C"
barrio$zona[barrio$comuna1 %in% c(5, 6, 7, 10, 11)] <- "Oeste D"
barrio <- barrio[!is.na(barrio$comuna1), ]
barrio<-barrio[c("BARRIO","comuna1", "zona")]
head(barrio)


## ----MERGE-------------------------------------------------------------------------------------------------------
# Realiza un join 
datos$l3 <- toupper(datos$l3)
datos_total <- merge(datos, barrio, by.x = "l3", by.y = "BARRIO")
head(datos_total)


## ----Sample------------------------------------------------------------------------------------------------------
#muestra de datos con seed 214
set.seed(214)
df <-sample_n(datos_total, size=500, replace=FALSE)



## ----Resumen-----------------------------------------------------------------------------------------------------

#datos duplicados
any(duplicated(df)) 


## ----Resumen1----------------------------------------------------------------------------------------------------
sum(is.na(df))# para saber cuantos na hay en la base de datos


## ----analisis de datos nulos-------------------------------------------------------------------------------------
#prices nulos
any(is.na(df$price))


## ----Eliminar outlier--------------------------------------------------------------------------------------------
# Crear una función para eliminar outliers de una columna
mark_outliers_as_na <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x[x < lower_bound | x > upper_bound] <- NA
  return(x)
}
# Especificar las columnas a las que se debe aplicar la función
columns_to_modify <- c("surface_total", "surface_covered", "price", "rooms")
# Aplicar la función a las columnas especificadas del dataframe, agrupado por comuna
datos_marked <- df %>%
  group_by(comuna1) %>%
  mutate(across(all_of(columns_to_modify), mark_outliers_as_na)) %>%
  ungroup() # Desagrupar al finalizar



## ----------------------------------------------------------------------------------------------------------------
#prices nulos
any(is.na(datos_marked$price))



## ----DatosFaltantes1---------------------------------------------------------------------------------------------
# Filtrar el dataframe para eliminar filas con valores NA en precio
df <- subset(datos_marked, !is.na(price))
# Filtrar el dataframe para eliminar filas con NA en supericie total y superficie cubierta, el resto voy a imputar
df <-subset(df,!is.na(surface_covered)& !is.na(!surface_total))


## ----DatosFaltantes2---------------------------------------------------------------------------------------------
# Imputar valores de 'bedrooms' y 'rooms'
df <- df %>%
  mutate(bedrooms = ifelse(is.na(bedrooms), rooms - 1, bedrooms),
         rooms = ifelse(is.na(rooms), bedrooms + 1, rooms))


## ----DatosFaltantes3---------------------------------------------------------------------------------------------
df<- df %>% mutate_if(is.numeric, ~ ifelse(is.na(.), median(., na.rm = TRUE), .))
colSums(is.na(df))


## ----dimensiones-------------------------------------------------------------------------------------------------
#Luego de la limpieza se realiza las dimensiones
dim(df)


## ----Summary-----------------------------------------------------------------------------------------------------
# Función para calcular estadísticas descriptivas
resumen_descriptivo <- function(df) {
  describe(df)
}

# Agrupar por comuna y aplicar la función de resumen descriptivo
resumen_por_comuna <- df %>%
  select(-lat,-lon,-luminoso,-reciclado,-expensas,-espectacular,-quincho,-terraza, -escalera, -galeria,)%>% #elimina estas columnas del analisis
  select_if(is.numeric) %>% #numerica continua
  group_by(comuna1) %>% #agrupa por comuna
  summarise(across(where(is.numeric), 
                   list(count = ~length(.),
                        min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE),
                        mean = ~mean(., na.rm = TRUE),
                        var = ~var(., na.rm = TRUE),
                        sd = ~sd(., na.rm = TRUE)),
                   .names = "{col}_{fn}"))
head(resumen_por_comuna)


## ----Muestreo Diferencia de medias-------------------------------------------------------------------------------
# Tamaño de la muestra
set.seed(214)
n=20
comuna1 <- 12
comuna2 <- 14

# Tomar muestras aleatorias para cada comuna
muestra_comuna1 <- sample(df$price[df$comuna1 == comuna1], n, replace=FALSE)
muestra_comuna2 <- sample(df$price[df$comuna1 == comuna2], n, replace=FALSE)

muestra_comuna1
muestra_comuna2

# Dataframe con las muestras
data <- data.frame(
  value = c(muestra_comuna1, muestra_comuna2),
  group = factor(rep(c("comuna1", "comuna2"), c(length(muestra_comuna1), length(muestra_comuna2))))
)



## ----------------------------------------------------------------------------------------------------------------
boxplot(value~group,data=data,xlab="Comuna",ylab="Precio",col="royalblue",border="darkblue", main= "Distribución de precios entre la comuna 12 y comuna 14",names = FALSE) # Desactivar las etiquetas de nombres automáticas

# Agregar etiquetas personalizadas en el eje x
axis(1, at = 1:2, labels = c("Comuna 12", "Comuna 14"))


## ----sh.difmedia-m1----------------------------------------------------------------------------------------------
# Prueba de normalidad - Shapiro-Wilk
shapiro.test(muestra_comuna1)


## ----sh.difmedia-m2----------------------------------------------------------------------------------------------
shapiro.test(muestra_comuna2)


## ----qqplot dif.medias-------------------------------------------------------------------------------------------
par(mfrow = c(1, 2))
# Ajustar los tamaños de las etiquetas y títulos usando par
par(cex.lab = 0.5, cex.main = 0.8, cex.axis = 0.5)
qqPlot(muestra_comuna1, 
       main = "QQ Plot de Muestra de Comuna 12",
       xlab = "Cuantiles Teóricos",
       ylab = "Cuantiles Muestra",
       col = "blue",
       pch = 19,
       grid = TRUE)
qqPlot(muestra_comuna2, 
       main = "QQ Plot de Muestra de Comuna 14",
       xlab = "Cuantiles Teóricos",
       ylab = "Cuantiles Muestra",
       col = "blue",
       pch = 19,
       grid = TRUE)


## ----Levene dif.medias, warning=FALSE----------------------------------------------------------------------------

# test de Levene
leveneTest(value ~ group, data = data)


## ----IC95--------------------------------------------------------------------------------------------------------
# Intervalo de confianza del 95% para la diferencia de medias
resultado <- t.test(muestra_comuna1, muestra_comuna2, 
                    mu = 0, 
                    alternative = "two.sided", 
                    var.equal = TRUE,  # Asumiendo varianzas iguales
                    conf.level = 0.95)

print(resultado$conf.int)


## ----t-Test------------------------------------------------------------------------------------------------------

# Intervalo de confianza del 95% para la diferencia de medias
resultado <- t.test(muestra_comuna1, muestra_comuna2, 
                    mu = 0, 
                    alternative = "two.sided", 
                    var.equal = TRUE,  # Asumiendo varianzas iguales
                    conf.level = 0.95)

print(resultado)


## ----Muestreo prueba Mann----------------------------------------------------------------------------------------
# Tamaño de la muestra
set.seed(214)
n=8
comuna1 <- 7
comuna2 <- 4
# Tomar muestras aleatorias para cada comuna
muestra_comuna1 <- sample(df$surface_covered[df$comuna1 == comuna1], n, replace=FALSE)
muestra_comuna2 <- sample(df$surface_covered[df$comuna1 == comuna2], n, replace=FALSE)

muestra_comuna1
muestra_comuna2

# Dataframe con las muestras
data <- data.frame(
  value = c(muestra_comuna1, muestra_comuna2),
  group = factor(rep(c("comuna7", "comuna4"), c(length(muestra_comuna1), length(muestra_comuna2))))
)



## ----------------------------------------------------------------------------------------------------------------
boxplot(value~group,data=data,xlab="Comuna",ylab="Precio",col="lightgreen",border="darkgreen", main= "Distribución de superficie cubierta entre las comunas 7 y 4",names = FALSE) # Desactivar las etiquetas de nombres automáticas

# Agregar etiquetas personalizadas en el eje x
axis(1, at = 1:2, labels = c("Comuna 7", "Comuna 4"))


## ----Shapiro-Manns} # Prueba de normalidad - Shapiro-Wilk--------------------------------------------------------
shapiro.test(muestra_comuna1)


## ----------------------------------------------------------------------------------------------------------------
shapiro.test(muestra_comuna2)


## ----------------------------------------------------------------------------------------------------------------
par(mfrow = c(1, 2))
# Ajustar los tamaños de las etiquetas y títulos usando par
par(cex.lab = 0.5, cex.main = 0.8, cex.axis = 0.5)
qqPlot(muestra_comuna1, 
       main = "QQ Plot de Muestra de Comuna 10",
       xlab = "Cuantiles Teóricos",
       ylab = "Cuantiles Muestra",
       col = "blue",
       pch = 19,
       grid = TRUE)
qqPlot(muestra_comuna2,
       main = "QQ Plot de Muestra de Comuna 4",
       xlab = "Cuantiles Teóricos",
       ylab = "Cuantiles Muestra",
       col = "blue",
       pch = 19,
       grid = TRUE)


## ----Levene_Mann-------------------------------------------------------------------------------------------------
# test de Levene 
leveneTest(value ~ group, data = data)


## ----Mann Whitney, warning=FALSE---------------------------------------------------------------------------------
# Realizar la prueba de Mann-Whitney
resultado_mann_whitney <- wilcox.test(muestra_comuna1, muestra_comuna2, alternative = "two.sided")
resultado_mann_whitney



## ----Muestra Anova-----------------------------------------------------------------------------------------------
# Tamaño de la muestra
set.seed(214)
n=15
comuna1 <- 12
comuna2 <- 13
comuna3 <- 14
comuna4 <- 15
#muestras aleatorias para cada comuna
muestra_comuna1 <- sample(df$price[df$comuna1 == comuna1], n, replace=FALSE)
muestra_comuna2 <- sample(df$price[df$comuna1 == comuna2], n, replace=FALSE)
muestra_comuna3 <- sample(df$price[df$comuna1 == comuna3], n, replace=FALSE)
muestra_comuna4 <- sample(df$price[df$comuna1 == comuna4], n, replace=FALSE)

# Dataframe con las muestras
data <- data.frame(
  value = c(muestra_comuna1, muestra_comuna2, muestra_comuna3, muestra_comuna4),
  group = factor(rep(c("comuna 12", "comuna 13", "comuna 14", "comuna 15"),
                     c(length(muestra_comuna1),length(muestra_comuna2),length(muestra_comuna3),length(muestra_comuna4))))
)


## ----------------------------------------------------------------------------------------------------------------
boxplot(value~group,data=data,xlab="Comuna",ylab="Precio",col="royalblue",border="darkblue", main= "Distribución de precios entre las comuna dee Norte A",names = FALSE) # Desactivar las etiquetas de nombres automáticas

# Agregar etiquetas personalizadas en el eje x
axis(1, at = 1:4, labels = c("12", "13", "14", "15"))


## ----Shapiro-Anova-----------------------------------------------------------------------------------------------
# Realizar el test de Shapiro-Wilk para cada grupo
shapiro_results <- data %>%
  group_by(group) %>%
  summarise(
    shapiro_p_value = shapiro.test(value)$p.value
  )

# Mostrar los resultados del test de Shapiro-Wilk en una tabla
kable(shapiro_results, 
      caption = "Resultados del Test de Shapiro-Wilk por Comuna",
      format = "html",  
      align = 'c',  
      row.names = FALSE,  
      digits = 3)  # Número de dígitos decimales
      


## ----qqplot Anova------------------------------------------------------------------------------------------------
# Configurar la ventana gráfica para 4 gráficos en una sola ventana
par(mfrow = c(2, 2), mar = c(1, 1, 1, 1), oma = c(0, 0, 0, 0))  # Ajustar márgenes y espacio exterior

# Ajustar los tamaños de las etiquetas y títulos usando par
par(cex.lab = 0.5, cex.main = 0.8, cex.axis = 0.5)

qqPlot(muestra_comuna1, 
       main = "QQ Plot de Muestra de Comuna 12",
       xlab = "Cuantiles Teóricos",
       ylab = "Cuantiles Muestra",
       col = "blue",
       pch = 19,
       grid = TRUE)

qqPlot(muestra_comuna2, 
       main = "QQ Plot de Muestra de Comuna 13",
       xlab = "Cuantiles Teóricos",
       ylab = "Cuantiles Muestra",
       col = "blue",
       pch = 19,
       grid = TRUE)
qqPlot(muestra_comuna3, 
       main = "QQ Plot de Muestra de Comuna 14",
       xlab = "Cuantiles Teóricos",
       ylab = "Cuantiles Muestra",
       col = "blue",
       pch = 19,
       grid = TRUE)

qqPlot(muestra_comuna4, 
       main = "QQ Plot de Muestra de Comuna 15",
       xlab = "Cuantiles Teóricos",
       ylab = "Cuantiles Muestra",
       col = "blue",
       pch = 19,
       grid = TRUE)


## ----Levene Anova------------------------------------------------------------------------------------------------

# test de Levene
leveneTest(value ~ group, data = data)


## ----anova-------------------------------------------------------------------------------------------------------
anova <- aov(data$value ~ data$group)
summary(anova)


## ----Chequeo de normalidad de residuos---------------------------------------------------------------------------
shapiro.test(residuals(anova)) #Chequeo de normalidad de residuos


## ----Tukey-------------------------------------------------------------------------------------------------------
#tukey
tukey_result <- TukeyHSD(anova)
tukey_result


## ----plot Tukey--------------------------------------------------------------------------------------------------

# Plot 
plot(tukey_result, las = 1, cex.axis = 0.4, col = "blue")




## ----------------------------------------------------------------------------------------------------------------

# Crear el gráfico de barras
ggplot(resumen_por_comuna, aes(x = reorder(comuna1, -surface_covered_mean), y = surface_covered_mean)) +
  geom_bar(stat = "identity", width = 0.7) +  # valores basados en 'mean_surface'
  geom_errorbar(aes(ymin = surface_covered_mean - surface_covered_sd, ymax = surface_covered_mean + surface_covered_sd), width = 0.2) +
  labs(title = "Media de Superficie Cubierta por Comuna",
       x = "Comuna",
       y = "Media de Superficie Cubierta") +
  theme_minimal()
    # Rotar etiquetas de x para mejor lectura


## ----------------------------------------------------------------------------------------------------------------
# Filtrar los datos para mostrar solo los barrios de la comuna 12
df_comuna12 <- df %>% filter(comuna1 == 12)

# Crear el gráfico de dispersión para la comuna 12
ggplot(data = df_comuna12, aes(x = surface_covered, y = price, color = comuna1)) +
  geom_point(alpha = 0.6, size = 3) +
  labs(title = "Gráfico de dispersión: Precio vs Superficie Cubierta (Barrios Comuna 12)",
       x = "Superficie Cubierta",
       y = "Precio") +
  theme_minimal()


## ----Filtro df_rs------------------------------------------------------------------------------------------------
# Filtrar las comunas con media y desvio similar
comuna_interes <- 12
df_xy <- df_comuna12 


## ----df_train y test rs------------------------------------------------------------------------------------------
set.seed(214)
#setear la semilla

df_rs <- df_xy %>% select(c('l3','price','surface_covered'))

# Dividir los datos en conjuntos de entrenamiento y prueba
df_split <- initial_split(df_rs,prop = 0.8)#para conservar la proporción de las clases

# Crear los conjuntos de entrenamiento y prueba
df_train <- df_split %>%training()
df_test <- df_split %>%testing()

# Número de datos en test y train
paste0("Total del dataset de entrenamiento: ", nrow(df_train))


## ----------------------------------------------------------------------------------------------------------------
paste0("Total del dataset de testeo: ", nrow(df_test))


## ----Modelo de Regresion Simple----------------------------------------------------------------------------------

modelo1<-lm(`price` ~ `surface_covered`,data=df_train)



## ----rls residuos, warning=FALSE---------------------------------------------------------------------------------
#Calculamos los residuos y los predichos
e<-resid(modelo1) # residuos
re<-rstandard(modelo1) #residuos estandarizados
pre<-predict(modelo1) #predichos
res<-cbind(df$`surface_covered`,df$`price`,pre,e,round(re,2))
#head(res)


## ----------------------------------------------------------------------------------------------------------------
#Supuestos
par(mfrow = c(1, 2))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="Grafico de dispersion de RE vs PRED" )
abline(0,0)
qqPlot(e,main = "QQ Plot de Ph",
       xlab = "Cuantiles Teóricos",
       ylab = "Cuantiles Muestra",
       col = "blue",
       pch = 19,
       grid = TRUE)


## ----------------------------------------------------------------------------------------------------------------
shapiro.test(e)


## ----------------------------------------------------------------------------------------------------------------
summary(modelo1)


## ----------------------------------------------------------------------------------------------------------------
pred_m1 <- modelo1 |>  
           predict(df_test) |> 
            bind_cols(df_test)
pred_m1


## ----------------------------------------------------------------------------------------------------------------
# Evaluamos en df_test

rmse_result <- pred_m1 %>%
  metrics(truth = "surface_covered", estimate = "...1") %>%
  filter(.metric == "rmse")

# Mostrar el resultado del RMSE
print(rmse_result)


## ----------------------------------------------------------------------------------------------------------------
glance(modelo1)


## ----------------------------------------------------------------------------------------------------------------

set.seed(214)
#setear la semilla

df_rm<- df_xy %>% select(c('l3','price','surface_covered','rooms','bathrooms', 'bedrooms', 'surface_total'))

# Dividir los datos en conjuntos de entrenamiento y prueba
df_split <- initial_split(df_rm,prop = 0.8) #para conservar la proporción de las clases

# Crear los conjuntos de entrenamiento y prueba
df_train_rm <- df_split %>%training()
df_test_rm <- df_split %>%testing()

# Número de datos en test y train
paste0("Total del dataset de entrenamiento: ", nrow(df_train))





## ----------------------------------------------------------------------------------------------------------------


ggpairs(df_train_rm, 
        legend = 1, 
        columns = 3:7, diag = list(continuous = "blankDiag"))+
  theme(legend.position = "bottom")


## ----------------------------------------------------------------------------------------------------------------
modelo2<-lm(`price` ~ `surface_covered`+ `bathrooms`, 
            data=df_train_rm)


## ----------------------------------------------------------------------------------------------------------------
#Calculamos los residuos y los predichos
e_m2<-resid(modelo2) # residuos
re_m2<-rstandard(modelo2) #residuos estandarizados
pre_m2<-predict(modelo2) #predichos
res_m2<-cbind(df$`surface_covered`, df$`lon`, df$`price`,pre_m2,e_m2,round(re_m2,2))
head(res_m2)


## ----------------------------------------------------------------------------------------------------------------
par(mfrow = c(1, 2))
plot(pre_m2, re_m2, xlab="Predichos", ylab="Residuos estandarizados",main="RE vs PRED" )
abline(0,0)
qqPlot(e_m2,main = "QQ Plot de Ph",
       xlab = "Cuantiles Teóricos",
       ylab = "Cuantiles Muestra",
       col = "blue",
       pch = 19,
       grid = TRUE)


## ----------------------------------------------------------------------------------------------------------------
shapiro.test(resid(modelo2))


## ----------------------------------------------------------------------------------------------------------------
summary(modelo2)


## ----------------------------------------------------------------------------------------------------------------
pred_rm <- modelo2 |>  
           predict(df_test_rm) |> 
           bind_cols(df_test_rm)
pred_rm


## ----------------------------------------------------------------------------------------------------------------
rmse_result_rm <- pred_rm %>%
  metrics(truth = "price", estimate = "...1") %>%
  filter(.metric == "rmse")

print(rmse_result_rm)


## ----------------------------------------------------------------------------------------------------------------
glance(modelo2)


## ----------------------------------------------------------------------------------------------------------------
importancia <- vip(modelo2)

plot(importancia)


## ----lda variables originales------------------------------------------------------------------------------------
# Filtrar las comunas para hacer analisis discriminante
comunas_interes <- c(12, 11)
# Transformacion de variables
df$log_price <- log(df$price)
df$log_surface_total <- log(df$surface_total)
df$log_surface_covered<-log(df$surface_covered)
# Analisis con variables originales
df_ad <- df %>% select(surface_total,surface_covered, price,comuna1)%>%
  filter(comuna1 %in% comunas_interes) %>%
  mutate(comuna1 = factor(comuna1))


## ----------------------------------------------------------------------------------------------------------------
set.seed(214)#setear la semilla

df_split <- initial_split(df_ad,
                          prop = 0.85,
                          strata = comuna1)#para conservar la proporción de las clases

df_train_ad <- df_split |> 
              training() |> 
              mutate(across(where(is.numeric), scale))

df_test_ad <- df_split |> 
              testing()|> 
              mutate(across(where(is.numeric), scale))

# Número de datos en test y train
paste0("Total del dataset de entrenamiento: ", nrow(df_train_ad))


## ----------------------------------------------------------------------------------------------------------------
paste0("Total del dataset de testeo: ", nrow(df_test_ad))


## ----------------------------------------------------------------------------------------------------------------
comuna12<- subset(df_train_ad[,1:3], df_train_ad$comuna1 == 12)
comuna11<- subset(df_train_ad[,1:3], df_train_ad$comuna1 == 11)



## ----Normalidad _LDA---------------------------------------------------------------------------------------------

mvShapiro.Test(as.matrix(comuna12))


## ----Normalidad_LDA2---------------------------------------------------------------------------------------------
mvShapiro.Test(as.matrix(comuna11))


## ----lda variables transformadas---------------------------------------------------------------------------------
# Filtrar las comunas para hacer analisis discriminante
comunas_interes <- c(12, 11)
# Transformacion de variables
df$log_price <- log(df$price)
df$log_surface_total <- log(df$surface_total)
df$log_surface_covered<-log(df$surface_covered)
# Analisis con variables transformadas
df_ad <- df %>% select(log_surface_total,log_surface_covered, log_price,comuna1)%>%
  filter(comuna1 %in% comunas_interes) %>%
  mutate(comuna1 = factor(comuna1))


## ----------------------------------------------------------------------------------------------------------------
set.seed(214)#setear la semilla

df_split <- initial_split(df_ad,
                          prop = 0.85,
                          strata = comuna1)#para conservar la proporción de las clases

df_train_ad <- df_split |> 
              training() |> 
              mutate(across(where(is.numeric), scale))

df_test_ad <- df_split |> 
              testing()|> 
              mutate(across(where(is.numeric), scale))

# Número de datos en test y train
paste0("Total del dataset de entrenamiento: ", nrow(df_train_ad))


## ----------------------------------------------------------------------------------------------------------------
paste0("Total del dataset de testeo: ", nrow(df_test_ad))


## ----------------------------------------------------------------------------------------------------------------
comuna12<- subset(df_train_ad[,1:3], df_train_ad$comuna1 == 12)
comuna11<- subset(df_train_ad[,1:3], df_train_ad$comuna1 == 11)



## ----Normalidad _LDA_log-----------------------------------------------------------------------------------------

mvShapiro.Test(as.matrix(comuna12))


## ----Normalidad_LDA2_log-----------------------------------------------------------------------------------------
mvShapiro.Test(as.matrix(comuna11))


## ----Homocedasticidad_LDA----------------------------------------------------------------------------------------
# Realizar la prueba de Box's M
boxM_result <- boxM(df_train_ad[, 1:3], df_train_ad$comuna1)

# Ver los resultados
print(boxM_result)


## ----Modelo LDA--------------------------------------------------------------------------------------------------
model_lda <- lda(comuna1~., data =df_train_ad)
model_lda


## ----------------------------------------------------------------------------------------------------------------
prop = model_lda$svd^2/sum(model_lda$svd^2)
prop #varianza entre grupos explicada por cada FD



## ----------------------------------------------------------------------------------------------------------------
predictions <- model_lda |>  
                predict(df_test_ad)
predictions


## ----MC train LDA------------------------------------------------------------------------------------------------
cm_lda<-table(predict(model_lda,type="class")$class,df_train_ad$comuna1)


## ----------------------------------------------------------------------------------------------------------------
# Calcular la precisión
accuracy <- sum(diag(cm_lda)) / sum(cm_lda)
print(paste("Accuracy:", round(accuracy, 3)))


## ----Parti-------------------------------------------------------------------------------------------------------
partimat (comuna1~. , data=df_train_ad , method="lda")


## ----------------------------------------------------------------------------------------------------------------
lda.test <- predict(model_lda,df_test_ad)
df_test_ad$lda <- lda.test$class
cm_lda_t<-table(df_test_ad$lda,df_test_ad$comuna1)#matriz confusion test
cm_lda_t


## ----------------------------------------------------------------------------------------------------------------
accuracy_lda_test <- sum(diag(cm_lda_t)) / sum(cm_lda_t)
print(paste("Accuracy:", round(accuracy_lda_test, 3)))


## ----------------------------------------------------------------------------------------------------------------
# Convertir tibble a data.frame
df_train_knn <- as.data.frame(df_train_ad)
df_test_knn <-as.data.frame(df_test_ad[-5])


## ----Modelo knn--------------------------------------------------------------------------------------------------

# Defino modelo knn
set.seed(214)
task <- makeClassifTask(data = df_train_knn, target = "comuna1") 
lrn_knn <- makeLearner("classif.knn", predict.type = "response",par.vals = list("k" = 2))
mod_knn <- mlr::train(lrn_knn, task)



## ----knn_prediccion_test-----------------------------------------------------------------------------------------
# Predicción TEST
pred_knn<- predict(mod_knn, newdata = df_test_knn)
pred_knn
acc_knn <- round(measureACC(as.data.frame(pred_knn)$truth, as.data.frame(pred_knn)$response),3)
acc_knn




## ----matriz confusion--------------------------------------------------------------------------------------------
# Obtener y visualizar la matriz de confusión
conf_matrix <- calculateConfusionMatrix(pred_knn)
print(conf_matrix)


## ----------------------------------------------------------------------------------------------------------------
# Predicción TRAIN (naive)
pred_knn1 = predict(mod_knn, newdata = df_train_knn) # por si quiero ver naive sobre training
acc_knn1 <- round(measureACC(as.data.frame(pred_knn1)$truth, as.data.frame(pred_knn1)$response),3)
pred_knn1
acc_knn1




## ----analisis con distintos k, warning=FALSE---------------------------------------------------------------------
# Cambio los k
acc=NULL
acc2=NULL
ks = seq(1,67,1)
for (i in 1:length(ks)) {
        lrn_knn = makeLearner("classif.knn", predict.type = "response",par.vals = list("k" = i)) 
        mod_knn = mlr::train(lrn_knn, task)
        pred_knn= predict(mod_knn, newdata = df_test_knn)
        acc[i] = measureACC(as.data.frame(pred_knn)$truth, as.data.frame(pred_knn)$response)
        pred_knn_ = predict(mod_knn, newdata = df_train_knn) # por si quiero ver naive sobre training
        acc2[i] = measureACC(as.data.frame(pred_knn_)$truth, as.data.frame(pred_knn_)$response)
        
}
        
par(mfcol = c(1,2))

new_df1 <- as.data.frame(cbind(ks,acc))
new_df1 <- new_df1%>%mutate(sub_data='test')
new_df2 <- as.data.frame(cbind(ks,acc2))
colnames(new_df2) <- c('ks','acc')
new_df2 <- new_df2%>%mutate(sub_data='train')

new_df <- as.data.frame(rbind(new_df1,new_df2))
new_df1[which.max(new_df1$acc),"ks"] 
new_df2[which.max(new_df2$acc),"ks"] 

# Encontrar el mejor valor de 'k' y 'threshold'
best_k_test <- new_df1[which.max(new_df1$acc), "ks"]
best_k_train <- new_df2[which.max(new_df2$acc), "ks"]

print(paste("Mejor k_test:", best_k_test))
print(paste("Mejor K_train:", best_k_train))


## # Gráfco de cómo varía la métrica de performance accuracy, de acuerdo al umbral elegido
## 
## ggplot(new_df, aes(x = ks, y = acc, color = sub_data)) +
##   geom_line() +
##   geom_point() +
##   labs(title = "Precisión del Modelo KNN para Diferentes Valores de k",
##        x = "Valor de k",
##        y = "Métrica de performance (accuracy)") +
##   labs(color='Conjunto de\n evaluación',linetype='Conjunto de\n evaluación')+
##   theme_minimal()+
##  scale_x_continuous(breaks = seq(1, 67, by = 10))  # Cambiar etiquetas del eje X
## 
## 
## 

## ----Metricas de knn---------------------------------------------------------------------------------------------
# Métricas del modelo de knn
Métrica <- c('valor','datos')
Accuracy <- c(acc_knn,'prueba')
Accuracy. <- c(acc_knn1,'entrenamiento')
# Imprimo resultados de métricas de performance
kable(rbind(Métrica, Accuracy, Accuracy.))


## ----Grafico knn-------------------------------------------------------------------------------------------------
lrn_knn = makeLearner("classif.knn", predict.type = "response",par.vals = list("k" = 2)) 
plotLearnerPrediction(lrn_knn,task,features = c("log_surface_covered", "log_surface_total"),cv=100L,gridsize=100)+scale_fill_manual(values=c("#ff0061","#11a6fc"))+theme_bw()


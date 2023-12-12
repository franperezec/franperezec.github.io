#----Primera parte carga, preparación y depuración de datos -----

#install.packages("lubridate")
#install.packages("modeest")
library(lubridate)
library(modeest) # moda
library(dplyr)
library(fastDummies)
library(pROC)
library(caret)

# Cargar los datos en R

dataMarzo23 <- read.table("C:\\Users\\econo\\OneDrive\\FLACSO\\Ciclo IV\\Machine Learning Teoría y práctica\\drive-download-20231031T033606Z-001\\ProyectoFinal\\proyecto\\datos\\solicitudesMarzo.csv", sep="\t", header=TRUE)
dataAbril23 <- read.table("C:\\Users\\econo\\OneDrive\\FLACSO\\Ciclo IV\\Machine Learning Teoría y práctica\\drive-download-20231031T033606Z-001\\ProyectoFinal\\proyecto\\datos\\solicitudesAbril.csv", sep=";", header=TRUE)
dataMayo23 <- read.table("C:\\Users\\econo\\OneDrive\\FLACSO\\Ciclo IV\\Machine Learning Teoría y práctica\\drive-download-20231031T033606Z-001\\ProyectoFinal\\proyecto\\datos\\solicitudesMayo.csv", sep="\t", header=TRUE)

# Convertir fecha caracter a formato de fecha
dataAbril23$date.fecha_hora_registro. <- as.Date(dataAbril23$date.fecha_hora_registro., format = "%d/%m/%Y")
class(dataAbril23$date.fecha_hora_registro.)
dataMarzo23$date.fecha_hora_registro. <- as.Date(dataMarzo23$date.fecha_hora_registro.)
dataMayo23$date.fecha_hora_registro. <- as.Date(dataMayo23$date.fecha_hora_registro.)


#la base abril se debe arreglar la latitud y longitud están en otro formato

# Procesar la variable 'latitud' en el data frame 'dataAbril23'

dataAbril23$latitud <- gsub("\\.", "", dataAbril23$latitud)

# Convertir la cadena de texto a un formato numérico con punto decimal.

dataAbril23$latitud <- as.numeric(paste0(substr(dataAbril23$latitud, 1, 2), ".", substr(dataAbril23$latitud, 3, 8)))

# Verificar el tipo de la variable 'latitud' para confirmar que ahora es numérica.

class(dataAbril23$latitud) # Debería mostrar 'numeric'


# Procesar la variable 'longitud' en el data frame 'dataAbril23'

dataAbril23$longitud <- gsub("\\.", "", dataAbril23$longitud)

# Convertir la cadena de texto a un formato numérico con punto decimal.

dataAbril23$longitud <- paste0(substr(dataAbril23$longitud, 1, 3), ".", substr(dataAbril23$longitud, 4, 8))
dataAbril23$longitud <- as.numeric(dataAbril23$longitud)

cantidad_de_nas <- sum(is.na(dataAbril23$longitud))
cantidad_de_nas

# Eliminar filas con NA en cualquier columna
dataAbril23 <- na.omit(dataAbril23)

cantidad_de_nas <- sum(is.na(dataAbril23$longitud))
cantidad_de_nas

# Verificar el tipo de la variable 'longitud' para confirmar que ahora es numérica.

class(dataAbril23$longitud) # Debería mostrar 'numeric'

table(dataAbril23$estado, dataAbril23$IF.estado.in..9.10...Correcta..Incorrecta.)

# Unir los tres data frames
data <- rbind(dataMarzo23, dataAbril23, dataMayo23)

sort(data$latitud)

which.max(data$latitud)
which.max(data$longitud)
#ver dato atípico
info_cliente_max_longitud <- data[538611, "cliente"]
info_cliente_max_longitud

# Índices de las filas a eliminar
filas_a_eliminar <- c(538611, 119642)

# Eliminar las filas del data frame
data <- data[-filas_a_eliminar, ]

summary(data$latitud)
summary(data$latitud)
which.max(data$latitud)


#cambiar nombres de variables
# Imprimir los nombres actuales de las columnas
print(names(data))

# Cambiar los nombres de las variables "date.fecha_hora_registro." y "time.fecha_hora_registro." a "fecha" y "hora"
nombres_originales <- c("date.fecha_hora_registro.", "time.fecha_hora_registro.","IF.estado.in..9.10...Correcta..Incorrecta.")
nombres_nuevos <- c("fecha", "hora","transaccion")

# Reemplazar los nombres
names(data)[names(data) %in% nombres_originales] <- nombres_nuevos

# Verificar los cambios
print(names(data))

# Contar las transacciones por cliente
transacciones_por_cliente <- table(data$cliente)

# Filtrar el data frame para el cliente específico
datos_cliente_especifico <- subset(data, cliente == "0482f70d3de71f5042d654f5943184d0")

# Verificar la distribución de la latitud
hist(datos_cliente_especifico$latitud, main="Distribución de la Latitud para Cliente Específico", xlab="Latitud")

class(data$fecha)

# crear variable 'transaccion_exitosa'
data$transaccion_exitosa <- ifelse(data$transaccion == "Correcta", 1, 0)

#id transacción
data$idtrans <- seq_len(nrow(data))

# Ver las primeras filas del data frame para verificar el resultado
head(data)



#quitar valores atípicos de latitud y longitud
quantile(data$latitud,0.0001)
quantile(data$latitud,0.9999)
quantile(data$longitud,0.0001)
quantile(data$longitud,0.9999)
# Eliminar filas donde mi_variable es mayor que valor_superior o menor que valor_inferior
data <- subset(data, data$latitud <= -3.906496 & data$latitud >= -4.057175 & data$longitud <= -79.17404 & data$longitud >= -79.25308)

summary(data$latitud)
summary(data$longitud)


# Asegúrate de que tu data frame se llama 'data'
# y tiene las columnas 'latitud', 'longitud', 'fecha', 'hora' y 'transaccion_exitosa'
names(data)
# Convertir 'fecha' y 'hora' a tipo POSIXct para extraer información relevante
data$fecha <- as.Date(data$fecha, format="%Y-%m-%d")
data$hora_dia <- as.POSIXct(data$hora, format="%H:%M:%S")



data <- data %>%
  mutate(dia = weekdays(fecha),  
         mes = format(fecha, "%m"),
         hora_dia = format(hora_dia, "%H"))

# Crear las dummies 
data <- data %>%
  mutate(lunes = as.numeric(dia == "lunes"),  
         martes = as.numeric(dia == "martes"),
         miercoles = as.numeric(dia == "miércoles"),
         jueves = as.numeric(dia == "jueves"),
         viernes = as.numeric(dia == "viernes"),
         sabado = as.numeric(dia == "sábado"),
         domingo = as.numeric(dia == "domingo"))

# Crear variables dummy solo para marzo, abril y mayo
data <- data %>%
  mutate(marzo = as.numeric(mes == "03"),
         abril = as.numeric(mes == "04"),
         mayo = as.numeric(mes == "05"))

table(data$hora_dia)

#horas de día con rangos de 3 horas
data <- data %>%
  mutate(
    hora_dia = as.numeric(substr(hora_dia, 1, 2)), # Extraer la hora como número
    rango_hora = cut(hora_dia, 
                     breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24), # Define los intervalos de 3 horas
                     labels = c("00_02", "03_05", "06_08", "09_11", "12_14", "15_17", "18_20", "21_23"),
                     include.lowest = TRUE,
                     right = FALSE) # El intervalo incluye el límite inferior pero no el superior
  )

data <- data %>%
  mutate(
    r00_02 = as.integer(rango_hora == "00_02"),
    r03_05 = as.integer(rango_hora == "03_05"),
    r06_08 = as.integer(rango_hora == "06_08"),
    r09_11 = as.integer(rango_hora == "09_11"),
    r12_14 = as.integer(rango_hora == "12_14"),
    r15_17 = as.integer(rango_hora == "15_17"),
    r18_20 = as.integer(rango_hora == "18_20"),
    r21_23 = as.integer(rango_hora == "21_23")
  )

#---- Base de clientes -----

names(data)

RFS <- data %>% group_by(cliente) %>%
  summarise(Recencia = max(fecha),
            Frecuencia = n()/3, #frecuencia mensual promedio
            Seriedad = sum(transaccion_exitosa) / n(), # Proporción de pedidos correctos
            MedianaLat = median(latitud),
            MedianaLong = median(longitud),
            MediaLat = mean(latitud),
            MediaLong = mean(longitud),
            DesvEstLat = sd(latitud),
            DesvEstLong = sd(longitud),
            RangoLat= max(latitud) - min(latitud),
            RangoLong= max(longitud) - min(longitud),
            RIQLat = IQR(latitud),
            RIQLong = IQR(longitud),
            ModaLatitud = as.numeric(mfv(latitud, method = "mfv")[1]),
            ModaLongitud = as.numeric(mfv(longitud, method = "mfv")[1]),
            sumLun = sum(lunes),
            sumMart = sum(martes),
            sumMie = sum(miercoles),
            sumJue = sum(jueves),
            sumVie = sum(viernes),
            sumSab = sum(sabado),
            sumDom = sum(domingo),
            sumMar = sum(marzo),
            sumAbr = sum(abril),
            sumMay = sum(mayo),
            sumR00_02 = sum(r00_02),
            sumR03_05 = sum(r03_05),
            sumR06_08= sum(r06_08),
            sumR09_11 = sum(r09_11),
            sumR12_14 = sum(r12_14),
            sumR15_17 = sum(r15_17),
            sumR18_20 = sum(r18_20),
            sumR21_23 = sum(r21_23)
            
  )

RFS$Recencia <- as.numeric(max(RFS$Recencia)-RFS$Recencia)
RFS$id <- seq_len(nrow(RFS))

summary(RFS$DesvEstLat)
summary(RFS$DesvEstLong)
# Reemplazar NA por 0 directamente
RFS$DesvEstLat[is.na(RFS$DesvEstLat)] <- 0
RFS$DesvEstLong[is.na(RFS$DesvEstLong)] <- 0


RFS <- data.frame(RFS)
rownames(RFS) <- RFS$id
RFS$id <- NULL
head(RFS)

#calcular en metros 

RFS$RangoLat <- RFS$RangoLat*111000
summary(RFS$RangoLat)

RFS$RangoLong <- RFS$RangoLong*111320
summary(RFS$RangoLong)



#coeficiente variación de la moda de latitud y longitud

RFS$cvLatitud <- (abs(RFS$DesvEstLat / RFS$MediaLat))*100

summary(RFS$cvLatitud)

RFS$cvLongitud <- (abs(RFS$DesvEstLong/ RFS$MediaLong))*100

summary(RFS$cvLongitud)

#---- Creación variable dependiente -----



#creo dummy de cliente serio y dummy de cliente focal

summary(RFS$Seriedad)
summary(data$transaccion_exitosa)
prop.table(table(data$transaccion,data$dia))

RFS$clienteserio <- ifelse(RFS$Seriedad>0.6785,1,0)
RFS$clientefocal <- ifelse(RFS$RangoLat<100 & RFS$RangoLong<50,1,0)

RFS$clientepr <- RFS$clienteserio * RFS$clientefocal


summary(RFS$clientepr)
summary(RFS$RangoLat)
summary(RFS$RangoLong)
summary(RFS$ModaLatitud)
summary(RFS$ModaLongitud)


summary(RFS$clientefocal)
summary(RFS$clienteserio)


#---- Crear base train y test de clientes----

# Crear una muestra aleatoria de índices para dividir el conjunto de datos para training
set.seed(7919)
sam <- sample(1:nrow(RFS),nrow(RFS)*0.8)

# Crear un conjunto de entrenamiento seleccionando las filas muestreadas
# y excluyendo algunas columnas específicas
train <- RFS[sam, names(RFS)] #filas sam , columnas names(datos)

# Crear un conjunto de prueba con las filas restantes y excluyendo las mismas columnas
#test <- datos[-sam, !(names(datos) %in% c("id_viv", "id_hogar", "id_per"))]
test <- RFS[-sam, names(RFS)] #filas no de sam , columnas names(datos) -sam porque es un número si pongo !sam no funciona porque es operador booleano



#---- Modelo de probabilidad lineal global ----
names(RFS)


modelo_lineal_serio <- lm(clientepr ~ (Recencia + Frecuencia + Seriedad + ModaLatitud + ModaLongitud + DesvEstLat + DesvEstLong )^2, 
                          data = RFS)
summary(modelo_lineal_serio)

modelo_lineal_serio <- lm(clientepr ~ (Recencia + Frecuencia + ModaLatitud + ModaLongitud +
                                         sumLun + sumMart + sumMie + sumJue + sumVie + sumSab + 
                                         sumDom + sumMar + sumAbr + sumMay + sumR00_02 + sumR03_05 +
                                         sumR06_08 + sumR09_11 + sumR12_14 + sumR15_17 + sumR18_20 + 
                                         sumR21_23), 
                          data = RFS)
summary(modelo_lineal_serio)


modelo_lineal_focal <- lm(clientefocal ~ (Recencia + Frecuencia + Seriedad + ModaLatitud + ModaLongitud), 
                          data = RFS)
summary(modelo_lineal_focal)


modelo_lineal_focal <- lm(clientefocal ~ (Recencia + Frecuencia + Seriedad + 
                                            ModaLatitud + ModaLongitud +
                                            sumLun + sumMart + sumMie + sumJue + sumVie + sumSab + 
                                            sumDom + sumMar + sumAbr + sumMay + sumR00_02 + sumR03_05 +
                                            sumR06_08 + sumR09_11 + sumR12_14 + sumR15_17 + sumR18_20 + 
                                            sumR21_23),
                          data = RFS)
summary(modelo_lineal_focal)


#---- Modelo de probabilidad lineal train ----


#sin interacciones
modelo_pl <- lm(clientepr ~ (Recencia + Frecuencia + 
                               ModaLatitud + ModaLongitud +
                               sumLun + sumMart + sumMie + sumJue + sumVie + sumSab + 
                               sumDom + sumMar + sumAbr + sumMay + sumR00_02 + sumR03_05 +
                               sumR06_08 + sumR09_11 + sumR12_14 + sumR15_17 + sumR18_20 + 
                               sumR21_23), 
                data = train)
summary(modelo_pl)

library(Matrix)
library(glmnet)
library(caret)

# Convertir las variables independientes en una matriz para glmnet
X_train <- model.matrix(clientepr ~ (Recencia + Frecuencia  + 
                                       ModaLatitud + ModaLongitud +
                                       sumLun + sumMart + sumMie + sumJue + sumVie + sumSab + 
                                       sumDom + sumMar + sumAbr + sumMay + sumR00_02 + sumR03_05 +
                                       sumR06_08 + sumR09_11 + sumR12_14 + sumR15_17 + sumR18_20 + 
                                       sumR21_23), 
                        data = train)[,-1]  # Eliminar la primera columna (intercepto)
y_train <- train$clientepr

# Preparar los datos de prueba
X_test <- model.matrix(~ (Recencia + Frecuencia  + 
                            ModaLatitud + ModaLongitud +
                            sumLun + sumMart + sumMie + sumJue + sumVie + sumSab + 
                            sumDom + sumMar + sumAbr + sumMay + sumR00_02 + sumR03_05 +
                            sumR06_08 + sumR09_11 + sumR12_14 + sumR15_17 + sumR18_20 + 
                            sumR21_23), 
                       data = test)[,-1]
y_test <- test$clientepr


library(Matrix)
library(glmnet)
library(caret)

# Validación cruzada para  modelo de probabilidad lineal Ridge
set.seed(2320)
cv.out_ridge <- cv.glmnet(X_train, y_train, alpha = 0)
plot(cv.out_ridge) 

bestlam_ridge <- cv.out_ridge$lambda.min
bestlam_ridge

# Validación cruzada para modelo de probabilidad lineal Lasso
set.seed(2320)
cv.out_lasso <- cv.glmnet(X_train, y_train, alpha = 1)
plot(cv.out_lasso)

bestlam_lasso <- cv.out_lasso$lambda.min
bestlam_lasso

# Validación cruzada para modelo logit Ridge
set.seed(2320)
cv.out_ridgelogit <- cv.glmnet(X_train, y_train, alpha = 0, family="binomial")
plot(cv.out_ridgelogit) 

bestlam_ridgelogit <- cv.out_ridgelogit$lambda.min
bestlam_ridgelogit

# Validación cruzada para modelo logit Lasso 
set.seed(2320)
cv.out_lassologit <- cv.glmnet(X_train, y_train, alpha = 1, family="binomial")
plot(cv.out_lassologit)

bestlam_lassologit <- cv.out_lassologit$lambda.min
bestlam_lassologit




# Realizar predicciones de los modelos
predictionslin <- predict(modelo_pl,  newdata = test)
#incluye valores de lamdas óptimos
predictions_cv_ridge <- predict(cv.out_ridge, s = bestlam_ridge, newx = X_test)
predictions_cv_lasso <- predict(cv.out_lasso, s = bestlam_lasso, newx = X_test)

predictions_cv_ridgelogit <- predict(cv.out_ridgelogit, s = bestlam_ridgelogit, newx = X_test)
predictions_cv_lassologit <- predict(cv.out_lassologit, s = bestlam_lassologit, newx = X_test)

# Calcular R2 y RMSE para los modelos
r2 <- data.frame(
  LinealR2 = R2(predictionslin, test$clienteserio),
  Ridge_R2 = R2(predictions_cv_ridge, y_test),
  Lasso_R2 = R2(predictions_cv_lasso, y_test),
  Ridge_R2logit = R2(predictions_cv_ridgelogit, y_test),
  Lasso_R2logit = R2(predictions_cv_lassologit, y_test)
)
r2


rsme <- data.frame(
  Lineal_RMSE = RMSE(predictionslin, y_test),
  Ridge_RMSE = RMSE(predictions_cv_ridge, y_test),
  Lasso_RMSE = RMSE(predictions_cv_lasso, y_test),
  Ridge_RMSElogit = RMSE(predictions_cv_ridgelogit, y_test),
  Lasso_RMSElogit = RMSE(predictions_cv_lassologit, y_test)
)
rsme

#ver coeficientes
lasso1 <- train(y= factor(y_train),
                x = X_train,
                method = 'glmnet', #metodo a utilizar "glmnet", para poder aplicar RIDGE o LASSO
                tuneGrid = expand.grid(alpha = 1, lambda = 0.001878075)) 

ridge1 <- train(y= factor(y_train),
                x = X_train,
                method = 'glmnet', #metodo a utilizar "glmnet", para poder aplicar RIDGE o LASSO
                tuneGrid = expand.grid(alpha = 0, lambda = 0.1181772)) 

a <- data.frame(
  as.data.frame.matrix(coef(lasso1$finalModel, lasso1$bestTune$lambda)),
  as.data.frame.matrix(coef(ridge1$finalModel, ridge1$bestTune$lambda))
)%>%
  rename(Lasso_coef = s1, Ridge_coef =s1.1)


#---- Ajsute Modelo de probabilidad lineal y lineal con Lasso y Ridge y modelo Logit con Lasso ----

# Curva ROC modelo lineal con datos de testeo base
roc_modelo_linealbase <- roc(test$clientepr, predictionslin)

# Graficar la curva ROC
plot(roc_modelo_linealbase, main= "Curva ROC - Modelo Lineal base", col="green", lwd=2)
print(roc_modelo_linealbase) # Area bajo la curva ROC
corte_modelo_linealbase <- coords(roc_modelo_linealbase, "best") # Punto de corte

cortemodelolinealbase <- corte_modelo_linealbase[1,1]

# Curva ROC modelo lineal con ridge con datos de testeo 
roc_modelo_linealr <- roc(factor(y_test), as.vector(predictions_cv_ridge))

# Graficar la curva ROC
plot(roc_modelo_linealr, main= "Curva ROC - Modelo Lineal Ridge", col="blue", lwd=2)
print(roc_modelo_linealr) # Area bajo la curva ROC
corte_modelo_linealr <- coords(roc_modelo_linealr, "best") # Punto de corte

cortemodelolinealr <- corte_modelo_linealr[1,1]


# Curva ROC modelo lineal con lasso con datos de testeo 
roc_modelo_lineal <- roc(factor(y_test), as.vector(predictions_cv_lasso))

# Graficar la curva ROC
plot(roc_modelo_lineal, main= "Curva ROC - Modelo Lineal Lasso", col="red", lwd=2)
print(roc_modelo_lineal) # Area bajo la curva ROC
corte_modelo_lineal <- coords(roc_modelo_lineal, "best") # Punto de corte

cortemodelolineal <- corte_modelo_lineal[1,1]

# Curva ROC modelo Logit con ridge con datos de testeo 
roc_modelo_ridgelogit <- roc(factor(y_test), as.vector(predictions_cv_ridgelogit))

# Graficar la curva ROC
plot(roc_modelo_ridgelogit , main= "Curva ROC - Modelo Logit Ridge", col="pink", lwd=2)
print(roc_modelo_ridgelogit ) # Area bajo la curva ROC
corte_modelo_ridgelogit <- coords(roc_modelo_ridgelogit , "best") # Punto de corte

cortemodelologitridge <- corte_modelo_ridgelogit[1, 1]

# Curva ROC modelo Logit con lasso con datos de testeo
roc_modelo_lassologit <- roc(factor(y_test), as.vector(predictions_cv_lassologit))

# Graficar la curva ROC
plot(roc_modelo_lassologit , main= "Curva ROC - Modelo Logit Lasso", col="brown", lwd=2)
print(roc_modelo_lassologit ) # Area bajo la curva ROC
corte_modelo_lassologit <- coords(roc_modelo_lassologit , "best") # Punto de corte
corte_modelo_lassologit

cortemodelologitlasso  <- corte_modelo_lassologit[1, 1]


library(pROC)

# Asumiendo que ya tienes las ROC calculadas como roc_modelo_linealbase, roc_modelo_linealr, etc.

# Graficar la primera curva ROC
plot(roc_modelo_linealbase, main="Comparación de Curvas ROC", col="green", lwd=2)

# Agregar las otras curvas ROC
lines(roc_modelo_linealr, col="blue", lwd=2)
lines(roc_modelo_lineal, col="red", lwd=2)
lines(roc_modelo_ridgelogit, col="pink", lwd=2)
lines(roc_modelo_lassologit, col="brown", lwd=2)

# Agregar una leyenda si es necesario
legend("bottomright", legend=c("Modelo Lineal Base", "Modelo Lineal Ridge", "Modelo Lineal Lasso", "Modelo Logit Ridge", "Modelo Logit Lasso"),
       col=c("green", "blue", "red", "pink", "brown"), lwd=2)



# Imprimir las áreas bajo la curva ROC y los puntos de corte óptimos

cortemodelolinealbase
cortemodelolinealr 
cortemodelolineal 
cortemodelologitridge
cortemodelologitlasso 


roc_modelo_linealbase
roc_modelo_linealr
roc_modelo_lineal
roc_modelo_ridgelogit
roc_modelo_lassologit

#Matrices de confusión con datos test


matrizconfplbase <- table ((predictionslin>cortemodelolinealbase)*1, test$clientepr)
matrizconfplr <- table ((as.vector(predictions_cv_ridge)>cortemodelolinealr)*1, factor(y_test) )
matrizconfpl <- table ((as.vector(predictions_cv_lasso)>cortemodelolineal)*1, factor(y_test) )
matrizconflogitridge <- table ((predictions_cv_ridgelogit>cortemodelologitridge)*1, factor(y_test))
matrizconflogitlasso <- table ((predictions_cv_lassologit>cortemodelologitlasso)*1, factor(y_test))



matrizconfplbase
matrizconfplr
matrizconfpl
matrizconflogitridge
matrizconflogitlasso


#matriz de confusión lineal 
caret::confusionMatrix(matrizconfplbase)
#Accuracy : 0.6108 modelo lineal básico

#matrices de confusión mod lineal ridge con Caret
caret::confusionMatrix(matrizconfplr)
#Accuracy : 0.6165 lineales cúbicos ridge

#matrices de confusión mod lineal lasso con Caret
caret::confusionMatrix(matrizconfpl)
#Accuracy : 0.6184 lineales cúbicos lasso


#matrices de confusión mod logit ridge con Caret
caret::confusionMatrix(matrizconflogitridge)
#Accuracy : 0.6376

caret::confusionMatrix(matrizconflogitlasso)
#Accuracy : 0.6129 logit lasso segunda corrida

#Accuracy: 0.5863  logit lineal lasso
#Accuracy : 0.6293  logit cuadrático lasso
#Accuracy : 0.5982 #logit cúbico lasso


# ---- K- medias y ACP -----

# análisis descriptivo y preparación datos

#Número de días que han pasado desde el último pedido
plot(density(RFS$Recencia))
#número promedio de pedidos mensuales
plot(density(RFS$Frecuencia))
#seriedad proporción pedidos correctos / total
plot(density(RFS$Seriedad))
#coeficiente variación de latitud
plot(density(RFS$cvLatitud))
#coeficiente variación de latitud
plot(density(RFS$cvLongitud))

#coeficiente variación de latitud
plot(density(RFS$ModaLatitud))
#coeficiente variación de longitud
plot(density(RFS$ModaLongitud))
#coeficiente variación de longitud
plot(density(RFS$RangoLat))
#coeficiente variación de longitud
plot(density(RFS$RangoLong))




# Asegúrate de tener tus datos cargados y el paquete 'graphics' disponible.
# Si no lo tienes, puedes cargarlo con: library(graphics)

# Graficar la primera densidad
plot(density(RFS$Recencia), main="Comparación de Densidades", xlab="Valor", ylab="Densidad", 
     xlim=range(c(RFS$Recencia, RFS$Frecuencia, RFS$Seriedad, RFS$cvLatitud, RFS$cvLongitud)), 
     col="blue", lwd=2)

# Añadir las demás densidades con 'lines()'
lines(density(RFS$Frecuencia), col="red", lwd=2)
lines(density(RFS$Seriedad), col="green", lwd=2)
lines(density(RFS$cvLatitud), col="purple", lwd=2)
lines(density(RFS$cvLongitud), col="orange", lwd=2)

# Añadir una leyenda para distinguir las curvas
legend("topright", legend=c("Recencia", "Frecuencia", "Seriedad", "CV Latitud", "CV Longitud"), 
       fill=c("blue", "red", "green", "purple", "orange"))

summary(RFS$cvLatitud)
summary(RFS$cvLongitud)



#para que recency empiece en 1
RFS$Recencia <- RFS$Recencia + 1
#Log recencia y frecuencia
RFS$LogRecencia<- log(RFS$Recencia)
RFS$LogFrecuencia<- log(RFS$Frecuencia)

#log Número de días que han pasado desde el último pedido
plot(density(RFS$LogRecencia))
#log número del promedio de pedidos por mes
plot(density(RFS$LogFrecuencia))


#creo data frame para K medias
names(RFS)
RFSk <-RFS[, c("LogRecencia", "LogFrecuencia", "Seriedad","cvLatitud","cvLongitud")]

#GRÁFICO DE CODO, para ver número de grupos #parametrizar código cambio 
nk <- 10
n <- nrow(RFSk) #número de filas
wss <- rep(0, nk) #creo vector vacío, cuántas veces vas a iterar se van guardando las varianzas withn
wss[1] <- (n - 1) * sum(sapply(RFSk, var)) #el primer valor será el valor de la var global
for (i in 2:nk) #para las siguientes posiciones
  wss[i] <- sum(kmeans(RFSk, centers = i)$withinss)

plot(1:nk, wss, type = "b", xlab = "Num de Grupos",
     ylab = "Suma de cuadrados dentro de los grupos")


modelk <- kmeans(RFSk, centers = 3)

#proporciones grupos
table(modelk$cluster)

#qué características tienen los clientes

by(RFSk, modelk$cluster, summary)


#árbol de clasificación

ar <- rpart::rpart(factor(modelk$cluster) ~ RFSk$LogRecencia +  RFSk$LogFrecuencia
                   +  RFSk$Seriedad +  RFSk$cvLatitud +  RFSk$cvLongitud)

library(visNetwork)

visTree(ar)


#tener un score que junte esta información

#debo hacer que apunten hacia el mismo lado las varaibles
#aumento de recency no es muy bueno
#si uso acp para un score entonces importa esto



RFSk$LogRecencia <- max(RFSk$LogRecencia) - RFSk$LogRecencia
RFSk$cvLatitud <- max(RFSk$cvLatitud) - RFSk$cvLatitud
RFSk$cvLongitud <- max(RFSk$cvLongitud) - RFSk$cvLongitud


plot(RFSk$LogFrecuencia,RFSk$cvLongitud)

#acp
modelacp <- princomp(RFSk)

#scores de cada cliente
modelacp$scores
#combinaciones lineales
modelacp$loadings

#primer componente explica el 80% de la varianza
summary(modelacp)

#quedo con primera componente
scoreRFSk <- modelacp$scores[, 1]
#esto es complicado de comunicar
summary(scoreRFSk )

#les puedo pasar por una función logit para solucionar esto

curve(1/(1+exp(-x)), xlim = c(-10,10))

flogit <- function(x){
  1/(1+exp(-x))
}
scoreRFSk  <- flogit(scoreRFSk ) * 100
summary(scoreRFSk )
summary(modelacp)
modelacp$loadings

#summary por cluster
tapply(scoreRFSk , modelk$cluster, summary)

aux <- data.frame(RFM = scoreRFSk , grupo = factor(modelk$cluster))

library(ggplot2)

ggplot(aux,aes(x = RFM, fill = grupo)) + geom_density(alpha = 0.7)

#recency, freq, amount


# Extraer las dos primeras componentes principales
scoreRFSk_1 <- modelacp$scores[, 1]
scoreRFSk_2 <- modelacp$scores[, 2]

# Aplicar la función logit a las dos componentes
scoreRFSk_1 <- flogit(scoreRFSk_1) * 100
scoreRFSk_2 <- flogit(scoreRFSk_2) * 100

# Crear un data frame con las dos componentes y la información del grupo
aux <- data.frame(CP1 = scoreRFSk_1, CP2 = scoreRFSk_2, grupo = factor(modelk$cluster))

# Usar ggplot2 para hacer un gráfico de dispersión de las dos componentes
ggplot(aux, aes(x = CP1, y = CP2, color = grupo)) + 
  geom_point() +
  theme_minimal() +
  labs(x = "Componente Principal 1", y = "Componente Principal 2", title = "Análisis de Componentes Principales")



# Cargar las librerías necesarias
# install.packages("fpc")
# install.packages("factoextra")
library(fpc)
library(factoextra)
library(dbscan)


# Configurar la semilla para reproducibilidad
set.seed(123) 

# Ejecutar DBSCAN en RFSk

kNNdistplot(RFSk,k=3);abline(h=10, lty=2, col="red") 

kNNdistplot(scale(RFSk),k=3);abline(h=10, lty=2, col="red") 


# Ajusta los valores de 'eps' y 'MinPts' según las características de tus datos
f <- dbscan::dbscan(scale(RFSk), eps = 0.25, MinPts = 3)

# Imprimir el resultado
print(f)

# Ejecutar DBSCAN en datos escalados de RFSk
f_escalado <- dbscan::dbscan(scale(RFSk), eps = 0.4, MinPts = 5)

# Imprimir el resultado del DBSCAN escalado
print(f_escalado)

# Visualización de los clusters con DBSCAN
# Si 'RFSk' es una matriz o data frame de dos columnas, puedes usar fviz_cluster
# Si tienes más de dos dimensiones, considera hacer una reducción de dimensionalidad primero (como PCA) para visualización
fviz_cluster(list(data = RFSk, cluster = f$cluster), geom = "point")

# Ejemplo de k-means para comparación
km <- kmeans(RFSk, 3)
fviz_cluster(km, RFSk, geom = "point")

# Visualización de k-means en datos escalados de RFSk
fviz_cluster(km, scale(RFSk), geom = "point")

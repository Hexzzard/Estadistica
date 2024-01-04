library(dplyr) #dummy
library(ggcorrplot) #crear matriz de correlacion
library(caret) #matriz de confusion

set.seed(911)
datos <- read.csv("heart.csv")

#Problematica
#este codigo consiste en determinar que factores son los que tienen mayor influencia
#en el hecho de que una persona posea o no problemas cardiacos
#con esto nos referimos por ejemplo cuanto afecta a la edad o el colesterol
#en la sagre a la probabilidad de que ese individuo posea problemas cardiacos

#Dataset:
#Heart Failure Prediction Dataset
#https://www.kaggle.com/datasets/rashikrahmanpritom/heart-attack-analysis-prediction-dataset/discussion

#Variables

#AGE: edad del paciente [años]
#Sex: sexo del paciente [1: M, 0: F]
#ChestPainType: tipo de dolor en el pecho [TA: angina típica, 
#ATA: angina atípica, NAP: dolor no anginoso, ASY: asintomático]
#RestingBP: presión arterial en reposo [mm Hg]
#cholesterol: colesterol sérico [mm/dl]

#FastingBS: azúcar en sangre en ayunas [1: si FastingBS > 120 mg/dl,
#0: en caso contrario]

#RestingECG: resultados del electrocardiograma en reposo [Normal: normal, 
#ST: con anomalía de la onda ST-T (inversiones de la onda T y/o elevación o depresión del ST > 0,05 mV),
#HVI: muestra probable o definitiva hipertrofia ventricular izquierda según los criterios de Estes]

#MaxHR: frecuencia cardíaca máxima alcanzada [Valor numérico entre 60 y 202]
#ExersiceAngina: angina inducida por el ejercicio [Y: Sí, N: No]
#Oldpeak: oldpeak = ST [Valor numérico medido en depresión]
#ST_Slope: la pendiente del segmento ST del ejercicio máximo [Up: ascendente, Flat: plano, Down: descendente]
#HeartDisease: enfermedades cardiacas [1: enfermedad cardíaca, 0: normal]


#Paso 1: pulir el dataframe

#convertimos las variables cualitativas a numeros
datos <- datos %>%
  mutate(Sex = as.numeric(as.factor(Sex)) - 1,
         ExerciseAngina = as.numeric(as.factor(ExerciseAngina)) - 1)

#a las variables cualitativas con mas de una clase aplicamos el criterio de dummy
#esto significa que por cada categoria dentro de la variable
#generaremos una nueva culumna en el df indicando la presencia de dicho elemento

#iteramos por cada variable categorica
columnas <- c("ChestPainType", "ST_Slope", "RestingECG")

for (columna in columnas) {
  dummy_vars <- model.matrix(as.formula(paste("~", columna, " - 1")), data = datos)
  dummy_df <- as.data.frame(dummy_vars)
  datos <- cbind(datos, dummy_df)
  datos <- datos %>% select(-columna)
}
#Paso 2: dividir entre prueba y entrenamiento

#80% de los datos seran de entrenamiento y el 20% restante sera para pruebas
train_ind <- sample(c(TRUE, FALSE), nrow(datos), replace = TRUE, prob = c(0.8, 0.2))

train_set <- datos[train_ind, ]
test_set <- datos[!train_ind, ]

#para obtener buenos resultados deberemos asegurarnos que en el conjunto de entrenamiento
#tenga el mismo numero de personas con y sin problemas cardiacos

#igualaremos ambos conjuntos a el conjunto de menor datos
datos_train_split <- split(train_set, f = train_set$HeartDisease)
min_size <- min(sapply(datos_train_split, nrow))

indices <- sample(nrow(datos_train_split[[1]]), min_size)
train_set <- rbind(datos_train_split[[1]][indices, ], 
                   datos_train_split[[2]][indices, ])

#los datos que fueron eliminados del conjunto de entrenamiento seran añadidos a el conjunto de prueba
datos_train_removed <- rbind(datos_train_split[[1]][-indices, ], 
                             datos_train_split[[2]][-indices, ])
test_set <- rbind(test_set, datos_train_removed)

#Paso 3: generar una matriz de correlacion con los datos
#Para cumplir los supuestos de gauss-markov debemos asegurarnos que las variables que
#utilizaremos no tengan correlaccion entre si

cor_matrix <- cor(datos)
ggcorrplot(cor_matrix, hc.order = TRUE, type = "full", outline.col = "white")

#por ejemplo no podemos usar ST_slopeUP y ST_slopeDown a la vez en el modelo

#Paso 4: Generar modelos de regresion logisitica
#para esto definiremos que un 1 es que el paciente posee problemas cardiacos
#y un 0 es que no presenta este problema

#Nuestro primer modelo sera utilizando todas las variables que no presentan correlaccion entre si
modelo1<-glm(HeartDisease~ChestPainTypeASY+ChestPainTypeATA+ChestPainTypeNAP+ST_SlopeDown+ST_SlopeFlat+RestingECGLVH+RestingECGNormal+FastingBS+Sex+Oldpeak+MaxHR+Age+Cholesterol+ExerciseAngina,data=datos,family = binomial(link = "logit"))
summary(modelo1) #AIC=624

#el segundo modelo sera sin utilizar las variables dummy
modelo2<-glm(HeartDisease~FastingBS+Sex+Oldpeak+MaxHR+Age+Cholesterol+ExerciseAngina,data=datos,family = binomial(link = "logit"))
summary(modelo2) #AIC=803

#el tercer modelo sera considerando solo las variables dumy
modelo3<-glm(HeartDisease~ChestPainTypeASY+ChestPainTypeATA+ChestPainTypeNAP+ST_SlopeDown+ST_SlopeFlat+RestingECGLVH+RestingECGNormal,data=datos,family = binomial(link = "logit"))
summary(modelo3) #AIC=747

#En base a lo anterior nuestro modelo sera el que posea el AIC mas bajo
#es decir el modelo 1, tambien llegamos a la conclusion que las variables dummy
#son mas significativas computacionalmente que las variables numericas
#lo cual tiene sentido ya que estos de por si ya segemnentaron a la poblacion

#Ahora determinaremos el rendimiento de nuestro mejor modelo anterior
predicciones <- predict(modelo1, newdata = test_set, type = "response")

#la predicion es binaria (tiene o no problemas cardiacios segun del cual se encuentra mas cercano)
predicciones_binarias <- ifelse(predicciones > 0.5, 1, 0)

#generamos la matriz de confusion
matriz_confusion <- confusionMatrix(as.factor(predicciones_binarias), as.factor(test_set$HeartDisease))
print(matriz_confusion)
#Nuestro primer modelo tiene 85% de precision

#Revisamos el comportamiento de los datos
plot(modelo1)
#Grafico 1: distribuccion de residuos
#Grafico 2: QQplot
#Grafico 3: Scale-location

#Corroboramos que cumpla los supuestos de markov

#Hipotesis 1: los residuos forman una distribuccion normal
shapiro.test(modelo1$residuals)

#se obtiene un p< 2.2e-16, lo cual comprueba la hipotesis
#es decir los residuos forman una distribuccion normal

#Hipotesis 2: los residuos son homocedasticos
library(lmtest)
library(car)
bptest(modelo1)
#se obtiene un p = 2.816e-07, lo cual comprueba la hipotesis
#es decir los residuos son homocedasticos

#Hipotesis 3: los residuos son incorrelados
dwtest(modelo1)
#la hipotesis se confirma (p=0.9996)
#no hay autocorrelacion

library(margins)

#calcular los efectos marginales medios (AME) para todas las variables en el modelo
ames <- margins(modelo1)

# Resumen de los AMEs, mostrando el efecto promedio de cada predictor en la probabilidad
# de la variable de respuesta, con sus respectivos errores estándar, valores z, valores p
# e intervalos de confianza.
summary(ames)

#Paso 5: generamos el mejor modelo computacional
#en base a lo anterior ahora generaremos el mejor modelo computacionalmente hablando
#solo utilizando las variables que son significativas en base a lo obtenido en
#los valores marginales (AME)

modelo4<-glm(HeartDisease~ChestPainTypeASY+ST_SlopeDown+ST_SlopeFlat+FastingBS+Sex+Oldpeak+ExerciseAngina+Cholesterol,data=datos,family = binomial(link = "logit"))
summary(modelo4) #AIC=618

#evaluamos los resultados de este modelo
predicciones <- predict(modelo4, newdata = test_set, type = "response")
predicciones_binarias <- ifelse(predicciones > 0.5, 1, 0)

#generamos la matriz de confusion
matriz_confusion <- confusionMatrix(as.factor(predicciones_binarias), as.factor(test_set$HeartDisease))
print(matriz_confusion)
#perdimos un poco de presicion, cercano al 2%
#sin embargo cabe mencionar que este modelo es computacionalmente mejor
#debido a que obtenemos un mejor AIC (disminucion en 6 unidades)

#Analizando un poco podemos ver que la perdida de precision vs el costo computacional
#para este caso no renta del todo, por lo que el mejor en terminos generales
#viene siendo el modelo 1.


ames <- margins(modelo4)
summary(ames)

#Para terminar hemos llegado a las siguientes conclusiones

#el efecto mas significativo en funcion de la escala es
#ST_slopeFlat, es decir que durante un electrocardiograma, la pendiente obtenida
#sea plana, con una probabilidad del 25% de poseer problemas cardiacos
#Tambien cabe mencionar que todos los factores estan muy cercanos en terminos de
#importancia
#de los dolores de pecho el mas signiticativo es el de tipo "ASY", y padecer de este
#resulta en una mayor probabilidad de tener ataques cardiacos
#nos llamo la atencion que los hombres tienen mayor tendencia a tener problemas cardiacos
#sobre las mujeres en un 15%
#ademas encontramos que un nivel alto de colesterol ayuda a disminuir 
#los problemas cardiacos*

#PD: revisando el dataset nos encontramos que hay una gran cantidad de mediciones
#de colesterol en 0, los cuales son biologicamente imposibles
#este defecto en la medicion hace que el colesterol sea inversamente proporcional
#a los problemas cardiacos, sin embargo quitando los valores atipicos se obtiene
#que son directamente proporcionales
#sin embargo por defecto de las predicciones se dejo como estaba, aclarando el hecho
#de que el valor obtenido no es algo que encontremos en la realidad


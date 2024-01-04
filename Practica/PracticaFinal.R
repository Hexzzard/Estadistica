library(readxl) #leer excel
library(dplyr)  #manejo de dataframe
library(ggcorrplot) #crear matriz de correlacion
library(margins)#ame

set.seed(911)
raw_df <- read_excel("DATOS CLEANED FILTRADOS.xlsx")

#Se trabaja en base a la teoria de cuantos dias retrasa la titulacion de una persona
#una serie de factores, debido a no poder operar sobre los estudiantes reprobados
#o expulsados de la falcultad de "Ingeneria civil industrial" y 
#"ingeneria civil en obras civiles"


#variables a no considerar para el modelo, por redundancia y experimentacion
df <- raw_df %>% select(-'ID',-'REGION',-'COLEGIO', -'COMUNA', -'COLEGIO_PROVINCIA', -'COLEGIO_COMUNA')

#valores extremos (quite datos para tantear la influencia de datos extremos)
#df <- df %>% filter(!row_number() %in% c(3,11,12,61, 63,66,68,70,83,86,89,213))
#si quito los 12 valores mas extremos obtengo un TSS de 60millones, 17millones de diferencia


#variables para dummy

columnas <- 'COLEGIO_GRUPO_DEPENDENCIA'
for (columna in columnas) {
  dummy_vars <- model.matrix(as.formula(paste("~", columna, " -1")), data = df)
  dummy_df <- as.data.frame(dummy_vars)
  names(dummy_df) <- sub(paste0("^", columna), "", names(dummy_df))
  df <- cbind(df, dummy_df)
  df <- df %>% select(-columna)
}

df <- na.omit(df)
cor_matrix <- cor(df)
ggcorrplot(cor_matrix, hc.order = TRUE, type = "full", outline.col = "white")

#modelo 1
modelo1<-glm(DURACION_CARRERA~CREDITOS_REPROBADOS+GENERO+PUEBLO_ORIGINARIO+COHORTE+NOMBRE_PROGRAMA+PUNTAJE_MAT+PUNTAJE_LEN+PUNTAJE_CIENCIA+PUNTAJE_HISTO+PROMEDIO_PONDERADO+PUNTAJE_NEM+`PARTICULAR SUBVENCIONADO`+`PARTICULAR PAGADO`,data=df,family = gaussian(link = "identity"))

summary(modelo1) #AIC=3164.8
plot(modelo1)

ames <- margins(modelo1)
summary(ames)

#Hipotesis 1: los residuos forman una distribuccion normal
shapiro.test(modelo1$residuals)

#Hipotesis 2: los residuos son homocedasticos
library(lmtest)
library(car)
bptest(modelo1)

#Hipotesis 3: los residuos son incorrelados
dwtest(modelo1)

predicciones <- predict(modelo1, newdata = df, type = "response")
RSS <- sum((df$DURACION_CARRERA - predicciones)^2)
TSS <- sum((df$DURACION_CARRERA - mean(df$DURACION_CARRERA))^2)
ESS <- sum((predicciones - mean(df$DURACION_CARRERA))^2)
cu <-  RSS+ESS
#RSS= 52.527.394
#ESS= 25.108.765
#TSS= 77.636.159


#modelo 2 (en base a criterios de AME)
modelo2<-glm(DURACION_CARRERA~CREDITOS_REPROBADOS+GENERO+COHORTE,data=df,family = gaussian(link = "identity"))

summary(modelo2) #AIC=3151.1
#plot(modelo2)
#Intercept            105129.0
#CREDITOS_REPROBADOS  13.12    
#GENERO               177.03
#COHORTE             -51.00

ames <- margins(modelo2)
summary(ames)

#Hipotesis 1: los residuos forman una distribuccion normal
shapiro.test(modelo2$residuals)

#Hipotesis 2: los residuos son homocedasticos
library(lmtest)
library(car)
bptest(modelo2)

#Hipotesis 3: los residuos son incorrelados
dwtest(modelo2)

predicciones <- predict(modelo2, newdata = df, type = "response")
RSS <- sum((df$DURACION_CARRERA - predicciones)^2)
TSS <- sum((df$DURACION_CARRERA - mean(df$DURACION_CARRERA))^2)
ESS <- sum((predicciones - mean(df$DURACION_CARRERA))^2)
cu <-  RSS+ESS
#RSS= 54.179.593
#ESS= 23.456.565
#TSS= 77.636.159

#modelo 3 (sin considerar cohorte)
modelo3<-glm(DURACION_CARRERA~CREDITOS_REPROBADOS+GENERO+NOMBRE_PROGRAMA,data=df,family = gaussian(link = "identity"))
summary(modelo3) #AIC=3156.5
#plot(modelo3)

ames <- margins(modelo3)
summary(ames)

#Hipotesis 1: los residuos forman una distribuccion normal
shapiro.test(modelo3$residuals)

#Hipotesis 2: los residuos son homocedasticos
library(lmtest)
library(car)
bptest(modelo3)

#Hipotesis 3: los residuos son incorrelados
dwtest(modelo3)

predicciones <- predict(modelo3, newdata = df, type = "response")
RSS <- sum((df$DURACION_CARRERA - predicciones)^2)
TSS <- sum((df$DURACION_CARRERA - mean(df$DURACION_CARRERA))^2)
ESS <- sum((predicciones - mean(df$DURACION_CARRERA))^2)
cu <-  RSS+ESS
#RSS= 55.625.643
#ESS= 22.010.516
#TSS= 77.636.159

#modelo 4 (en base a regiones)

df_regiones <- raw_df %>% select(-'ID',-'COLEGIO', -'COMUNA', -'COLEGIO_COMUNA')
columnas <- c('COLEGIO_GRUPO_DEPENDENCIA', 'REGION', 'COLEGIO_PROVINCIA')

for (columna in columnas) {
  dummy_vars <- model.matrix(as.formula(paste("~", columna, " -1")), data = df_regiones)
  dummy_df <- as.data.frame(dummy_vars)
  names(dummy_df) <- sub(paste0("^", columna), "", names(dummy_df))
  df_regiones <- cbind(df_regiones, dummy_df)
  df_regiones <- df_regiones %>% select(-columna)
}

modelo4<-glm(DURACION_CARRERA~CREDITOS_REPROBADOS+GENERO+COHORTE+`De Aisen del Gral. C. Ibanez del Campo`+`De La Araucania`+`De Los Lagos`+`De Los Lagos`+`De Los Rios`+`De Magallanes y de La Antartica Chilena`+`De Valparaiso`+`Del Biobio`+`Aisen`+`Arica`+`Bio- Bio`+`Cachapoal`+`Cautin`+`Chiloe`+`Coihaique`+`Concepcion`+`Cordillera`+`Llanquihue`+`Maipo`+`Malleco`+`Melipilla`+`nuble`+`Osorno`+`Petorca`+`Ranco`+`Santiago`,data=df_regiones,family = gaussian(link = "identity"))

summary(modelo4) #AIC=3151.1
#plot(modelo4)

ames <- margins(modelo4)
summary(ames)

#Hipotesis 1: los residuos forman una distribuccion normal
shapiro.test(modelo4$residuals)

#Hipotesis 2: los residuos son homocedasticos
library(lmtest)
library(car)
bptest(modelo4)

#Hipotesis 3: los residuos son incorrelados
dwtest(modelo4)

predicciones <- predict(modelo4, newdata = df_regiones, type = "response")
RSS <- sum((df_regiones$DURACION_CARRERA - predicciones)^2)
TSS <- sum((df_regiones$DURACION_CARRERA - mean(df_regiones$DURACION_CARRERA))^2)
ESS <- sum((predicciones - mean(df_regiones$DURACION_CARRERA))^2)
cu <-  RSS+ESS
#RSS= 45.876.356
#ESS= 31.759.803 
#TSS= 77.636.159

#modelo 5 (revisando los datos de mayor significancia en la base de datos unclean)
df_unclean <- read_excel("DATOS CLEANED FILTRADOS.xlsx", sheet = 2)

modelo5<-glm(DURACION_CARRERA~CREDITOS_REPROBADOS+GENERO+COHORTE,data=df_unclean,family = gaussian(link = "identity"))

summary(modelo5) #AIC=5338.4
#plot(modelo5)
#CREDITOS_REPROBADOS= 12.7
#GENERO= 14.6
#COHORTE= -58
#intercept = 119337

ames <- margins(modelo5)
summary(ames)

#Hipotesis 1: los residuos forman una distribuccion normal
shapiro.test(modelo5$residuals)

#Hipotesis 2: los residuos son homocedasticos
library(lmtest)
library(car)
bptest(modelo5)

#Hipotesis 3: los residuos son incorrelados
dwtest(modelo5)

predicciones <- predict(modelo5, newdata = df_unclean, type = "response")
RSS <- sum((df_unclean$DURACION_CARRERA - predicciones)^2)
TSS <- sum((df_unclean$DURACION_CARRERA - mean(df_unclean$DURACION_CARRERA))^2)
ESS <- sum((predicciones - mean(df_unclean$DURACION_CARRERA))^2)
cu <-  RSS+ESS
#RSS= 103.004.559
#ESS=  39.545.800
#TSS= 142.550.359






#Acotaciones
#Partiendo se modifico el problema, con los datos no es viable determinar
#que factores influyen en que un estudiante no se titule,
#asi que adapte el problema a cuanto es la duracion de la carrera en dias

#el dataset entregado contenia varios datos nulos, asi que realice un filtro
#eliminando a las personas que no contenian un dato, por ejemplo no rindio la PSU,
#ademas converti la duracion de la carrera en dias para tener algo mas continuo sobre
#lo cual trabajar, sin embargo es influido considerablemente por los valores extremos.

#tengo la teoria de que aunque no lo paresca, con suficientes datos esto podria
#tender a una distribuccion normal

#cualquier modelo que se realice tendra 77.636.158 de TSS, y el RSS y ESS
#variaran conservando la propiedad TSS = RSS+ESS

#Varia en promedio 43 dias de la fecha de titulacion

#el rendimiento en la PSU y el colegio de donde venga no es significativo para 
#el modelo lo unico que importa es su rendimiento el primer año,el año en el 
#que entro y el genero.
#en donde el cohorte toma el mayor peso (51 dias aproximadamente tiende a reducirse
#la duracion para cada año posterior), lo cual si se extrapola a 2023, la 
#duracion de la carrera se redujo 2.2 años, tambien puede ser efecto de que
#los estudiantes de años 2018 siguen cursando, por lo cual no se ve reflejado 
#en los datos.

#cada credito reprobado retrasa 13.12 dias la duracion de la carrera
#y las mujeres tienden a retrasarse aproximadamente medio año de duracion de 
#la carrera (177 dias)

#Como adicional revisando las localizaciones de los alumnos, en la gran mayoria
#las locacion no es significativa, sin embargo los alumnos de la 
#"De Aisen del Gral. C. Ibanez del Campo" tienen un alto indice de retrazarse
#en la carrera, con un estimador de 1917 dias (5.2 años), seguido por los 
#colegios en la localidad de "Cachapoal", en donde el estimador marca
#que se retrasan 1396 dias (3.8 años). Para las demas localidades
#la probabilidad de que el estimador sea 0, supera el 11%

#Comparando las 3 variables mas significativas con la base de datos sin limpiar
#observamos que los estimadores son ligeramente distintos, siendo el que mas cambia
#el estimador de genero, pasando de 177 a 14, es decir que ya no hay una diferencia
#significativa entre hombres y mujeres

#los estimador de cohorte tambien tiende a aumentar, pasando de -51 a -58
#marcando una ligera diferencia, y por otro lado el estimador de los creditos
#reprobados pasa de 13.1 a 12.7, en donde no hay mucho cambio.


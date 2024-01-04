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
df <- raw_df %>% select(-'ID', -'DURACION_CARRERA',-'NUMERO_REGION',-'REGION',-'COLEGIO', -'COMUNA', -'COLEGIO_PROVINCIA', -'COLEGIO_COMUNA')

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
modelo1<-glm(DURACION_CARRERA_DIAS~CREDITOS_REPROBADOS+GENERO+PUEBLO_ORIGINARIO+COHORTE+NOMBRE_PROGRAMA+PUNTAJE_MAT+PUNTAJE_LEN+PUNTAJE_CIENCIA+PUNTAJE_HISTO+PROMEDIO_PONDERADO+PUNTAJE_NEM+`PARTICULAR SUBVENCIONADO`+`PARTICULAR PAGADO`,data=df,family = gaussian(link = "identity"))

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
RSS <- sum((df$DURACION_CARRERA_DIAS - predicciones)^2)
TSS <- sum((df$DURACION_CARRERA_DIAS - mean(df$DURACION_CARRERA_DIAS))^2)
ESS <- sum((predicciones - mean(df$DURACION_CARRERA_DIAS))^2)
cu <-  RSS+ESS
#RSS= 52.527.394
#ESS= 25.108.765
#TSS= 77.636.159


#modelo 2 (en base a criterios de AME)
modelo2<-glm(DURACION_CARRERA_DIAS~CREDITOS_REPROBADOS+GENERO+COHORTE,data=df,family = gaussian(link = "identity"))

summary(modelo2) #AIC=3151.1
plot(modelo2)

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
RSS <- sum((df$DURACION_CARRERA_DIAS - predicciones)^2)
TSS <- sum((df$DURACION_CARRERA_DIAS - mean(df$DURACION_CARRERA_DIAS))^2)
ESS <- sum((predicciones - mean(df$DURACION_CARRERA_DIAS))^2)
cu <-  RSS+ESS
#RSS= 54.179.593
#ESS= 23.456.565
#TSS= 77.636.159

#modelo 3 (sin considerar cohorte)
modelo3<-glm(DURACION_CARRERA_DIAS~CREDITOS_REPROBADOS+GENERO+NOMBRE_PROGRAMA,data=df,family = gaussian(link = "identity"))
summary(modelo3) #AIC=3156.5
plot(modelo3)

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
RSS <- sum((df$DURACION_CARRERA_DIAS - predicciones)^2)
TSS <- sum((df$DURACION_CARRERA_DIAS - mean(df$DURACION_CARRERA_DIAS))^2)
ESS <- sum((predicciones - mean(df$DURACION_CARRERA_DIAS))^2)
cu <-  RSS+ESS
#RSS= 55.625.643
#ESS= 22.010.516
#TSS= 77.636.159


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


library(readr) #libreria que lee el csv, es mas rapida que la por defecto en R
set.seed(123)  #seed

#Paso 1: refinar los datos
#como primer paso realizaremos un recuento de las palabras en el csv proporcionado
#creando de esta forma un dataframe que contiene el numero total de aparciones de cada palabra

#funcion que suma los valores de un vector columna
contar <- function(vector) {
  conteo <- sum(vector, na.rm = TRUE)
  return(conteo)
}

#funcion que en base a la funcion anterior crea un df (df_words) con el numero total de ocurrencias por palabra en funcion de su clase
crear_df_contar <- function(df){
  correos_ham <- subset(df, Prediction == 0, select = -ncol(df))
  correos_spam <- subset(df, Prediction == 1, select = -ncol(df))
  
  df_words_ham <- data.frame(clase = "ham")
  df_words_spam <- data.frame(clase = "spam")
  
  for (i in 1:ncol(correos_ham)) {
    df_words_ham[[names(correos_ham[, i])]]<-contar(correos_ham[, i])
    df_words_spam[[names(correos_spam[, i])]]<-contar(correos_spam[, i])
  }
  
  df_words_ham[["totalPalabras"]] <- sum(df_words_ham[1, -1])  #recuento del total
  df_words_spam[["totalPalabras"]] <- sum(df_words_spam[1, -1]) #recuento del total
  
  df_words <- rbind(df_words_ham, df_words_spam)
  return(df_words)
}

#Paso 2: determinar la probabilidad de cada palabra dado su clase "P(palabra|clase)"

#funcion del profe en base a el suavizado de laplace (alpha=1)
#esta funcion calcula la probabilidad de ocurriencia de una palabra dado su clase
#trabaja en escala logaritmica y toda palabra aunque no este en el dataset tendra probabilidad > 0
probabilidad_dado_clase <- function(interseccion, total, m){
  probabilidad <- log10((interseccion+1)/(total+m))
  
  #PD: como interseccion y total estan siendo dividos por la muestra, podemos despreciar este valor
  return(probabilidad)
}

#funcion que crea un dataframe en base a la funcion anterior (df_probabilidad_palabra_dado_clase)
crear_df_probabilidad_palabra_dado_clase<- function(df){
  m <- ncol(df)-1 #numero total de palabras (una es de prediccion por lo que se resta 1)
  probabilidad_ham = df$totalPalabras[1]
  probabilidad_spam = df$totalPalabras[2]
  
  df_probabilidad_dado_ham <- data.frame(clase = "ham") #crea un df para cada clase
  df_probabilidad_dado_spam <- data.frame(clase = "spam")
  
  P_probabilidad_ham <- log10(df_words$totalPalabras[1]/(df_words$totalPalabras[1]+df_words$totalPalabras[2]))
  P_probabilidad_spam <- log10(df_words$totalPalabras[2]/(df_words$totalPalabras[1]+df_words$totalPalabras[2]))
  
  for (i in 2:(ncol(df)-1)) {
    probabilidad_ham_palabra = probabilidad_dado_clase(df[, i][1], probabilidad_ham, m)
    df_probabilidad_dado_ham[[colnames(df)[i]]]<- probabilidad_ham_palabra
    
    probabilidad_spam_palabra = probabilidad_dado_clase(df[, i][2], probabilidad_spam, m)
    df_probabilidad_dado_spam[[colnames(df)[i]]]<- probabilidad_spam_palabra
    
    #PD: [[]] es un metodo de asignacion de nombres de columnas a un df, de tal forma que permite
    #asignarle como nombre de una columna el contenido de una variable (por defecto es el nombre de la variable)
    
  }
  
  #añadimos el total
  df_probabilidad_dado_ham[["totalPalabras"]] <- P_probabilidad_ham
  df_probabilidad_dado_spam[["totalPalabras"]] <- P_probabilidad_spam
  
  df_probabilidad <- rbind(df_probabilidad_dado_ham, df_probabilidad_dado_spam)
  
  return(df_probabilidad)
}

#Paso 3: determinar la probabilidad de un correo dado clase "P(correo|clase)
#en este paso asumimos enfoque naive (independencia)
#como asumimos independencia la probabilidad de la ocurrencia de multiples eventos consecutivos se define como
#P(evento global) = P(evento1)* P(evento2).... * P(eventoN)
#Nuestro codigo trabaja en forma vectorial por temas de rendimiento

#esta funcion se encarga de calcular P(correo|clase) en funcion del enfoque anterior
#usamos el producto punto (%*% en R), por temas de rendimiento
#la formula presentada por el profesor es
#P(correo|clase) = Σ (ocurrencias)*log(palabra|clase) + (1-ocurrencias)*(1-log(palabra|clase))
#el profe lo penso como si fuera binario, pero en realidad es vectorial, y aqui nos dimos cuenta
#que el primer elemento de la sumatoria tiende a 0 y el segundo tiende a 1,
#como un evento que tiende a 1 en la ocurrencia de multiples eventos se anula, realizar una aproximacion
#y eliminar el segundo elemento de la sumatoria quedandonos
#P(correo|clase) = Σ (ocurrencias)*log(palabra|clase)

probabilidad_correo_dado_ham <- function(correo){ #para ham (varia el vector de probabilidades)
  sum <- vector_palabras_ham %*% correo #producto punto en R opera a nivel de bits
  return(sum)
  
}

probabilidad_correo_dado_spam <- function(correo){ #para spam (varia el vector de probabilidades)
  sum <- vector_palabras_spam %*% correo #producto punto en R opera a nivel de bits
  
  return(sum)
}

#Paso 4: invertir los terminos de la probabilidad
#en el paso anterior hemos determinado "P(correo|clase)", sin embargo la que nos interesa es
#"P(clase|correo), por lo que recurrimos al teorema de bayes
#P(clase|correo) = P(correo|clase)*P(clase)/P(correo)
#de la expresion anterior todas las variables las hemos calculado en el paso 2, ahora nos queda reemplazar

#funcion logaddexp del profe (suma dos logartimos)
logaddexp <- function(a, b) { 
  max_val <- max(a, b)
  return(max_val + log10(1 + exp(-abs(a - b))))
}

determinar_probabilidad_clase_dado_correo <- function(correo){
  P_correo_spam <- probabilidad_correo_dado_spam(correo)
  P_correo_ham <- probabilidad_correo_dado_ham(correo)
  
  P_spam <- df_probabilidad_palabra_dado_clase$totalPalabras[2]
  P_ham <- df_probabilidad_palabra_dado_clase$totalPalabras[1]
  
  a <- P_correo_spam + P_spam
  b <- P_correo_ham + P_ham
  
  P_correo <- logaddexp(a,b)

  probabilidad_spam_dado_correo <- P_correo_spam + P_spam - P_correo
  probabilidad_ham_dado_correo <- P_correo_ham + P_ham - P_correo
  
  #Una vez hemos obtenida la probabilidad que necesitabamos solo queda clasificar
  #devuelve 1 si es mas probable de ser spam o un 0 en caso contrario
  if (probabilidad_spam_dado_correo > probabilidad_ham_dado_correo){
    return(1)
  }else{
    return(0)
  }
}

#llamada a las funciones

df <- subset(read_csv("emails.csv"), select = -1) #eliminamos la columna N° de email
train_ind <- sample(c(TRUE, FALSE), nrow(df), replace = TRUE, prob = c(0.8, 0.2)) #80% entrenamiento 20% test

#segmentacion del dataset
train_set <- df[train_ind, ]
test_set <- df[!train_ind, ]

#llamada al Paso 1
df_words <- crear_df_contar(train_set)

#llamada al Paso 2
df_probabilidad_palabra_dado_clase <- crear_df_probabilidad_palabra_dado_clase(df_words)

#llamada al Paso 3 y 4

#creamos un vector de palabras spam, este debe ser constante, por lo que la definimos como una cosntante global
#contiene las probabilidades de cada palabra de ser o no perteneciente a la clase
vector_palabras_spam <- df_probabilidad_palabra_dado_clase[2,-ncol(df_probabilidad_palabra_dado_clase)]
names(vector_palabras_spam) <- NULL #hay un bug que al pasar un df a vector no se elimina el nombre de la columna
vector_palabras_spam <- unlist(vector_palabras_spam[,-1])

#creamos un vector para ham
vector_palabras_ham <- df_probabilidad_palabra_dado_clase[1,-ncol(df_probabilidad_palabra_dado_clase)]
names(vector_palabras_ham) <- NULL
vector_palabras_ham <- unlist(vector_palabras_ham[,-1])

#contadores de presicion
correctos<- 0
incorrectos <- 0
FP <-0
FN <-0
VP <-0
VN <-0

#iterar sobre todos los correos en el conjunto de prueba
for (i in 1:nrow(test_set)){
  #Extraer el correo y lo convertimos a vector
  correo <- test_set[i,]
  names(correo) <- NULL
  vector_correo <- unlist(correo[,-ncol(df)])
  
  #determinamos si el correo es spam o ham
  clase = determinar_probabilidad_clase_dado_correo(vector_correo)
  #actualizamos las métricas de precisión basándose en la comparación entre la clasificación y la etiqueta real
  if (clase == test_set[i,ncol(test_set)] ){
    if (clase==1){
      VP <- VP +1
    }else{
      VN <- VN +1
    }
    correctos <- correctos +1
  }else{
    if (clase==1){
      FN <- FN +1
    }else{
      FP <- FP +1
    }
    incorrectos <- incorrectos +1
  }
  #imprimir el progreso de los cálculos
  if ((i %% 100) == 0){
    print(paste("calculando", i/10, "% completado", sep = " "))
  }
}

#revision de los calculos
print(correctos)
print(incorrectos)

# Crear una matriz de confusión
matriz_confusion <- matrix(c(VP, FP, FN, VN), nrow = 2)
colnames(matriz_confusion) <- c("Prediccion +", "Prediccion -")
rownames(matriz_confusion) <- c("Real +", "Real -")

# Imprimir las métricas de precisión
#Exactitud
exactitud <- (VP + VN) / (VP + FP + FN + VN)

#Precisión
precision <- VP / (VP + FP)

#Sensibilidad
sensibilidad <- VP / (VP + FN)

#Especificidad
especificidad <- VN / (VN + FP)

#Imprimir la matriz de confusión
print(matriz_confusion)


#Imprimir las métricas
print(paste("Exactitud: ", exactitud))
print(paste("Precisión: ", precision))
print(paste("Sensibilidad: ", sensibilidad))
print(paste("Especificidad: ", especificidad))

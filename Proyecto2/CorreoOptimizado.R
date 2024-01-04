library(readr)
set.seed(123)

contar <- function(vector) {
  conteo <- sum(vector, na.rm = TRUE)
  return(conteo)
}

crear_df_contar <- function(df){
  correos_ham <- subset(df, Prediction == 0, select = -ncol(df))
  correos_spam <- subset(df, Prediction == 1, select = -ncol(df))
  
  df_words_ham <- data.frame(clase = "ham")
  for (i in 1:ncol(correos_ham)) {
    df_words_ham[[names(correos_ham[, i])]]<-contar(correos_ham[, i])
  }
  df_words_ham[["totalPalabras"]] <- sum(df_words_ham[1, -1])
  
  df_words_spam <- data.frame(clase = "spam")
  for (i in 1:ncol(correos_spam)) {
    df_words_spam[[names(correos_spam[, i])]]<-contar(correos_spam[, i])
  }
  df_words_spam[["totalPalabras"]] <- sum(df_words_spam[1, -1])
  
  df_words <- rbind(df_words_ham, df_words_spam)
  return(df_words)
}

probabilidad_dado_clase <- function(interseccion, total, m){
  probabilidad <- log10((interseccion+1)/(total+m))
  return(probabilidad)
}

crear_df_probabilidad_palabra_dado_clase<- function(df){
  m <- ncol(df)-1
  probabilidad_ham = df$totalPalabras[1]
  probabilidad_spam = df$totalPalabras[2]
  
  df_probabilidad_dado_ham <- data.frame(clase = "ham")
  for (i in 2:(ncol(df)-1)) {
    probabilidad = probabilidad_dado_clase(df[, i][1], probabilidad_ham, m)
    df_probabilidad_dado_ham[[colnames(df)[i]]]<- probabilidad
  }
  
  df_probabilidad_dado_spam <- data.frame(clase = "spam")
  for (i in 2:(ncol(df)-1)) {
    probabilidad = probabilidad_dado_clase(df[, i][2], probabilidad_spam, m)
    df_probabilidad_dado_spam[[colnames(df)[i]]]<- probabilidad
  }
  df_probabilidad <- rbind(df_probabilidad_dado_ham, df_probabilidad_dado_spam)
  return(df_probabilidad)
}
probabilidad_correo_dado_clase <- function(correo){
  sum <- vector_palabras_ham %*% vector_correo
  print(sum)
  
  #print(paste("probabilidad correo", sum, sep = " "))
  return(sum)
    
  }

probabilidad_correo_dado_spam <- function(correo){
  sum <- vector_palabras_spam %*% vector_correo
  print(sum)

  #print(paste("probabilidad correo", sum, sep = " "))
  return(sum)
}

df <- subset(read_csv("emails.csv"), select = -1)
train_ind <- sample(c(TRUE, FALSE), nrow(df), replace = TRUE, prob = c(0.8, 0.2))

train_set <- df[train_ind, ]
test_set <- df[!train_ind, ]

df_words <- crear_df_contar(train_set)

df_probabilidad_palabra_dado_clase <- crear_df_probabilidad_palabra_dado_clase(df_words)

print("terminado")

determinar_ham_o_spam<- function(correo){
  prob_spam <- probabilidad_correo_dado_spam(correo)+log10(df_words$totalPalabras[2])
  prob_ham <- probabilidad_correo_dado_clase(correo)+log10(df_words$totalPalabras[1])
  if (prob_spam > prob_ham){
    #print("el correo es SPAM")
    return(1)
  }else{
    #print("el correo es HAM")
    return(0)
  }
}
vector_palabras_spam <- df_probabilidad_palabra_dado_clase[2,]
names(vector_palabras_spam) <- NULL
vector_palabras_spam <- unlist(vector_palabras_spam[,-1])

vector_palabras_ham <- df_probabilidad_palabra_dado_clase[1,]
names(vector_palabras_ham) <- NULL
vector_palabras_ham <- unlist(vector_palabras_ham[,-1])
correctos<- 0
incorrectos <- 0
FP <-0
FN <-0
VP <-0
VN <-0
for (i in 1:nrow(test_set)){
  correo <- test_set[i,]
  names(correo) <- NULL
  vector_correo <- unlist(correo[,-ncol(df)])
  clase = determinar_ham_o_spam(vector_correo)
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
  if ((i %% 100) == 0){
    print(paste("calculando", i/10, "% completado", sep = " "))
  }
}

print(VN)
print(FP)
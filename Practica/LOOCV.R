set.seed(123)
library(readxl) #leer excel
library(lightgbm)
library(Metrics)
library(dplyr)   #manejo de dataframe
library(reshape2)

#grafico de la tabla
library(gridExtra)
library(grid)

dataset <- read_excel("PowerBi.xlsx")

#todo numerico 
#PD: dummyzando COLEGIO_GRUPO_DEPENDENCIA no se perciben cambios asi que se
#ignorara para ahorrar tiempo
dataset <- dataset %>% select(-'DURACION_CARRERA',-'ID', -'REGION', -'COMUNA',
                              -'COLEGIO', -'COLEGIO_PROVINCIA', -'COLEGIO_COMUNA', -'COLEGIO_GRUPO_DEPENDENCIA')

predictions <- numeric(nrow(dataset))

#la primera columna contendra las caracteristicas ordenadas alfabeticamente para seguir un orden
#importance altera el orden en funcion del Gain
df_importance <- data.frame(features = sort(setdiff(names(dataset), "Semestres"), decreasing = FALSE))

params <- list(
  objective = "regression",
  metric = "12",
  num_leaves = 31,
  learning_rate = 0.05,
  n_estimators = 100,
  min_data_in_bin = 1,
  min_data_in_leaf = 5,
  verbose = -1
  
)

#algoritmo LOOCV, sacar 1 dato, hacer un modelo con los restantes y calcular la
#prediccion para el dato faltante
for (i in 1:nrow(dataset)) {
  train_data <- dataset[-i, ]
  valid_data <- dataset[i, ]
  
  dtrain <- lgb.Dataset(data = as.matrix(train_data[, -which(names(train_data) == "Semestres")]),
                        label = as.numeric(train_data$Semestres), free_raw_data = FALSE)
  
  lgb_model <- lgb.train(params, dtrain)
  valid_data <- as.matrix(valid_data[, -which(names(valid_data) == "Semestres")]) 
  predictions[i] <- predict(lgb_model, valid_data)
  
  #para no verse influido por el valor de Gain, se sigue el orden alfabetico
  importance <- lgb.importance(model = lgb_model)
  importance <- importance[order(importance[[1]]), ]
  df_importance <- cbind(df_importance, importance[, 2])
  
  if((i %% 20)-1 == 0){
    print(paste(10*(i%/%20),"% completado"))
  }
  
}
rmse <- sqrt(mean((dataset$Semestres - predictions)^2))
print(rmse) #1.80975 semestres^2

df_importance$Gain_mean <- rowMeans(df_importance[-1])
df_importance <- df_importance[order(-df_importance$Gain_mean), ]
gain_median_importances <- df_importance[, c(1, ncol(df_importance))]

grob <- tableGrob(head(gain_median_importances, 5), rows = NULL)
grid.draw(grob)

#                  features  Gain_mean
#             ANO_TITULACION 0.32134952
#                    COHORTE 0.30569124
#  TOTAL_CREDITOS_PRIMER_ANO 0.13443281
#        CREDITOS_REPROBADOS 0.11088779
#        CREDITOS_APPROBADOS 0.03415259

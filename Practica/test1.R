set.seed(123)
library(readxl) #leer excel
library(lightgbm)
library(Metrics)
library(dplyr)  #manejo de dataframe
library(ggplot2)
library(reshape2)

dataset <- read_excel("PowerBi.xlsx")

#todo numerico y dummyzado
dataset <- dataset %>% select(-'DURACION_CARRERA',-'ID', -'REGION', -'COMUNA',
                              -'COLEGIO', -'COLEGIO_PROVINCIA', -'COLEGIO_COMUNA')

columnas <- 'COLEGIO_GRUPO_DEPENDENCIA'
for (columna in columnas) {
  dummy_vars <- model.matrix(as.formula(paste("~", columna, " -1")), data = dataset)
  dummy_df <- as.data.frame(dummy_vars)
  names(dummy_df) <- sub(paste0("^", columna), "", names(dummy_df))
  dataset <- cbind(dataset, dummy_df)
  dataset <- dataset %>% select(-columna)
}
#debido a que algunas columnas en el modelo de lightGBM eran tan insignificantes que daban 0
#se aplico un filtro mediante la matriz PPS con un treshold de 0.3

#dataset <- dataset %>% select(-'COHORTE',-'TOTAL_CREDITOS_PRIMER_ANO', -'PROMEDIO_PSU_LM',-'PROMEDIO_PONDERADO','PARTICULAR SUBVENCIONADO', -'PARTICULAR PAGADO')
dataset <- dataset %>% select(-'PARTICULAR PAGADO')
#colnames(dataset)[colnames(dataset) == "PARTICULAR PAGADO"] <- "PARTICULAR_PAGADO"
colnames(dataset)[colnames(dataset) == "PARTICULAR SUBVENCIONADO"] <- "PARTICULAR_SUBVENCIONADO"
matriz_pps <- ppsr::score_matrix(dataset)
datos <- melt(matriz_pps)
names(datos) <- c("Var1", "Var2", "value")
mapa <- ggplot(datos, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Mapa de Calor")
print(mapa)


predictions <- numeric(nrow(dataset))
#la primera columna contendra las caracteristicas ordenadas alfabeticamente para seguir un orden
#importance altera el orden en funcion del Gain
df_importance <- data.frame(features = sort(setdiff(names(dataset), "Años"), decreasing = FALSE))

suppressWarnings(for (i in 1:nrow(dataset)) {
  train_data <- dataset[-i, ]
  valid_data <- dataset[i, ]
  
  dtrain <- lgb.Dataset(data = as.matrix(train_data[, -which(names(train_data) == "Años")]),
                        label = as.numeric(train_data$Años), free_raw_data = FALSE)

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
  lgb_model <- lgb.train(params, dtrain)
  valid_data <- as.matrix(valid_data[, -which(names(valid_data) == "Años")]) 
  predictions[i] <- predict(lgb_model, valid_data)
  
  #para no verse influido por el valor de Gain, se sigue el orden alfabetico
  importance <- lgb.importance(model = lgb_model)
  importance <- importance[order(importance[[1]]), ]
  df_importance <- cbind(df_importance, importance[, 2])
lg
  if((i %% 20)-1 == 0){
    print(paste(10*(i%/%20),"% completado"))
  }
  
})
rmse <- sqrt(mean((dataset$Años - predictions)^2))
print(rmse)

df_importance$Gain_mean <- rowMeans(df_importance[-1])
df_importance <- df_importance[order(-df_importance$Gain_mean), ]
gain_median_importances <- df_importance[, c(1, ncol(df_importance))]
print(head(gain_median_importances, 5))

#guardar en un df la importancia media de de cada carasteristica, guardarlo como vector
#y buscar las 5 mejores

#intentar replicarlo en python

#como hacer para que no saque los warning

#leaves one out
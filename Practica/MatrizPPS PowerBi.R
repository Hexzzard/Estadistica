library(ggplot2)
library(readxl) #leer excel
library(dplyr)  #manejo de dataframe
library(reshape2)
raw_df <- read_excel("DATOS FILTRADOS.xlsx")
df <- raw_df %>% select(-'COLEGIO_GRUPO_DEPENDENCIA',-'ID',-'REGION',-'COLEGIO', -'COMUNA', -'COLEGIO_PROVINCIA', -'COLEGIO_COMUNA',-'TOTAL_CREDITOS_PRIMER_ANO',-'ANO_TITULACION', -'NOMBRE_PROGRAMA',-'PROMEDIO_PSU_LM',-'PUNTAJE_LEN', -'PROMEDIO_PONDERADO',-'CREDITOS_APPROBADOS',-'DURACION_CARRERA',-'PUNTAJE_CIENCIA')
matriz_pps <- ppsr::score_matrix(df)
datos <- melt(matriz_pps)
names(datos) <- c("Var1", "Var2", "value")
mapa <- ggplot(datos, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Mapa de Calor")
print(mapa)
#no puedo creer que esta wea funcione

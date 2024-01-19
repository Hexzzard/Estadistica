library(ggplot2)
library(readxl) #leer excel
library(dplyr)  #manejo de dataframe
library(margins)

raw_df <- read_excel("DATOS FILTRADOS.xlsx")
#modelo 1
modelo1<-glm(DURACION_CARRERA~CREDITOS_REPROBADOS+GENERO+PUEBLO_ORIGINARIO+COHORTE+PUNTAJE_MAT+PUNTAJE_HISTO+PUNTAJE_NEM,data=raw_df,family = gaussian(link = "identity"))

ames <- margins(modelo1)
df_margens <- as.data.frame(summary(ames))
df_margens <- df_margens[, c("factor","AME", "p")]
df_margens$p <- round(df_margens$p, 3)

grob <- tableGrob(df_margens, rows = NULL)
grid.draw(grob)
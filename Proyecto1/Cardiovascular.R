library(readr)
library(tidyverse)
library(dplyr)
library(fmsb)

datos <- read_csv("cardiovascular.csv")

Personas_sanas <- datos %>% filter(Heart_Disease =="No")
Very_good <- Personas_sanas%>%filter(General_Health == "Very Good")
Prop_very_good <- nrow(Very_good)/nrow(Personas_sanas)

Good <- Personas_sanas%>%filter(General_Health == "Good")
Prop_good <- nrow(Good)/nrow(Personas_sanas)

Excellent <- Personas_sanas%>%filter(General_Health == "Excellent")
Prop_excellent <- nrow(Excellent)/nrow(Personas_sanas)

Fair <- Personas_sanas%>%filter(General_Health == "Fair")
Prop_fair <- nrow(Fair)/nrow(Personas_sanas)

Poor <- Personas_sanas%>%filter(General_Health == "Poor")
Prop_poor <- nrow(Poor)/nrow(Personas_sanas)


Personas_enfermas <- datos %>% filter(Heart_Disease =="Yes")

Very_good2 <- Personas_enfermas%>%filter(General_Health == "Very Good")
Prop2_very_good <- nrow(Very_good2)/nrow(Personas_enfermas)

Good2 <- Personas_enfermas%>%filter(General_Health == "Good")
Prop2_good <- nrow(Good2)/nrow(Personas_enfermas)

Excellent2 <- Personas_enfermas%>%filter(General_Health == "Excellent")
Prop2_excellent <- nrow(Excellent2)/nrow(Personas_enfermas)

Fair2 <- Personas_enfermas%>%filter(General_Health == "Fair")
Prop2_fair <- nrow(Fair2)/nrow(Personas_enfermas)

Poor2 <- Personas_enfermas%>%filter(General_Health == "Poor")
Prop2_poor <- nrow(Poor2)/nrow(Personas_enfermas)

df <- data.frame(
  Excelente = c(100,0)
)

df <- data.frame(
  Excelente = c(1,0, Prop_excellent, Prop2_excellent),
  MuyBuena = c(1,0,Prop_very_good, Prop2_very_good),
  Buena = c(1,0, Prop_good, Prop2_good),
  Regular = c(1,0, Prop_fair, Prop2_fair),
  Pobre = c(1,0, Prop_poor, Prop2_poor)
)

areas <- c(rgb(1, 0, 0, 0.25),
           rgb(0, 1, 0, 0.25))

radarchart(df,
           cglty = 1,       # Tipo de línea del grid
           cglcol = "gray", # Color del grid
           pcol = 2:4,      # Color para cada línea
           plwd = 2,        # Ancho para cada línea
           plty = 1, 
           title = "Proporcion de autopercepcion de salud de gente que tiene y no tiene cardiopatia",
           pfcol = areas)

legend("topright",
       legend = c("Personas con cardiopatía", "Personas sin cardiopatía"),
       bty = "n", pch = 20, col = areas,
       text.col = "grey25", pt.cex = 2)
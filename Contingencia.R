#1.4 TRAMOS
#LIBRERIAS
install.packages('tidyr')
library(dplyr)
library(tidyr)
library(data.table)
library(sjmisc)
library (sjPlot)
library(faraway)
library(ppcor)
library(WRS2)
library(relaimpo)
library(hier.part)
library(car)
library(robustbase)
library(rapportools)
library(lmtest)
library(stats)
library(rrcov)
library(e1071)
library(readxl)
library(rJava)
library(xlsxjars)
library(xlsx)
library(data.table)
library(ggplot2)
library(rJava)
library(psych)

#DATOS
tabla<-read_excel("C:/Users/pamej/Documents/Datahack/MASTER/DATA ANALYTICS/IVAN GUERRERO/Teoria/04.- Análisis Estadístico de Datos - Práctica.xlsx")


#VENTAS
tramoventas <- (max(tabla$VENTAS)-min(tabla$VENTAS))/sqrt(4403)
tramoventas <- as.integer(tramoventas)
tabla$Tramo_Ventas <- tabla$VENTAS/tramoventas
tabla$Tramo_Ventas <- as.integer(tabla$Tramo_Ventas)
tramos <- unique(tabla$tramoventas)
order(tramos)
#EMPLEADOS
#Microempresa (1-9 trabajadores), 1
#PequeÃ±a empresa (10-49 trabajadores), 2
#Mediana empresa (50-249 trabajadores) 3
#Gran empresa (250 y mÃ¡s trabajadores).4

tabla$Tipo_Empresa <- 
  ifelse (tabla$`NÚMERO DE EMPLEADOS` < 9,"Microempresa",
          ifelse(tabla$`NÚMERO DE EMPLEADOS`< 49,"Pequeña empresa",
          ifelse(tabla$`NÚMERO DE EMPLEADOS`< 249,"Mediana empresa",
          ifelse(tabla$`NÚMERO DE EMPLEADOS`>250,"Gran empresa")))) 
unique(tabla$empresa)

#CONTINGENCIA

tablaresumen <- tabla[,-c(1:18)] 

tablaresumen2 <- as.data.frame(table(tablaresumen))

#1era FORMA TABLA CONTINGENCIA
contingencia <- dcast(tablaresumen2, 
                      tablaresumen2$tramoventas ~ tablaresumen2$empresa, fun.aggregate = sum)

#2DA FORMA TABLA CONTINGENCIA (MÁS ROBUSTA)
sjt.xtab(tabla$Tramo_Ventas,tabla$Tipo_Empresa, show.summary = TRUE, 
         show.cell.prc = TRUE, tdcol.n = 'blue', tdcol.cell='red',
         tdcol.row='green',tdcol.col='blue2', emph.color = 'green',
         file="tabla")


#RELACION ENTRE VARIABLES

x2 <- 4813.988

#COEFICIENTE DE CONTINGENCIA
coefcont <- sqrt(x2/(x2+4403))
coefcont
#PHI
phi <- sqrt(x2/(4403))
phi        

tablavariables <- tabla[,-c(1:18,20)] 

pairs(tablavariables, col="lightcoral",pch=21, bg='lightcoral')
cor(tablavariables)
pairs.panels(tablavariables,
             method = "pearson", # metodo de correlacion
             col = "red4",
             hist.col="lightcoral",
             density = TRUE,  # densidad de los puntos
             ellipses = TRUE, # mostrar elipses
             cex.cor = 1.5
             
)

#-----------------------------------------

write.csv(contingencia,"contingencia.csv")









 

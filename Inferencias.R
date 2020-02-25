#LIBRERIAS
library(readxl)
library(xlsx)
library(stats)

#DATOS
tabla<-read_excel("C:/Users/pamej/Documents/Datahack/MASTER/DATA ANALYTICS/IVAN GUERRERO/Teoria/04.- Análisis Estadístico de Datos - Práctica.xlsx")

#CONJUNTO DE DATOS
madrid <- subset(tabla,PROVINCIA=='Madrid')
barcelona <- subset(tabla,PROVINCIA=='Barcelona')

#ESTADISTICOS
#funcion que devuelve pearson, curtosis, asimetria y sd
estadistic <- function(x)
  return(c(pearson=sd(x)/mean(x),curt=kurtosis(x),
           sk=skewness(x),desv=sd(x)))

summary(madrid$VENTAS)
summary(barcelona$VENTAS)
estadistic(madrid$VENTAS)
estadistic(barcelona$VENTAS)

write.csv(barcelona,"barcelona.csv")

#IGUALAR Nº MUESTRAS DE BARCELONA A MADRID

test.rows <- sample(1:nrow(barcelona), 473); 
test.set <- barcelona[test.rows, ]; 
train.set <- barcelona[-test.rows, ];


# VENTAS MADRID:
#Hipotesis nula: la distribucion es normal.
shapiro.test(madrid$VENTAS)
# p-valor<0,05. Nos vemos obligados a negar
#la hipotesis nula, la distribucion no es normal.

# VENTAS BARCELONA:
# Hipotesis nula: la distribucion es normal.
shapiro.test(test.set$VENTAS)
# p-valor<0,05. Nos vemos obligados a negar
#la hipotesis nula, la distribucion no es normal.

# H0: las varianzas son iguales.
var.test(test.set$VENTAS, madrid$VENTAS)
# p-valor<0,05. Negamos H0, las varianzas no son iguales.
#intervalo varianzas 0,62 y 0,89, no esta el 1, no son iguales

# H0: las medias son iguales.
t.test(test.set$VENTAS, madrid$VENTAS, var.equal=F, paired=T)
# p-valor>0,05. Aceptamos H0, las medias son iguales.
#intervalo entre -2181.61 y 1137.12, 0 esta entre el intervalo


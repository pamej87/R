#MODELO SIN DATOS LIMPIOS

#LIBRERIAS
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
library(dplyr)
library(data.table)
library(ggplot2)
library(rJava)
library(psych)

#DATOS
tabla<-read_excel("C:/Users/pamej/Documents/Datahack/MASTER/DATA ANALYTICS/IVAN GUERRERO/Teoria/04.- AnÃ¡lisis EstadÃ­stico de Datos - PrÃ¡ctica.xlsx")

#FUNCIONES
options(scipen=100)
#funcion que devuelve pearson, curtosis, asimetria y sd
estadistic <- function(x)
  return(c(pearson=sd(x)/mean(x),curt=kurtosis(x),
           sk=skewness(x),desv=sd(x)))


#PARAMETROS ESTADISTICOS
head(tabla)
summary(tabla)
estadistic(tabla$VENTAS)
estadistic(tabla$`NÚMERO DE EMPLEADOS` )
estadistic(tabla$PRODUCTIVIDAD)

mean(Por_ciudad$`skewness(VENTAS)`)
mean(Por_ciudad$`kurtosis(VENTAS)`)

#GRAFICO DE CAJAS
boxplot(tabla$VENTAS)
qqnorm(tabla$`NÚMERO DE EMPLEADOS`,col="greenyellow" )
qqline(tabla$`NÚMERO DE EMPLEADOS`, lty=1, col="blue4")
qqnorm(tabla$VENTAS,col="royalblue4" )
qqline(tabla$VENTAS, lty=1, col="deeppink")
qqnorm(tabla$PRODUCTIVIDAD,col="deeppink" )
qqline(tabla$PRODUCTIVIDAD, lty=1, col="greenyellow")

#RESUMEN POR CIUDAD
Por_ciudadfinal <- tablafinal %>% group_by(PROVINCIA 
                                           = sub("\\d+$", "", PROVINCIA)) %>% 
  summarise(Ventatotal = sum(VENTAS), media=mean(VENTAS),kurtosis(VENTAS),
            skewness(VENTAS), sum(`NÚMERO DE EMPLEADOS`),
            PEARSON_VENTAS=sd(VENTAS)/mean(VENTAS),
            PEARSON_EMP=sd(`NÚMERO DE EMPLEADOS`)/mean(`NÚMERO DE EMPLEADOS`))



#TABLA CON VARIABLES A COMPARAR

tablafiltrada <- tabla[,-c(3,4,5,6,10:18)] 

#GRAFICOS ESTADISTICOS Y VARIABLES
resumen <- summary(tablafiltrada[,c(-2)])
resumen

#MATRIZ DE CORRELACION Y GRAFICA DE DISPERSION


tabla2 <- tabla[,-c(1:2)] 
names(tabla2)[names(tabla2)=="PRODUCTIVIDAD" ]<- "PRODUCT"
as.matrix(tabla2)
tabla2

mfrow=c(1,1)
mar=c(0,0,0,0)
Mat <- cor(tabla2)


corrplot(Mat, method="number", tl.cex=0.5, cl.cex =  0.9, number.cex = 0.5 )


#OTRO GRAFICO
png(file="corr.png", res = 100)
corrplot(Mat, tl.cex = 0.5, tl.col = "black", method = "color", 
         outline = T,  order="hclust", 
         addCoef.col = "black", number.digits = 2, number.cex = 0.45, 
         cl.pos = 'b', cl.cex = 0.5, addrect = 3, rect.lwd = 3, 
         col = colorRampPalette(c("midnightblue", "white","darkred"))(100))
dev.off()
pairs.panels(tablafiltrada[,c(-1,-2)],
             method = "pearson", # metodo de correlacion
             col = "deepskyblue",
             hist.col="chartreuse",
             density = TRUE,  # densidad de los puntos
             ellipses = TRUE, # mostrar elipses
)
warnings()


cov(tablafiltrada[,c(-1,-2)])
cor(tablafiltrada[,c(-1,-2)])
cor(tablafiltrada[,c(-1,-2)])^2 #r cuadrado
#existe una correlacion alta entre empleados y ventas

#Modelo 1:
#creo un modelo de regresion lineal simple entre las dos variables

regresionb1<-lm(VENTAS~`NÚMERO DE EMPLEADOS`, data=tablafiltrada)
summary(regresionb1)

#ventas= 441,08empleados -557,3 por cada empleado vendo 441, sin
#empleados pierdo 557
#con este primero modelo tengo un r cuadrado de 0,79 ya
#un p valor menor al 0,05 lo cual me indica que la probabilidad
#de encontrar evidencias que nieguen que la variable 
#afecta negativamente a la otra es bastante baja. por ende
#la variable es buena, el intercepto por el contrario estan afectando 
#negativamente

#compruebo la pendiente
pendienteb1<-cov(tablafiltrada$`NÚMERO DE EMPLEADOS`,
                tablafiltrada$VENTAS)/var(tablafiltrada$`NÚMERO DE EMPLEADOS`)
pendienteb1

#compruebo el independiente
independienteb1<-mean(tablafiltrada$VENTAS)-
  pendienteb1*mean(tablafiltrada$`NÚMERO DE EMPLEADOS`)
independienteb1

#grafico de puntos y recta de minimos cuadrados
plot(tablafiltrada$`NÚMERO DE EMPLEADOS`, 
     tablafiltrada$VENTAS, xlab="N.Empleados", ylab="Ventas")
abline(regresionb1)

#prueba del modelo
#creo un nuevo conjunto de empleados 
#este seria mi conjunto de test
nuevos_empleados<-data.frame(`NÚMERO DE EMPLEADOS`=seq(1,200))
names(nuevos_empleados)[names(nuevos_empleados)=="NÚMERO.DE.EMPLEADOS"] <- "NÚMERO DE EMPLEADOS"
predict(regresionb1,nuevos_empleados)

(residuosb1<- rstandard(regresionb1)) 
(valores.ajustadosb1<-fitted(regresionb1))
plot(valores.ajustadosb1, residuosb1) #
# mi modelo es mas acertado con valores
#mas bajos, es decir que dependiendo del tramo
#donde nos movamos, vamos a acertar menos en valores mas elevados,
#vamos a sobreestimar, con mas empleados fallaremos mas en
#nuestra prediccionn de ventas, el modelo es heterocedastico
#se ve claramente que los residuos  disminuyen mientras mas ventas
#hay, el modelo es bueno solo para valores mas bajos


tablafiltrada$prediccion1 <- valores.ajustadosb1
tablafiltrada$residuos_cuadrado1 <- (tablafiltrada$VENTAS-
                                            tablafiltrada$prediccion1)^2

#veo si los residuos son normales

skewness(tablafiltrada$residuos_cuadrado1)
qqnorm(residuosb1)
qqline(residuosb1)
# Los puntos no se ven alineados, la normalidad no es aceptable.
plot(regresionb1)
#PLOT 1: La linea roja no estan lo suficientemente recta
#no me acerco mucho a la prediccion
#PLOT 2: las bolas serpentean, otro indicador mas de que no son
#normales por lo tanto con menos probabilidad de acertar la prediccion
#los outliers estan en las colas entre el primero y el cuarto cuartil
#la determinacion de los otuliers viene dada tanto por despegarse de
#la normal como por despegarse de los valores inmediatos anteriores 
skewness(residuosb1)
kurtosis(residuosb1)
shapiro.test(residuosb1)# el p valor es menor a 0,05 por lo que me
#veo en la obligaciion de rechazar que los residuos son normales
#lo mismo me dice la kurtosis y la asimetria de los residuos.

#tras todo esto el MODELO 1 que relaciona directamente las variables
#ventas y numero de empleados, no es del todo bueno.

#-------------------------------------------------------------------#

#MODELO 2
#CONSIDERA QUE VARIABLES: NUMERO DE EMPLEADOS Y PRODUDCTIVIDAD AFECTAN A VENTAS

regresionb2<-lm(VENTAS~`NÚMERO DE EMPLEADOS`+PRODUCTIVIDAD,data=tablafiltrada)
summary(regresionb2)

#ventas= 447.42 empleados + 85,24 productividad -2043.82 
#segundo modelo con r cuadrado ajustado mejor 0,81 que el modelo 1
#un p valor menor al 0,05 en las 3 variables lo cual me indica que la probabilidad
#de encontrar evidencias que nieguen que la variable 
#afecta negativamente a la otra es bastante baja. por ende
#las variables son buenas


#grafico de puntos y recta de minimos cuadrados
plot(tablafiltrada$`NÚMERO DE EMPLEADOS`, 
     tablafiltrada$VENTAS, xlab="N.Empleados", ylab="Ventas")
abline(regresionb2)

#prueba del modelo
#creo un nuevo conjunto de empleados 
#este seria mi conjunto de test
nuevos_empleados$PRODUCTIVIDAD <- (seq(1,200))
predict(regresionb2,nuevos_empleados)

(residuosb2<- rstandard(regresionb2)) 
(valores.ajustadosb2<-fitted(regresionb2))
plot(valores.ajustadosb2, residuosb2) #
# mi modelo es mas acertado con valors
#mas bajos, es decir que dependiendo del tramo
#donde nos movamos, vamos a acertar menos en valores mas elevados,
#vamos a sobreestimar, con mas empleados fallaremos mas en
#nuestra predccionn de ventas, el modelo es heterocedastico
#se ve claramente que los residuos  disminuyen mientras mas ventas
#hay, el modelo es bueno solo para valores mas bajos


tablafiltrada$prediccion2 <- valores.ajustadosb2
tablafiltrada$residuos_cuadrado2 <- (tablafiltrada$VENTAS-
                                            tablafiltrada$prediccion2)^2


#veo si los residuos son normales

skewness(tablafiltrada$residuos_cuadrado2)
qqnorm(residuosb2)
qqline(residuosb2)
# Los puntos no se ven alineados, la normalidad no es aceptable.
plot(regresionb2)

skewness(residuosb2)
kurtosis(residuosb2)
shapiro.test(residuosb2)# el p valor es menor a 0,05 por lo que me
#veo en la obligacion de rechazar que los residuos son normales
#lo mismo me dice la kurtosis y la asimetria de los residuos
#la curtosis es mayor que en el modelo 1

#MODELO 2 MEJOR QUE 1

#-----------------------------------------------------------------#

#MODELO 3
#CONSIDERA QUE VARIABLE NUMERO DE EMPLEADOS Y PRODUCTIVIDAD ESTAN
#RELACIONADAS Y ESTO EN CONJUNTO AFECTA A LAS VENTAS

regresionb3<-lm(VENTAS~`NÚMERO DE EMPLEADOS`*PRODUCTIVIDAD, data=tablafiltrada)
summary(regresionb3)

residuosb3<- rstandard(regresionb3)
valores.ajustadosb3<-fitted(regresionb3)

#ventas= 96.86empleados - 5.46productividad +24,87empleados*productividad -499.7 
#tercer modelo con r cuadrado ajustado mejor 0,92,67 
#me pide quitar la variable productividad pero com estan vinculada a la
#variable numero de empleados, si la quito me quita tambien esta vinculacion
# y vuelvo al modelo 1 que es menos efectivo. por lo tanto acepto
#la variable productividad como buena, aun asi todas son menores a
#0,05

#----------------------------------------------------------------#
#MODELO 4

#CONSIDERA QUE VARIABLE NUMERO DE EMPLEADOS Y PRODUCTIVIDAD ESTAN
#RELACIONADAS Y ESTO EN CONJUNTO AFECTA A LAS VENTAS - EL INTERCEPTO

regresionb4<-lm(VENTAS~`NÚMERO DE EMPLEADOS`*PRODUCTIVIDAD -1, data=tablafiltrada)
summary(regresionb4)



#PRUEBA DEL MODELO


#CONJUNTO DE TEST

nuevos_empleados<-data.frame(`NÚMERO DE EMPLEADOS`=sample(1:200,1700,replace =TRUE),PRODUCTIVIDAD=sample(1:200,1700,replace = TRUE))
names(nuevos_empleados)[names(nuevos_empleados)=="NÚMERO.DE.EMPLEADOS"] <- "NÚMERO DE EMPLEADOS"
nuevos_empleados$PRODUCTIVIDAD_EMPLEADOS <- (nuevos_empleados$PRODUCTIVIDAD
                                             *nuevos_empleados$`NÚMERO DE EMPLEADOS`)

names(nuevos_empleados)[names(nuevos_empleados)=="PRODUCTIVIDAD_EMPLEADOS"] <- "NÚMERO DE EMPLEADOS*PRODUCTIVIDAD"
nuevos_empleados$ventasnuevas <- predict(regresionb4,nuevos_empleados)

#RESIDUOS
residuosb4<- rstandard(regresionb4)
valores.ajustadosb4<-fitted(regresionb4)

#COMPARACION DE RESIDUOS DEL MODELO 3 Y 4
par(mar=c(2,2,2,2))
plot(valores.ajustadosb4, residuosb4, col=c('chocolate','chocolate3'), 
     main='Modelo 4')
plot(valores.ajustadosb3, residuosb3, col=c('seagreen3','seagreen'),
     main='Modelo 3')

#modelo heterocedastico

#VALORES AJUSTADOS EN TABLA
tablafiltrada$prediccionb4 <- valores.ajustadosb4
tablafiltrada$residuos_cuadradob4 <- (tablafiltrada$VENTAS-
                                            tablafiltrada$prediccionb4)^2

tablafiltrada$residuos_cuadrado4=NULL

#NORMALIDAD DE LOS RESIDUOS

par(mfrow=c(1,1))
qqnorm(residuosb4, col='lightseagreen')
qqline(residuosb4)

#en los cuartiles 2 y 3 se ve meas normalidad que en los otros cuartiles
# sin embargo no estan del todo alineados,
#no se puede asumir normalidad en los residuos

plot(regresionb4, col='lightseagreen',cex.lab=0.5)

skewness(residuosb4)
kurtosis(residuosb4)
shapiro.test(residuosb4)
# el p valor es menor a 0,05 por lo que me
#veo en la obligacion de rechazar que los residuos son normales
#lo mismo me dice la kurtosis y la asimetria de los residuos


#IMPORTANCIAS RELATIVAS
names(tablafiltrada)[names(tablafiltrada)=="EMPLEADOS"] <- "NÚMERO DE EMPLEADOS"

relat.imp.RMSPE=hier.part(tablafiltrada$VENTAS, 
                          tablafiltrada[,c(3,5)], 
                          family="gaussian", gof="RMSPE", barplot=TRUE)
relat.imp.RMSPE

normresid<-read_excel("C:/Users/pamej/Documents/Datahack/MASTER/DATA ANALYTICS/IVAN GUERRERO/PrÃ¡ctica/importancia de las variables.xlsx")

attach(normresid) 
pie(normresid$Valor, normresid$Name, main = 
                      "Importancia de los Residuos", cex.main=1.55, 
    clockwise=FALSE, init.angle = 0, radius = 1) 


## Importancias relativas de las variables segun RMSPE hacia
#las ventas.
# Empleados  97,39%
# Productividad  2,61%
#es decir la variable que realmente tiene peso para la prediccion de
#ventas con el r cuadrado determinado es la cantidad de empleados.

#PRUEBA DE DURBIN WATSON

# Comenzamos viendo que se satisface uno de los supuestos del modelo lineal, pues
# existe independencia de los residuos.

# Prueba de Durbin-Watson. H0: no existe autocorrelacon.
dwtest(regresionb4)
# p-valor=0,002383<0,05: Negamos H0. 
# Existe autocorrelacion 
#Esto significa que los errores de las observaciones adyacentes
#estan correlacionados, por lo que la regresion de los minimos cuadrados
#puede subestimar el error estandar de los coeficientes lo que
#puede causar que los predictores parezcan significativos cuando
#en realidad no lo son



#DISTANCIAS DE COOK

# Graficamos los outliers sobre las variables analizadas.
cooks=cooks.distance(regresionb4)
cooks
quantile(0,1)
par(mfrow=c(2,1))
plot(regresionb4, col=ifelse(cooks>quantile(cooks,.9),'olivedrab1','black'),pch=20)

par(mfcol=c(1,2))
plot(tablafiltrada$VENTAS+tablafiltrada$`NÚMERO DE EMPLEADOS`,predict(regresionb4),
     col=ifelse(cooks>quantile(cooks,.9),'red','black'),pch=20)

par('mar')
par("mar")
par(mar=c(1,1,1,1))




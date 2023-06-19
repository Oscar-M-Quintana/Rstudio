rm(list=ls())
getwd()
setwd("~/R/probabilidad")
datosTitanic <- read.csv("datos_titanic.csv")
View(datosTitanic)
names(datosTitanic)
attach(datosTitanic)
idPasajer <- datosTitanic$PassengerId
class(Survived)
class(Pclass)
class(Sex)
class(Age)
Survived <- as.factor(Survived)
Pclass <- as.factor(Pclass)
Sexo <- factor(Sex)
class(Survived)
class(Pclass)
class(Sexo)
table(Survived) ## tabla de frecuencias absolutas
sum(table(Survived))
table(Pclass)
table(Sexo)
tablaSexo <- table(Sexo)
barplot(tablaSexo, width = 0.02, xlim = c(0,0.10), col = 'blue', density = 12)
tablaSobr <- table(Survived)/sum(table(Survived)) ## tabla de frecuencias relativas
barplot(tablaSobr,width=0.02,xlim=c(0,0.15),col="red")
tablaClase <- table(Pclass)
tablaClase
prop.table(tablaSobr)
#grafico de Torta
porcentajes <- as.numeric(round(((prop.table(tablaClase))*100),2))
etiquetas <- c("Clase 1", "Clase 2", "Clase 3")
etiquetas <- paste(etiquetas, porcentajes)
etiquetas <- paste(etiquetas, "%", sep = "")
pie(porcentajes,etiquetas,col=c("blue","green","red"),main="Gráfico de Torta - Clases")
#grafico de barras
contingencia <- table(Sex,Pclass)
contingencia
barplot(contingencia,width=0.02,xlim=c(0,0.15),col= c("blue","red"),main="Sexo por Clase",legend = rownames(contingencia),args.legend = list(x = "topleft") )
barplot(contingencia,beside=TRUE,col= c("blue","red"),main="Sexo por Clase",legend = rownames(contingencia),args.legend = list(x = "topleft") )

contingencia2 <- table(Pclass,Sex)/sum(table(Pclass,Sex)) ## frecuencias relativas
barplot(contingencia2,width=0.02,xlim=c(0,0.15),col=c("blue","red","green"),main="Clase por Sexo",legend = rownames(contingencia2),args.legend = list(x = "topleft") )

#histograma
hist(Fare,probability = TRUE,main="Histograma de datos de la variable Tarifa (Fare)")

#quantiles
datos <- c(-2.78, -1, 4.4, -0.52, 0.02, -1.77, -0.12, -1.07, -2.54, 1.48, 7.8)
quantile(datos,0.25)
quantile(datos,0.75)
quantile(datos,0.25,type=1)
quantile(datos,0.75,type=1)



datosNormales <- rnorm(150)
hist(datosNormales,probability = TRUE)
datosUnif <- runif(150)
hist(datosUnif,probability = TRUE)
datosGamma <- rgamma(150,shape=3.5,rate=2)
hist(datosGamma,probability = TRUE)
boxplot(datosNormales,datosUnif)
boxplot(Fare~Sexo)
qqnorm(datosNormales)
qqline(datosNormales,col=2)
qqnorm(datosUnif)
qqline(datosUnif,col=2)
qqnorm(datosGamma)
qqline(datosGamma,col=2)
qgamma(0.5,shape=8,rate=2) ## mediana poblacional de una gamma(8,2)
qnorm(0.75)-qnorm(0.25)


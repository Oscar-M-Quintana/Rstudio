#Inicio del Trabajo Practico obligatorio en R

#Se remueven los Objetos previos en el ambiente de trabajo 
rm(list=ls())

#Se configura la carpeta de trabajo
setwd("~/R/probabilidad/ejercicio_obligatorio")

#Se cargan los datos del archivo TXT en formato de tabla 
#parámetro "header" es para informar que tiene encabezado la tabla
nacimientos <- read.table("ENNyS_menorA2.txt", header = TRUE)

#Obtenemos acceso a las columnas sin referirnos a la base de dato
attach(nacimientos)
names(nacimientos)

#Ejercicio 1

#Transformamos las variables Sexo y Tipo_embarazo a variables categóricas
Sexo <- as.factor(Sexo)
Tipo_embarazo <- as.factor(Tipo_embarazo) 

#Variable Categórica "Sexo"

#a) Generamos y vemos variables con tablas de frecuencia
tabla_Sexo <- table(Sexo)
tabla_Sexo

#b) Generamos gráfico de barras con el comando "barplot"
barras_sexo <- barplot(tabla_Sexo,
                       main = "Gráfico de Barras - Sexo de los bebés",
                       width=0.02,
                       xlim=c(0,0.20),
                       ylim = c(0, 3200),
                       col=c("red", "blue")
                       )
text(barras_sexo, tabla_Sexo + 1, labels = tabla_Sexo, adj = c(0.5,-.5))

#c) Generamos gráfico de tortas con el comando "pie"
porcentajes <- as.numeric(round(((prop.table(tabla_Sexo))*100),2))
etiquetas <- c("Mujer", "Varon")
etiquetas <- paste(etiquetas, porcentajes)
etiquetas <- paste(etiquetas, "%", sep = "")
pie_sexo <- pie(porcentajes,
                main="Gráfico de Torta - Sexo de los bebés",
                etiquetas,
                col=c("red", "blue")
                )


#Variable Categórica "Tipo de embarazo"

#a) Generamos y vemos variables con tablas de frecuencia
tabla_tipo_embarazo <- table(Tipo_embarazo)
tabla_tipo_embarazo

#b) Generamos gráfico de barras con el comando "barplot"
barras_t_embarazo <- barplot(tabla_tipo_embarazo, 
                             main = "Gráfico de Barras - Tipo de embarazo",
                             width=0.02,
                             xlim=c(0,0.20),
                             ylim = c(0, 6500),
                             col=c("green", "yellow")
                             )
text(barras_t_embarazo,
     tabla_tipo_embarazo + 1,
     labels = tabla_tipo_embarazo,
     adj = c(0.5,-.5)
     )

#c) Generamos gráfico de tortas con el comando "pie"
porcentajes2 <- as.numeric(round(((prop.table(tabla_tipo_embarazo))*100),2))
etiquetas2 <- c("Multiple", "Simple")
etiquetas2 <- paste(etiquetas2, porcentajes2)
etiquetas2 <- paste(etiquetas2, "%", sep = "")
pie_t_embarazo <- pie(porcentajes2,
                      main="Gráfico de Torta - Tipo de embarazo",
                      etiquetas2,
                      col=c("green", "yellow")
                      )

# Ejercicio 2

# Generamos la tabla de contingencia y la visualizamos
contingencia <- table(Sexo, Tipo_embarazo)
contingencia

# Generamos el gráfico de Barras de la Tabla de contingencia

# Modo 1
barras_contingencia1 <- barplot(contingencia,
                               width=0.02,
                               xlim=c(0,0.3),
                               ylim=c(0,6000),
                               col= c("blue","red"),
                               main="Sexo por tipo de embarazo",
                               legend = rownames(contingencia),
                               args.legend = list(x = "topleft")
                               )
text(barras_contingencia1,
     contingencia + 1.5,
     labels = contingencia,
     adj = c(0.5,-.5)
)

# Modo 2
barras_contingencia2 <- barplot(contingencia,
                                beside=TRUE,
                                xlim= c(0,0.15),
                                col= c("blue","red"),
                                main="Sexo por tipo de embarazo",
                                legend = rownames(contingencia),
                                args.legend = list(x = "topleft") 
                                )
text(barras_contingencia2,
     contingencia + 1,
     labels = contingencia,
     adj = c(0.5,-.5)
)

# Ejercicio 3

# a) Histograma Edad con curva normal
histograma_edad <- hist(Edad, 
                        prob = TRUE, 
                        breaks = 24, 
                        ylim=c(0,.75),
                        main = "Histograma de edad con curva normal",
                        ylab = "Densidad"
                        )

x <- seq(min(Edad), max(Edad), length = 40)
f <- dnorm(x, mean = mean(Edad), sd = sd(Edad))
lines(x, f, col = "blue", lwd = 2)

# Histograma Peso con curva normal
histograma_peso <- hist(Peso, 
                        prob = TRUE, 
                        breaks = 25, 
                        ylim=c(0,.18),
                        main = "Histograma de peso con curva normal",
                        ylab = "Densidad"
)

x <- seq(min(Peso), max(Peso), length = 40)
f <- dnorm(x, mean = mean(Peso), sd = sd(Peso))
lines(x, f, col = "blue", lwd = 2)

# Histograma Perímetro Encefálico con curva normal
his_per_encefalico <- hist(Perim_encef, 
                        prob = TRUE, 
                        breaks = 50, 
                        ylim=c(0,.17),
                        main = "Histograma de Perimetro encefalico con curva normal",
                        ylab = "Densidad"
)

x <- seq(min(Perim_encef), max(Perim_encef), length = 50)
f <- dnorm(x, mean = mean(Perim_encef), sd = sd(Perim_encef))
lines(x, f, col = "blue", lwd = 2)

# Histograma Talla con curva normal
histograma_talla <- hist(Talla, 
                           prob = TRUE, 
                           breaks = 50, 
                           ylim=c(0,.05),
                           main = "Histograma de Perimetro encefalico con curva normal",
                           ylab = "Densidad"
)

x <- seq(min(Talla), max(Talla), length = 50)
f <- dnorm(x, mean = mean(Talla), sd = sd(Talla))
lines(x, f, col = "blue", lwd = 2)

min(Talla)
max(Talla)


tabla_Edad <- table(Edad)
tabla_Edad
tabla_Peso <- table(Peso)
tabla_Peso
tabla_Perim_encef <- table(Perim_encef)
tabla_Perim_encef
tabla_Talla <- table(Talla)
tabla_Talla


### Ejercicios de regresion con R codigo de libro

library(tidyverse)

file <- "https://raw.githubusercontent.com/fhernanb/datos/master/propelente"
datos <- read.table(file=file, header=TRUE)
head(datos) # shows the first 6 rows

ggplot(data= datos, aes(x= Edad, y= Resistencia))+
  geom_point()+
  theme_light()


mod1 <- lm(Resistencia ~ Edad, data=datos)
mod1 # Para imprimir el objeto mod1

summary(mod1)

ggplot(data= datos, aes(x= Edad, y= Resistencia))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, col= 'orange2')+
  theme_light()


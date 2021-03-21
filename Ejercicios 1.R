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



####Clase lm #####

class(mod1)

names(mod1)


datos <- datos %>% mutate(predicciones = predict(mod1))


ggplot(data = datos, aes( x= Edad, y = Resistencia))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, col = 'navy',se = FALSE)+
  geom_segment(aes(xend= Edad, yend= predicciones), col = 'red', lty = 'dashed')+
  geom_point(aes(y= predicciones), col = 'red')+
  theme_minimal()



##### Regresion ponderada #####


x <- c(4, 6, 8, 7, 8, 5)
y <- c(1, 2, 3, 4, 5, 4)
w <- c(0.1, 0.1, 0.2, 0.1, 0.2, 0.9)


mod_sin_pesos <- lm(y ~ x)
mod_con_pesos <- lm(y ~ x, weights=w)


summary(mod_con_pesos)
summary(mod_sin_pesos)


symbols(x=x, y=y, circles=w, pch=20, las=1, inches=0.1, fg='red', lwd=2)
abline(mod_sin_pesos, col='seagreen')
abline(mod_con_pesos, col='dodgerblue1')
legend('topleft', legend=c('Sin pesos', 'Con pesos'), 
       col=c('seagreen', 'dodgerblue1'), bty='n', lwd=1)



#### Regresion lineal multiple ####
library(MPV)
colnames(softdrink) <- c('tiempo', 'cantidad', 'distancia')
head(softdrink)


library(scatterplot3d)
attach(softdrink)
scatterplot3d(x=cantidad, y=distancia, z=tiempo, pch=16, cex.lab=1,
              highlight.3d=TRUE, type="h", xlab='Cantidad de cajas',
              ylab='Distancia (pies)', zlab='Tiempo (min)')



library(plotly)

plot_ly(x=cantidad, y=distancia, z=tiempo, type="scatter3d", color=tiempo) %>% 
  layout(scene = list(xaxis = list(title = 'Cantidad'),
                      yaxis = list(title = 'Distancia (pies)'),
                      zaxis = list(title = 'Tiempo (min)')))


mod <- lm(tiempo ~ cantidad + distancia, data=softdrink)
summary(mod)



# Se crea el grafico 3d y se guarda en un objeto, por ejemplo mi_3d
mi_3d <- scatterplot3d(x=cantidad, y=distancia, z=tiempo, pch=16, cex.lab=1,
                       highlight.3d=TRUE, type="h", xlab='Cantidad de cajas',
                       ylab='Distancia (pies)', zlab='Tiempo (min)')
# Para agregar el plano usamos $plane3d( ) con argumento modelo ajustado
mi_3d$plane3d(mod, lty.box="solid", col='mediumblue')




longley

mod <- lm(Employed ~ Unemployed + Armed.Forces + Year, data=longley)

library(broom)

tidy(mod, quick=TRUE)

nuevo <- data.frame(Year=c(1963, 1964),
                    Unemployed=c(420, 430),
                    Armed.Forces=c(270, 250))
nuevo



predict(object=mod, newdata=nuevo)


file <- "https://raw.githubusercontent.com/fhernanb/datos/master/propelente"
datos <- read.table(file=file, header=TRUE)

mod1 <- lm(Resistencia ~ Edad, data=datos)
nuevo <- data.frame(Edad=13.3625)
predict(object=mod1, newdata=nuevo, interval="confidence", level=0.95)
nuevo <- data.frame(Edad=10)
predict(object=mod1, newdata=nuevo, interval="prediction", level=0.95)


future_y <- predict(object=mod1, interval="prediction", level=0.95)
nuevos_datos <- cbind(datos, future_y)



###agregar intervalo de confianza
ggplot(nuevos_datos, aes(x=Edad, y=Resistencia))+
  geom_point() +
  geom_line(aes(y=lwr), color="red", linetype="dashed") +
  geom_line(aes(y=upr), color="red", linetype="dashed") +
  geom_smooth(method=lm, formula=y~x, se=TRUE, level=0.95, col='blue', fill='pink2') +
  theme_light()




gen_dat <- function(n) {
  varianza <- 16
  x <- runif(n=n, min=-5, max=6)
  media <- 4 - 6 * x
  y <- rnorm(n=n, mean=media, sd=sqrt(varianza))
  marco_datos <- data.frame(y=y, x=x)
  return(marco_datos)
}


gen_dat(5)
gen_dat(200)


datos <- gen_dat(n=200)


ggplot(datos, aes(x=x, y=y)) +
  geom_point() + theme_light()





mod <- lm(y ~ x, data=datos)
theta_hat <- c(coef(mod), sigma=summary(mod)$sigma)
theta_hat

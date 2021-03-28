### Ejercicios de regresion con R codigo de libro Modelos de Regresion con R de Freddy Henandez

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



##SImulacion

datos <- cars[1:5, ]
mod <- lm(dist ~ speed, data=datos)
simulate(object=mod, nsim=1, seed=1234)



file <- "https://raw.githubusercontent.com/fhernanb/datos/master/propelente"
datos <- read.table(file=file, header=TRUE)
mod1 <- lm(Resistencia ~ Edad, data=datos)

confint(object = mod1, parm = "Edad", level = 0.95)
confint(object = mod1, level = 0.95)




Precio <- c(12, 15, 25, 11, 16, 7)
Area <- c(3, 4, 1, 6, 5, 3)
Pisci <- factor(x=c('Grande', 'Sin', 'Pequena', 'Pequena', 'Sin', 'Grande'),
                levels=c('Sin','Pequena','Grande'))


# se usa model:matriz para obtener las variables indicadoreas



model.matrix(Precio ~ Area + Pisci)



require(MASS)
str(Cars93[, c('Price', 'EngineSize', 'Type')])



library(ggplot2)
ggplot(Cars93, aes(x=EngineSize, y=Price, color=Type)) + 
  geom_point() + theme_light()




mod <- lm(Price ~ EngineSize + Type, data=Cars93)
summary(mod)


require(MASS)
data("Cars93")
mod <- lm(Price ~ EngineSize + Type, data=Cars93)
anova(mod)


require(MASS)
data("Cars93")
mod_redu <- lm(Price ~ EngineSize, data=Cars93)
mod_comp <- lm(Price ~ EngineSize + Type, data=Cars93)
anova(mod_redu, mod_comp)


set.seed(8867)  # this makes the example exactly reproducible
y <- c(rnorm(10, mean=0,    sd=1),
       rnorm(10, mean=-0.5, sd=1),
       rnorm(10, mean=0.5,  sd=1))
g <- rep(c("A", "B", "C"), each=10)



#### Pruebas de Hipotesis en los B ####


file <- "https://raw.githubusercontent.com/fhernanb/datos/master/propelente"
datos <- read.table(file=file, header=TRUE)
mod <- lm(Resistencia ~ Edad, data=datos)
summary(mod)




require(MPV)
colnames(softdrink) <- c('tiempo', 'cantidad', 'distancia')
mod <- lm(tiempo ~ cantidad + distancia, data=softdrink, x=TRUE)


y <- softdrink$tiempo
n <- length(y)
ss_t <- sum(y^2) - sum(y)^2 / n
ss_r <- matrix(coef(mod), nrow=1) %*% t(mod$x) %*% matrix(y, ncol=1) - sum(y)^2 / n
ss_res <- ss_t - ss_r
ms_r <- ss_r / (length(coef(mod))-1)
ms_res <- ss_res / (n-length(coef(mod)))
F0 <- ms_r / ms_res
valorP <- pf(F0, df1=length(coef(mod))-1, df2=(n-length(coef(mod))), lower.tail=FALSE)
tabla <- matrix(NA, ncol=5, nrow=3)
tabla[1, ] <- c(ss_r, length(coef(mod))-1, ms_r, F0, valorP)
tabla[2, 1:3] <- c(ss_res, n-length(coef(mod)), ms_res)
tabla[3, 1:2] <- c(ss_t, n-1)
colnames(tabla) <- c('Suma Cuadrados', 'gl', 'Cuadrado medio', 'F0', 'Valor-P')
rownames(tabla) <- c('Reg', 'Resid', 'Total')
tabla



require(MPV)
data(table.b4) 
head(table.b4, n=4)

redu_mod <- lm(y ~ x1 + x2, data=table.b4, x=TRUE)
comp_mod <- lm(y ~ x1 + x2 + x3 + x4, data=table.b4, x=TRUE)

n <- 24 # numero de observaciones
p0 <- 3 # numero de betas en modelo reducido
p1 <- 5 # numero de betas en modelo completo

ssr_reduced  <- sum(table.b4$y) - sum(redu_mod$residuals^2)
ssr_complete <- sum(table.b4$y) - sum(comp_mod$residuals^2)
ms_res <- summary(comp_mod)$sigma^2
F0 <- ((ssr_complete - ssr_reduced) / (p1-p0)) / ms_res
F0



beta2 <- matrix(c(0.3233333, -0.2176622), ncol=1)
x1 <- comp_mod$x[, 1:3]
x2 <- comp_mod$x[, 4:5]
a1 <- t(beta2) %*% t(x2)
a2 <- diag(n) - x1 %*% solve(t(x1) %*% x1) %*% t(x1)
a3 <- x2 %*% beta2
lambda <- (a1 %*% a2 %*% a3) / summary(comp_mod)$sigma^2
lambda


pf(q=F0, df1=p1-p0, df2=n-p1, ncp=lambda, lower.tail=FALSE)
pf(q=F0, df1=p1-p0, df2=n-p1, lower.tail=FALSE)


library(MASS)
mod0 <- lm(Price ~ 1, data=Cars93)
mod1 <- lm(Price ~ Horsepower, data=Cars93)
anova(mod0, mod1)

mod2 <- lm(Price ~ Horsepower + Type, data=Cars93)
anova(mod1, mod2)

mod3 <- lm(Price ~ Horsepower + Type + Weight, data=Cars93)
anova(mod2, mod3)

anova(mod3)



require(MPV)
data(table.b4) 
head(table.b4, n=4)

mod1 <- lm(y ~ x1 + x2, data=table.b4)
mod2 <- lm(y ~ x1 + x2 + x3 + x4, data=table.b4)

anova(mod1, mod2, test='F')

set.seed(8867)  # this makes the example exactly reproducible
y <- c(rnorm(10, mean=0,    sd=1),
       rnorm(10, mean=-0.5, sd=1),
       rnorm(10, mean=0.5,  sd=1))
g <- rep(c("A", "B", "C"), each=10)

model <- lm(y ~ g)

summary(model)

anova(model)

require(MPV)
data(table.b4) 
head(table.b4, n=4)

mod0 <- lm(y ~ x1 + x2, data=table.b4)
mod1 <- lm(y ~ x1 + x2 + x3 + x4, data=table.b4)

lambda <- -2 * (logLik(mod0) - logLik(mod1))
lambda
pchisq(q=lambda, df=2, lower.tail=FALSE)


require(MPV)
data(table.b4) 
mod <- lm(y ~ x1 + x2 + x3 + x4, data=table.b4)
coef(mod)



library(multcomp)
C <- matrix(c(0, 0, 0, 1, 0,
              0, 0, 0, 0, 1), ncol=5, byrow=TRUE)
mult_test <- glht(model=mod, linfct=C, alternative='two.sided', rhs=c(0, 0))
summary(mult_test, test = adjusted(type="single-step"))



residuals(object, type=c("working", "response", "deviance", "pearson", "partial"))
rstandard(object)
rstudent(object)

residuals(mod1)

x <- c(4, 6, 8, 7, 8, 5)
y <- c(1, 2, 3, 4, 5, 4)
w <- c(0.1, 0.1, 0.2, 0.1, 0.2, 0.9)


mod <- lm(y ~ x, weights=w)

ei <- y - fitted(mod)
pi <- ei * sqrt(mod$weights)
hii <- lm.influence(mod)$hat
di <- ei * sqrt(mod$weights) / sqrt(summary(mod)$sigma^2 * (1-hii))
ri <- ei * sqrt(mod$weights) / sqrt(lm.influence(mod)$sigma^2 * (1-hii))

cbind(ei=ei, pi=pi, di=di, ri=ri)

cbind(ei=residuals(mod, type='working'),
      pi=residuals(mod, type='pearson'),
      di=rstandard(mod),
      ri=rstudent(mod))


gen_dat <- function(n) {
  varianza <- 16
  x <- runif(n=n, min=-5, max=6)
  media <- 4 - 6 * x
  y <- rnorm(n=n, mean=media, sd=sqrt(varianza))
  marco_datos <- data.frame(y=y, x=x)
  return(marco_datos)
}

datos <- gen_dat(n=500)
mod <- lm(y ~ x, data=datos)


par(mfrow=c(2, 2))
plot(mod, las=1, col='deepskyblue4', which=1:3)


gen_dat <- function(n) {
  varianza <- 16
  x <- runif(n=n, min=-5, max=6)
  media <- 4 - 6 * x + 2 * x^2
  y <- rnorm(n=n, mean=media, sd=sqrt(varianza))
  marco_datos <- data.frame(y=y, x=x)
  return(marco_datos)
}

datos <- gen_dat(n=500)
mod <- lm(y ~ x + I(x^2), data=datos)

par(mfrow=c(2, 2))
plot(mod, las=1, col='darkseagreen3', which=1:3)




library(car)
prestige_mod1 <- lm(prestige ~ education + income + type, data=Prestige)

residualPlots(prestige_mod1, las=1)


marginalModelPlots(prestige_mod1, las=1)


prestige_mod2 <- lm (prestige ~ education + income + I(income^2) + type, data=Prestige)

residualPlots(prestige_mod2, las = 1)

anova(prestige_mod1, prestige_mod2)

y <- c(2, 3, 6, 5)
x <- c(3, 5, 6, 7)
z <- c(5, 4, 6, 3)
x_2 = 4
z_2 = 1

X <- cbind(1, x, z)

H <- X %*% solve(t(X) %*% X) %*% t(X)

diag(H)
H


# Forma 2
mod <- lm(y ~ x + z)
hatvalues(mod)


# Forma 3
lm.influence(mod)$hat
library(ggplot2)

ggplot()+
  geom_point(aes(x = x, y = z ))+
  geom_point(aes(x = x_2, y = z_2), color= "red")
  


url <- "https://raw.githubusercontent.com/fhernanb/datos/master/medidas_cuerpo2"
datos <- read.table(file=url, sep="\t", header=TRUE)

mod <- lm(Peso ~ Estatura + circun_cuello + circun_muneca, data=datos)

library(car)
outlierTest(mod, cutoff=Inf, n.max=4)

influenceIndexPlot(mod, vars="Bonf", las=1)


head(datos)


panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr=c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex=cex.cor * r, col=gray(1-r))
}

# Creamos el grafico SOLO para las variables cuantitativas
pairs(datos[, c("Peso", "Estatura", "circun_cuello", "circun_muneca")], 
      pch=19, las=1,
      upper.panel=panel.smooth, lower.panel=panel.cor)


mod1 <- lm(Peso ~ Estatura + circun_cuello + circun_muneca, data=datos)
summary(mod1)

mod2 <- mixlm::backward(mod1, alpha=0.04)
abline


# Para construir el grafico de dispersion
with(datos, 
     plot(x=circun_cuello, y=Peso, pch=19, las=1,
          xlab="Circunferencia cuello (cm)", ylab="Peso (Kg)"))
# Ahora agregamos la linea de tendencia
abline(mod2, lwd=3, col='blue2')
# por ultimo un texto con la ecuacion o modelo ajustado
text(x=34, y=95, expression(hat(Peso) == -44.61 + 3.10 * C.cuello), 
     col='blue3' )


cooks.distance(mod)



cutoff <- 4 / (26-2-2)  # Cota
plot(mod, which=4, cook.levels=cutoff, las=1)
abline(h=cutoff, lty="dashed", col="dodgerblue2")



library(car)
influenceIndexPlot(mod, vars="Cook")

plot(mod)


x <- c(2, 5, 3, 4, 7)
y <- c(5, 9, 8, 7, 19)

plot(x=x, y=y, pch=19, col="tomato", las=1)

mod <- lm(y ~ x)

dffits(mod)

dfbetas(mod)

dfbeta(mod)


PRESS <- function(linear.model) {
  # calculate the predictive residuals
  pr <- residuals(linear.model) / (1-lm.influence(linear.model)$hat)
  # calculate the PRESS
  PRESS <- sum(pr^2)
  return(PRESS)
}

pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1-PRESS(linear.model)/(tss)
  return(pred.r.squared)
}


require(MPV)
colnames(softdrink) <- c('tiempo', 'cantidad', 'distancia')
mod <- lm(tiempo ~ cantidad + distancia, data=softdrink)

PRESS(mod)
pred_r_squared(mod)


gen_data <- function(n) {
  x1 <- rpois(n, lambda=5)
  x2 <- rbinom(n, size=6, prob=0.4)
  ei <- rnorm(n=n, sd=x2)
  y <- -3 + 2 * x1 + 4 * x2 + ei
  data.frame(y, x1, x2)
}

n <- 200
datos <- gen_data(n=n)
mod <- lm(y ~ x1 + x2, data=datos) # Modelo de interes



ei <- resid(mod)
fit <- lm(ei^2 ~ x1 + x2, data=datos) # Modelando ei^2 ~ x1 + x2
R2 <- summary(fit)$r.squared
k <- 2
estadistico <- n * R2
valorP <- pchisq(q=estadistico, df=k, lower.tail=FALSE)
cbind(estadistico, valorP)


library(lmtest)
bptest(mod)
### para pruebas no lineales###
bptest(mod, varformula = ~ x1 * x2 + I(x1^2) + I(x2^2), data=datos)



fit <- lm(resid(mod)^2 ~ x1 + x2 + x1 * x2 + I(x1^2) + I(x2^2), data=datos) 
R2 <- summary(fit)$r.squared
estadistico <- n * R2
valorP <- pchisq(q=estadistico, df=5, lower.tail=FALSE)
cbind(estadistico, valorP)



library(car)
ncvTest(mod)


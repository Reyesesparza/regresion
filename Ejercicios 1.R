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



## generate regressor
x <- rep(c(-1, 1), 50)

## generate the AR(1) error terms with parameter rho = 0 (white noise)
err1 <- rnorm(100)
## generate dependent variable
y1 <- 1 + x + err1
library(lmtest)
mod1 <- lm(y1 ~ x)
dwtest(mod1) ## perform Durbin-Watson test


plot(residuals(mod1), pch=19, col="deepskyblue1")

## generate the AR(1) error terms with parameter rho = 0.9 respectively
err2 <- stats::filter(x=err1, filter=0.9, method="recursive")
## generate dependent variable
y2 <- 1 + x + err2

mod2 <- lm(y2 ~ x)
dwtest(mod2) ## perform Durbin-Watson test


plot(residuals(mod2), pch=19, col="tomato")



library(lmtest)
mod1 <- lm(y1 ~ x)
bgtest(mod1) ## perform Durbin-Watson test


mod2 <- lm(y2 ~ x)
bgtest(mod2) ## perform Durbin-Watson test


##Modelos Polinomiales




conc <- c(1, 1.5, 2, 3, 4, 4.5, 5, 5.5, 6, 6.5, 7, 8, 9, 10, 11, 12, 13, 14, 15)
resis <- c(6.3, 11.1, 20, 24, 26.1, 30, 33.8, 34, 38.1, 39.9, 42, 46.1, 53.1, 
           52, 52.5, 48, 42.8, 27.8, 21.9)
datos <- data.frame(concentracion=conc, resistencia=resis)


library(ggplot2)
ggplot(datos, aes(x=concentracion, y=resistencia)) + 
  geom_point() + theme_light()


mod1 <- lm(resistencia ~ concentracion, data=datos)
mod2 <- lm(resistencia ~ concentracion + I(concentracion^2), data=datos)


ggplot(datos, aes(x=concentracion, y=resistencia)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=FALSE, col='dodgerblue1') +
  geom_smooth(method='lm', formula=y~x+I(x^2), se=FALSE, col='tomato') +
  theme_light()



anova(mod1, mod2)

anova(mod1, mod2)

par(mfrow=c(1, 2))
plot(mod1, which=1, caption='Modelo lineal')
plot(mod2, which=1, caption='Modelo cuadratico')




drop <- c(8.33, 8.23, 7.17, 7.14, 7.31, 7.60, 7.94, 8.30, 8.76, 8.71, 9.71,
          10.26, 10.91, 11.67, 11.76, 12.81, 13.30, 13.88, 14.59,
          14.05, 14.48, 14.92, 14.37, 14.63, 15.18, 14.51, 14.34, 
          13.81, 13.79, 13.05, 13.04, 12.60, 12.05, 11.15, 11.15, 
          10.14, 10.08,9.78,9.80,9.95,9.51)
time <- seq(from=0, to=20, by=0.5)
datos <- data.frame(time=time, drop=drop)



plot(datos, ylab="Voltage drop", xlab="Time (seconds)", pch=19, ylim=c(0, 15), las=1)
abline(v=6.5, lty="dotted", col='tomato')
abline(v=13, lty="dotted", col='tomato')
text(x=6.5, y=0.3, 't=6.5', col='tomato')
text(x=13, y=0.3, 't=13', col='tomato')


mod1 <- lm(drop ~ time + I(time^2) + I(time^3), data=datos)
summary(mod1)



xplus <- function(x) ifelse(x >= 0, x, 0)  # Auxiliar function
time65 <- xplus(time - 6.5)               # New variable 1
time13 <- xplus(time - 13)                 # New variable 2
mod2 <- lm(drop ~ time + I(time^2) + I(time^3) + I(time65^3) + I(time13^3), data=datos)
summary(mod2)



plot(datos, ylab="Voltage drop", xlab="Time (seconds)", pch=19, ylim=c(0, 15), las=1)
i <- order(time)
lines(time[i], fitted(mod1)[i], col=2, lwd=3)
lines(time[i], fitted(mod2)[i], col=4, lwd=3)
legend("bottomright", lwd=3, col=c(4,2), bty="n",
       legend=c("Cubic spline model", "Cubic polynomial model"))




require(splines)
mod3 <- lm(drop ~ bs(time, knots=c(6.5, 13), degree=3), data=datos)
summary(mod3)




plot(datos, ylab="Voltage drop", xlab="Time (seconds)", pch=19, ylim=c(0,15))
lines(time[i], fitted(mod1)[i], col='red', lwd=3)
lines(time[i], fitted(mod2)[i], col='blue', lwd=6)
lines(time[i], fitted(mod3)[i], col='orange', lwd=1)
legend("bottomright", lwd=c(3, 6, 2), col=c('red', 'blue', 'orange'),
       legend=c("Cubic polynomial model", 
                "Cubic spline manually",
                "Using bs()"), bty="n")
abline(v=c(6.5, 13), lty='dotted', col="tomato") # adding cutpoints




library(MPV)
colnames(softdrink) <- c('tiempo', 'cantidad', 'distancia')

library(splines)

mod1 <- lm(tiempo ~ cantidad + distancia, data=softdrink)
mod2 <- lm(tiempo ~ bs(cantidad) + bs(distancia), data=softdrink)



y_true <- softdrink$tiempo
y_hat1 <- predict(mod1, newdata=softdrink)
y_hat2 <- predict(mod2, newdata=softdrink)

mse1 <- mean((y_true - y_hat1)^2)
mse2 <- mean((y_true - y_hat2)^2)
cbind(mse1, mse2)



library(car)
plot(prestige ~ income, xlab="Average Income",
     ylab="Prestige", data=Prestige, pch=19)


mod_lowess <- lowess(x=Prestige$income, y=Prestige$prestige, f=2/3)




plot(prestige ~ income, xlab="Average Income",
     ylab="Prestige", data=Prestige, pch=19)
lines(mod_lowess, lwd=4, col='tomato')



library(plotly)
plot_ly(x=Prestige$income, 
        y=Prestige$education, 
        z=Prestige$prestige, type="scatter3d", color=Prestige$prestige) %>% 
  layout(scene = list(xaxis = list(title = 'Income'),
                      yaxis = list(title = 'Education'),
                      zaxis = list(title = 'Prestige')))



mod_loess <- loess(prestige ~ income + education, data=Prestige, 
                   degree=2, span=0.75)


library("plot3D")
scatter3D(x=Prestige$income,
          y=Prestige$education,
          z=Prestige$prestige, ticktype="detailed", pch=20, 
          bty="f", colkey=FALSE, phi=30, theta=45, type="h",
          xlab='Income',
          ylab='Education',
          zlab='Prestige',
          surf=list(x=inc, y=edu, z=fit.prestige,  
                    NAcol="black", shade=0.1))



temp <- c(200, 250, 200, 250, 189.65, 260.35, 225, 225, 225, 225, 225, 225)
conc <- c(15, 15, 25, 25, 20, 20, 12.93, 27.07, 20, 20, 20, 20)
rend <- c(43, 78, 69, 73, 48, 76, 65, 74, 76, 79, 83, 81)



library(scatterplot3d)
scatterplot3d



mod <- lm(rend ~ temp + conc + I(temp^2) + I(conc^2) + temp * conc)




# Se crean 30 valores de las variables para crear la rejilla
Temperatura   <- seq(from=189.65, to=260.35, length.out=30)
Concentracion <- seq(from=12.93, to=27.07, length.out=30)
# Rend es la funcion a dibujar
Rend <- function(temp, conc) {
  res <- coef(mod) * c(1, temp, conc, temp^2, conc^2, temp * conc)
  sum(res)
}
Rend <- Vectorize(Rend) # La funcion a dibujar debe estar vectorizada
# La matriz Rendimiento con las alturas de la superficie se crea con outer
Rendimiento <- outer(Temperatura, Concentracion, Rend)
# Para dibujar la superficie de respuesta
persp(x=Temperatura, y=Concentracion, z=Rendimiento,
      theta=40, phi=30, ticktype = "detailed", col='salmon1')


contour(x=Temperatura, y=Concentracion, z=Rendimiento,
        nlevels=10, col=gray(0.3), lwd=2, lty='solid',
        xlab='Temperatura', ylab='Concentracion', las=1)


filled.contour(x=Temperatura, y=Concentracion, z=Rendimiento,
               nlevels=10, xlab='Temperatura', ylab='Concentracion',
               las=1, color.palette = cm.colors)


minus_rend <- function(x) {
  temp <- x[1]
  conc <- x[2]
  new.data <- data.frame(temp=c(1, temp), conc=c(1, conc))
  -predict(mod, new.data)[2]
}
inicio <- c(192, 15)  # valores iniciales para la busqueda
names(inicio) <- c('Temperatura', 'Concentracion') # Colocando nombres
res <- nlminb(start=inicio, objective=minus_rend,
              lower=c(189.65, 12.93), # minimos de las variables
              upper=c(260.35, 27.07), # maximos de las variables
              control=list(trace=0))
res$par  # Valores optimos


library(MPV)  # Aqui estan los datos
table.b3[22:26, ] # Can you see the missing values?

datos <- table.b3[-c(23, 25), ]



full.model <- lm(y ~ ., data=datos)
summary(full.model)

library(MASS)  # Para poder usar la funcion stepAIC
modback <- stepAIC(full.model, trace=TRUE, direction="backward")


modback$anova

summary(modback)



empty.model <- lm(y ~ 1, data=datos)
horizonte <- formula(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11)



modforw <- stepAIC(empty.model, trace=FALSE, direction="forward", scope=horizonte)
modforw$anova

modforw <- update(modforw, y ~ x1)
summary(modforw)


modboth <- stepAIC(empty.model, trace=FALSE, direction="both", scope=horizonte)
modboth$anova


summary(modback)$adj.r.squared
summary(modforw)$adj.r.squared


mod1 <- lm(y ~ x2 + x5, data=datos)
maximo <- formula(~ x1 + x2 + x3 + x4 + x5 + x6)
addterm(mod1, scope=maximo)


model <- lm(mpg ~ disp + hp + wt + qsec, data=mtcars)

library(olsrr)
res <- ols_step_all_possible(model)
res

plot(res)




library(MPV) # Aqui estan los datos
datos <- table.b3[-c(23, 25), ] # Eliminando 2 observaciones con NA
modelo <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11,
             data=datos)

library(mixlm)
backward(modelo, alpha=0.04)




#### Multicolinealidad ###

gen_dat <- function(n) {
  x1 <- runif(n=n, min=0, max=10)
  x2 <- x1 * 2 + rnorm(n=n, sd=0.01) # x2 es el doble de x1 + ruido
  y <- rnorm(n=n, mean= - 3 + 2 * x1 - 4 * x2, sd=2)
  data.frame(y, x1, x2)
}



set.seed(12345)
datos <- gen_dat(n=40)
datos1 <- datos[1:20, ]
datos2 <- datos[21:40, ]



mod1 <- stats::lm(y ~ x1 + x2, data=datos1)
summary(mod1)
mod2 <- stats::lm(y ~ x1 + x2, data=datos2)
summary(mod2)



mtcars %>% cor(method="pearson") %>% round(digits=2) -> mat_cor
mat_cor


library(corrplot)


corrplot(mat_cor, type="upper", order="hclust", tl.col="black", tl.srt=45)



gen_dat <- function(n) {
  x1 <- sample(5:25, size=n, replace=TRUE)
  x2 <- rpois(n, lambda=5)
  x3 <- rbinom(n, size=10, prob=0.4)
  x4 <- rbeta(n=n, shape1=0.5, shape2=0.7)
  ruido1 <- runif(n=n, min=-0.5, max=0.5)
  ruido2 <- runif(n=n, min=-0.5, max=0.5)
  x5 <- x1 - 3 * x2 + ruido1
  x6 <- x2 - 4 * x3 + ruido2
  y <- rnorm(n=n, mean= - 3 + 2 * x1 - 4 * x2, sd=2)
  data.frame(y, x1, x2, x3, x4, x5, x6)
}




datos <- gen_dat(n=30)
mod <- lm(y ~ ., data=datos)
car::vif(mod)




library(dplyr)
datos %>% select(-y) %>% cor(method="pearson") %>% round(digits=2) -> mat_cor
library(corrplot)
corrplot(mat_cor, type="upper", tl.col="black", tl.srt=45)




M <- matrix(c(1, 2,
              1, 5), nrow=2, byrow=TRUE)
M



datos <- data.frame(Precio = c(12, 15, 25, 11, 16, 7),
                    Area = c(3, 4, 1, 6, 5, 3), 
                    Pisci = factor(x=c('Grande', 'Sin', 'Pequena', 'Pequena', 'Sin', 'Grande'),
                                   levels=c('Sin','Pequena','Grande')))
datos



form <- formula(Precio ~ Area + Pisci)
X <- model.matrix(object=form, data=datos)
X


mf <- model.frame(Precio ~ Area + Pisci, data=datos)
y <- model.extract(mf, "response")
y



y <- c(4, 2, 3, 1, 5)
x <- c(2, 5, 7, 9, 1)


X <- cbind(intercepto=1, variable=x)
X


fit <- lm.fit(x=X, y=y)
coef(fit)

#Codigo para problema 2

mis_dades <- iris
mis_dades

x <- mis_dades$Petal.Length
y <- mis_dades$Sepal.Length

plot(x,y)

x_bar <- mean(x)
y_bar <- mean(y)

m <- sum((x-x_bar)*(y-y_bar))/sum((x-x_bar)^2)
b <- y_bar - m*x_bar

pred1.5 <- m*1.5+b
x_pred <- x
y_pred <- m*x_pred+b

plot(x,y)
lines(x_pred,y_pred)

# Coeficiente de determinación
Rsq <- sum((y_pred-y_bar)^2)/sum((y-y_bar)^2)

# Coef. de correlación
R <- sqrt(Rsq)
cor.test(x,y)


# Alternativamente,

mod <- lm(y~x)
summary(mod)

y_pred2 <- predict(mod, data.frame(x=x))

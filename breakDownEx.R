# Paczki ----
library(MASS)
library(dplyr)
library(randomForest)
library(breakDown)
library(mlr)
library(ggplot2)
library(DALEX)
# Pierwsze podejście: contributions w przeciwnych kierunkach ----
## Problem: regresja nie umie inaczej, dobrze byłoby znaleźć przykład,
## gdzie nie jest to ograniczenie modelu, tylko to, czego nauczył się
## konkretny model.
### Symulacja danych
set.seed(14)
X <- mvrnorm(200, rep(0, 2), matrix(c(1, 0, 0, 1), nrow = 2))
beta1 <- c(2, 3)
Y1 <- X[1:100, ]%*%beta1
beta2 <- c(2, -3)
Y2 <- X[101:200, ]%*%beta2
Y <- c(Y1, Y2)# + rnorm(200)
dt <- data.frame(x1 = X[, 1], x2 = X[, 2], y = Y)
tsk <- makeRegrTask(data = dt, target = "y")
lrns <- makeLearners(c("lm", "randomForest", "svm"), type = "regr")
### Benchmark
por <- benchmark(lrns, tsk)
por
### Porównanie, czego nauczyły się modele.
ml <- getLearnerModel(train(lrns[[1]], tsk))
rf <- getLearnerModel(train(lrns[[2]], tsk))
sv <- getLearnerModel(train(lrns[[3]], tsk))
br1 <- broken(rf, dt[2, ], dt[, -3], baseline = "Intercept")
br2 <- broken(rf, dt[177, ], dt[, -3], baseline = "Intercept")
plot(br1)
plot(br2)
br1l <- broken(ml, dt[2, ], dt[, -3], baseline = "Intercept")
br2l <- broken(ml, dt[177, ], dt[, -3], baseline = "Intercept")
plot(br1l)
# plot(br2l) # mało odkrywcze
# Zabawy z kwartetem Anscombe'a ----
plot(anscombe[, c(3, 7)])
rff <- randomForest(y3~x3, data = anscombe, nodesize = 1)
mll <- lm(y3~x3, data= anscombe)
expll <- explain(rff, data = anscombe)
explll <- explain(mll, data = anscombe)
plot(single_variable(expll, variable = "x3"),
     single_variable(explll, variable = "x3")) +
  geom_point(data = anscombe, aes(x = x3, y = y3), inherit.aes = F) +
  geom_point(data = data.frame(x = anscombe$x3, y = predict(mll)),
             aes(x, y), inherit.aes = F, color = "red") +
  geom_point(data = data.frame(x = anscombe$x3, y = predict(rff)),
             aes(x, y), inherit.aes = F, color = "blue")

rff <- randomForest(y2~x2, data = anscombe, nodesize = 1)
mll <- lm(y2~x2, data= anscombe)
mean((predict(rff)-anscombe$y2)^2)
mean((predict(mll)-anscombe$y2)^2)
expll <- explain(rff, data = anscombe)
explll <- explain(mll, data = anscombe)
plot(single_variable(expll, variable = "x2"),
     single_variable(explll, variable = "x2")) +
  geom_point(data = anscombe, aes(x = x2, y = y2), inherit.aes = F) +
  geom_point(data = data.frame(x = anscombe$x2, y = predict(mll)),
             aes(x, y), inherit.aes = F, color = "red") +
  geom_point(data = data.frame(x = anscombe$x2, y = predict(rff)),
             aes(x, y), inherit.aes = F, color = "blue")
# Pierwsze podejście: poprawianie regresji liniowej na podstawie lasu. ----
# Single variable
n <- 1000
nc <- 3
nl <- 2
leffect <- 1
# leffects <- rep(leffect, nl)
leffects <- c(1, 4)
transf <- exp
c <- 1
X3 <- mvrnorm(n, rep(0, nc), diag(1, nc))
Y <- X3[, 1:nl, drop = F]%*%leffects + c*apply(X3[, (nl + 1):nc, drop = F], 2, transf)%*%rep(1, nc - nl) + rnorm(n)
X3 <- data.frame(cbind(X3, Y))
colnames(X3)[nc+1] <- "y"
tsk <- makeRegrTask(data = X3, target = "y")
benchmark(c("regr.lm", "regr.randomForest"), tsk)
lm3 <- lm(y ~., data = X3)
# mean((predict(lm3) - Y)^2)
rf3 <- randomForest(y ~., data = X3, ntree = 2000)
# mean((predict(rf3) - Y)^2)
expl_lm <- explain(lm3, data = X3, y = X3$y)
expl_rf <- explain(rf3, data = X3, y = X3$y)
plot(variable_dropout(expl_lm), variable_dropout(expl_rf))
# x3elm <- single_variable(expl_lm, variable = "X3")
x3erf <- single_variable(expl_rf, variable = "X3")
plot(x3erf)
plot(X3[, 3], residuals(lm3))
x2erf <- single_variable(expl_rf, variable = "X2")
plot(x2erf)
x1erf <- single_variable(expl_rf, variable = "X1")
plot(x1erf) # +
  # geom_point(data = data.frame(x=X3$X1,y=predict(lm3)), aes(x,y), inherit.aes=F)
# Jak się doda odkomentowaną część, może widać zysk z pdp w porównaniu z analizą reszt
# Single prediction ----




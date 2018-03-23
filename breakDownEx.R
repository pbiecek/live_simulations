library(MASS)
library(dplyr)
library(randomForest)
library(breakDown)
library(mlr)
library(ggplot2)
library(DALEX)

set.seed(14)
X <- mvrnorm(200, rep(0, 2), matrix(c(1, 0, 0, 1), nrow = 2))
beta1 <- c(2, 3)
Y1 <- X[1:100, ]%*%beta1
beta2 <- c(2, -3)
Y2 <- X[101:200, ]%*%beta2
Y <- c(Y1, Y2)# + rnorm(200)
dt <- data.frame(x1 = X[, 1], x2 = X[, 2], y = Y)
tsk <- makeRegrTask(data = dt, target = "y")
lrns <- makeLearners(c("lm", "randomForest"), type = "regr")
por <- benchmark(lrns, tsk)
por

ml <- getLearnerModel(train(lrns[[1]], tsk))
rf <- getLearnerModel(train(lrns[[2]], tsk))
br1 <- broken(rf, dt[2, ], dt[, -3], baseline = "Intercept")
br2 <- broken(rf, dt[177, ], dt[, -3], baseline = "Intercept")
plot(br1)
plot(br2)
br1l <- broken(ml, dt[2, ], dt[, -3], baseline = "Intercept")
br2l <- broken(ml, dt[177, ], dt[, -3], baseline = "Intercept")
plot(br1l)
# plot(br2l)

#
# X2 <- mvrnorm(200, rep(0, 2), diag(1, 2))
# Y22 <- X2[, 1]*4 + (X2[, 2])^2
# dt2 <- data.frame(x1 = X2[, 1], x2 = X2[, 2], y = Y22)
# tsk2 <- makeRegrTask(data = dt2,
#                      target = "y", id = "tsk2")
# benchmark(makeLearners(c("randomForest", "lm"), type = "regr"), tsk2)
# library(DALEX)
# expl1 <- explain(getLearnerModel(train("regr.lm", tsk2)), data = dt2)
# plot(single_variable(expl1, "x2"))
# expl2 <- explain(getLearnerModel(train("regr.randomForest", tsk2)), data = dt2)
# plot(single_variable(expl2, "x2"))

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


#
n <- 1000
nc <- 3
nl <- 2
transf <- exp
X3 <- mvrnorm(n, rep(0, nc), diag(1, nc))
Y <- X3[, 1:nl, drop = F]%*%rep(4, nl) + apply(X3[, (nl + 1):nc, drop = F], 2, transf)%*%rep(1, nc - nl) + rnorm(n)
X3 <- data.frame(cbind(X3, Y))
colnames(X3)[nc+1] <- "y"
tsk <- makeRegrTask(data = X3, target = "y")
benchmark(c("regr.lm", "regr.randomForest"), tsk)
lm3 <- lm(y ~., data = X3)
# mean((predict(lm3) - Y)^2)
rf3 <- randomForest(y ~., data = X3, ntree = 2000)
# mean((predict(rf3) - Y)^2)
expl_lm <- explain(lm3, data = X3)
expl_rf <- explain(rf3, data = X3)
# x3elm <- single_variable(expl_lm, variable = "X3")
x3erf <- single_variable(expl_rf, variable = "X3")
plot(x3erf)
plot(X3[, 3], residuals(lm3))
x2erf <- single_variable(expl_rf, variable = "X2")
plot(x2erf)
x1erf <- single_variable(expl_rf, variable = "X1")
plot(x1erf)

library(MASS)
library(live)
library(breakDown)
library(randomForest)
library(kernlab)

# Strong correlation case
iksy <- mvrnorm(500, rep(0, 2), matrix(c(1, 0.95, 0.95, 1), byrow = T, nrow = 2))
y = rowMeans(iksy)

m <- data.frame(x1 = iksy[, 1], x2 = iksy[, 2], y = as.vector(y))

summary(lm(y~., data = m))
rf <- ksvm(y ~., data = m)

oto <- sample_locally(m, m[1, ], "y", 1000)
oto1 <- add_predictions(m, oto, rf, predict_fun = function(m, x, ...) predict(m, x, ...)[, 1])
f <- fit_explanation(oto1, "regr.lm")

live::plot_explanation(f, "waterfallplot", m[1, ])
plot(broken(rf, m[1, ], m[, -3], direction = "down"),
     predict.function = function(m, x) predict(m, x)[, 1])
# lm(y ~., data = dd)

load("winequality_red.rda")
mlm <- lm(quality ~., data = winequality_red)
summary(mlm)
mean(predict(mlm))
brk <- broken(mlm, winequality_red[1, ], winequality_red[, -12], direction = "down")
brk
library(mlr)
tsk <- makeRegrTask("wine", winequality_red, "quality")
devtools::install_github("redichh/shapleyR")
library(shapleyr)
shp <- shapley(1, model = train("regr.lm", tsk), task = tsk)
# plot.shapley.singleValue(1, model = train("regr.lm", tsk), task = tsk)
library(tidyr)
library(stringr)
library(dplyr)
brk2 <- data.frame(var = brk$variable_name, brk$contribution)
shp %>%
  gather(var, score) %>%
  full_join(brk2, by = "var")
# Same for more complicated model.
rf <- randomForest(quality ~., data = winequality_red)
brkr <- broken(rf, winequality_red[1, ], winequality_red[, -12], direction = "down")
shp2 <- shapley(1, model = train("regr.randomForest", tsk), task = tsk)
brkr2 <- data.frame(var = brkr$variable_name, brkr$contribution)
shp2 %>%
  gather(var, score) %>%
  full_join(brkr2, by = "var")
# Simulation for linear case
n <- 500
diffs <- vector("numeric", n)
library(MASS)
for(i in 1:n) {
  dt <- mvrnorm(500, rep(0, 15), diag(1, 15)) %>%
    as.data.frame() %>%
    mutate(y = rowMeans(.))
  mlm <- lm(y ~., data = dt)
  brk <- broken(mlm, dt[1, ], dt[, -16], direction = "down")
  tsk <- makeRegrTask("tsk", dt, "y")
  shp <- shapley(1, model = train("regr.lm", tsk), task = tsk)
  brk2 <- data.frame(var = brk$variable_name, cont = brk$contribution)
  diffs[i] <- shp %>%
    gather(var, score) %>%
    full_join(brk2, by = "var") %>%
    filter(grepl("V", var)) %>%
    mutate(diff = (score - cont)^2) %>%
    summarise(s = sum(diff)) %>%
    unlist(use.names = F)
}
hist(diffs)
mean(diff)

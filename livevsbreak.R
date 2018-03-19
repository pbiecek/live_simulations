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

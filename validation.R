# LIVE in Break Down validation
library(live)
library(breakDown)

data(wine)
head(wine)

library(randomForest)
rfw <- randomForest(quality ~., data = wine)
?broken.default
dtl <- broken(rfw, wine[5, ], wine, baseline = "Intercept")
dtl2 <- broken(rfw, wine[5, ], wine)
p <- plot(dtl)
p$data
p2 <- plot(dtl2)
p2$data

conts1 <- dtl$contribution[-c(1, length(dtl$contribution))]
conts2 <- dtl2$contribution[-c(1, length(dtl2$contribution))]

sum(conts1)
sum(conts2)

predict(rfw, wine[5, ]) - mean(predict(rfw))
predict(rfw, wine[5, ]) - p2$data
names(p2)
p2$layers[[3]]
names(p2$data)

similars <- sample_locally2(wine, wine[5, ], "quality", 50)

similar_obs <- add_predictions2(similars, rfw)$data

macierz <- as.matrix(similar_obs)

jako_lista <- as.list(as.data.frame(t(as.matrix(similar_obs))))

podzialy <- lapply(jako_lista, function(x) broken(rfw, wine[5, ], wine)$contribution[-c(1,14)])

sr <- mean(predict(rfw, wine))
podzialy_przeskalowane <- lapply(podzialy, function(x) x/(sum(x)))
nm <- as.character(dtl$variable_name[-c(1,14)])
podzialy_przeskalone <- lapply(podzialy_przeskalowane, function(x) {
  tmp <- as.data.frame(x)
  colnames(tmp) <- nm
  tmp
})
podzialy[[1]]

xd <- function (model, new_observation, data, predict.function = predict)
{
  new_data <- new_observation[rep(1L, nrow(data)), ]
  baseline_yhat <- mean(predict.function(model, data))
  baseline_yhat
}
xd(rfw, wine[5, ], wine)
mean(predict(rfw, wine))
mean(predict(rfw))
mean(predict(rfw, wine[-5, ]))


added_preds <- add_predictions2(similars, rfw)
fitted_lm <- fit_explanation2(added_preds)
plot(fitted_lm, type = "waterfall")

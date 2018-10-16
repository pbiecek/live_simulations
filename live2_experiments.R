library(DALEX)
library(randomForest)
library(ceterisParibus)

data('HR')

mrf <- randomForest(status ~., data = HR, ntree = 50)

rf_expl <- DALEX::explain(mrf, HR[, -6], y = HR[, 6])

predict_rf_prob <- function(x, y) {
  predict(x, y, type = "prob")
}

new_observation <- data.frame(gender = factor("male", levels = c("male", "female")),
                              age = 57.7,
                              hours = 42.3,
                              evaluation = 2,
                              salary = 2)

ecp <- ceterisParibus::ceteris_paribus(
  rf_expl,
  new_observation
)
# class(ecp) <- 'data.frame'
# nrow(ecp)
# head(ecp)

table(HR$evaluation, HR$status)


proba_live2 <- local_approximation(rf_expl, new_observation,
                                   size = 1000, predict_function = predict_rf_prob)
plot(proba_live2)


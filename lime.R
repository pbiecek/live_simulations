library(lime)
library(tidyverse)
library(breakDown)
library(live)
library(mlr)
library(randomForest)
data("winequality_red")


library(randomForest)
similar <- sample_locally(winequality_red, winequality_red[5, ],
                          "quality", 100, TRUE)
similar <- add_predictions(winequality_red, similar, "regr.randomForest")

to_explain_lime <- lime(winequality_red[, -12], similar$model)

model_type.randomForest <- function(x, ...) "regression"
explanation <- lime::explain(winequality_red[5, -12],
                       to_explain_lime,
                       n_features = 5)
